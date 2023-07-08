import gymnasium as gym
import torch
import torch.nn as nn
import utils as u
import math
import gymnasium.envs.mujoco.half_cheetah_v4
#env_pe1 = gym.wrappers.TimeLimit(pe1.ProbeEnv01())
device = torch.device('cpu')


class HalfCheetahAgent(nn.Module):
    def __init__(self, env):
        super(HalfCheetahAgent, self).__init__()
        input_dim = env.observation_space.shape[0]
        self.action_dim = env.action_space.shape[0]
        self.env = env
        out_dim = self.action_dim * 2 # for mean and std
        self.name = '64x64'
        self.pi = u.mlp([input_dim, 64, 64, out_dim], nn.ReLU, nn.ReLU)
        self.v = u.mlp([input_dim, 64, 64, 1], nn.ReLU)


    def _pi(self, state):
        alpha, beta = self.pi(torch.tensor(state, dtype=torch.float).to(device)).reshape((2, self.action_dim))
        return nn.functional.relu(alpha) + 1e-6, nn.functional.relu(beta) + 1e-6

    def action_probdensity(self, state, action):
        alpha, beta = self._pi(state)
        dist = torch.distributions.Beta(alpha, beta)
        action = self.inv_scale_action(action)
        print(action, alpha, beta)
        log_probability_density = dist.log_prob(torch.tensor(action, dtype=torch.float)).sum(0)
        return log_probability_density, dist.entropy().sum(-1)

    def scale_action(self, action):
        # action is [0,1]
        # scale it to action_space.[low, high]
        range = self.env.action_space.high - self.env.action_space.low
        return self.env.action_space.low + action * range

    def inv_scale_action(self, action):
        range = self.env.action_space.high - self.env.action_space.low
        return (action - self.env.action_space.low) / range

    def action_prob(self, state):
        alpha, beta = self._pi(state)
        dist = torch.distributions.Beta(alpha, beta)
        action = dist.sample()
        log_probability_density = dist.log_prob(action).sum(0)
        action = self.scale_action(action.detach().numpy())
        return action, log_probability_density

    def action(self, state):
        with torch.no_grad():
            alpha, beta = self._pi(state)
            mean  = alpha / (alpha + beta)
        return self.scale_action(mean)

    def value(self, state):
        value = self.v(torch.tensor(state, dtype=torch.float).to(device))
        return value

trained_agent = False
def ppo(env, steps=100_000, batch_size=32, exp_size=256, entropy_beta=0.005, gamma=0.99, gae_lambda = 1):
    global trained_agent, device
    agent = HalfCheetahAgent(env).to(device)

    pi_optimizer = torch.optim.Adam(agent.pi.parameters())
    v_optimizer = torch.optim.Adam(agent.v.parameters())

    import time
    secs = time.clock_gettime_ns(0) // 10**6
    experiment_name  = 'runs/' + env.spec.name + '/PPO/'+ agent.name + '/t' + str(exp_size) + '/n' + str(batch_size) + '/ent' + str(entropy_beta) + '/' + str(secs)

    logger = u.Logger(dir=experiment_name)

    epsilon = 0.1

    episode = 0
    step = 0
    v_stats = u.Stats('Value Est.')
    adv_stats = u.Stats('Advantage Est.')
    print(env.reset())
    o, _ = env.reset()
    t = 0
    total_reward = 0
    total_discount = 1

    def on_episode_end_reset_logs(log=True):
        nonlocal episode, v_stats, t, total_reward, total_discount
        episode += 1
        if log:
            logger.log('Return/Train', total_reward, step)
            #print('Ep: ', episode, ' => ', t, env._elapsed_steps)
            if t != env._elapsed_steps:
                print(t, 'steps', env._elapsed_steps)
                raise "Error!"
            v_stats.log(logger, step)

        t = 0
        total_reward = 0
        total_discount = 1
        v_stats.reset()

    def on_episode_step(reward):
        nonlocal t, total_discount, total_reward
        t+=1
        total_reward += total_discount * reward
        total_discount *= gamma

    def collect_exp(prev_obs, steps):
        end_info = False
        reset = False
        loss_pi = 0
        loss_v = 0

        exp_buffer = []

        def adv(o, v_o, n):
            nonlocal  loss_pi, loss_v, end_info, reset, _step, steps
            v_stats.add(v_o.item())

            # Take action
            a, log_p = agent.action_prob(o)
            o2, r, terminated, truncated, _ = env.step(a)
            on_episode_step(r)


            if terminated or truncated:
                on_episode_end_reset_logs()
                reset = True
                o2, _ = env.reset()

            # Compute Advantage
            v_o2 = agent.value(o2)

            delta = r - v_o if terminated else r + gamma * v_o2 - v_o

            if terminated or truncated or _step + n >= steps:
                advantage = delta
                end_info = (o2, n)
            else:
                advantage = delta + gamma * gae_lambda * adv(o2, v_o2, n+1)

            adv_stats.add(advantage.item())
            # Loss
            ret = (advantage + v_o).item()
            exp_buffer.append([o, a, log_p.detach(), advantage.detach(), ret, v_o])

            return advantage

        _step = 0
        while _step < steps:
            end_info = False
            adv(prev_obs, agent.value(prev_obs), 1)
            if reset:
                adv_stats.log(logger, step)
                adv_stats.reset()
                reset = False

            prev_obs, n = end_info

            _step += n

        return exp_buffer, _step

    def pi_loss(exp):
        o, a, log_p, advantage, ret, v_o = exp
        log_pnew, entropy = agent.action_probdensity(o, a)
        r = (log_pnew - log_p).exp()
        logger.log('Advantage', advantage, step)
        logger.log('pi ratio', r, step)
        loss = - min(r * advantage, torch.clip(r, 1 - epsilon, 1 + epsilon) * advantage)
        #print(a, advantage, loss.item()
        logger.log('Loss/CLIP', loss, step)

        if entropy_beta != 0:
            entropy_loss = - entropy_beta * entropy
            logger.log('Entropy', entropy.item(), step, False)
        else:
            entropy_loss = 0
        return loss + entropy_loss


    def v_loss(exp):
        o, a, p, advantage, ret, v_o = exp
        v_o = agent.value(o)
        return 1/2 * (ret - v_o)**2

    while step < steps:
        exp, n = collect_exp(o, exp_size)

        count = 0
        while count < n:
            batch = exp[:batch_size]
            exp = exp[batch_size:]

            if len(batch) == 0:
                break

            pi_optimizer.zero_grad()
            v_optimizer.zero_grad()
            loss_pi, loss_v = 0, 0
            for e in batch:
                step+=1
                loss_pi += pi_loss(e)
                loss_v += v_loss(e)

            loss_pi /= len(batch)
            loss_v /= len(batch)
            (loss_pi).backward()
            (loss_v).backward()
            pi_optimizer.step()
            v_optimizer.step()

            logger.log('Loss/pi', loss_pi.item() , step)
            logger.log('Loss/v', loss_v.item(), step)

        #print(agent.action_prob([0.0]))
        # Test
        if logger.log_progress(step, steps):
            on_episode_end_reset_logs(log=False)
            reward, avg_steps = u.evaluate_agent(env, agent, 50)
            o, _ = env.reset()
            logger.log('Return/Test', reward, step)
            print('Avg. Test Reward:', reward)

    avg_reward = step / episode
    torch.save(agent.state_dict(), experiment_name + '/model')
    trained_agent = agent
    return agent


def test_in_probe():
    env_probe = gym.wrappers.TimeLimit(ProbeEnv04(), 10)
    agent = ppo(env_probe, steps=10_000, entropy_beta=0)
    return agent

def test_in_pendulum():
    env_probe = gym.make('Pendulum-v1')
    agent = ppo(env_probe, batch_size=64, exp_size=1024, steps=100_000, entropy_beta=0, gamma=0.999, gae_lambda=0.95)
    return agent

def test_in_halfcheetah():
    env_probe = gym.make('HalfCheetah-v4')
    agent = ppo(env_probe, batch_size=32, exp_size=512, steps=100_000, entropy_beta=1, gamma=0.999, gae_lambda=0.95)
    return agent

def vizualize_agent(agent):
    env_viz =  gym.make('HalfCheetah-v4', render_mode='human')
    reward = u.evaluate_agent(env_viz, agent, 2)
    env_viz.close()
    return reward

def vizualize_pendulum_agent(agent):
    env_viz =  gym.make('Pendulum-v1', render_mode='human')
    reward = u.evaluate_agent(env_viz, agent, 10)
    env_viz.close()
    return reward

if __name__ == '__main__':
    import sys
    print(sys.argv)
    steps = int(sys.argv[1])
    batch_size = int(sys.argv[2])
    exp_size = int(sys.argv[3])
    entropy_beta = float(sys.argv[4])
    env =  gym.make('HalfCheetah-v4')
    ppo(env, steps, batch_size, exp_size, entropy_beta)
