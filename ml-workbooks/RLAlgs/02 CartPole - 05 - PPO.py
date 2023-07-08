# from https://gymnasium.farama.org/tutorials/training_agents/reinforce_invpend_gym_v26/
import gymnasium as gym
import torch
import torch.nn as nn
import utils as u
import math

#env_pe1 = gym.wrappers.TimeLimit(pe1.ProbeEnv01())
device = torch.device('cpu')


class CartPoleAgent(nn.Module):
    def __init__(self, env):
        super(CartPoleAgent, self).__init__()
        input_dim = env.observation_space.shape[0]
        out_dim = env.action_space.n # Discrete Action Space
        self.name = '64x64'
        self.pi = u.mlp([input_dim, 64, 64, out_dim], nn.ReLU, lambda: nn.Softmax(dim=-1))
        self.v = u.mlp([input_dim, 64, 64, 1], nn.ReLU)

    def action_prob(self, state):
        probs = self.pi(torch.tensor(state).to(device))
        action = torch.distributions.Categorical(probs).sample().item()
        return action, probs

    def action(self, state):
        probs = self.pi(torch.tensor(state).to(device))
        action  = probs.max(-1, keepdims = True).indices.item()
        return action

    def value(self, state):
        value = self.v(torch.tensor(state).to(device))
        return value

def ppo(env, steps=100_000, batch_size=32, exp_size=256, entropy_beta=0.005):
    agent = CartPoleAgent(env).to(device)
    gamma = 0.99
    gae_lambda = 1

    max_entropy = - math.log(1/env.action_space.n)
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
    o, _ = env.reset()
    t = 0

    def on_episode_end_reset_logs(log=True):
        nonlocal episode, v_stats, t
        episode += 1
        if log:
            logger.log('Return/Train', t, step)
            #print('Ep: ', episode, ' => ', t, env._elapsed_steps)
            if t != env._elapsed_steps:
                print(t, 'steps', env._elapsed_steps)
                raise "Error!"
            v_stats.log(logger, step)

        t = 0
        v_stats.reset()

    def on_episode_step():
        nonlocal t
        t+=1

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
            a, p = agent.action_prob(o)
            o2, r, terminated, truncated, _ = env.step(a)
            on_episode_step()


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
            exp_buffer.append([o, a, p.detach(), advantage.detach(), ret, v_o])

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
        o, a, p, advantage, ret, v_o = exp
        anew, pnew = agent.action_prob(o)
        r = pnew[a] / (p[a].item() + 1e-12) # p must be detached
        logger.log('Advantage', advantage, step)
        logger.log('pi ratio', r, step)
        loss = - min(r * advantage, torch.clip(r, 1 - epsilon, 1 + epsilon) * advantage)
        #print(a, advantage, loss.item()
        logger.log('Loss/CLIP', loss, step)

        if entropy_beta != 0:
            entropy =  - (pnew * torch.log(pnew + 1e-10)).sum()
            entropy_loss = - entropy_beta * entropy
            logger.log('Entropy', entropy.item() / max_entropy, step, False)
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
            avg_steps = u.evaluate_agent(env, agent, 100)
            o, _ = env.reset()
            logger.log('Return/Test', avg_steps, step)
            print('Avg. Test Steps:', avg_steps)

    avg_reward = step / episode
    torch.save(agent.state_dict(), experiment_name + '/model')
    return agent

if __name__ == '__main__':
    import sys
    print(sys.argv)
    steps = int(sys.argv[1])
    batch_size = int(sys.argv[2])
    exp_size = int(sys.argv[3])
    entropy_beta = float(sys.argv[4])
    env =  gym.make('CartPole-v1')
    ppo(env, steps, batch_size, exp_size, entropy_beta)
