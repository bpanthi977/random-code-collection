# from https://gymnasium.farama.org/tutorials/training_agents/reinforce_invpend_gym_v26/
import gymnasium as gym
import torch
import torch.nn as nn
import utils as u

device = torch.device('cpu')

class CartPoleAgent(nn.Module):
    def __init__(self, env):
        super(CartPoleAgent, self).__init__()
        input_dim = env.observation_space.shape[0]
        out_dim = env.action_space.n # Discrete Action Space
        self.pi = u.mlp([input_dim, 8, out_dim], nn.ReLU, lambda: nn.Softmax(dim=-1))
        self.v = u.mlp([input_dim, 8, 1], nn.ReLU)

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

def actor_critic(env, steps=100_000, n_steps=32, entropy_beta=0.005, lrv=4e-3):
    agent = CartPoleAgent(env).to(device)
    gamma = 0.99
    gae_lambda = 1

    pi_optimizer = torch.optim.Adam(agent.pi.parameters())
    v_optimizer = torch.optim.Adam(agent.v.parameters(), lr=lrv)
    import time
    secs = time.clock_gettime_ns(0) // 10**6
    experiment_name  = 'runs/ActorCritic/8/n' + str(n_steps) + '/beta' + str(entropy_beta) + '/lr' + str(lrv) + '/' + str(secs)
    logger = u.Logger(dir=experiment_name)


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

    def compute_loss(prev_obs):
        end_info = False

        loss_pi = 0
        loss_v = 0

        def adv(o, v_o, n):
            nonlocal  loss_pi, loss_v, end_info
            v_stats.add(v_o.item())

            # Take action
            a, p = agent.action_prob(o)
            o2, r, terminated, truncated, _ = env.step(a)
            on_episode_step()

            # Compute Advantage
            if terminated or truncated:
                on_episode_end_reset_logs()
                o2, _ = env.reset()

            v_o2 = agent.value(o2)

            delta = r - v_o if terminated else r + gamma * v_o2 - v_o

            if terminated or truncated or n == n_steps:
                advantage = delta
                end_info = (o2, n)
            else:
                advantage = delta + gamma * gae_lambda * adv(o2, v_o2, n+1)

            #print(episode, o, v_o.item(), o2, v_o2.item(), r, delta.item(), advantage.item())

            adv_stats.add(advantage.item())
            # Loss
            log_action_prob = torch.log(p)[a]
            loss_pi += - advantage.item() * log_action_prob
            loss_v += 1/2 * ((advantage + v_o).item() - v_o)**2 # equivalent to TD(\lambda) error
            #print("{}: {:.2f} {:.2f}".format(o, (advantage + v_o).item(), v_o.item()))
            entropy =  - (p * torch.log(p)).sum()
            loss_pi += - entropy_beta * entropy

            logger.log('Entropy', entropy.item() / 0.6931, step)

            return advantage

        adv(prev_obs, agent.value(prev_obs), 1)

        return loss_pi, loss_v, end_info


    while step < steps:
        loss_pi, loss_v, (o, n) = compute_loss(o)
        step += n


        pi_optimizer.zero_grad()
        v_optimizer.zero_grad()

        loss_pi = loss_pi / n
        loss_v = loss_v / n

        loss_pi.backward()
        loss_v.backward()
        pi_optimizer.step()
        v_optimizer.step()

        logger.log('Loss/pi', loss_pi.item() , step)
        logger.log('Loss/v', loss_v.item(), step)
        #print('Loss pi:', loss_pi.item(), 'Entropy:', entropy.item(), 'Loss v:', loss_v.item())
        #print(episode, 'episode_steps', t, 'n', n)
        loss_pi, loss_v  = 0, 0

        # Test
        if logger.log_progress(step, steps):
            on_episode_end_reset_logs(log=False)
            avg_steps = u.evaluate_agent(env, agent, 100)
            o, _ = env.reset()
            logger.log('Return/Test', avg_steps, step)
            print('Avg. Test Steps:', avg_steps)

    avg_reward = step / episode
    return agent

if __name__ == '__main__':
    import sys
    print(sys.argv)
    steps = int(sys.argv[1])
    n_steps = int(sys.argv[2])
    entropy_beta = float(sys.argv[3])
    lrv = float(sys.argv[4])
    env =  gym.make('CartPole-v1')
    actor_critic(env, steps, n_steps, entropy_beta, lrv)
