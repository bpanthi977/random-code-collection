# from https://gymnasium.farama.org/tutorials/training_agents/reinforce_invpend_gym_v26/
import gymnasium as gym
import torch
import torch.nn as nn
import utils as u

device = torch.device('mps')

class CartPoleAgent(nn.Module):
    def __init__(self, env):
        super(CartPoleAgent, self).__init__()
        input_dim = env.observation_space.shape[0]
        out_dim = env.action_space.n # Discrete Action Space
        self.pi = u.mlp([input_dim, 128, out_dim], nn.ReLU, lambda: nn.Softmax(dim=-1))
        self.v = u.mlp([input_dim, 128, 1], nn.ReLU)

    def action_prob(self, state):
        probs = self.pi(torch.tensor(state).to(device))
        action = torch.distributions.Categorical(probs).sample().item()
        return action, probs

    def action(self, state):
        action, _ = self.action_prob(state)
        return action

    def value(self, state):
        value = self.v(torch.tensor(state).to(device))
        return value

def actor_critic(steps=100_000):
    env = gym.make("CartPole-v1")
    agent = CartPoleAgent(env).to(device)
    gamma = 0.99
    batch_size = 64

    pi_optimizer = torch.optim.Adam(agent.pi.parameters())
    v_optimizer = torch.optim.Adam(agent.v.parameters())
    import time
    secs = time.clock_gettime_ns(0) // 10**6
    logger = u.Logger(dir='runs/ActorCritic/Batch Size' + str(batch_size)+ '/' + str(secs))

    episode = 0
    step = 0

    o, _ = env.reset()
    t = 0
    loss_pi = 0
    loss_v = 0
    v_stats = u.Stats('Value Est.')
    entropy = 0
    for step in range(steps):
        # Take action
        a, p = agent.action_prob(o)
        o2, r, terminated, truncated, _ = env.step(a)

        # Compute Loss
        g = r if terminated else r + gamma * agent.value(o2)
        v = agent.value(o)
        δ = (g - v).detach() # approx. TD error as critic (or say approx. to approx. Advantage function)

        loss_pi += - (gamma**t) * δ * torch.log(p)[a]
        with torch.no_grad():
            entropy += - (p * torch.log(p)).sum()
        loss_v += 1/2 * (g - v)**2

        t += 1
        v_stats.add(v.item())
        if terminated or truncated:
            episode += 1
            logger.log('Return/Train', t, episode)
            v_stats.log(logger, episode)
            o, _ = env.reset()
            t = 0
            v_stats.reset()

        else:
            o = o2


        # BATCH Optimization
        if (step + 1) % 32 == 0:
            pi_optimizer.zero_grad()
            v_optimizer.zero_grad()
            loss_pi = loss_pi / batch_size
            loss_v = loss_v / batch_size
            entropy = entropy / batch_size

            loss_pi.backward()
            loss_v.backward()

            pi_optimizer.step()
            v_optimizer.step()

            logger.log('Loss/pi', loss_pi.item() , episode)
            logger.log('Loss/v', loss_v.item(), episode)
            logger.log('Entropy', entropy.item(), episode)
            print('Loss pi:', loss_pi.item(), 'Entropy:', entropy.item(), 'Loss v:', loss_v.item())
            loss_pi, loss_v  = 0, 0

        # Test
        if logger.log_progress(step, steps):
            avg_steps = u.evaluate_agent(env, agent, 100)
            logger.log('Return/Test', avg_steps, episode)
            print('Avg. Test Steps:', avg_steps)

    avg_reward = step / episode

if __name__ == '__main__':
    actor_critic(100_000)
