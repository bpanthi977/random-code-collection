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
        self.net = u.mlp([input_dim, 128, out_dim], nn.ReLU, lambda: nn.Softmax(dim=-1))

    def action_prob(self, state):
        probs = self.net(torch.tensor(state).to(device))
        action = torch.distributions.Categorical(probs).sample().item()
        return action, probs

    def action(self, state):
        action, _ = self.action_prob(state)
        return action

def reinforce(steps=100_000):
    env = gym.make("CartPole-v1")
    pi = CartPoleAgent(env).to(device)
    gamma = 0.99

    optim = torch.optim.Adam(pi.parameters())
    logger = u.Logger('runs/REINFORCE/No Baseline')


    def sample_episode():
        o, info = env.reset()
        terminated = False
        oagp = []

        def rec(o):
            nonlocal oagp
            a, probs = pi.action_prob(o)
            o, r, terminated, truncated, info = env.step(a)
            if terminated or truncated:
                g = r
            else:
                g = r + gamma * rec(o)

            oagp = [(o, a, g, probs)] + oagp
            return g

        rec(o)
        return oagp

    def pi_loss(oagp):
        loss = 0
        entropy = 0
        for (t , (o, a, g, p)) in enumerate(oagp):
            # L = - \gamma^t G ln \pi(a)
            loss += - (gamma ** t) * g * torch.log(p)[a]
            ## L = entropy penalty
            with torch.no_grad():
                entropy += - (p * torch.log(p)).sum()         # H = - \sum p_i log p_i
                #loss += - ENTROPY_BETA * entropy          # increase entropy

        loss = loss/len(oagp)
        entropy = entropy/len(oagp)

        print('Loss:', loss.item(), 'Entropy:', entropy.item())
        logger.log('Loss', loss.item(), episode)
        logger.log('Entropy', entropy.item(), episode)
        return loss

    episode = 0
    step = 0
    while step < steps:
        oagp = sample_episode()

        step += len(oagp)
        episode += 1
        logger.log('Return/Episode', len(oagp), episode)
        if logger.log_progress(step, steps):
            avg_steps = u.evaluate_agent(env, pi, 100)
            logger.log('Test Return/Episode', avg_steps, episode)
            print('Avg. Test Steps:', avg_steps)

        optim.zero_grad()
        loss = pi_loss(oagp)
        loss.backward()
        optim.step()

    avg_reward = step / episode

if __name__ == '__main__':
    reinforce(100_000)
