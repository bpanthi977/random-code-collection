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

def reinforce(steps=100_000):
    env = gym.make("CartPole-v1")
    agent = CartPoleAgent(env).to(device)
    gamma = 0.99
    ENTROPY_BETA = 0.5

    pi_optimizer = torch.optim.Adam(agent.pi.parameters())
    v_optimizer = torch.optim.Adam(agent.v.parameters())
    logger = u.Logger('runs/REINFORCE/Baseline')

    def sample_episode():
        o, info = env.reset()
        terminated = False
        oagp = []

        def rec(o):
            nonlocal oagp
            a, probs = agent.action_prob(o)
            o, r, terminated, truncated, info = env.step(a)
            if terminated or truncated:
                g = r
            else:
                g = r + gamma * rec(o)

            oagp = [(o, a, g, probs)] + oagp
            return g

        rec(o)
        return oagp

    def pi_v_loss(data):
        loss_pi = 0
        loss_v = 0
        entropy = 0
        loss_entropy = 0

        for (t , (o, a, g, p)) in enumerate(data):
            v = agent.value(o)
            δ = (g - v).detach() # δ
            # ∇L_pi = - γ^t δ ∇ln \pi(a|s)
            loss_pi += - (gamma ** t) * δ * torch.log(p)[a]
            # ∇L_v = - δ * ∇v(s)
            # L_v = 1/2 * (g - v)^2
            loss_v += 1/2 * (g - v)**2
            ## L = entropy penalty
            with torch.no_grad():
                entropy += - (p * torch.log(p)).sum() # H = - \sum [p ln(p)]
                loss_pi += - ENTROPY_BETA * entropy          # increase entropy

        loss_pi = loss_pi /len(data)
        loss_v = loss_v / len(data)
        entropy = entropy/len(data)
        print('Loss pi:', loss_pi.item(), 'Entropy:', entropy.item(), 'Loss v:', loss_v.item())
        logger.log('Loss pi', loss_pi.item(), episode)
        logger.log('Loss v', loss_v.item(), episode)
        logger.log('Entropy', entropy.item(), episode)

        return loss_pi, loss_v

    episode = 0
    step = 0
    while step < steps:
        oagp = sample_episode()

        step += len(oagp)
        episode += 1
        logger.log('Return/Episode', len(oagp), episode)
        if logger.log_progress(step, steps):
            avg_steps = u.evaluate_agent(env, agent, 100)
            logger.log('Test Return/Episode', avg_steps, episode)
            print('Avg. Test Steps:', avg_steps)

        pi_optimizer.zero_grad()
        v_optimizer.zero_grad()
        loss_pi, loss_v = pi_v_loss(oagp)
        loss_pi.backward()
        loss_v.backward()
        pi_optimizer.step()
        v_optimizer.step()

    avg_reward = step / episode

if __name__ == '__main__':
    reinforce(100_000)
