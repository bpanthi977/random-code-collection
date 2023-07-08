from torch.utils.tensorboard import SummaryWriter
import torch.nn as nn

def mlp(sizes, activation, output_activation=nn.Identity):
    layers = []
    for j in range(len(sizes)-1):
        act = activation if j < len(sizes)-2 else output_activation
        layers += [nn.Linear(sizes[j], sizes[j+1]), act()]
    return nn.Sequential(*layers)


def evaluate_agent(env, agent, episodes=100, gamma=1):
    observation, info = env.reset()
    steps = 0
    episode = 0
    total_reward = 0
    discount = 1
    while episode < episodes:
        action = agent.action(observation)
        observation, reward, terminated, truncated, info = env.step(action)
        steps += 1
        total_reward += discount * reward
        discount *= gamma
        if terminated or truncated:
            observation, info = env.reset()
            episode += 1
            discount = 1

    return total_reward / episodes, steps/episodes

class RandomAgent():
    def __init__(self, env):
        self.env = env

    def action(self, state):
        return self.env.action_space.sample()


class Logger():
    def __init__(self, dir = 'runs'):
        self.writer = SummaryWriter(dir)
        self.last_progress = 0

    def log(self, title, y, x, pprint=False):
        self.writer.add_scalar(title, y, x)
        if pprint:
            print(x, title)


    def log_progress(self, n, total):
        if (n / total) >= self.last_progress + 0.1:
            print('Progress: %3.2f'%(n/total * 100))
            self.last_progress = n / total
            return True

        return False

class Stats():
    def __init__(self, name):
        self.name = name
        self.total = False
        self.max = False
        self.min = False
        self.count = 0

    def add(self, val):
        if self.count == 0:
            self.total = val
            self.max = val
            self.min = val
        else:
            self.total += val
            self.max = max(self.max, val)
            self.min = min(self.min, val)

        self.count+=1

    def log(self, logger: Logger, x):
        name = self.name
        logger.log(name + '/avg', (self.total/self.count), x)
        logger.log(name + '/max', self.max, x)
        logger.log(name + '/min', self.min, x)

    def reset(self):
        self.count = 0


class EWMA():
    def __init__(self, alpha=1/50):
        self.alpha = alpha
        self.value = False

    def add(self, val):
        if not self.value:
            self.value = val
        else:
            self.value = self.value * (1 - self.alpha) + self.alpha * val

        return self.value
