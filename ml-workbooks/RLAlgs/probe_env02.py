import gymnasium as gym

class ProbeEnv02(gym.Env):
    action_space = gym.spaces.Discrete(1)
    observation_space = gym.spaces.Box(0,2)

    def __init__(self):
        self.state = 0.0

    def step(self, action):
        self.state+=1.0
        reward = 1
        if (self.state >= 2.0):
            terminated = True
        else:
            terminated = False

        truncated = False
        info = dict()
        return [self.state], reward, terminated, truncated, info

    def reset(self):
        self.state = 0.0
        info = dict()
        return [self.state], info

    def check_agent(agent):
        v = agent.value([1.0]).item()
        assert(abs(v - 1.00) < 0.0001)
        print("v[1.0] == 1.0", "OK", v)

        v = agent.value([0.0]).item()
        assert(abs(v - 1.99) < 0.0001)
        print("v[1.0] == 1.99", "OK", v)
