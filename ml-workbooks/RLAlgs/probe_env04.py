import gymnasium as gym

class ProbeEnv04(gym.Env):
    action_space = gym.spaces.Box(-1.0, 1.0, (1,))
    observation_space = gym.spaces.Box(0,2)
    spec = gym.envs.registration.EnvSpec(id='Probe04')
    def __init__(self):
        self.state = 0.0

    def step(self, action):
        self.state = 1.0
        action = action.item()
        if action > 0:
            reward = min(1.0, (action - 0.7)/0.3)
        else:
            reward = -1

        terminated = True
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
