import gymnasium as gym

class ProbeEnv01(gym.Env):
    action_space = gym.spaces.Discrete(1)
    observation_space = gym.spaces.Box(0,1)

    def step(self, action):
        obs = 1.0 # second state
        reward = 1
        terminated = True
        truncated = False
        info = dict()
        return [obs], reward, terminated, truncated, info

    def reset(self):
        obs = 0.0
        info = dict()
        return [obs], info

    def check_agent(agent):
        v = agent.value([0.0]).item()
        assert(abs(v - 1.00) < 0.0001)
        print("v[0.0] == 1.0", "OK")
