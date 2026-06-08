# Step 1b: neutral prompt + Sonnet GEN_MODEL (second model != Haiku) — does husk-saturation persist?
import os, sys
sys.path.insert(0, "/home/scott/bin/structural_dynamics_model")  # repo root for `import agent`
os.environ["DR_GEN_PROMPT"] = "prompts/constraint_story_generation_prompt_DRIFTNEUTRAL.md"
import agent.generate_kernel_corpus as g
g.GEN_MODEL = "claude-sonnet-4-5-20250929"   # second model (Haiku was the saturating one)
sys.argv = ["gkc", "--scope", "--run-tag", "pilot_s1b_sonnet",
            "--seeds", "prolog/kernel_seeds.json", "--limit", "8", "--skip-search"]
g.main()
