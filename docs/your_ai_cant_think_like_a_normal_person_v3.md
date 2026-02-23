# Your AI Can't Think Like a Normal Person (And That Matters More Than You Think)

You've probably asked an AI to help you with a college essay. Maybe you said something like, "Write about gun control from the perspective of an average American family." Or maybe you were prepping for Model UN and asked it to argue like "an ordinary citizen of Brazil." You got output. It sounded convincing. You used it.

Here's the problem: the AI almost certainly didn't give you the perspective you asked for.

I recently ran an experiment testing what happens when you ask a large language model to adopt different social viewpoints — not just "be nice" or "be formal," but to actually see a problem the way different kinds of people would see it. The results were striking. The AI handled three viewpoints well: the person with no power at all, the person running the institution, and the detached analyst looking at the whole system from above. But when asked to think like someone in the middle — someone with some power but not a lot, some options but not unlimited ones, the ability to leave but at real cost — the model dropped that perspective 63% of the time — just didn't produce it. And when it did produce something, the output was incoherent: one test case generated five completely different analyses across thirteen attempts at the same middle-ground viewpoint, while the other perspectives were perfectly consistent every single time.

That middle perspective? That's most people. That's probably you.

Why? AI simulates clarity effortlessly, but not conflict. The powerless perspective has clear pathos. The institutional perspective has clear strategy. The analyst's perspective has clear logic. The moderate perspective requires holding unresolved tension between competing values, obligations, and incentives — and language models are optimized for coherence, not ambivalence. When forced into the ambiguous middle, the model doesn't produce a wrong answer. It often produces no answer at all, or *hallucinates* clarity where a real person would feel conflicted.

## What the experiment actually measured

I used a structured analytical framework to test perspective-taking across five different topics — everything from how academic peer review works to tax policy to the nature of knowledge itself. For each topic, I had the AI generate an analysis from four viewpoints: someone trapped in the system with no real power, someone with moderate power and real but limited options, someone running the institution, and a detached observer analyzing the structure. I did this 86 times, with two different ways of describing each viewpoint — one using plain language ("imagine you have no power to change this") and one using precise technical definitions.

The experiment measured three things that matter.

**First, whether the AI distorts facts depending on the viewpoint.** This is the most important test, and the AI passed cleanly. When evaluating how much a system extracts from the people in it — basically, how much it takes versus gives — the model produced statistically identical assessments regardless of which perspective it was supposed to be adopting. The formal statistical test showed no effect. For two of the five topics, the numbers were literally identical across all four viewpoints, both runs, zero variance. The AI isn't generating text that spins the facts to match a narrative. It recognizes that how extractive a system is doesn't change depending on who's looking at it. That's sophisticated. And it's arguably more impressive than the missing-middle finding, because many critics assume that role-playing necessarily warps an AI's structural assessments. Here, it doesn't.

**Second, whether the middle perspective even exists in the AI's outputs.** It mostly doesn't. Across all 86 generated analyses, the moderate viewpoint was absent 63% of the time. The AI actively replaced it with other positions — sometimes a collective-action frame, sometimes upgrading the person's agency beyond what was specified. This wasn't a problem with how the question was phrased. When I switched from plain-language descriptions to precise technical definitions of the moderate position, the success rate went from 40% to 50% — a difference too small to be meaningful given the sample size. The moderate viewpoint is a capacity limitation, not a communication failure. The model cannot reliably produce it regardless of how clearly you ask.

**Third, whether the classification system that detected all this is itself stable.** It is. Despite key metrics varying by up to 17 percentage points between identical runs (the same analysis run twice could produce noticeably different numbers), the overall analytical conclusions were remarkably consistent — above 79% stability across all topics, and hitting 100% for the extreme viewpoints. The instrument wasn't fooled by noise. The gap is real.

## Why this matters if you use AI for anything involving perspective

The moderate perspective is the one that requires holding ambiguity. You're not desperate, so the AI can't generate pathos. You're not in charge, so it can't generate strategy. You're not detached, so it can't generate clean analysis. You're somewhere in the messy middle where most actual human life happens, and the AI defaults away from that toward positions that generate clearer signal.

An important distinction: the AI isn't failing at empathy. It's failing at something more specific — generating text that statistically represents a perspective for which the training data is thin and structurally incoherent. Fiction, journalism, policy analysis, and academic writing overwhelmingly feature extreme or analytically clean viewpoints. The moderate position ("I have some power, some options, some but not total mobility") generates less distinctive content and appears less often as a named perspective in the text that trains these models. The ordinary person's ambivalence doesn't make for viral content or memorable literature, so it's statistically faint in the training corpus. The AI defaults to what's overrepresented.

That has practical consequences.

**College essays.** If you ask an AI to help you understand "what it's like to be a first-generation college student from a middle-class family," you're asking for the exact perspective it's worst at. You'll get either the hardship narrative (powerless frame) or the strategic calculation (institutional frame), not the genuine ambiguity of having enough privilege that your struggles feel illegitimate but not enough that the path is clear. The AI will write something that sounds like a perspective but is actually a composite of more extreme positions dressed in moderate clothing.

**Debate and research.** When you ask "how would an ordinary voter see this trade policy," the AI will give you the version that makes narrative sense — the harmed worker, the benefiting executive, or the economist's analysis. The voter who is mildly affected, somewhat informed, and genuinely uncertain? That's a perspective the model cannot reliably construct.

**Understanding other people.** This is the deepest issue. If you're using AI to understand viewpoints different from your own — which is one of the most genuinely valuable things it can do — you should know that the model has a systematic blind spot for the most common human experience: having some agency but not enough, facing constraints you could theoretically escape but at costs you can't easily bear.

## What we can and can't conclude

Here's where intellectual honesty requires some care.

**What the data shows directly:** One AI model (Gemini 2.5 Pro, at a specific temperature setting) drops the moderate perspective 63% of the time across five topics in four domains, and this is not fixed by clearer prompting. When it does produce the moderate perspective, the output is analytically unstable. Meanwhile, the factual underpinnings remain solid regardless of perspective — the model doesn't distort structural reality when role-playing.

**What we can reasonably infer:** The moderate perspective is underrepresented in AI training data. The model's generation space has stable, high-probability basins for the powerless, institutional, and analytical perspectives, and an unstable, low-probability region for the moderate position. When pushed into that region, it drifts toward the nearest stable attractor rather than sustaining the ambiguous middle.

**What we don't yet know:** Whether this is specific to one model, or general across all large language models. Whether it varies by domain — some topics may anchor the moderate position better than others. And crucially, whether a much more specific prompt can rescue the missing middle. There's a real difference between asking an AI to adopt "the moderate perspective" and telling it: "You are a dental hygienist making $68,000 in suburban Ohio with two kids and a mortgage you can afford but that locks you into your current city. Your oldest just started college and you're proud but terrified of the cost." That level of concrete detail might give the model enough distributional traction to avoid drifting to the poles. It might not. That experiment is next.

## What you can do about it

You don't need to stop using AI. But you should adjust how you use it when perspectives matter.

When you ask an AI for a moderate or everyday perspective, treat the output with extra skepticism. Check whether it actually sounds like someone in the middle, or whether it reads as an extreme position with the volume turned down. Here's how to spot the difference: a genuinely moderate perspective includes ambivalence, trade-offs that don't resolve cleanly, and the specific texture of having options that are all somewhat unsatisfying. If the AI gives you clarity where a real person would feel conflicted — if the narrative resolves too neatly, if the trade-offs feel too clean, if the emotion is too legible — it's probably giving you the wrong perspective in a moderate wrapper.

When you need multiple viewpoints, generate them separately and compare. The AI is excellent at the extremes and at detached analysis. Use those strengths. But notice what's missing from the set, and fill the gap yourself — because you, as a person who actually lives in the messy middle, understand that perspective better than the model does.

The finding isn't that AI is unreliable. The finding is that it has a specific, measurable blind spot in its theory of how people see the world — and the blind spot falls exactly where most people actually are.

---

*The formal experimental results, methodology, and data are published separately: [LLM Perspective Coherence Under Deferential Realism: Experimental Results](/llm-presheaf-diagnostic-results). The experiment used 86 story generations across five constraints, four observer perspectives, and two framings, with all classification performed by an unmodified Prolog diagnostic pipeline. Planned next: cross-model replication (Claude, GPT, Grok) and concrete anchoring (e.g., "dental hygienist in Ohio") to determine whether the missing middle is universal or rescuable.*
