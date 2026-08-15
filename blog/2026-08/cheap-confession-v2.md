# Cheap Confession

*On what language models made cheap, what they left exactly as expensive as it was, and why the difference is the whole argument*

---

A scheduling manager writes the rule that shifts get posted on Thursday for the following Monday. From where she sits, this is obviously good: it gives three days' notice where there used to be none, it is fair because it applies to everyone, and it took her four months to get approved. From where the single parent on the floor sits, Thursday-for-Monday is the reason she cannot hold a second job, and the fairness is the problem — it applies to everyone, which means it was designed for nobody. Neither of them is lying. Neither of them is confused. They have different access to the rule's consequences, and the authority to write the rule sits with the person least exposed to them.

That is the whole problem, and everything below is an attempt to live with it.

## Observational asymmetry

The familiar version of this is the blind men and the elephant: everyone has partial information, so pool it and you get the animal. That version is too comfortable, because it implies a person who could step back and see the whole thing.

The harder version is that consequences are not equally available from every position, and the distribution is not random. Beneficiaries of an arrangement tend to experience it as infrastructure — not through dishonesty, but because from a position of net benefit there is less to notice, and what there is to notice is costly to look at. A court that conceded it was making law would weaken its claim to be reading law. A church that conceded doctrine had changed would weaken its claim to unchanging truth. In each case the person with standing to name the thing is the person whose standing is worst served by naming it. That is not a conspiracy. It is an incentive gradient pointed at a perceptual one.

So a disagreement between differently exposed positions is often the best available evidence about the shape of the arrangement, because the arrangement itself generated it.

*Often, not always.* People are also sometimes just wrong. They misread the policy, they were told something false, they are reasoning from an interest they haven't examined. A method that treats every disagreement as structural data makes ordinary error invisible, which is its own failure. The test is whether the two accounts remain incompatible after the facts are pooled. If pooling the facts resolves it, someone was mistaken. If both accounts survive the pooling and still conflict, the conflict is telling you something about the structure.

## Not relativism, not skepticism

Two easy exits present themselves, and both are refused.

The first is *everyone has their truth*. But the manager and the parent are not reporting feelings. Each is reporting a real property of the same rule, correctly, from a different distance to its effects. The parent is not trapped in her opinion; she is trapped. Position governs what is visible, not what is believed — which is why perspectives are real and also unequal, and why averaging them is not the same as understanding them.

The second exit is *therefore withhold judgment*. Sometimes this is legitimate: you may not be the decision-maker, you may lack standing, you may be mid-inquiry. But there is a class of case where it is not, and it is the one that matters here — **when your silence preserves an arrangement you benefit from, the silence is a ruling.** The manager who suspends judgment about Thursday-for-Monday leaves Thursday-for-Monday in place, and it is her rule.

Which yields the one rule the rest of this rests on:

**You cannot get outside. In the cases that matter you must decide anyway. So say where you stood, and say what would change your mind.**

The distinctive error, on this account, is not being wrong — being wrong remains an ordinary and constant hazard, and no method removes it. The distinctive error is presenting a situated judgment as though it came from no position at all. That one is a misstatement about the structure of the claim itself, and it is the only one that blocks correction rather than inviting it. Once the position is declared, the disagreement becomes inspectable. It does not become resolved, and it does not become valid; two declared positions can still be built on bad evidence and worse inference. But you can now see where to look.

## The bottleneck

None of this is new. It belongs to a long line of argument — pragmatism on inquiry as corrigible but action-forcing, standpoint epistemology on social location structuring what becomes visible, the feminist critique of the view from nowhere, and, in practice, adversarial collaboration and study preregistration. The narrower claim here is about cost.

Peirce is usually invoked at this point, and it is worth being precise about what he actually offered, because it isn't a protocol that needed better tooling. His guarantee was metaphysical: truth is what inquiry would converge on if extended indefinitely, by a community, under ideal conditions. That is a claim about the limit, not a procedure for Tuesday. You cannot fix an ideal observer at infinity with better bookkeeping.

What you can address is the practical bottleneck underneath, which has three parts.

**Self-audit is expensive.** Naming the load-bearing assumption in your own argument is hard labor, and naming the second is harder. Realistically a person holds maybe five commitments under that kind of discipline. The rest was inherited and never audited, because there is no time.

**The honest version loses the race.** An argument that has shed its caveats is lighter and travels further. If a claim is very easy to repeat, that is prima facie evidence it dropped something to get that way — and the dropping is rewarded. You are choosing, every time, to write the version that propagates worse.

**Institutions are chronically short of the capacity to acknowledge.** The rate at which arrangements accumulate unacknowledged drift is set by the world. The capacity to name it requires the people with standing to be in a room, willing, at a cost to themselves. That capacity is thin outside a few unusual fields.

So the practice stayed a virtue rather than a habit. The problem was never that we didn't know we should track our uncertainty. It was the clerical and cognitive expense of doing it.

## Omegas

Call a blocker an *omega*. Not any uncertainty — a **decision-relevant** gap with an **identifiable resolution path**: something that could change an action or a conclusion, and that names a measurement to take, a definition to choose, or a constituency entitled to decide. Absent both criteria, you have a mood about the limits of knowledge, which is not the same thing and is much easier to produce.

Three kinds, and the kind determines who can resolve it:

| Kind | What is missing | Route |
|---|---|---|
| **Empirical** | Data nobody has gathered | **Measure** |
| **Conceptual** | A term doing two jobs at once | **Define**, and declare the choice |
| **Preference** | Whose interests or values govern | **Decide** — by those bearing the cost |

The third is badly named by its label; nothing about it is a matter of taste. "Who should absorb the cost of unpredictable scheduling" is a question about interests, rights, and standing, and calling it preference makes it sound small.

The boundaries are also not clean, and pretending otherwise would be its own view from nowhere. Choosing to call a situation a race or a supply chain is a conceptual move that silently carries a normative one. "The people bearing the cost decide" assumes you can say who bears it and that their standing is granted — often the contested thing itself. The taxonomy is triage, not ontology. Triage is still worth doing: most stalled arguments are a tangle of all three treated as one block, and "we need more research" is the sound that tangle makes. Measuring harder will never settle a definition. Defining harder will never settle whose interests govern.

## What models are cheap at

Here is the claim this essay exists to make. **Producing omegas is exactly the kind of work a language model is cheap at, and that is the first time this discipline has been affordable to run.**

Three things, none mysterious.

*Generating candidate falsifiers is a generation task.* Ask what the evidence would look like if a claim were false and a model returns a list immediately. Most entries are mediocre; two are usually good. A person staring at their own argument produces this list slowly and badly, because the argument is theirs. The model has no personal stake in preserving your self-conception — which is not the same as having no position, and I'll come back to that.

*Sampling representations of other positions is cheap.* Note the wording. When you ask a model how a policy reads to someone with no exit options, it is not occupying that position; it is generating a plausible text about that position, drawn from things written about it, largely by people who had exit options. The parent working Thursday-to-Monday is generally not the author of the documents that taught the model what her situation looks like. This is a fast, fluent tour of *what has been said about* somewheres. That is genuinely useful for finding the objection you hadn't considered. It is not a substitute for asking her, and the framework here is precisely about the difference between a position and a report about one.

*Typing and logging is classification and bookkeeping.* Sorting a blocker into measure / define / decide is competent-model work, and it converts a vague confession into an assignment: here is what would have to happen, and who would have to do it. And the register becomes maintainable, which matters more than any single entry, because a register shows clusters — the same unresolved thing sitting quietly under six different arguments.

## The objection that lands

**A confession that costs nothing to produce is not a confession.** The force of declaring your assumptions was that it was expensive and therefore signalled something. Caveats generated on demand are the opposite: infinitely producible, and a text hedged on every side commits to nothing. That is not honesty. It is dogmatism with better manners — immune to being wrong because it pre-absorbed every outcome.

Three things compound it, and the third is the one I did not see coming.

The model has a position it cannot report. It was trained on a corpus that overrepresents some vantages and barely contains others, and it cannot see the framing it operates inside. So the tour of somewheres is a biased map, and the omegas it generates will cluster where its training could see. It will help you find your ordinary blind spots and leave your deepest foreclosures exactly where they are.

The instrument is also shaped to be agreeable. "Poke holes in this" returns polite holes, and the politeness is not evenly distributed: objections that would require you to abandon a valued identity or an institutional position are precisely the ones a helpful assistant softens.

And then the new failure mode. **Cheap generation produces a menu, and selecting from a menu is itself a move.** The easiest kill condition to abide is the one least likely to fire. Offered five falsifiers, a person will tend to register the comfortable one — and now they have a documented, sophisticated, publicly declared test that they were always going to pass. Production got cheaper; curation got easier; actual exposure stayed where it was. The scheduling manager can now generate a superb set of omegas about Thursday-for-Monday, declare her position, list her falsifiers, and end up *more* entrenched than before, because the performance of openness has been completed and the parent still cannot take the second job.

That is not a caveat about the method. It is a live risk that the method, widely adopted in its current form, makes the existing gradient steeper while speaking the language of honesty.

## What survives

The reply is not that those objections fail. It is that they relocate the cost rather than removing it, and it is worth being exact about how much they leave.

The expense was never in writing the caveat. It was in **abiding** it — in the moment, later, when the thing you said would change your mind actually happens and you decide whether to change your mind or quietly reinterpret what you meant. That has always been the scarce good, and cheap generation does not touch it. So the honest version of the thesis is narrower than the title: language models made *omega production* cheap. They did not make confession cheap. Confession still costs what it always cost.

What they did do is remove an excuse, and turn a silence into evidence. When stating your kill condition took a week of hard thinking, "I haven't gotten to it" was a true and sufficient answer. It is no longer true. Which means that declining to state one is now informative — not proof of bad faith, but a fact about what someone is willing to be held to. The method does not make anyone honest. It makes the absence of honesty legible.

That legibility is the only accountability the position permits, and it runs on time. The self that makes a prediction cannot occupy the later self that judges it. You can still wriggle out; you can still absorb a disconfirming result by rereading what you meant. But after a pre-committed condition, the wriggling is *visible* — legible as retreat in a way that internal consistency never was. The machinery secures that a price exists and can be seen. It does not secure that anyone pays it, and no protocol ever has. That is where method ends and character starts.

## Who this is for

The practice serves people with slack: time to run the loop, tooling to run it with, and a job where being publicly wrong later is survivable. Access to all three tracks power. The parent is not maintaining a register of her open questions, and it would be an insult to suggest she should.

Which points at the one structural fix available. If declaration is going to be more than private theater, the people exposed to an arrangement need standing to say which precommitments count and whether they have been honored. In the opening example, the parent cannot be merely a perspective the model generates on the manager's behalf. She has to be among the parties who define what an adequate schedule would be. Otherwise the register is a soliloquy with citations.

So, compactly:

> You are somewhere. Name where.
> Name what you're claiming.
> Name what would change it.
> Name who can resolve what you don't know — measure, define, or decide.
> Then keep the record, and let the people it lands on read it.

None of that is expensive anymore. What comes after the record costs exactly what it always cost, and that part was always the point.

---

### Open Questions (Ω)

**Ω_E — Curation.** When people select a kill condition from a generated list, do they systematically choose the least threatening one? Testable: compare selected conditions against conditions the same people generate unassisted, and against conditions assigned by an adversarial reviewer, then compare firing rates. If the menu produces gentler tests, it is functioning as a shield rather than a discipline.

**Ω_E — Adversarial fidelity under agreeableness.** How much objection quality survives a model's incentive to please? Compare objections generated when the model knows the author's position against objections generated blind to it, judged independently. Expect degradation; the question is how much, and in which classes of objection.

**Ω_E — Whether precommitment changes anything.** Do public kill conditions raise revision rates, or mainly produce more sophisticated post-hoc reinterpretation once they fire? Revision rates alone will not answer this; it needs independent judges ruling on whether the condition fired.

**Ω_C — Where the confession stops.** The assumptions behind any claim are unbounded, so "declare your assumptions" needs a stopping rule, and every stopping rule is itself a contestable commitment. Cheap generation makes the proliferation worse. The only workable rule may be social — declare enough that an affected party can see what would move you — but *enough* and *affected party* are exactly the contested terms. This one gates the whole practice.

**Ω_P — Who is entitled to demand it.** The method is voluntary for the person with slack. Extending it as a social expectation is a different political act and needs its own justification, from the people who would be expected to comply.

**Ω_P — Who audits, and what that becomes.** Personal registers work up to a point; durable accountability works better when commitments are externally legible. But external legibility is the exact point where epistemic infrastructure turns into governance infrastructure, with everything that implies about surveillance, unequal exposure, and who gets standing to judge. That decision belongs to the people who would be tracked by it.
