# The Blind Reviewer

### What context does to a reviewer's jurisdiction

*Draft v0.3, 2026-08-20. Receipts live in the repository and are pointed at, not restated.*

---

I built a reviewer that couldn't see, and the platform showed me I had blinded it along the wrong
axis. That failure is the essay — not as a cautionary tale about harnesses, but because it separated
two things I had been treating as one, and the separation turns out to be the useful part.

Here is the short version, before the narrative that produced it.

A reviewer's usefulness can come from what it *lacks*. But "lacks" is not one variable. A reviewer
can be denied the artifacts, or denied the project's categories, or denied the history of the
conversation that produced the draft — and these are independent. Restricting one does not restrict
the others. My mistake was to build an instrument that varied one and to describe it as though it
had varied all three.

| | substrate | project framing | negotiation history |
|---|---|---|---|
| **cold-reader simulation** needs | absent or present | **present** — the later reader will have it | **absent** |
| **outside-the-framing check** needs | absent | **absent** — the rules *are* the frame | absent |
| **presentation audit** needs | either | either | absent from *this* assembly |
| what I actually built | absent | **present** (injected) | absent |

Read the last row against the second: the instrument I built could not do the job I most wanted it
for, and could do a different job better than I'd designed for. Both facts were invisible until
something varied one column without the others.

---

## How the columns came apart

I do research with language-model instances as the workers. They don't persist — each arrives with
no memory of the last — and the substrate is a repository rather than a conversation. Over a few
months it has accumulated three papers, a couple hundred audit directories, an issue tracker running
to a few hundred open questions, a dated session log, and a small file of rules loaded into every
session. One human, which is me. The subject of the work is how claims come to look
better-supported than they are, which puts the apparatus under permanent suspicion of being an
instance of its own subject.

I wanted a reviewer that couldn't cheat. Not smarter — blinder. I spawned a sub-agent with a
restricted tool list: no file reads, no shell, no grep. It would have to work from what it was
handed. I wrote this up as making blindness "structurally enforced rather than depending on
discipline about what gets pasted."

The first-use check tested exactly that. The tool probe returned
`Error: No such tool available: Read`. Arithmetic on the other side of the control returned correct.
Two-sided pass.

Then the reviewer volunteered something I hadn't asked about: its context had arrived pre-loaded
with both project rule files, a memory index, and a git status block naming four recent commits. I
checked the commit subjects against the log rather than taking its word — all four real, current,
byte-accurate, not reconstructible by an agent with only web search. The harness injects project
context into every spawned sub-agent. As of August 2026 there was no setting that turned it off.

Blind in tools. Sighted in context.

I had restricted the channel I happened to think of. The platform delivered through channels nobody
had enumerated. The shape of that mistake is one I'd written a paper about: an inventory assembled
under an unstated selection rule reads as complete, because every item genuinely belongs. My
inventory was *access channels*. My unstated rule was *tools are the channel worth restricting*.

What made this productive rather than merely embarrassing is that the injection hit one column and
not the others. The cold-reader job survived — arguably improved, since the future reader I want to
simulate *is* a fresh instance that will have the rule file loaded, so injecting it raises fidelity.
"Cold reader" is a bad name for that target and I'll keep it only because it's the term of art:
what's being simulated is not total ignorance but the *next worker's actual starting condition* — no
lived history of this session, but whatever standing context the system routinely hands everyone.
The outside-the-framing job did not survive at all, because the rule file *is* the framing. A reader
handed my standing rules is a reader I have already told what to look for.

One agent, two jobs, and I had never noticed they were two, because from inside the design they
looked like a single property: "doesn't have the repo." That property decomposes.

**So the claim, stated as narrowly as I can state it:**

> Some evaluative functions are available only to a worker denied specific context, because that
> context is what would let the worker close the gap without noticing it had closed anything.

Note what this does *not* say. It does not say blind reviewers are better reviewers, and it does not
say the seat produces superior detection. It says the functions are different, and that a
configuration which supplies context cannot perform the function whose precondition is that
context's absence. That much follows from what the reader has, not from how well the reader reads.

**The firing condition.** I'll name it here so the callback later isn't unmoored. Hold the artifact
fixed and vary the columns: a reviewer with no substrate and no project framing; one with both but
no negotiation history; one with all three; one with framing but no substrate. Multiple draws per
cell, because instance variation is real. Amendments scored by a third party who doesn't know which
cell produced them. The comparison that matters most is the last against the first — framing held,
substrate varied. If findings don't move across those cells, the seat is doing nothing and
everything below is a story about careless drafting.

And the part I had left out, which is the part that decides whether the experiment is worth running:
**clean presentations mixed in, where the correct output is no amendment at all.** Without those, a
reviewer that always finds something scores identically to one that discriminates, and I would have
built an instrument whose falsifying branch cannot fire — the exact defect this essay spends its
last section on. A control demonstrates discrimination, not detection; planting the target only
shows the thing can fire.

**None of this has been run.** Until it is, I can tell you the functions differ and I cannot tell
you by how much.

What follows is four observations from different parts of the apparatus. Not independent lines —
one and four both concern iterative review, two and three both concern outside position — and I'd
rather say so than invite the objection.

---

## One: detection is not retention of detection

A plan went through a genuine five-round review loop — real revisions each round, not a scripted
walk to convergence — and the reviewer declared it ready. I handed the converged text to a
brand-new instance with no negotiation history. It came back saying another round was needed, with
two findings.

The first: a sentence defining the comparator set was self-contradictory, using a phrase and a count
that denote different sets. **This one is weaker than it looks and I want to say why.** Without the
sentence in front of you, you cannot distinguish "the fresh reviewer caught a real inconsistency"
from "the fresh reviewer failed to infer an intended shorthand that the negotiating reviewer had
correctly learned." Both are consistent with what I've told you. Context causing accommodation and
context causing competence produce the same observation here.

The second finding does discriminate, and it's the reason this section exists. A file the plan
leaned on three times had never been added to its substrate list. That gap was found **by the
negotiating reviewer itself, at round four** — and explicitly waived as "non-blocking, carried,"
before it declared the plan ready.

So the phenomenon is not that a reviewer failed to see something. It saw it, named it, classified
it, and then stopped treating it as live. Effort-based explanations don't reach a finding that was
made and then released. Neither does the competence account: you cannot correctly-interpret-away a
defect you have already written down as a defect.

I called this "charity" in an earlier draft and I'm keeping the word as a *description of the
function*, with the flag that it is not a mechanism. I don't know the mechanism. Candidates,
all live: accumulated agreement raises the bar for re-raising something already dispositioned;
recency in the conversation outweighs the distant original text; a fine-tuned disposition to accept
a user's assertion that a matter is settled. These are testable and I haven't tested them. One of
them has a cheap repair that doesn't require a fresh instance at all — instruct the *same* reviewer
to re-read the converged text from scratch, ignoring the history — and if that works, the seat is
not the active ingredient. That's worth someone running before they build what I built.

**Altitude.** The two reads differ in history, in instance, and in draw — three confounds in one
comparison, unpartialled. I'm not claiming the fresh seat is better. What this establishes is
narrower: a fresh-read step whose only possible verdict was "looks fine" would be a ritual wearing a
check's costume, and this pair shows the decline branch is reachable. Reachability survives the
confounding. Discrimination does not.

The practical consequence I did take: the fresh-eyes pass became a mandatory production step rather
than a validation exercise. It isn't testing whether the loop works. It asks whether the thing being
handed over still carries gaps — a property of the deliverable, not of the process.

---

## Two: part of the jurisdiction compiles into a checklist, and I can't tell you how much

Before any of the agent tooling existed, I ran eighteen held decisions through a session with three
parties: an instance with repo access assembling evidence and a suggested ruling per item, a second
instance following the session but unable to touch the repository, and me ruling.

Across all eighteen, the reviewing instance's amendments never touched the evidence. Counts, greps,
controls held every time. But the first three rulings were each amended, and each on the same three
axes:

- **shape** — is the decision living in the right entry? No live rider hung on a closing entry; a
  general principle recorded as a principle rather than patched in locally.
- **record** — how will this read cold, later? Stale baselines reconciled; "zero true positives"
  distinguished from "never tested."
- **reviver** — what does this leave for whoever picks it up? Passive triggers declared, sequencing
  against other open work recorded, sibling claimants named.

The regularity was strong enough that we adopted it mid-session: the assembling instance ran
shape/record/reviver over each item *before* presenting. After that the amendments changed
character — they stopped falling into the three buckets and became item-specific.

Part of what the outside reader contributed was therefore compressible into a rule, and once
compressed could be run from inside the loop. That part was never about the seat. It was a check the
sender knew how to perform and wasn't performing.

The residue is what carries the argument, and here I owe you both a number I don't have and a
confound I missed until someone else pointed at it.

The number: I can tell you item-specific amendments kept arriving after the checklist landed. I
cannot tell you how many, or what fraction of the total, or whether each genuinely resists reduction
to a rule rather than merely not having been reduced yet.

The confound is worse, because it survives the number. Adopting the checklist changed what the
*presenter* did. Better presentations arrive, so the amendments that follow are different — and
"different amendments after the checklist" is exactly what you'd see whether or not the outside seat
was supplying anything non-compressible. I read that shift as evidence of a residue. It is equally
consistent with the presenter having improved and the reviewer contributing nothing the checklist
couldn't have. The control that separates them is specific: an inside-loop reviewer running the same
checklist, against an outside reviewer, on the same presentations. If the outside one still produces
item-specific amendments the inside one doesn't, the residue is real. That has not been run either,
and until it is, this section is an observation I have interpreted, not a result.

Read at its honest altitude: one operator, one session, eighteen items, no denominator. It
establishes that *some* of the jurisdiction compiles and *some* doesn't. The relative sizes are open,
and if the residue turned out to be small the right conclusion would be "write the checklist," not
"build the seat."

---

## Three: checking what reaches the decider

At one of the eighteen items the ruling was purely mine — whether to circulate a document, which
turns on audience, timing, and what I'm prepared to defend. Nothing about the evidence settles it.

The reviewing instance stated its boundary before doing anything: it could check whether the framing
being handed to me was accurate, not whether I should sign. Then it flagged one sentence in the
assembled presentation as a quality claim wearing a jurisdictional argument's clothes — a sentence
that, had it stayed, would have made a judgment call look like a finding on its way into my hands.

**This is not a blindness result and I want to be plain about that.** The reviewer here needs no
particular deprivation. It needs to not be the assembler. What it audits is the transmission from
evidence to decision, and the failure it catches is that a presentation, assembled in good faith,
quietly acquires the grammar of evidence on the way up. Nobody lies. The claims just put on the
wrong clothes in transit.

I'm keeping it in an essay about deprivation because it marks the boundary of the thesis. Three of
the four observations here are about what a reader lacks. This one is about where a reader sits, and
it's the cheapest to transplant: if you have anything resembling a decision memo, the check to run
before it lands is whether each of its claims is presented as evidence, inference, or judgment, and
whether that matches what it is.

---

## Four: the fixer is a source of rounds

*From a contaminated instrument, kept as a specimen.*

In one measured round, the fix pass closed most of the reviewer's findings while opening roughly as
many new ones — some of them defects the fixing instance itself introduced, including a fix-label
attached to something that wasn't a fix. **The source is the primed reviewer's own transcribed
report, and that instrument was holding its own test specification in its system prompt at the time,
so this is a contaminated observation carrying no numbers and no weight beyond its shape.**

The shape suggests a hypothesis worth testing rather than a finding: where the fixer is the same
kind of worker as the reviewer, fixes generate reviewable changes, and the loop has no *guarantee*
of endogenous termination absent an explicit stopping rule — an acceptance threshold, a monotonic
measure, a round cap. If you have not set one of those, you have not set a budget; you have set an
open-ended one. Whether a clean run would show the same is unknown, and I have no basis for
guessing.

---

## The channels, and why grepping doesn't reach them

Looking for context arriving through unenumerated routes, I found six. Four are findable: the
project rule file, a global rule file, a memory index plus dozens of sibling memory files pulled in
per-turn on content match, and a live git status block. A fifth was project-scoped agent definitions.

The sixth is the one worth the price of admission. The git status block includes untracked
filenames. Two were `payload_fire.md` and `payload_decline.md`. No content leaked — the payloads
were blinded, stripped of dates, identifiers and paths. What leaked was that the test had a fire
side and a decline side. Two instances noticed independently.

I want to be exact about what that reveals, because it's easy to overstate. The filenames disclose a
*partition of the test space*, not the semantics of the test — a reviewer learning there are two
payload classes does not thereby know what it's being asked to do. But a partition is enough to
change how a reviewer reads: knowing one of these is meant to be declined is a prior, and it arrives
before the payload does. **An injection channel can leak the shape of a test without leaking any of
its content**, which puts it out of reach of payload blinding and content-level grepping alike. I
don't have a general fix. What worked was moving the run to a path outside the project, which closes
path-keyed channels structurally rather than by instruction — the difference between not having the
key and promising not to use it.

Then the finding I'd most want someone to check against their own setup. I needed payloads the
reviewer couldn't have seen. Sweeping the audit directories turned up thirteen candidates. **Seven
of the top eight were burned** — named in the always-loaded rule file, or in a memory file, or in
recent commit subjects.

That isn't luck. The always-loaded file is a *promotion channel*: its function is to lift witnessed
defects into standing rules so future workers don't repeat them. So the better an incident was — the
more it taught — the likelier its lesson has already been promoted into the context every worker
receives, and the worse it serves as a test case. The system exerts a structural pressure toward
anti-correlation between a payload's quality and its availability. Seven of eight is consistent with
that pressure; it doesn't measure the correlation, and I'm not claiming it does.

The conditions that produce it are specific enough to check for: lessons promoted into universal
context, test cases drawn from the same incident population, lessons retaining identifying detail,
and no independent test population. Where those hold — and they hold in a lot of places with a
lessons-learned channel — your best material is in your onboarding doc, and your onboarding doc is
in everyone's head. The two facts are the same fact. Where they don't hold, you have an escape I
don't.

There's an epistemic consequence I should state rather than leave implied. If clean payloads can
only be drawn from incidents that weren't worth promoting, then the instrument gets tested on the
low-signal end of its own population, and its performance on novel high-stakes failures is
extrapolation. I don't have a way around that. It is a real bound on how much any of this can
validate.

I didn't edit the rule file to unburn the test, which was tempting and would have been a bad trade:
giving up a live defense against a witnessed failure for a cleaner control on an instrument I hadn't
shown works.

---

## Sorted by what I actually know

Rather than scatter caveats, here is the whole epistemic position in one place.

**Observed.** Tool restriction did not restrict context, and the injected commits verified
byte-accurate against ground truth. A fresh read returned two findings on text a five-round loop had
marked ready. One of those findings had been made and waived by the negotiating reviewer at round
four. Shape/record/reviver captured the first three amendments and stopped capturing them once
adopted. Test-shape leaked through untracked filenames. Seven of eight top candidate payloads were
burned. Five reviewer refusals were each a real defect.

**Hypotheses the observations motivate.** That accumulated history makes an already-dispositioned
defect cheap to leave alone. That context reallocates scrutiny rather than destroying visibility —
which the waiver fits better than a blindness account does, since nothing was invisible. That
same-kind reviewer/fixer loops lack endogenous termination. That the residue of the jurisdiction
resists compilation.

**Open.** Whether the seat effect exists at any measurable size — the firing condition, unrun.
Whether context, framing, or history is the operative variable; the case I have varies all three at
once. Whether any of this survives a second operator, a different disposition, or a deadline. And
there is no rate available anywhere in the record: the apparatus that tracks whether my verification
machinery catches anything has never once logged a "caught nothing," because audits get written when
something is already suspected. A falsifier with no decline in its history licenses nothing in
either direction, so the instrument prints a refusal instead of a number. That refusal is the correct
output and it is not a good one.

**And a case I'm deliberately not counting.** The failure I opened with fits a pattern I've
documented twice — a defensible criterion applied by everyone and never articulated — and it arose
in a different arc, which is the bar I set for admitting a third. I'm still not counting it. A
pattern that keeps finding new members in the arcs where its author is looking has stopped being
falsifiable, and the count staying at two is worth more than the count going to three. Naming a
fitting case and declining to count it is the only way I know to make restraint visible; every other
kind leaves no artifact at all.

---

## What I'd want someone else to take

Five times during this work the reviewer refused an instruction, and every refusal was a real
defect: a room it couldn't reach, a registry that couldn't be refreshed mid-session, a canary its own
system prompt had already named, a pass criterion no run could satisfy, and an instruction
contradicting the spec it was meant to follow. All five sat in prompts written by the instance that
had, days earlier, documented the rule they violated.

Refusal here is not obstruction. It's an instrumentation event — a recorded mismatch between what
was asked and what the task's actual world can satisfy. Five of them, none caught by the sender
re-reading. For this class of defect, sender re-reading is a weak check: not because self-review
never works, but because the failure is one the sender's own context makes hard to see. It's the
same seat twice.

Then the part that makes this a worse paragraph than it reads as. *Every refusal was a real defect*
is not a hit rate, because I logged refusals and did not log acceptances. Refusals are salient;
compliance is invisible. Somewhere in the instructions that were accepted without complaint there
may be defects nobody has looked for, and the record cannot tell me how many, because the branch
where a reviewer accepts a bad instruction produces no artifact. That is the same unreachable
falsifying branch I've been objecting to all essay, sitting in my most quotable paragraph. I'm
keeping the observation — five refusals, five real defects, none self-caught — and withdrawing any
reading of it as a measure of how well the seat performs.

So, operationally: when you run agents against a substrate you control, **enumerate what arrives,
not what you granted.** Permissions are one access surface. Injected rules, memory retrieval,
repository state, agent definitions, paths and filenames are others, and only one of those lists is
written down anywhere you can read it. Spawn the thing and ask it what it can see, then check its
answer against ground truth — a report and a recall are byte-identical, and so, for that matter, are
a recall and a fabrication.

And then the harder thing, which is not a security exercise. If a reviewer's value depends on not
knowing the project well enough to repair its omissions in imagination, then giving it more context
does not improve the instrument. It changes which instrument you have. The pressure to let it into
the room will be constant and will always sound like efficiency — it knows the project now, it's
slow to get up to speed each time, just give it the context file.

Which context, though. That is the whole discipline, and it's why I can't end on a line about
ignorance in general. Look back at the table: the cold-reader job *needs* the standing rules, and
starving it of them would wreck it. Only the frame-independence job needs them gone. Total ignorance
is not available anyway — the platform saw to that — and it wouldn't be the goal if it were.

So: don't give it the context you're testing against. That one, and only that one, is the
instrument.

---

*Everything above lives in a repository as dated audit directories with the runs pasted in. The
pieces I'd most like to be wrong about are the ones with no denominator, which is most of them. If
you run the firing condition, I'd like to hear what happened — particularly if the substrate-holding
instance found the same things, which would mean I've spent a month building a seat that does
nothing a careful reader wouldn't.*
