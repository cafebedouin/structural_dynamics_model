# Comparing Incident Taxonomies

**A cross-taxonomy comparison presupposes intra-taxonomy stability, and nobody measures it.**

*Methods note, 2026-08-12. CC0.*

---

## Who this is for, and what it should change

You have a taxonomy of failures derived from an incident record, and you have found someone else's,
derived independently from a different system. The obvious move is a blind two-directional
cross-coding: code their incidents against your classes, yours against theirs, publish the
confusion matrix, treat disagreement as the result rather than as a problem.

**Do one cheap thing before you design that experiment.** This note says what it is, why it can
retire the experiment outright, and what else the attempt reliably exposes. It is written from one
worked case in which the cross-coding was fully pre-registered, funded, and then abandoned on
evidence — and in which the abandonment produced a better result than the experiment would have.

---

## 1. Measure self-agreement first, on both sides

We reconstructed the comparison set's 22 incidents from its published artifacts rather than from
its prose, because we needed coded units and the paper carried narrative. The two published
artifacts — a failure-modes catalog and a labeled dataset, carrying the same 22 incident IDs —
**assigned different classes to 10 of them. 55% agreement.**

Single author. His own incidents. His own five classes. Full knowledge of the system that produced
them. Those are the most favourable conditions a taxonomy will ever be applied under.

> **Rule 1. Before comparing two taxonomies, measure whether each reproduces against itself, and
> report the rate as a precondition rather than as a robustness check.**

This is not a criticism of that taxonomy — we thought the five classes were good and were using
them. It is a fact about the *instrument* that the comparison depends on, and it is invisible from
the prose.

**Why it retires the experiment.** A confusion matrix against a reference that self-agrees at 55%
cannot separate *our taxonomy disagrees with theirs* from *theirs disagrees with itself.* That is
precisely the ambiguity the comparison exists to resolve. No sample size fixes it; the reference is
the problem. Every per-class effect-size floor would have had to be set against observed
self-agreement rather than against the published class distribution, and we had pre-registered
neither.

---

## 2. The disagreement is structured, and the structure is diagnostic

We expected noise. It was not noise.

Of the 10 disagreeing incidents, **8 involved a single class**, bidirectionally — that class both
absorbed and shed roughly equally. Only 5 of the 10 possible class-pairs were occupied at all. The
**most stable class appeared in just 1 of the 10.** Our pre-registered guess about which pair would
be the seam was wrong, and we recorded it as wrong.

Our reading, offered as inference and not as finding: **that class was defined on a different axis
from the other four.** Four sorted by mechanism — *what went wrong*. The unstable one sorted by
whether the system's account of itself matched what it did — *declared versus runtime*. That axis
**cross-cuts** the mechanism axis instead of partitioning alongside it. Every incident with a
declaration mismatch also has a mechanism, so it can be filed either way depending on which axis
the coder reaches for first — and one coder, on two occasions, reached differently. The stable
class was stable for the mirror reason: it named a mechanism nothing else named, so nothing
competed for it.

> **Rule 2. A taxonomy carrying two orthogonal cuts inside one flat enumeration will be unstable
> exactly where they cross. Check that your categories sort on a single axis before you count
> agreement — instability at one member is evidence of a hidden second axis, not of coder error.**

If that reading is right, the disagreement is not drift. It is the trace of genuine multi-class
membership: incidents that legitimately belong to two classes, in a scheme whose single-label field
cannot carry both. Two mechanical predictors were consistent with it — rows carrying a
secondary-class annotation, and rows whose own citation named a class other than their label,
disagreed at well above base rate — but with small *n*, post-hoc predictor selection, and no
correction for multiple comparisons, those are descriptive.

---

## 3. It will be symmetric, and yours is the one you cannot see

Our own six patterns lived in two documents — an always-loaded rules file and the detail document
it pointed to as authoritative — that **disagreed on two of six.** Same indices, different patterns,
all four disjoint members carrying real dated exemplars. Undetected for as long as both documents
had existed.

Nobody noticed until the experiment forced the question *"which set do we code against?"* Designing
the comparison is what surfaced it; no amount of using the taxonomy had.

And the collision was not random. The branch that won publication had **the thinnest exemplar
record of the four** — no dated instance, no artifact, no number, alone among its siblings. On
investigation it turned out not to be a fork at all but an index collision: two distinct patterns
assigned the same number, and the number was reusable precisely because nothing was firmly attached
to it.

> **Rule 3. An index is reusable in proportion to how little is attached to it. The taxonomy entry
> with no dated exemplar is the one that will collide — and the collision will be invisible from
> inside either document, because both read as complete.**

We eventually retired that member: it named a *discipline* (prove before you replace) rather than a
*defect shape*, and a failure-shape search over the full history found zero instances in the period
the rule existed. The vacated index was left visibly empty rather than reused, because every dated
citation of the later numbers would otherwise become ambiguous against its own history.

---

## 4. What the preregistration failed to pin, and why the check stayed green

The freeze pinned sixteen artifacts: two specification documents, two source artifacts, a lexicon,
both coder prompts, an amendment, six control files, two rulings.

**Every one of them was a text. Nothing executable was pinned** — not the driver that would execute
the run, not the packet builder, not the scorer.

Nobody wrote that rule down, and everybody applied it, the operator included. The pinning instinct
followed **genre**: things that *read like specifications* got pinned; things that *run* did not. It
is invisible precisely because it produces a manifest that looks complete — every item in it
genuinely belongs, and the omission has no shape.

The consequence is not subtle. The freeze check passed green while carrying **no information about
whether the run could produce data at all**, because the instrument that would produce it was
outside the manifest.

> **Rule 4. Pin what the result causally depends on, not what reads like a specification. Genre is
> not a criterion. If substituting an artifact would change a number in the writeup, it is pinned —
> and that includes every executable in the path, plus the fixtures its controls run against.**

The rule has a second face that catches you on the rebound: when you *do* build the scorer, the same
instinct will treat it as downstream plumbing rather than as part of the design.

---

## 5. Specify the analysis stage before the collection stage

The design specified every stage up to the point where data lands — extraction, redaction, blinding,
packet assembly, ordering, capture, unanimity, leak sweeps — all pinned, all controlled.

It specified **no stage after.** No scorer, no overlap-pair identification, no matrix construction,
in code or in text. A perfect run would have produced 219 scoreable answers that nothing in the
system could score: a producer with no consumer, sitting one stage downstream of the stage that
actually failed.

Whether scoring was intended as a manual step at writeup or was an unnoticed gap, the frozen text
does not say — and that ambiguity is itself the finding.

> **Rule 5. Control density goes to zero exactly at the stage where the findings are made, unless
> you specify the analysis before the collection. The scorer is an instrument: it gets pinned and it
> gets two-sided controls like everything else.**

---

## 6. Gate the output, not only the input

The run was authorized and executed. It made **all 219 model calls and persisted zero answers** —
the driver had no code path that wrote responses.

Every gate it passed was an **input** gate: count captured payloads, count fixtures, sweep for
leaks. All green. Expected totals printed. Nothing on disk.

The asymmetry that produced it is worth naming, because it is not carelessness. Before the spend,
the *refusal* path — the check that prevents an unauthorized run — was exhaustively controlled,
because it had a red light on it and nobody spends through a red light. The *capture* path received
nothing, because **a writer that does not exist emits no error, no warning, and nothing to
inspect.** Attention followed the signal, and the signal was uncorrelated with the cost.

> **Rule 6. Gate the output, not only the input. Persist the raw datum before parsing it,
> write-then-verify per unit so a run that dies at call 140 leaves 140 recoverable, assert
> non-emptiness and expected vocabulary rather than count alone, and count from the artifact rather
> than from the loop — a length taken from the loop is a claim about persistence sourced from the
> one thing that is not persistence.**
>
> **A verification stack audited by following its red lights will systematically miss every defect
> whose signature is silence.**

---

## 7. What to do instead

1. **Measure self-agreement on both sides first.** It is a day's work against published artifacts
   and it may be the entire result.
2. **If either side is unstable, do not run the cross-coding.** Report the instability; it is a
   finding about incident taxonomies in general and it does not need the other party's cooperation.
3. **Ask whether the unstable class is sorting on a second axis.** If it is, the repair is to split
   the axis, not to retrain the coder.
4. **Turn the question symmetric.** *Does my own taxonomy reproduce against itself?* needs nobody's
   permission, uses your own record, and is the reciprocal of what you are asking of them.
5. **Only then**, if both references reproduce, is a confusion matrix interpretable.

---

## 8. The uncomfortable summary

The experiment never ran. The arc around it produced more than the experiment would have: the
self-agreement finding, the cross-cutting-axis diagnosis, our own index collision, the pin-criterion
failure, the missing analysis half, and the output-gate rule.

That is not a consolation. It is a datum about where the value sits in this kind of work. **Every
one of those came from reconstructing someone's data from their published artifacts rather than
reading their prose** — and none of them came from the measurement, which is the part that was
funded, designed, controlled and spent.

If you take one thing: the reconstruction *is* the experiment. Budget for it accordingly, and be
prepared for it to make the measurement unnecessary.

---

## Status and limits

**One pair of taxonomies, one author on each side.** The self-agreement figure is one researcher's
two artifacts; three readings of it — coding instability, genuine multi-class membership, or two
artifacts produced at different times for different purposes and never expected to agree — cannot
be distinguished from outside, and only the author can settle which. We wrote to him with the
reconciliation table and did not wait on a reply, because the letter's function was to enable a
re-derivation rather than to request an adjudication.

**The cross-cutting-axis diagnosis (§2) is inference, not finding.** Its named falsifier is a blind
re-coding of the disagreeing units by a third party; we specified that test and did not run it,
because it is a third coding direction and our preregistration forbade adding one without a new
freeze.

**Rules 4–6 are single-instance.** They are stated as rules because each has a mechanism and a
stated failure mode, not because they have been measured across projects.
