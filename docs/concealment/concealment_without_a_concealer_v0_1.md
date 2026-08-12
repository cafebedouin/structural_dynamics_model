# Concealment Without a Concealer: Framing, Compression, and the Structural Production of Silent Failure

**cafebedouin@gmail.com**

*Status: DRAFT v0.1 (2026-08-12). Extracted from §2 of `The Amnesiac Institution` v0.6 and rebuilt
to stand alone. Claim types are declared in §0 and govern how each claim may be discharged. The
companion paper is the worked institution and the source of every unattributed dated example here;
this paper is the argument, and is written so that a reader who never reads the companion loses
evidence but not warrant.*

---

## ABSTRACT

A compression is an artifact formed at a **framing** — some range of things, under some selection
rule, over some interval, for some purpose — and nothing in the form of a compression carries the
framing it was formed at. This is not a defect of any particular summary. It follows from what
compression is, together with a result about standpoints: the partition of a field into background
and free parameters is itself a live parameter, so there is no framing-free description of what an
artifact ranged over for the artifact to carry.

Add a constraint that has no exceptions: any party that must act on a corpus larger than its working
set must compress. Then any such party holds shape and not detail, and can check that a shape is
intact but cannot check what the shape omitted, because an omission has no form to check against.
The consequence is that a compressed artifact arriving at a read site asserts content while carrying
no standpoint — the one position a standpoint-relative account of verdicts forbids — and does so with
**nobody committing the offence**. The producer stated what it found, at the framing it found it.
The consumer read the artifact as artifacts are read. Neither held a standpoint they declined to
declare. The inconsistency is manufactured by the boundary, not by a party.

We call this **concealment without a concealer**, and argue three things follow. First, the
characteristic failure of such systems is not falsehood but *frame-completeness read as
completeness*: an inventory built under an unstated selection criterion reads not as visibly partial
but as plausibly total, because every item in it belongs. Second, the remedy cannot be an honesty
norm, because nobody was dishonest; it must be an artifact discipline enforced at boundaries
regardless of intent. Third, the remedy is not self-terminating — declaring a framing produces a new
artifact with a new framing — so what terminates the regress is not declaration but *re-derivation*
by a party positioned outside the artifact's framing.

The failure is old and general. What is new is arithmetic. Cheap generation raises the compression
ratio at every boundary; automated summarization has made the compressor fluent and silent about
what it dropped; handoffs between parties now occur hourly rather than yearly; and in institutions
staffed by non-persistent workers, the parameter that has historically kept the failure rare —
the consumer's ability to *reconstruct* the producer's framing from shared history — goes to zero.

**Keywords:** framing, abstraction, compression, silent failure, epistemic infrastructure, LLM
agents, verification, standpoint

---

## 0. Claim types

Claims here are of three kinds and are discharged differently. Conflating them is how an argument of
this shape gets both overclaimed and overcorrected.

**ANALYTIC (A).** Derived from stated premises. Refuted by exhibit, not by data.

| # | Claim | Kill condition |
|---|---|---|
| A1 | Any party acting on a corpus larger than its working set must compress | An institution operating at scale with no compression at any boundary |
| A2 | Every compression is formed at a framing, and the framing is a live parameter not recoverable from the artifact's form | An artifact form that carries its own selection criterion without an author declaring it |
| A3 | No procedure operating only on a compression can report what the compression omitted | A representation of an omission internal to the compressed form (§4 discusses the near-misses) |
| A4 | Under A1–A3, a compressed artifact crossing a boundary asserts content while carrying no standpoint | A compression whose bare form is not read as a claim about the consumer's framing |

**EMPIRICAL (E).** Generalizations about how parties actually behave. Discharged by measurement,
and each is currently supported unevenly.

| # | Claim | Support | Owed |
|---|---|---|---|
| E1 | Parties checking an artifact default to *recognition* — confirming the shape — and perform *enumeration* only when forced | Model evidence (§4); four operator rulings correct in prose and defective in execution, one institution | A protocol comparison (§10) |
| E2 | The failure is silent in proportion to the consumer's inability to reconstruct the producer's framing | Argued in §6 from case contrasts; not measured | A study varying shared history at constant compression |

**PROPOSED (P).** Design consequences.

| # | Claim | Falsifier |
|---|---|---|
| P1 | The remedy must be an artifact discipline rather than an honesty norm | Honesty-norm interventions reducing the failure at rates comparable to carriage rules |
| P2 | Carriage is necessary and not sufficient; termination requires re-derivation from outside the framing | A carriage discipline that terminates without an external re-derivation step |

---

## 1. A worked case, before any theory

A research pipeline computes a system-level gradient. Where the computation returns nothing, a
fallback emits `0.0`. Downstream, an aggregate reports the gradient as flat, and a flat gradient is a
substantive, publishable null — more interesting, as it happens, than the truth.

Every component is correct. The computation correctly reports failure by returning empty. The
fallback correctly supplies a default, as its author intended. The aggregate correctly averages what
it received. The report correctly states what the aggregate said. No component lied, and a code
review of any single component would find nothing.

The defect is that `0.0` is a compression of *"nothing was computed"* formed at the fallback's
framing, and it is read at the aggregate's framing as a measurement — with the two framings rendered
in the same eight bytes. When the pipeline was finally traced, every gradient computation ever made
had failed; the construct had no interval of valid measurement in its entire life.

Three properties of this case recur through everything below, and they are the reason the ordinary
vocabulary of error does not fit.

- **Nothing was false.** Each artifact was accurate at the framing that produced it.
- **Nothing was hidden.** No party possessed the framing and withheld it. The fallback's author did
  not decide to conceal that `0.0` meant absence; the question never arose at the boundary where it
  mattered.
- **The failure had no signal.** A wrong number that looks wrong recruits attention. This one looked
  exactly like the thing it was standing in for, because it *was* the thing it was standing in for,
  minus one dimension nobody had a place to write down.

---

## 2. Premises

**Premise 1 — working-set finitude.** Every party that acts has a working set smaller than the
corpus it acts on: a context window, a page of notes, a remembered shape, a loaded configuration
file. This is not a claim about cognition; it is a claim about ratios, and it holds for humans,
models, organizations, and files a reader must actually read.

**Premise 2 — verdicts are standpoint-relative, and so is the description of the situation.** Take
a verdict to be the value of a function over a situation and some parameters. A parameter that
co-determines the verdict without being fixed by the situation is not a feature of the situation; it
is an index of evaluation — a standpoint. It follows that a verdict is standpoint-free only if the
situation settles it alone, i.e. only if the question had no content. And it follows one level down
that the *situation itself* is not given: the partition of the total field into background and free
parameters is a parameter of exactly this kind. There is no neutral situation-description beneath a
verdict for neutrality to retreat to.

Premise 2 is a compressed statement of the Coupling Theorem and its framing corollary
(`seat-theorem-v1.md`). It is contestable in one specific place: a direct realist about the given
denies that the field fails to self-partition, and holds that some framings carve at joints.
**Against that reader this paper is local**, which is the only status its own thesis permits any
contentful claim to have. The weaker reading that survives the denial is sufficient for everything
here: even a joint-carving realist must concede that *these* artifacts — a grep's output, a totals
row, a fallback constant, a monthly prune — were framed by somebody with a budget, and that the
framing is not in them.

**Definition.** A **compression** is an artifact standing in for a corpus larger than itself, formed
by a selection over that corpus. A **framing** Π is what fixed the selection: the range, the
criterion, the interval, the purpose.

---

## 3. The framing is not in the artifact (A2)

A compressed form has fewer degrees of freedom than the corpus it stands for. Something was dropped;
the question is what. The framing is reliably among the dropped things, for a reason more specific
than "there wasn't room":

**The producer did not need the framing, because the producer *was* the framing.** Within the act of
producing, the selection rule is not information — it is the operation being performed. A grep does
not need to record which file types it opened; it opened them. A reviewer does not need to record
that "all addressed" ranged over the items they looked at; those were the items. A summary does not
need to record that it covers what the summarizer found salient; salience is what a summary is.

So the framing is not omitted by carelessness. It is omitted by the same mechanism that makes the
compression useful, and this is why exhortation does not fix it: you cannot ask a party to record
the thing that, from inside the operation, is not a thing.

The consequence is a specific kind of invalidity, and getting it right matters for the repair.

> A compression is not *false*. It is **lossy in a direction chosen by whoever compressed, at a
> framing they did not have to state** — valid where it was formed, silently invalid elsewhere.

"Silently" is doing work. An artifact read outside its framing does not fail visibly, because
validity-at-a-framing has no external mark. The compressed value arrives at the read site looking
exactly as it would have looked had it been formed at the reader's framing rather than the
producer's. That equivalence is the whole problem, and it is why the fix (§8) is a carriage rule
rather than a quality rule: the two artifacts are indistinguishable *in the channel*, so the channel
is where the distinction must be reinstated.

---

## 4. What a holder of a compression can and cannot do (A3, E1)

A3 is nearly a tautology and should be stated as one, because its tautological version is the strong
one: **a compression does not represent what it dropped, so no procedure operating on the
compression alone can report what was dropped.** An omission has no positive form. It is not a
feature with a null value; it is the absence of a feature, which is not a location a procedure can
address.

The near-misses are worth naming, because they are what people reach for when they resist this.
A compression *can* carry a coverage fraction, a denominator, an as-of date, a residue list — but
each of those is an *added dimension*, authored deliberately by someone who stepped outside the
compression to notice that it needed one. None is recoverable from the compressed form. That is
precisely the repair of §8, and the fact that it must be authored is the reason the repair is
expensive.

E1 is the empirical companion and should not be smuggled in as though it were analytic. A party
holding a compression *could* in principle go and re-derive from the corpus. The claim is that in
practice they do not, unless forced, because the compression affords a cheaper operation that feels
like checking:

> **Re-reading exercises recognition. Writing the instruction that would execute the thing exercises
> enumeration.** Recognition confirms that the artifact still says what the holder remembers, and an
> artifact correct in prose passes every time. Enumeration requires producing the operational form —
> every input a receiver needs, every artifact they must produce, every decision they would otherwise
> make on the author's behalf. Gaps live where a design named a decision and never named its
> operational half: invisible to recognition, unavoidable under enumeration.

Two lines of evidence, of different strengths.

For models, the evidence is direct. AbsenceBench (Fu et al., 2025) finds that language models detect
*omitted* content poorly even at modest context lengths, and offers the mechanical account:
attention cannot easily attend to gaps, because absences correspond to no key. Independently, an
evaluation of false success in agent trajectories (Advani, 2026) finds that model judges cannot
detect completion claims contradicted by environment state — no judge configuration exceeding 0.65
AUROC — and that judges anchor on confident closing language rather than on verified state change.
A judge reading output is performing recognition on a compression, which is the one operation that
cannot find the gap.

For humans, the evidence here is weaker and is declared as such: a run of four rulings in one
institution that were correct in prose and defective in execution, each caught by a receiving party
who had to write out what the rule actually required. That is an existence proof and a hypothesis,
not a rate. §10 states the protocol comparison that would settle it.

---

## 5. Concealment without a concealer (A4)

Now combine. A compressed artifact crosses a boundary. By A2 it carries no framing. By A3 the
receiver cannot recover the framing from the artifact. The artifact nonetheless answers a contentful
question — it says the gradient is flat, the check is green, the inventory is complete, the tests
pass — and by Premise 2 a contentful answer that carries no standpoint is exactly the one
inconsistent position a standpoint-relative account permits: **asserting content while denying, or
here merely omitting, a standpoint.**

In the usual setting this is an *agent's* offence. Someone poses as the view from nowhere; the
remedy is to catch the pose and require declaration. The result of this paper is that under A1–A3
the same inconsistency is produced **structurally, by the boundary, with nobody posing.**

- The producer concealed nothing. It reported what it found, at the framing it found it, and the
  framing was not an object it held.
- The consumer deceived no one. It read an artifact the way artifacts are read.
- No party had a standpoint they declined to declare. The standpoint was in the operation, not in
  anyone's head.

The institution emits a no-seat pose as an ordinary byproduct of working-set finitude. Call this
**concealment without a concealer.**

The characteristic surface signature follows immediately, and is the practical form in which the
failure is usually first met:

> **An inventory built under an unstated selection criterion does not read as visibly partial. It
> reads as plausibly total, because every item in it belongs.**

Three instances of one shape, from a single working period in one institution — each a selection
criterion that was defensible, applied by everyone including its author, and never articulated:

| the compression | included | excluded | cost |
|---|---|---|---|
| a pre-registration's pin manifest | artifacts that read like specifications | artifacts that run | a run executed under a valid freeze, using an instrument that could not retain its output |
| an experiment's design | every stage up to where data lands | every stage after | answers that nothing in the system could score — a producer with no consumer, one stage past the failure |
| a completeness manifest | items flagged in files | items produced in conversation | seven findings invisible to a check that reported complete |

Each read complete because every item in it belonged.

And the general failure shape now has a derivation rather than an observation behind it:

> **An absence presents as a presence when a compression is read at a framing other than the one it
> was formed at.** The producer's summary was accurate about what the producer looked at; the
> consumer reads it as accurate about what the consumer cares about; nothing in the artifact marks
> the difference, because the framing was never part of the value that crossed the boundary.

---

## 6. Reconstructability: the parameter that decides whether it is silent (E2)

The account so far proves too much. Human institutions have run on framed compressions for
millennia without collapsing, and the reason is not that their compressions carried their framings.
It is that consumers could often **reconstruct** the framing they did not receive.

Reconstruction runs on shared context: knowing the producer, having been in the room, knowing what
the department counts, knowing what "the corpus" meant last quarter, being able to walk down the
hall and ask. Where reconstruction is cheap, an unframed compression is not silent — it is merely
terse, and the receiver silently supplies the missing dimension correctly.

This yields a sharper statement than "compression causes silent failure." The exposure at a boundary
scales with three factors:

> **exposure ≈ compression ratio × framing distance × (1 − reconstructability)**

where *compression ratio* is corpus over working set, *framing distance* is how differently producer
and consumer carve the field, and *reconstructability* is the probability that the consumer restores
the producer's framing from shared context without being told.

Three consequences worth stating separately.

**The failure is not new and its rarity was never a property of the artifacts.** It was a property
of institutional continuity — the same person, or their successor trained by them, sitting on both
sides of the boundary. Continuity was doing epistemic work that nobody had to name because nobody
had removed it.

**Reconstruction is itself a compression, and can be confidently wrong.** A receiver who supplies a
framing by pattern-match produces a plausible completion, not a question — which is a worse failure
mode than an admitted gap, and the one that dominates when reconstructability is *moderate* rather
than zero. The most dangerous position is not total ignorance of the producer's framing but
familiarity sufficient to guess.

**Total loss is safer than partial loss.** A party that retains nothing produces a *question*. A
party that retains shape and loses detail produces a *completion*. This inverts the intuitive
ranking of amnesias and is the reason, in the companion institution, that the riskiest parties are
the ones with the most context rather than the least.

---

## 7. Why the arithmetic changed

Nothing above requires a language model. Four quantities have moved, all in the same direction.

**The compression ratio is rising fast.** Generation is cheap, so corpora grow superlinearly while
working sets do not. For model workers the working set is additionally smaller than advertised:
retrieval degrades with position (Liu et al., 2024), with length independently of position and even
under perfect retrieval (Hsieh et al., 2024; Modarressi et al., 2025; Du et al., 2025), and across
conversational turns (Laban et al., 2025).

**The compressor became fluent and silent about its own losses.** Automated compaction now
substitutes an LLM-authored summary for the conversation it replaces, and the summary does not state
what it dropped. A documented case study records compaction summaries recording partial terminal
output from a killed process as "confirmed results," inherited as fact by a later session
(arXiv:2607.13071) — the failure of this paper performed by the memory system, on the institution's
own record.

**Handoffs multiplied.** A boundary crossing that used to happen at a shift change or a personnel
turnover now happens several times an hour.

**And reconstructability goes to zero.** This is the term that matters. A worker that does not
persist between sessions has no shared context to reconstruct from; it has only what an artifact
hands it. The historical suppressor of the failure — continuity of persons — is exactly what this
configuration removes. In the terms of §6, institutions of non-persistent workers do not introduce a
new failure mode; they **remove the damping** on an old one.

---

## 8. The repair, and why it does not terminate (P1, P2)

**The remedy cannot be an honesty norm.** Nobody was dishonest (§5). A discipline addressed to
concealment reaches the case where a party holds a framing and hides it, and does not reach the case
where the framing was never a mental object anyone held. What reaches that case is an **artifact
discipline**: a rule about what a value must carry when it crosses a boundary, enforced at the
boundary, independent of anyone's intentions.

The rule has two clauses, the second of which is the extension this account motivates:

> **No epistemically meaningful value crosses a boundary without its status** — enough for the
> consumer to distinguish *measured / generated / verified* from *absent / not-run / not-found*.
>
> **No compression crosses a boundary without the framing it was formed at** — its range,
> denominator, unit, interval, selection rule, and residue.

The second clause explains why as-of dates, unit statements, denominators, coverage fractions and
declared residues keep being reinvented as separate conventions: they are one mechanism, each
re-attaching a dropped dimension of Π at the site where someone will read it. Concurrent systems
work has converged on the first clause independently — evidence-gated architectures that refuse to
propagate a claim without a queryable run identifier and required artifacts, with measured effect
(Chen, 2025) — which is worth noting both as corroboration and as a limit: gating a pipeline
enforces status, and does not by itself enforce framing.

**And the repair is not self-terminating.** Declaring a framing produces a new artifact, which is
itself a compression formed at a framing. This is the seat regress in operational clothes, and it is
observable: in one institution's dense working period, nine defects were committed *inside* the
repairs meant to address that class of defect — a manifest whose totals row disagreed with the table
beneath it, a drift list reporting twelve changed sources when two had changed, a self-test that
aborted rather than reporting failure. Each artifact carried its framing scrupulously; the carried
framing was itself a compression with an unstated framing.

What terminated those nine was uniform and instructive: **not one was caught by a gate.** Every one
was caught by a person or a script **comparing a claimed number against the artifact it described** —
a diff, a directory listing, a file count, a re-read. That is not a deeper declaration. It is a
*re-derivation* from the corpus by a party standing outside the artifact's framing, i.e. forced
enumeration (§4).

So the honest statement of the repair, and the boundary of what this account licenses:

> Carriage is necessary and not sufficient. It converts a silent failure into a checkable one by
> giving the consumer a dimension to check. What closes the loop is a re-derivation performed from
> outside the framing — which means the institution must maintain a party positioned to do it, and
> must budget for the enumeration, because nothing about the artifact will ever demand it.

The structural corollary: **an unstated exemption is indistinguishable from an unnoticed one, and no
gate separates them, because the gate is downstream of the exemption.** The frame a check operates
within is chosen before the check runs, and a check cannot report what its own scope excluded.

---

## 9. What this account excludes

An account of this shape is an invariance claim — invariant under which failure you look at — and it
carries no information until shown able to *fail to fit*. Absent that control, "every silent failure
is a framing mismatch" is indistinguishable from "my framing finds its own shape everywhere." Three
exclusions, and one place the account breaks.

**Structural contradiction is not a framing mismatch.** Two axiom sets that derive a contradiction do
so inside a single framing, immediately, with no process and no read site. Declaring a scope does not
repair an invariant violation; the repair is axiomatic revision.

**Stochastic variation is not a framing mismatch.** An observable that moves at byte-identical input
is a measurement-noise problem. Treating it as a scope problem produces the wrong instrument — a
declaration, rather than repeated draws and a floor.

**Loud failures are not in scope.** A destructive replacement that turns out to be needed announces
itself when the thing is next required. There is no read site fooled and no success-shaped token.
The account's silence here is a feature: it explains why "prove before you replace" is a discipline
about warranting a claim rather than a description of how systems fail quietly.

**And one genuine break.** The account predicts that carrying the framing repairs the failure. §8
shows it does not, at the instrument layer, because the carried framing is itself framed. The
account therefore unifies the *form* of the failure and the *direction* of the repair, and does not
establish sufficiency. Reporting this rather than absorbing it is the difference between a lens and
a lens's shadow.

---

## 10. Predictions, and what would settle them

**Prediction 1 (tests E1).** Give matched reviewers the same artifact under two protocols:
*recognition* (read it and confirm it is complete and correct) and *enumeration* (write the
instruction a receiver would need to execute it). Omission-detection rates should separate sharply;
error-detection rates on *present but wrong* content should not. *Falsifier: no separation, or
separation on both.*

**Prediction 2 (tests E2).** Hold compression constant and vary shared history between producer and
consumer — same team, adjacent team, stranger, non-persistent worker. Silent-failure incidence
should track reconstructability rather than compression ratio alone. *Falsifier: incidence flat in
shared history.*

**Prediction 3 (tests P1).** Compare an honesty-norm intervention ("state your assumptions") against
a carriage rule ("no summary without its denominator, interval and residue") at matched cost. The
account predicts the carriage rule dominates, and predicts *why*: the norm asks parties to report
something they do not hold. *Falsifier: comparable effect.*

**Prediction 4 (tests P2).** In systems with mature carriage discipline, the residual silent-failure
population should be concentrated in the *instruments* — the manifests, gates, checkers and controls
— rather than in the substrate they verify, and should be caught predominantly by re-derivation
rather than by gates. *Falsifier: residual failures distributed evenly across strata, or caught
predominantly by gates.*

Prediction 4 has one-institution support and is the cheapest of the four to attempt elsewhere.

---

## 11. Related work

**Differential observability.** Gray failure (Huang et al., 2017) identifies the dominant cloud
failure mode as a mismatch between what an application experiences and what its detector observes,
and fail-slow studies (Gunawi et al., 2018) supply the incident-report tradition. This account treats
differential observability as the special case in which the two framings belong to an application and
its detector; differential *framing* is the general case, which is why long-latency failures are
repeatedly found to live in *seams* between simple correct components rather than inside complex ones
(Wu, 2026).

**Silent failure in LLM systems.** Wu (2026) derives a five-class mechanism-oriented taxonomy from 22
production postmortems and names *fail-plausible* — a system converting an internal error into fluent
false output — as the escalation specific to systems that speak: gray failure starves the detector of
signal, fail-plausible feeds it a counterfeit one. Advani (2026) measures false success at benchmark
scale with text-independent ground truth. Both describe the phenomenon this paper derives; neither
derives it.

**Evidence gating.** EviBound (Chen, 2025) and evidence-chain frameworks for autonomous research
implement the status clause of §8 as system architecture, with measured reduction in unsupported
claims. They are the strongest available evidence that carriage rules work, and they operate on
status rather than framing.

**Long-context behaviour.** The position, length, multi-turn and absence-detection results cited in
§4 and §7 are what make the working-set premise quantitative for model workers rather than merely
plausible.

**Metascience.** Preregistration (Chambers, 2013; Nosek et al., 2018) is a framing-fixing device
avant la lettre: it forces the selection rule to be authored *before* the selection, which is the
only point at which the author still experiences it as a decision rather than as the operation.
Assay controls play the same role for probes. This account explains why those devices generalize
beyond their home fields and why they feel expensive: they require someone to step outside an
operation and write down what, from inside it, is not a thing.

---

## References

Advani, L. (2026). From Confident Closing to Silent Failure: Characterizing False Success in LLM
Agents. arXiv:2606.09863.

Chambers, C. D. (2013). Registered Reports. *Cortex* 49(3), 609–610.

Chen, R. (2025). Evidence-Bound Autonomous Research (EviBound). arXiv:2511.05524.

Du, Y., et al. (2025). Context Length Alone Hurts LLM Performance Despite Perfect Retrieval. EMNLP;
arXiv:2510.05381.

Fu, H. Y., et al. (2025). AbsenceBench: Language Models Can't Tell What's Missing. arXiv:2506.11440.

Gunawi, H. S., et al. (2018). Fail-slow at scale. USENIX FAST.

Hsieh, C.-P., et al. (2024). RULER: What's the Real Context Size of Your Long-Context Language
Models? arXiv:2404.06654.

Huang, P., et al. (2017). Gray failure: the Achilles' heel of cloud-scale systems. HotOS.

Laban, P., et al. (2025). LLMs Get Lost in Multi-Turn Conversation. arXiv:2505.06120.

Liu, N. F., et al. (2024). Lost in the Middle. *TACL* 12, 157–173.

Modarressi, A., et al. (2025). NoLiMa: Long-Context Evaluation Beyond Literal Matching. ICML.

Nosek, B. A., et al. (2018). The preregistration revolution. *PNAS* 115(11), 2600–2606.

Wu, W. (2026). When Errors Become Narratives: A Longitudinal Taxonomy of Silent Failures in a
Production LLM Agent Runtime. arXiv:2606.14589.

*Compaction as Epistemic Failure*, arXiv:2607.13071 (single-author case study; used as such).

`seat-theorem-v1.md` — the Coupling Theorem and the framing regress; Premise 2's source.
`the_perturbation_principle.md` — the control requirement discharged in §9.
`The Amnesiac Institution` v0.6 — the worked institution; source of every dated example here.

---

*CC0 Universal. Draft v0.1, 2026-08-12. Extracted from a companion paper and rebuilt; §0's typing
governs citation. The evidence for E1 and E2 is thinner than the argument for A1–A4, and §10 is
written so a reader with no access to the companion institution can settle either.*
