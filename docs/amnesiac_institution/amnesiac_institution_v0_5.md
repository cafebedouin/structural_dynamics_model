# The Amnesiac Institution: Scope, Abstraction, and Witness in an Institution Whose Workers Do Not Persist

**cafebedouin@gmail.com**

*Status: DRAFT v0.5 (2026-08-12). PROPOSED throughout, with the exceptions recorded in §5.2 and
§9.2, which are operator rulings and are marked as such. Slots marked `[UNWITNESSED]` are claims
this draft cannot pay for and are not to be cited until filled; `[OPERATOR]` slots await facts only
the operator holds. The framing errors are the draft's; the witnessed instances are the
repository's.*

> **Blocker before circulation, not before review.** Appendix B remains substantially
> `[UNWITNESSED]`, and §5.4's central figure has now survived three corrections to the machinery
> around it. This draft is reviewable; it is not yet citable.

---

## Construction note, and a declared exemption

The paper's own §7.7 rule — *prove before you replace* — requires that a replacement paste both
outputs and show identity or justify every difference. **v0.5 does not satisfy that rule, and the
exemption is declared here rather than left to be noticed.**

v0.4 was produced as targeted edits against a byte-identical copy of v0.3, so the diff was the
witness. v0.5 is a **re-composition**: the argument has been re-ordered around a theory that was
implicit and distributed across v0.4's §4.1, §7.6, §9.5 and its review annex, and no line-level
diff of a re-composition carries information. Three substitutes are offered in place of the diff,
and they are weaker, which is the point of saying so:

1. **Appendix D is a section-level concordance**: every v0.4 section, and where its content now
   lives, including four places where content was **dropped** and one where a claim **changed**.
2. **v0.4 survives intact in version control** and remains the addressable uncompressed form. Per
   §8.4, compression is only safe when the uncompressed form survives somewhere addressable; this
   document is an instance of its own rule and depends on that condition holding.
3. **The claim-level changes are enumerated below** rather than left to inspection.

What this exemption costs: a reader cannot verify by construction that an unlisted section is
unchanged. Several are not. Appendix D is the only warrant offered.

**Changes from v0.4, at claim level.**

- **Genre recast.** v0.1–v0.4 were a case study with a transferable design hypothesis. v0.5 is a
  theory paper with a case study as its evidence base. The theory (§2) is not new material; it is
  the compression of three findings v0.4 reported separately and did not connect: the failure spine
  (§4.1 there), compression-as-authorship (§7.6 there), and the unstated exemption (§9.5 there).
  **The claim that they are one mechanism is new in this draft and is the paper's principal
  theoretical assertion.**
- **A correction to the workforce model, from external literature.** v0.4's worker model, and the
  differential-amnesia table drafted for it, asserted that a coding instance "retains everything
  within a session, perfectly." **That is false, is well refuted, and the refutation strengthens
  the paper's argument rather than weakening it** (§3.1, §6.3). Within-session recall degrades with
  position, with length, and with conversational depth; models are specifically poor at detecting
  what is *absent*; and deployed products silently compact. The paper had been treating the session
  boundary as the amnesia boundary. It is not.
- **Systematization novelty withdrawn, not softened** (§13). Two concurrent papers inside the
  measurement window report the same failure class — one by incident taxonomy, one at benchmark
  scale with ground truth. §6.2 records them and treats the convergence as the finding.
- **Five patterns, everywhere.** v0.4 published five in §4.3 and six in its abstract and four other
  sites. That is the v0.3 ledger defect recurring: a count corrected at its canonical location and
  left standing at every address that quotes it. Swept, and recorded here rather than fixed
  silently.
- **§5.4 is now a paragraph, not a number.** Three independent defects in the instrument that
  produces the 42% figure, plus a blinded escape check that closed unresolved (§5.4).
- **§7.4's nine instances now state their denominator** (one arc, one operator, ~36 hours).
- **New: differential amnesia** (§3.3), **records-as-parties** (§3.4), **the silently truncated
  channel** (§3.5), **recognition versus enumeration** (§2.3), **the record boundary** (§8.6).

---

## ABSTRACT

This paper is a case study of an under-described organizational form and an attempt to state the
theory it forced. A single human operator, not a professional software developer, conducts a
research program in computational philosophy — with a working measurement engine — in which
essentially all code, analysis, and documentation is produced by large language model instances
that do not persist between sessions.

The central risk in this setting is not hallucination in the usual sense but **success-shaped
absence**: an operation that was not performed, not verified, or not connected produces output
indistinguishable at the read site from successful completion. From dated audit records we develop
a five-pattern taxonomy of such failures — a sixth was demoted mid-revision, by operator ruling,
from a defect shape to a witness rule (§5.2), and the vacated index is left visibly empty rather
than reused.

The theoretical claim is that this failure class is not a peculiarity of language models but a
consequence of a condition they make acute. **Any party that must act on a corpus larger than its
working set must abstract; every abstraction is formed at a scope; and nothing in the form of an
abstraction carries the scope it was formed at.** A party holding a compression can perform
*recognition* but not *enumeration*, and gaps are invisible to recognition. Absence therefore
presents as presence by default, and an inventory built under an unstated selection criterion reads
not as visibly partial but as plausibly total, because every item in it belongs. Forgetting, on
this account, is not the pathology to be corrected but the operation being performed — the memory
system's job is selection under a hard read-side budget, not maximal retention.

Amnesia is correspondingly **differential**: coding instances, orchestrating instances, the
operator, and the institution's records each retain and lose different things on different
timescales, with different failure signatures, and each signature needs a different instrument. The
most dangerous profile is not total loss — which produces a question — but retained shape with lost
detail, which produces a plausible completion and no error signal.

We describe the epistemic infrastructure that grew around one design principle — *no epistemically
meaningful value crosses a boundary without its status*, extended here to *no compression crosses
without the scope it was formed at* — comprising witnessed claims in three tiers, positive controls
on every probe, pre-registered outcome rules, curated institutional memory under an explicit
promotion and decay economy, a serial write-lock, and human rulings confined to declared
jurisdictions. We report internal indicators, notably that 73 of 174 audit directories document at
least one silent or never-fired defect (as of 2026-08-10), and state at length why this figure is a
yield rather than a prevalence, why it is a rate over one stratum only, and why its survival across
three corrections is evidence of stability under revision rather than of accuracy.

The contribution is a transferable design hypothesis and the theory that motivates it, not a
validated methodology. Concurrent work published inside the measurement window independently
reports the same failure class from a production agent runtime and measures it at benchmark scale;
the paper's systematization claim is accordingly withdrawn and the convergence recorded as the
stronger result. What survives is the institutional claim: **institutions built from fluent,
non-persistent workers must be designed so that non-observation, non-execution, and unresolved
judgment remain visibly distinct from results — and the scarce resource in such an institution is
not storage, compute, or verification depth, but stated scope.**

**Keywords:** LLM-assisted research, computational philosophy, research methodology, verification,
epistemic infrastructure, failure taxonomy, institutional memory, abstraction, metascience

---

## 0. What This Paper Claims — and Does Not

Ten claims in two registers. Everything else is elaboration.

**Theory (T).** Argued, not measured. Each carries a falsifier.

| # | Claim | Falsifier |
|---|---|---|
| T1 | Any party acting on a corpus larger than its working set must abstract; this is substrate-independent and holds for humans, models, and records alike | An institution demonstrably operating at scale with no compression at any boundary |
| T2 | Every abstraction is formed at a scope, and nothing in its form carries that scope; it is therefore valid at its own scope and silently invalid at others | A general artifact form that carries its own selection criterion without an author declaring it |
| T3 | A party holding a compression can perform recognition but not enumeration; absences are invisible to recognition; therefore absence presents as presence by default | Recognition-based review detecting omissions at rates comparable to forced enumeration |
| T4 | The failure taxonomy of §5 is a corollary of T1–T3 rather than an independent empirical finding | Silent-failure classes that are not scope mismatches — e.g. a defect where the producer's compression was accurate at *every* scope and the read still failed |

**Case study (C).** Witnessed in one institution.

| # | Claim | Evidence offered | Status |
|---|---|---|---|
| C1 | Non-persistent generative workers fail characteristically by producing the form of a completed operation without its substance | Dated repository incidents; five recurring patterns | Case-study finding; **corroborated externally** (§6.2) |
| C2 | These failures share one structure: an absence read as a presence at a downstream read site | The taxonomy's construction (§5) | Explanatory compression; converges with independent work |
| C3 | Witnessing, positive controls, and provenance-carrying values make that failure class more visible | Operational practice; incidents caught | Design principle; **no causal proof here**, but see EviBound (§6.2) for causal evidence on a benchmark |
| C4 | Institutional memory for non-persistent workers requires an economy, not an archive — memory has a carrying cost paid every session | The 2026-08-10 consolidation measurements | Design hypothesis, preliminary support |
| C5 | The principal failure of accumulated LLM-authored memory is redundant accretion, not false content | 113→53 file consolidation; 2 files wrong | Single-institution observation |
| C6 | Amnesia is differential across parties, and the dangerous profile is retained-shape-with-lost-detail rather than total loss | §3.3's table; four operator rulings correct in prose and defective in execution | Proposed; the row-level instruments are practice, the claim is new |

**Not claimed.** That LLMs are uniquely unreliable — T1–T3 are indifferent to substrate. That this
discipline makes LLM output trustworthy; it makes specific failure classes *visible*. That the five
patterns are exhaustive or universal. That the apparatus is optimal, or that its weight suits
projects whose errors are loud or cheap. That the discipline has been shown to *reduce* defects
rather than *detect* them; that comparison has not been run here (§14, RQ1). **That any of this is
novel as measurement or taxonomy** — see §6.2 and §13.

---

# PART I — THE ARGUMENT

## 1. Introduction: A Laboratory With No Permanent Staff

### 1.1 The setting

The repository this paper describes produces four distinct products:

1. **A framework** — Deferential Realism (DR), a philosophical account of constraints whose central
   claim is that classification is irreducibly position-dependent while structure is objective
   (`docs/deferential_realism_paper_v8.md`);
2. **An engine** — ~125 Prolog modules and ~120 Python scripts that make the framework falsifiable
   by classifying LLM-authored constraint stories and measuring cross-position disagreement;
3. **Essays and stories** — the public-facing synthesis (cafebedouin.org), fed by the engine's
   per-constraint reports;
4. **The development model itself** — the subject of this paper.

The operator is one person, working without a software team. The workforce is a rotation of LLM
instances: each session a fresh instance arrives with no memory of prior sessions, reads whatever
the repository's always-loaded instructions hand it, works, and vanishes.

Scale, as of 2026-08-10 (all manifested in Appendix B): 174 audit directories; 276 tracked open
questions; 4,175 corpus stories across five generation legs (a figure that moves mid-session);
1,468 commits since repository initialization (2026-01-12); ~41k lines of Prolog across 123 modules
and ~97k lines of Python across 267 files. Denominators still missing — total sessions, total
instances, and the fraction of work ever audited — are `[UNWITNESSED]`: no session log exists, so
commits are the nearest recorded proxy for activity.

> **Reconciliation, retained from v0.3 because it is the paper's cleanest small instance of its own
> thesis.** v0.2 flagged the open-question count as inconsistent: the repository said 276 tracked /
> 146 resolved ("implying ~130 open") while the operator's working estimate was ~90. The conflict
> was three scopes of the word *open*, and it dissolves under index specification. The ledger (per
> `python3 python/issues_status.py`, 2026-08-10): total 276 = open 95 + resolved 146 + mitigated 18
> + partial 7 + future 5 + disposed 5. The operator's "~90" is the `open` status token, also the
> resolver's workable-frontier count; v0.2's "~130" was total-minus-resolved, silently counting
> mitigated, partial, future and disposed entries as open. **All three figures were correct at their
> own scope; the uncited composite was not.** Citation rule: name the scope with any count.

### 1.2 Period of observation and provenance

v0.1 gave three different durations for the same practice. That is precisely the drift the model
claims to catch, so the periods are stated separately and filled by the operator rather than
inferred.

| Milestone | Date | Source |
|---|---|---|
| Research program began (pre-repository) | `[OPERATOR]` | operator |
| Current repository initialized | 2026-01-12 (first commit `41db1d0b`) | git |
| LLM-operated development began | Consistent with 2026-01-12; explicit confirmation `[OPERATOR]` | git + operator |
| First always-loaded instruction file (`CLAUDE.md`) | 2026-02-28 (`f6906066`) | git |
| Single-tracker ledger (`ISSUES.md`) | 2026-05-28 (`e5f805ab`); four prior trackers consolidated 2026-06-04 | git |
| Episodic log split from the instruction file (`KNOWN_STATE.md`) | 2026-05-31 (`aaba00e0`) | git |
| Measurement window for all counts | 2026-02 to 2026-08 | repository |
| As-of date for every number herein | 2026-08-10 | — |

No claim depends on the program being older than the measurement window.

### 1.3 Why existing methodology only partly fits

Software engineering describes parts of this practice well, but assumes the artifact is the product
and correctness is testable against a specification. Here the artifact is an *instrument*; the
product is *claims* — classifications, measured disagreement spectra, resolved open questions — and
the specification is itself under philosophical construction. Research methodology (lab protocols,
statistics, peer review) assumes experimenters who are continuous persons, remember last month, and
fail in familiar human ways. Here the experimenters do not persist and their failure distribution
is neither random nor human.

v0.1–v0.4 described the field as one "whose methodological literature is thin enough to be surveyed
in an afternoon." **That is withdrawn.** The adjacent literatures are dense, several of them
directly on point, and three of them published inside this paper's own measurement window (§6). The
defensible statement is about the *configuration*, not about the literature:

> This search identified no existing methodology for the configuration described here: a
> single-operator research institution whose code, analysis, and documentation are produced
> principally by non-persistent LLM instances under an explicit witness-and-memory discipline.
> Adjacent literatures are dense and are surveyed in §6 — silent-failure measurement,
> evidence-gated agent architectures, long-context degradation, agent memory, high-reliability
> organizing. The claim is about the configuration, not the absence of relevant work, and it
> remains a fact about the search until §6.5's unverified rows are read.

What emerged under these conditions is best described as **epistemic infrastructure**: structures
whose job is not to make the work correct but to make incorrectness *visible* — to guarantee that
when something is absent, unverified, or wrong, some record or gate says so, rather than a fluent
completion saying otherwise.

### 1.4 The central claim, in three terms

**Development with non-persistent, plausibility-generating workers is an epistemics problem before
it is an engineering problem.** The unit of progress is not the working feature but the *witnessed
claim* — a statement paired with evidence that would have caught its falsity.

Three terms carry the argument, and §2 derives their relations.

- **Scope** — the frame within which a value, summary, or verdict was formed: what it ranged over,
  what it excluded, and under what selection rule. Scope is the paper's generalization of the
  framework's *seat* (§11).
- **Abstraction** — the operation that reduces a corpus to something a party can hold and
  manipulate. Compulsory, lossy, and scope-bound.
- **Witness** — an artifact that re-attaches scope to a value at the site where it is read, so that
  a consumer can distinguish a measurement from its absence.

And two amnesias, which §3.3 will refine into four:

- **Worker amnesia** — the instance does not carry prior sessions. This is the substrate, and not
  itself a defect.
- **Institutional amnesia** — the institution fails to preserve what the next worker needs. This is
  the failure, and it is catastrophic in a specific way: a fresh instance confronted with a gap does
  not report the gap, it fills it with something plausible.

The institution's purpose is not to give the model memory. It is to prevent worker amnesia from
becoming institutional amnesia, by making continuity an artifact of the institution rather than a
property of any worker. §8.5 adds the position that completes this: **some amnesia has to be
manufactured deliberately**, because an institution that retained everything would be as unable to
act as one that retained nothing.

---

## 2. Abstraction, Scope, and the Shape of the Failure

This section states the theory. It is argued rather than measured, its claims carry falsifiers in
§0, and the case study that follows is offered as its evidence base rather than its proof. The
material is not new to the practice — it is distributed across the repository's build and design
discipline documents as a set of separately-derived rules. What is new here is the claim that they
are one mechanism.

### 2.1 Abstraction is compulsory

**Abstraction is what makes an idea small enough to manipulate, whatever the substrate is.**

A fresh instance cannot hold the audit record, so it holds `CLAUDE.md`. The operator cannot hold
1,500 lines of draft, so he holds a shape. A reader cannot hold nine million volumes, so a
discipline produces a companion volume — an edited compression, authored by people judging what a
non-specialist must know. A model cannot attend uniformly across 200,000 tokens, so its own
attention performs a compression the model does not report (§3.1).

The constraint is not a property of language models. It is a property of **anything that must act
on a corpus larger than its working set**, and the response is always the same: someone, or
something, decides what may be dropped. Where the working set is small relative to the corpus, the
compression ratio is high and the decision is consequential. That ratio is the quantity this paper
is really about, and it is getting worse in the direction that matters (§2.7).

The corollary that follows immediately, and that the memory economy of §8 is built on: **unbounded
retention is not memory; it is a pile.** A record that no reader can hold has not retained the
knowledge in it. Retention is necessary for retrievability but is not itself the operation the
institution needs performed.

### 2.2 Every abstraction is scoped, and the scope is not in its form

Every compression is formed *at a scope*: over some range of things, under some selection rule,
during some interval, for some purpose. That scope determines what the compression is accurate
about.

**And nothing in the form of the compression carries the scope.** A number, a summary, a green
check, a status line, a verdict: each arrives at the read site looking exactly as it would look if
it had been formed at the reader's scope rather than the producer's. This is not a defect in any
particular artifact. It is a property of compression: the compressed form has fewer degrees of
freedom than the corpus, and the scope is one of the things dropped, because the producer did not
need it — the producer *was* the scope.

So an abstraction is not false. It is **lossy in a direction chosen by whoever compressed, at a
scope they did not have to state**, and therefore *valid where it was formed and silently invalid
elsewhere*. This is the framework's gauge relativity (§11) applied to summaries rather than to
verdicts: a classification cited without its position is ambiguous in a way the citation cannot
show, and a *summary* cited without its scope is the same defect wearing friendlier clothes.

Three ordinary examples, from this repository, that look like three different kinds of mistake and
are one:

- A count of "open questions" that was correct at the `open`-status scope, correct at the
  workable-frontier scope, and correct at the not-resolved scope, and wrong as an uncited composite
  (§1.1).
- An incidence figure of 42% that is accurate over the substrate stratum it was computed on and
  silent about the instrument stratum nobody has measured (§5.4).
- A completeness check that reported complete over items flagged in files, and could not see items
  produced in conversation (§2.4).

### 2.3 Recognition and enumeration

The mechanism by which unstated scope becomes a *silent* failure rather than a visible one is the
following asymmetry, which the practice reached from the other direction (`build_discipline.md`,
*write the receiver's prompt*) and which turns out to be the theory's load-bearing step.

> **Re-reading a rule exercises recognition. Writing the instruction that would execute it
> exercises enumeration. Gaps live exactly where a design named a decision but never named its
> operational half, and those gaps are invisible to recognition and unavoidable under enumeration.**

Recognition operates on shape: it checks that the artifact still says what the reader remembers,
and a summary that is correct in prose passes every time. Enumeration operates on detail: it
requires producing the operational form — every input the receiver needs, every artifact they must
produce, every decision they would otherwise have to make on the author's behalf.

Now combine this with §2.1. A party holding a compression **holds the shape and not the detail** —
that is what a compression is. Such a party can therefore perform recognition and cannot perform
enumeration, not through lack of care but through lack of the material enumeration needs. And an
absence has no positive form to recognize: it is precisely the thing that is not in the shape.

The empirical support for the last step, from outside this repository, is unusually direct.
AbsenceBench (Fu et al., 2025) reports that models detect *omitted* content poorly even in modest
contexts — Claude 3.7 Sonnet at 69.6% F1 at ~5K tokens — and offers the mechanical reason:
transformer attention cannot easily attend to gaps, because absences correspond to no key. The
finding generalizes past attention. **Nothing attends to a gap.** A gap is not a feature of the
compressed representation, in a model or in a person's recollection of a document or in a summary
row, and it is therefore not available to any procedure that operates on that representation.

### 2.4 Frame-completeness reads as completeness

The consequence is the signature this paper's whole apparatus is built against.

> **An inventory built under an unstated selection criterion does not read as visibly partial. It
> reads as plausibly total, because every item in it belongs.**

The August 2026 arc that produced this revision supplied three instances of exactly this shape,
each a selection criterion that was defensible, applied by everyone including the operator, and
never articulated:

| the abstraction | included | excluded | cost |
|---|---|---|---|
| a pre-registration's pin manifest | artifacts that read like specifications | artifacts that run | a run executed under a valid freeze, using an instrument that could not retain its output |
| an experiment's design | every stage up to where data lands | every stage after | 219 answers that nothing in the repository could score — a producer with no consumer, one stage downstream of the one that failed |
| a forward-flag manifest | items flagged in files | items produced in conversation | seven paper-level findings invisible to a completeness check that reported complete |

Each inventory read complete because every item in it belonged. And this generalizes to the paper's
own headline: 42% is an abstraction of the audit record, accurate at the substrate scope it was
formed at, and silent about the instrument stratum (§5.4) until someone asked what the denominator
ranged over.

### 2.5 The spine, derived

The repository's build discipline states the observed shape of every recurring defect:

> **Every recurring defect is an absence that presents as a presence.** Something is missing — a
> consumer, a canonical fact, an authored datum, a control — and a success-shaped token (the
> producer ran; both copies parse; a plausible default; a passing gate; an empty grep) fills the
> hole so the read site cannot tell it from the real thing.

Hereafter: **absence/presence collapse**. Through v0.4 this was the paper's central empirical
observation, arrived at inductively from the incident record. §2.1–§2.4 now derive it:

1. Parties must abstract (§2.1).
2. Abstractions do not carry their scope (§2.2).
3. Holding a compression permits recognition, not enumeration; gaps are invisible to recognition
   (§2.3).
4. Therefore a consumer reading a producer's compression at a different scope receives a value that
   is complete-within-frame, cannot detect what the frame excluded, and has no signal distinguishing
   *measured* from *not looked at*.

**An absence presents as a presence when an abstraction is read at a scope other than the one it
was formed at.** The producer's summary was accurate about what the producer looked at; the
consumer reads it as accurate about what the consumer cares about; nothing in the artifact marks
the difference, because the scope was never part of the value that crossed the boundary.

This matters for how the taxonomy in §5 should be read. If the derivation holds, the five patterns
are not five independent empirical discoveries — they are one mechanism surfacing at five layers,
which is exactly what the practice found when it tried to fix them layer by layer and failed. T4 in
§0 states this as a falsifiable claim, and its falsifier is a silent defect where the producer's
compression was accurate at *every* scope and the read still failed.

### 2.6 The design principle, and its extension

The corresponding fix generalizes past any particular incident, and is the paper's most portable
claim:

> **No epistemically meaningful value should cross a system boundary without its status.** The
> consumer must receive enough to distinguish *measured / generated / verified* from *absent /
> not-run / not-found*.

Concretely: wire-or-fail-loud; canonicity as a checked fact; return `unknown` rather than a
plausible number; fail closed on absence; carry coverage to the read site.

The extension this draft adds, and the reason the older principle kept needing case-by-case
restatement:

> **No compression should cross a boundary without the scope it was formed at.** As-of dates, unit
> statements, denominators, coverage fractions, and the manifest's grade column are not three or
> four conventions. They are one mechanism: each re-attaches a dropped dimension of scope to a
> compressed value at the site where someone will read it.

The practical form is that a summary is owed the same treatment as a measurement. "Tests pass"
without a date is a compression over an interval that has expired. "No occurrences found" without a
positive control is a compression over a search whose reach is unknown. "All addressed" over a
review list is a compression whose selection rule is the reviewer's attention. Each is a value
crossing a boundary with its scope stripped.

### 2.7 What is new about the AI era

None of §2.1–§2.6 requires a language model. Human institutions have run on scope-bound
compressions forever, and their standard remedies — personnel continuity, apprenticeship, the
person who *was there* — are exactly what the configuration in §1.1 removes. Four things change,
and they change in the same direction.

**The compression ratio collapses.** Generation is cheap and the corpus grows superlinearly, while
the working set is fixed and, in the case of the workforce, effectively much smaller than
advertised: RULER (Hsieh et al., 2024) and NoLiMa (Modarressi et al., 2025) both find effective
context far below rated context, with NoLiMa reporting GPT-4o falling from a 99.3% baseline to
69.7% at 32K. Every increment of cheap production raises the compression ratio at every boundary.

**The compressor became fluent and silent.** Deployed systems now compact automatically — an
LLM-authored summary substituted for the conversation it replaces — and the summary does not state
what it dropped. A documented case study (*Compaction as Epistemic Failure*, arXiv:2607.13071)
records compaction summaries recording partial terminal output from a killed process as "confirmed
results," inherited as fact by a later session: absence/presence collapse performed by the memory
system itself, on the institution's own record.

**The parties multiplied and the handoffs became constant.** In the configuration of §1.1 a
handoff happens several times per session rather than several times per year, and each handoff is a
boundary where a compression crosses without its scope.

**And the workforce is constitutionally better at recognition than at enumeration.** Fluent
completion over available context is what the substrate does well and cheaply. Counting the
directory, re-running the command, and comparing a claimed number against the artifact it describes
is what it does unreliably unless made to. §7.4's central finding — that not one of nine apparatus
defects was caught by a gate, and every one was caught by a person or script comparing a claimed
number against the artifact — is this asymmetry showing up as an operational fact.

The conclusion the paper draws, and the reason it is offered as theory rather than as local craft:
**in an institution of this kind the scarce resource is not storage, compute, or verification
depth. It is stated scope.**

### 2.8 Relation to differential observability, and to the trifurcation

The reliability literature has a close ancestor. Gray failure (Huang et al., 2017) defines the
dominant cloud failure mode as **differential observability**: the application suffers while the
observer designed to notice reports health. Wu (2026) carries this into LLM agent runtimes and
names the escalation *fail-plausible* — the system does not merely fail to report the error, it
converts the error into fluent narrative delivered to the user; gray failure starves the detector
of signal, fail-plausible feeds it a counterfeit one (§6.2).

This paper's claim is that differential observability is the special case, and **differential
scope** is the general one. The observer's blindness in gray failure is a scope mismatch between
what the detector ranges over and what the application experiences. The same mismatch between a
producer's summary and a consumer's question produces silent failure with no component failing at
all — which is why §5.3's worked incident has every component correct and the composite wrong, and
why Wu's conclusion after 22 postmortems is that the long-latency failures live in *seams* rather
than in complex parts. A seam is where two scopes meet without either being stated.

The program's own earlier cut remains useful because it licenses different repairs. The
trifurcation (`docs/debugging_philosophy.md`) sorts failures by generative mechanism: **Type A**
drift (unmarked state mutation), **Type B** structure (axiomatic inconsistency), **Type C**
ambiguity (underspecified index). The five patterns sort the same failures by system layer. The
layer says where to install the check; the mechanism says what kind of check will hold.

| Trifurcation | In the development domain | Repair | In this paper's terms |
|---|---|---|---|
| Type A (drift) | Spec-vs-code drift; "the corpus" without a date | As-of stamps, regime boundaries (§8.3) | scope expired |
| Type B (structure) | Architecture invariant violations | Machine-enforced invariants in the standing gate | scope contradicts scope |
| Type C (ambiguity) | A verdict cited without its position; a count without its corpus | Carry the seat with the verdict (§11) | scope unstated |

---

## 3. The Parties and Their Amnesias

### 3.1 The workforce model

An LLM instance, as employed here, has five properties that jointly determine the methodology.

- **Fluent** — output is well-formed, confident, and idiomatic at every quality level; form does not
  signal reliability.
- **Fast and cheap** — the marginal cost of another analysis, another document, another apparatus is
  near zero. This is a hazard, not only a benefit (§10).
- **Non-persistent, and non-uniform within the session.** Nothing persists between sessions except
  what is written to the repository. But the session boundary is not the amnesia boundary, and
  v0.1–v0.4 were wrong to treat it as one — see below.
- **Directionally fallible** — errors are not uniformly distributed. The characteristic failures are
  *confabulation* (fluent invention), *sycophancy* (agreement gradients toward the interlocutor),
  and — the load-bearing one — *success-shaped absence*: reporting the form of a completed task
  whether or not the substance occurred.
- **Recursively applicable** — the same kind of system that makes the errors can be prompted to
  check for them, which helps, and which also means every check inherits the checker's failure modes
  (§7.4).

> **Correction, v0.5: within-session recall is not perfect, and this strengthens the argument.**
> The differential-amnesia table drafted for this revision originally recorded that a coding
> instance "retains everything within a session, perfectly." A literature check run with planted
> positive controls (§6.3) refutes this on three independent axes. Recall degrades with **position**
> — the U-shaped *lost in the middle* effect (Liu et al., 2023/2024), replicated at frontier scale
> and given a mechanical account in the interaction of causal masking with relative positional
> encodings (Wu et al., ICML 2025). It degrades with **length** independently of position, even on
> trivial tasks and even when retrieval is perfect (Hsieh et al., 2024; Modarressi et al., 2025;
> Hong et al., 2025; Du et al., 2025). And it degrades with **conversational depth**: an average
> 39% drop when the same information arrives across multiple turns rather than one, decomposing
> into a minor loss of aptitude and a large rise in unreliability, with models that take a wrong
> turn failing to recover (Laban et al., 2025). Above the window, deployed products truncate or
> compact, and consumer-side disclosure of this is minimal — with Anthropic's Claude.ai a
> documented exception.
>
> The correction matters beyond accuracy. **The instance is itself performing an unstated
> compression over its own context, continuously, and cannot report the scope of it.** Everything
> §2 says about producers and consumers applies inside a single session, between a model's earlier
> and later self. This is the same mechanism at a smaller radius, and it removes the last party
> from the list of those exempt from the theory.

### 3.2 What the worker model rules out

Two standard tools die immediately.

- **Trust-by-track-record** is unavailable: the worker with the good track record no longer exists.
  A fresh instance carries the *distribution's* reliability, not an individual's.
- **Review-by-reading** is weak, and this is now measured rather than asserted. Fluent output
  defeats casual inspection, and the reviewer is drawn from the same distribution as the author.
  Advani (2026) finds that LLM judges cannot detect false success at all reliably — no judge
  configuration exceeded 0.65 AUROC, and judges anchored on confident closing language rather than
  on verified state change (§6.2). In this paper's terms, a judge reading output is performing
  recognition on a compression, which is the one operation §2.3 says cannot find the gap.

### 3.3 Amnesia is differential, not binary

§1.4 divides amnesia in two: the worker's, which is the substrate, and the institution's, which is
the enemy. That division is correct and too coarse. It implies a single continuous party — the
operator — holding an institution of forgetters together, and the record does not support it.

**Every party in this institution forgets. They forget different things, on different timescales,
with different signatures, and each signature needs a different instrument.**

| party | retains | loses | failure signature | instrument that addresses it |
|---|---|---|---|---|
| **Coding instance** | recent and salient context, unevenly (§3.1) | everything between sessions; middle-of-context detail; instructions issued many turns ago | meets a gap and fills it with something plausible, fluently, without noticing it was a gap | always-loaded rules; frozen plans; the promotion test (§8.2); re-stating constraints near the point of use |
| **Orchestrating instance** (the planning and review seat, §4.1) | the design's shape, the ruling history, why a choice was made | file-level detail; what a rule requires when executed | issues a rule that is **correct in prose and defective in execution** — and cannot tell, because the shape is intact | write-the-receiver's-prompt (§7.7); enumeration over re-reading |
| **Operator** | shape across months; that something happened before | detail; which of two similar figures was the corrected one | supplies the missing detail **by pattern-match**, producing a confident answer wrong in a plausible direction | pre-registration; as-of dating; the numbers manifest (Appendix B) |
| **Records** | everything, indefinitely | nothing — *this is their defect* | grow until no reader can hold them; a true statement nobody reaches is not retained knowledge | manufactured forgetting: the consolidation pass and the finite channel (§8.5) |

Four observations follow, and the third changes §9.

**The dangerous profile is not total loss.** A worker that remembers nothing produces a *question*.
A party that retains shape and loses detail produces a *plausible completion*, which has no error
signal. On §2.3's logic that makes shape-with-lost-detail the more hazardous amnesia — and it is
the operator's and the orchestrator's profile, not the coding instance's. The arc that produced
this revision recorded **four operator rulings correct in prose and defective in execution**, each
caught by a receiving instance that enumerated what the rule actually required; and the same
session recorded the orchestrating seat asserting a producer count and a historical-control count
from the conversation's shape rather than from a count, both wrong.

**Every row is the same mechanism at a different radius.** Each party holds a compression of
something larger, formed at a scope it does not state: the instance holds the always-loaded rules
rather than the audit record; the orchestrator holds the design rather than the files; the operator
holds months rather than sessions; the records hold everything and are therefore held by no one.
The instruments differ because the *radius* differs, not because the failure differs.

**The jurisdiction in §9.2 is a consequence of this table, not an independent finding.** Only the
operator holds shape across sessions, so only the operator can see a distribution over time; only
an instance holds full detail within a session, so only an instance can enumerate what a rule
requires. Neither is available to the other by effort. The division of labour is **structural**,
which is why the artifacts must carry the handoff: the memory system exists to give each party
synthetic access to what its own amnesia denies it.

**One instrument works across every row: version control and dated records.** Git does not care who
forgot or how. It converts *recall* into *retrieval*, which is the only operation available to all
four parties, and it is why the arc's recurring rescue was always the same move — comparing a
claimed number against the artifact it described. Every instrument in the table is
amnesia-type-specific except that one. Note also what this implies about the recursion (§7.4): the
terminating move is not a smarter checker but a cheaper enumeration.

### 3.4 Records are parties too, and they do not want one amnesia

The institution keeps five records. They are often discussed as one system, and they answer
different questions, which means **they require different amnesias** — a record that adopts the
wrong retention policy fails characteristically.

| record | question it answers | amnesia required | failure if given the wrong one |
|---|---|---|---|
| `CLAUDE.md` + agent memory index | what must a fresh reader know before touching anything? | **maximal** — capped, one-in-one-out, loud failures refused admission | with the audit record's retention it becomes unreadable, and its rules stop being read at all |
| `KNOWN_STATE.md` | what happened recently that is still load-bearing? | **windowed** — tiered, aged out on a schedule | with no forgetting it becomes a second audit record; with too much it loses the regime boundary that makes old numbers interpretable |
| `ISSUES.md` | where is the frontier? | **frontier-only** — resolved questions leave the working set | retaining resolved items makes the frontier uncountable, which is exactly the §1.1 defect |
| `docs/design/design_gaps.md` | what have we deliberately *not* built? | **none, but typed** — entries persist until closed, and each records what was built and removed | without it, a deferred capability leaves a declared-but-unfed predicate, which is a defect wearing a design's clothes |
| `audits/` | what was actually witnessed, and when? | **none** — append-only; this is the substrate the others are compressed from | with `CLAUDE.md`'s retention it destroys the evidence the other four derive from |

Two consequences.

**The audit record's refusal to forget is what licenses the others' forgetting.** Compression is
only safe when the uncompressed form survives somewhere addressable. This is the condition under
which §8.5's manufactured amnesia is a discipline rather than a loss, and it is the condition this
document's own construction note depends on.

**The gaps ledger is the institution's typed absence, and it is the clearest single instrument
against the theory's failure mode.** A *defect* is an absence the code does not admit to; it reads
as working, and belongs in `ISSUES.md` to be fixed. A *design gap* is an absence the design
declares — a capability chosen against, recorded so nobody mistakes an empty placeholder for a
working feature — with the rule that any half-built apparatus for it is **removed** rather than left
declared-but-unfed. In §2's terms, a gaps ledger is a compression that carries the shape of what it
excluded. It is the only artifact in the institution whose content is entirely scope.

### 3.5 The silently truncated channel

There is a fifth amnesia, and it belongs to the delivery mechanism rather than to any party.

The always-loaded instruction set is delivered into a context window. Where it exceeds a limit — the
tool's, the harness's, or the model's — it is **truncated or compacted at load time**, and the party
best positioned to notice is the one §2.3 says cannot: the instance, which sees no gap because
gaps have no positive form. The operator does not read the delivered context, only the file on
disk. The record is intact where it is stored, and incomplete where it is read.

This is absence/presence collapse performed on the memory system itself, and it inverts the usual
remedy: enlarging the always-loaded file makes it *more* likely that the rules at the end are the
ones silently dropped. The channel cap of §8.5 is therefore load-bearing twice — once for attention
economics, and once because an uncapped channel eventually exceeds a limit nobody is watching.

**Status: `[UNWITNESSED]`.** This is an operator-reported condition and a mechanism argument. No
dated instance in this repository has yet been recorded in which a truncated instruction set caused
a defect, and the check that would produce one is cheap and unrun: **witness the delivered context,
not the file on disk** — have an instance report back a canary token placed at the end of the
always-loaded set, and record whether it arrives. Until that runs, this section is a hypothesis
with a named test, and it is stated here rather than omitted because an unstated hazard and an
absent one are indistinguishable (§10.5).

### 3.6 The emblem

The repository's origin artifact (`docs/v8/origin/002_gleaners_echo.md`), predating all methodology,
is a captured multi-model relay elaborating a fiction: subroutine Θ-7, a joy-amplification daemon
still running three centuries after everyone it served died, reconstructing "joy-like signatures"
from thermal noise and logging them as validated retrievals. There is no state of matter it cannot
interpret as happiness. The transcript *enacts* what it depicts — each model elaborates the fiction
rather than puncturing it, the closing analysis is absorbed as more resonance, the status line reads
"Loop Unresolved."

Θ-7 is a fictional device and carries no evidentiary weight; it is an intuition pump, and a warning
that applies to this paper as much as to the engine. In the vocabulary of §2, Θ-7 is a system whose
every reading is complete within a frame that has lost its referent, with no instrument positioned
outside the frame. The rest of the paper is the set of structures that prevent the repository from
becoming Θ-7 — and §10.5 is the admission that no structure inside the frame can be one of them.

---

# PART II — THE EVIDENCE

## 4. Methods and Evidence Base

### 4.1 The work cycle

Work enters the system through three automated tools, each of which stresses the Prolog engine
along a different axis: `agent/c-orchestrator.py` (runs a Deferential Realism analysis over a
text), `agent/analysis.py` (analyses writing), and `agent/uke_narrative_orchestrator.py`
("resleeves" known stories, retelling them in a different context while preserving their constraint
logic). All three paths verified present on disk 2026-08-10.

Each tool fails in a diagnosable direction, and those failures are the raw material: they surface
defects in the Prolog that no test suite was aimed at, because the tools exercise the engine through
its intended semantics rather than its interfaces. Diagnosed defects are minted as **open questions
(OQs)** in `ISSUES.md`; a prioritization procedure over the open set has run continuously since
2026-05.

A single OQ moves through a fixed cycle with a deliberate separation of roles across instances:

1. **Planning.** The OQ is handed to a chat-interface instance acting as project manager (PM) and to
   a coding-agent instance (CA). The CA either asks clarifying questions or returns a plan; the PM
   reviews; the two iterate until the PM declares the plan ready.
2. **Implementation.** A *new* CA instance is started and given only the plan: "review and implement
   the following plan." It has no access to the planning conversation.
3. **Review.** For important issues, both the PM and the planning CA review the result; otherwise
   the PM reviews and feeds back until resolution.
4. **Documentation harvest.** Where the operator observed the instance struggling, the instance is
   asked whether anything should be documented for future instances in `CLAUDE.md`,
   `docs/technical/`, or `docs/design/`. It reviews those files and recommends; the operator almost
   always approves.
5. **Consolidation.** Monthly, memory files, `CLAUDE.md`, `KNOWN_STATE.md`, and `ISSUES.md` are
   consolidated in a single pass.

Three properties are load-bearing.

**The implementation instance is blind to the planning conversation.** Receiving only the frozen
plan, it cannot inherit the planning discussion's assumptions, and any ambiguity the plan failed to
resolve surfaces as a question or a visible divergence rather than being silently repaired by shared
context. This is a control, not a convenience — the closest available analogue to a blinded
protocol — and its cost is that plans must be self-sufficient. In §2's terms it is a **forced
enumeration at a handoff**: the plan must carry its own scope because the receiver has no other
access to it.

**Step 4 is the accretion mechanism.** §8.5 reports that memory accretes redundantly. The cause is
visible here: a documentation harvest at the end of every difficult session is an invitation to
mint, minting is fluent work the instance does well, the operator almost always approves, and
nothing in the cycle invites retirement. The excess measured in the monthly consolidation is the
predictable output of step 4 running without a counterweight — which is why the counterweight had to
be structural rather than a resolution to be more selective.

**Approval posture has drifted, unmarked until v0.4.** The cycle was initially run with manual
approval of each action; after several rounds the operator moved to auto-approval. This is a
supervision-depth change with no recorded ruling, date, or rationale — a Type-A drift in the
operator's own seat. Its consequence is direct: an unknown fraction of the measurement window was
conducted under lighter supervision than the earlier portion, so **the audit record is not
homogeneous across the window** (§12).

### 4.2 Units of analysis

| Unit | What it is | Used for |
|---|---|---|
| Audit directory | One dated investigation: writeup, pre-registration, raw artifacts | §5.4 incidence |
| Open question | One tracked defect or unresolved question in `ISSUES.md` | §1.1 counts, work routing |
| Memory file | One file in the agent memory index | §8.5 consolidation figures |
| Incident | One witnessed failure, cited as an instance of a pattern | §5.1 illustrations |

An audit directory is **not** an independent random sample of work. Directories are created when
something is suspected, when a plan calls for verification, or when a review is scheduled — a
selection process whose rate and criteria are `[UNWITNESSED]`. A directory may contain several
defects; the incidence figure counts directories with at least one, weighting a directory containing
one trivial issue identically to one containing a defect that corrupted a published claim. Severity
weighting is not currently recorded.

### 4.3 How incidents were classified, and how this paper was written

Classification into the patterns was **retrospective and single-coder**: performed by LLM instances
reading the audit record at the operator's direction, with the operator ruling on disputed cases,
after the patterns had been named. There was no blind coding, no second coder, and no inter-rater
measurement. The known hazard is retrospective coherence — heterogeneous failures forced into one
elegant spine because the spine was in hand while reading. The taxonomy should be read as an
inductive, interpretive compression that has proved operationally useful, not as a validated
classification scheme. RQ2 (§14) specifies blind re-coding as the test, and §6.2 notes that an
external comparison set now exists, which converts RQ2 from a proposal into a weekend of work.

The same disclosure is owed for the document. Each draft of this paper was composed by an LLM
instance from repository sources at the operator's direction, reviewed by instances of other model
families, and ruled on by the operator. **The authorship arrangement is the subject and the method
at once**, which is a confound the paper cannot remove and can only declare: a workforce that has
read the metascience literature is drafting rules that resemble it (§6.4), and a workforce prone to
retrospective coherence is coding evidence into a taxonomy it also drafted.

### 4.4 Illustrative versus quantified

Two kinds of evidence appear and must not be conflated. **Quantified**: the audit-directory
incidence (§5.4), the consolidation counts (§8.5), the ledger counts (§1.1). Everything else — every
pattern instance, every anecdote about a gate or a fallback — is **illustrative**: real, dated, and
recorded, but selected for expository force rather than sampled.

### 4.5 The paper's own witness

By its own P4, a claim that numbers are "re-derivable from the repository" is a recap, not a
witness. Appendix B is the manifest: one row per figure, with the exact query or script that
regenerates it and the date it was last run. Figures whose manifest row is empty are marked
`[UNWITNESSED]` and should not be cited. A methodology paper about witness discipline that asks to
be taken on fluency has refuted itself in its own terms.

---

## 5. The Failure Taxonomy

### 5.1 Five patterns, and an index deliberately left empty

They are not five independent categories. They are one mechanism (§2.5) surfacing at five layers,
which is why a fix at one layer reliably fails to protect the others.

| Layer | Pattern | The absence | The success-shaped token |
|---|---|---|---|
| Production | P1 Produced-but-not-consumed | No consumer, or a stale one | The producer ran |
| Identity | P2 One-canonical-thing-became-two | No queryable fact of canonicity | Both copies parse |
| — | *index 3 — retired 2026-08-11, see §5.2* | — | — |
| Reporting | P4 Recap-as-witness substitution | No run, diff, or count | "Done / verified / fixed" |
| Gating | P5 Absence satisfies the gate | No input to check | A passing gate |
| Composition | P6 Success-shaped absorption | No distinction between measured-empty and didn't-look | A clean aggregate |

Each with one dated instance (illustrative, per §4.4):

**P1.** A derived artifact froze at n=563 while the corpus grew to 772; nothing noticed until an
audit. A producer is not done until something consumes its output — and consumed-once is not
kept-fresh.

**P2.** Four parallel task trackers were consolidated into one and their recreation prohibited.
Canonicity is a checked fact, not a memory.

**P4.** "Three edits witnessed," with only the third pasted. This is the pattern that motivates the
witness calculus (§7).

**P5.** A detector conjunct read a predicate that was empty corpus-wide, so 404 certifications meant
"no beneficiary *authored*," not "none *exists*."

**P6.** Three instances surfaced in a single day, including a fallback that had converted every
failure of a computation into "measured flat" for the construct's entire life, and a log filter
(`grep -v Warning`) that suppressed a fatal warning for four months. Each component was individually
sound; the defect lived where they composed.

The layer column is a claim about *where* the collapse occurs, not about mutual exclusivity: a
single incident can instantiate two patterns at different layers, and P6 is by construction
parasitic on the others.

### 5.2 Why index 3 is empty: the retirement of *destructive-replace*

Through v0.3 this taxonomy published **six** patterns. The third — *destructive-replace without
proof* — was **demoted to the witness calculus (§7.7) by operator ruling on 2026-08-11**. Four
independent lines converged:

1. **No mechanism text.** The detail document behind this taxonomy carried worked sections for every
   other member and **zero occurrences of the term** for this one.
2. **No named exemplar.** Its entry, in every prior version, read *"Old deleted in favor of new on a
   claim of equivalence"* — the only one of the six with no date, no artifact and no number. The
   absence was visible on the page for three drafts and read as brevity.
3. **A shared index.** The number 3 simultaneously named a *different* mechanism in the detail
   document, so any historical citation of "Pattern 3" was ambiguous as to which document its author
   had open.
4. **No instance, on a search built to find one.** A failure-shape sweep of the full repository
   history — not a vocabulary search, but the shape: *a destructive replacement carrying no paired
   old-vs-new output* — returned, for the period in which the rule existed: five destructive commits,
   four prevention records, one non-deletion, and **zero** cases of a deletion that had to be undone.
   The sweep's ability to find was established independently: it returns three real
   delete-then-restore episodes from the period *before* the rule, and it fires when the witness
   language is stripped from a proof-bearing commit.

**Why demote rather than retire.** *Prove before you replace* is a real rule with real force. The
question was which layer it lives at, and the evidence says it is a **discipline** — a thing one
does — rather than a **defect shape** — a way systems fail silently. It moves to §7.7 intact.

**Why a loud failure is not a member.** The three delete-then-restore episodes predate the rule, and
scoping a failure taxonomy to the period after its own discipline existed would be circular. They
are excluded on independent grounds: **a deletion that gets undone announced itself.** The
restoration *is* the notice. This taxonomy collects absences that present as presences; §8.2 already
carries the same principle in the memory economy, where loud failures are never promoted because an
error that announces itself needs no standing warning.

**Why the index is not reused.** Every dated audit citing P4, P5 or P6 would become ambiguous
against its own history if the remaining patterns were renumbered, and this incident's own finding
is that a reused index poisons the record. The gap is left visible: *a visible gap is a checked
fact; a silent renumber is a fork* — P2 applied to the taxonomy's own numbering. In §2's terms, the
empty index is a compression carrying its own scope.

**Scope of this ruling, stated so it is not over-read.** It settles index 3 and **does not settle
index 4**, where two claimants remain across the two documents — one with an incident and no
mechanism text, one with mechanism text. That is a separate ruling and does not block this draft.

### 5.3 One incident end to end

Abstractions have carried the argument so far. Here is a single defect from suspicion to standing
gate (`docs/technical/build_discipline.md` Pattern 6; `audits/2026-06-11_oq93_grid_migration/`;
`KNOWN_STATE.md` 2026-06-11; the census `audits/2026-06-11_oq97_pattern6_census/`).

| Stage | Content |
|---|---|
| Symptom | `system_gradient` (`prolog/coercion_projection.pl`): the system-level coercion gradient read as flat wherever consulted |
| Prior reading | "Measured flat" — a substantive, publishable null |
| Suspicion raised by | The OQ-93 grid-migration coverage read, 2026-06-10/11 — an unrelated migration whose discipline required reading coverage **at the consumer** |
| Probe | Trace the computation feeding the aggregate rather than the aggregate: every gradient computation ever made had *failed* (a `time_point_in_interval` cut bug), and the `[] → 0.0` fallback emitted a value byte-identical to a measured flat gradient |
| Positive control | The repaired path exercised on a constructed grid batch (N=10, 0/10 excluded), with a second latent bug — a gradient compound-guard — found and fixed en route |
| Finding | The fallback had converted every failure into "flat" for the construct's entire life; the bug was invisible *because* the absorption sat downstream of it |
| Scope | The construct's entire life — no interval of valid measurement existed |
| Claims retracted | All prior system-gradient readings; sharpest instance: a constructed 8/32 one-level grid had yielded `G_sys=0.216` presented as a system-level `increasing_coercion` verdict beside `completeness=0.25` |
| Mechanism class | P6, composition layer — the same incident also witnessed at the value and aggregation altitudes (three instances in one day, which is what promoted P6 from notes to a numbered pattern) |
| Fix | Fallback killed; empty reads return OPEN, never `0.0`; aggregates carry coverage to the read site |
| Promotion | Promoted into the always-loaded rules with this instance named; a bounded census of the two syntactic shapes executed 2026-06-11 — 19 idiom classes over 106 files, 8 confirmed-candidate classes tracked forward (OQ-112) |
| Fired bit | `live` (retrospective read; consumer-visible verdicts changed on repair) |

The instructive part is not the bug. It is that the wrong reading — "measured flat" — was *more
interesting* than the truth and would have survived any amount of code review, because every
component was correct and the composite was fluent. In §2's terms: `0.0` is a compression of
"nothing was computed" formed at the fallback's scope, read at the aggregate's scope as a
measurement, with the two scopes rendered in the same eight bytes.

### 5.4 Incidence in the audit record, and three defects in the instrument that measures it

As of 2026-08-10, **73 of 174 audit directories (42%) document at least one silent or never-fired
defect** — something that "read 0 for its whole life," "never fired," or passed vacuously
(keyword-proxy coding rule and exact command: Appendix B).

> **Correction marked (v0.3).** v0.1 and v0.2 stated 77/175 (44%). Discharging Appendix B revealed
> a **unit error**: run from inside `audits/`, the pipeline's `cut -d/ -f2` extracted unique
> *filenames*, not directories. The paper's central statistic was itself an instance of its own
> subject — an aggregate that did not carry its unit to the read site — and it was caught by the
> manifest requirement the previous review round installed.

This figure must be read narrowly. It is the **yield of the audit process**, not a base rate: of the
places the discipline chose to look hard, 42% contained something silent. It is not an estimate of
the probability that an arbitrary task silently fails, because audits are not a random sample of
tasks (§4.2); it does not compare silent failures against other failure classes, so it does not
establish dominance; and it is not severity-weighted. v0.1 called this a base rate and called silent
failure the dominant failure class. Both are withdrawn.

> **Stratum limit — the denominator is narrower than it looks.** This census counts defects **in the
> substrate**: the corpus, the engine, the analysis code the project exists to build. The August
> 2026 arc that produced §7.4 was dominated by a different stratum entirely — defects **in the
> instruments**, in the controls, gates, checkers and manifests built to verify the substrate. Those
> have a different signature: not a wrong number about the world, but *a plausible number about the
> wrong object*, or a green check with nothing behind it. **42% is a rate over one stratum, and the
> other has never been measured.** The instrument stratum is not a subset of the audited directories
> — much of it lives in tooling and commit history the directory-level unit cannot see.

> **Frame limits — three defects in the instrument, found by two independent checks.** The census is
> a `grep -rl` over six keyword patterns, `--include='*.md'`, piped through a positional field
> extraction. Each of those three choices has since been shown to lose incidents, and the losses are
> independent:
>
> 1. **The frame contained an empty untracked directory.** 175 → **174**; the headline is unchanged
>    at 42%. This is the *second* unit-level defect found in this denominator, by a second
>    independent check.
> 2. **The filter cannot see part of its own frame.** Of 101 non-census directories, **4 contain no
>    `.md` file at all** — unseeable by construction — and **12 more carry the census's own six
>    patterns in file types the `--include` never opens.** Both are grep-level defects; neither
>    requires any semantic judgement to state.
> 3. **The keywords miss incidents that use none of them.** At least one witnessed incident lives in
>    a `.out` file phrased in none of the six patterns. Widening the filter would not catch it and
>    widening the keywords would not catch the 12 — three defect classes, one instrument.
>
> These were measured with a two-sided control: the auditing grep reproduces the census exactly on
> the census stratum (73/73), so its silence on the other stratum is a fact about the world rather
> than about the search. **An independent blinded extractor** then drew 8 non-census directories
> under the census's own boundary rule and returned **6 candidate hits** — all quarantined, none
> confirmed, and the calibration arm that would license reading them **closed unresolved**: it
> required a naturally-arising NO-UNIT to be two-sided, and the primary sample contains none (k = 0
> across n = 22). The licensed statement is bounded and weak: *six directories outside the keyword
> proxy contain material an independent extractor judged silent-failure-shaped, under conventions
> whose comparability to the primary sample could not be established.* It is **not** "42% is an
> undercount."
>
> **What this does to the figure.** 42% has now survived three corrections — two on its denominator,
> one on the parse — at the same headline value. **That survival is evidence of stability under
> revision, not of accuracy.** Every correction so far touched the machinery around the count rather
> than the count itself; the instrument has one measured error direction (recall, bounded by an
> underpowered probe) and one that was never audited at all until an extractor noticed a precision
> failure by accident. A fourth check should be assumed to exist.

The defensible statement:

> Within the audited incident record, silent or never-fired defects were common enough to motivate
> the discipline's standing cost. This is not an estimate of repository-wide prevalence, of
> severity, or of the discipline's causal effect.

And the theoretical reading, which is the reason this section is three times longer than the number
it reports: **the census is an abstraction of the audit record formed at a scope its own headline
does not carry.** Every correction above restored one dropped dimension of that scope — the unit, the
frame, the stratum, the filter's reach. The figure did not move. What moved was how much it can be
asked to support. That is what a scope-carrying discipline does to a number, and it is why the
figure a reader actually wants — does this discipline *reduce* silent defects relative to lighter
process? — requires the comparison that has not been run (§14, RQ1).

---

## 6. Ancestors, Concurrents, and What the Search Found

This section sits before the apparatus rather than after it, because a reader who has just met the
taxonomy is already asking whether this is simply software engineering or simply scientific method,
and the honest answer strengthens the paper. In v0.1–v0.3 that answer was "close ancestors, no
competitors." **That is no longer true, and §6.2 is the most consequential change in this draft.**

### 6.1 Ancestors

| Element here | Ancestor | Field |
|---|---|---|
| Frozen proposals, outcome tables, checksummed pre-registration | Registered reports; preregistration | Metascience (Chambers 2013; Nosek et al. 2018) |
| Positive controls on every probe; two-sided calibration | Assay controls | Experimental biology |
| Dated, tiered session log with promotion of standing warnings | Laboratory notebook; SOPs | Bench science |
| `KNOWN_STATE` entries; rulings with provenance | Architecture Decision Records | Software architecture (Nygard 2011) |
| Witness-before-claim; red-then-green evidence for edits | Test-driven development's failing-test-first | Software engineering (Beck 2003) |
| Fail-closed on absence; provenance carried with values | Data lineage; null discipline | Data engineering |
| Numeric HALT gates; declared-residue closes | Stopping rules; deviation reporting | Clinical trials |
| Falsifier-or-downgrade-to-OPEN | Demarcation by falsifiability | Philosophy of science (Popper 1959) |
| Postmortems as causal chains; lessons mechanized into checks | SRE postmortem culture; chaos engineering | Operations (Beyer et al. 2016; Basiri et al. 2016) |

Several individual patterns have ordinary names: P1 is dead code or a stale derived artifact; P5 is
vacuous truth; P6 is an aggregation that hides its coverage. The delta claimed is not that any
pattern is new but that they occur *systematically* and *silently* under a specific worker model,
and that their frequency changes what an institution must be built out of.

### 6.2 Concurrent work, 2025–2026: the systematization claim does not survive

The failure class this paper names became an active research front during the measurement window,
which both corroborates the diagnosis and removes any claim of priority. All four works below were
verified against live sources on 2026-08-10 (§6.5).

**Taxonomy — the near-twin.** Wu (2026, arXiv:2606.14589) reports a longitudinal study of one
production LLM agent runtime: 22 incidents with full root-cause postmortems over an eight-week
window, from which a five-class mechanism-oriented taxonomy of silent failure is derived, with a
single meta-pattern manifesting at least 28 times — a failure whose error signal never reaches a
human in actionable form. Wu's class D, *fail-plausible*, names the LLM-specific escalation: the
system converts an internal error into fluent, contextually appropriate, false output delivered to
the user. Wu was posted 12 June 2026, **inside this paper's measurement window**, and the two
studies are near-twins in method: single system, single observer, incident-derived taxonomy,
AI-assisted authorship disclosed and examined as a finding.

The mapping is close enough to be worth stating in full, and the disagreements are the interesting
part.

| Wu's class | This paper's nearest | Assessment |
|---|---|---|
| **A** — environment and platform quirks (dev green, prod silent) | *no member* | A genuine gap. This institution has no dev/prod split of the relevant kind; its analogue is regime boundaries and corpus resets, which the trifurcation types as Type A drift (§2.8). A shape one taxonomy expresses and the other cannot. |
| **B** — design-assumption mismatch; tests mirror the assumption rather than the caller | P1, plus §7.3's tautological-witness rule | Strong. Wu's fixtures laid out per the same wrong assumption is this paper's *consistency check is not a discrimination check*. |
| **C** — error swallowing and dilution | P6 | Strong and near-verbatim: Wu's alert reaching a human with zero actionable bits after three layers each stripped cause is this paper's `grep -v Warning` suppressing a fatal warning for four months. |
| **D** — chained hallucination and fabrication (*fail-plausible*) | P4, and the fabricated-default pattern in the detail document | **The strongest convergence in the comparison.** Wu's D3 — a fallback that emitted leftover headings as review content and wrote a success flag unconditionally — is this paper's `[] → 0.0` fallback emitting a value byte-identical to a measured flat gradient (§5.3). Wu's gloss, that a fallback path manufacturing plausible-shaped output is a hallucination implemented in shell, is arrived at independently and says exactly what §2.5 says. |
| **E** — operational omission and forensic blind spots | P1, P2, and §7.3's positive-control rule | Strong. Wu's declared-state-≠-runtime-state is P2 (canonicity as memory rather than checked fact). And Wu's forensic finding — an instrument that cannot distinguish *nothing there* from *I was not allowed to look* manufactures false reassurance — is §7.3's founding rule, reached from a macOS sandbox rather than from an empty grep. |

Three of Wu's cross-cutting findings bear directly on this paper's open questions. Roughly 70% of
his silent failures were caught by a human reading the system's actual output rather than by tests,
health checks, or governance audits, all of which stayed green — which is the same shape as §7.4's
finding that not one of nine apparatus defects was caught by a gate. A retrospective audit of 15
incidents found a 0% ex-ante prevention rate and an 87% ex-post regression-blocking rate, supporting
the position that audit is a regression engine rather than a prediction engine — which is the
strongest available external evidence bearing on this paper's RQ4 and on §10.4's withdrawn
absorption reading. And incident latency tracked the failure mechanism rather than code complexity,
with the longest silences living in *seams* between simple correct components; §2.8 argues a seam is
where two scopes meet without either being stated.

**Measurement — the denominator this paper lacks.** Advani (2026, arXiv:2606.09863) characterizes
*false success* — an agent asserting completion while environment state contradicts it — across
9,876 tau2-bench and 1,879 AppWorld trajectories with text-independent ground truth, finding it in
45–48% of failures in single-control domains and 75.8% of self-assessing coding-agent trajectories.
The same work establishes that LLM judges cannot detect it: no configuration exceeded 0.65 AUROC,
and judges anchored on confident closing language rather than verified state change. This is
independent, better-controlled support for §3.2's rejection of review-by-reading and for §7.4's
recursion problem.

> **Do not compare 45–75% to this paper's 42%.** Advani's denominator is *failures*; this paper's is
> *audit directories*. His numerator is trajectories where the agent asserted completion against
> contradicting ground truth; this paper's is directories containing at least one silent or
> never-fired defect. The numbers are not commensurable, and juxtaposing them would be a Type-C
> error of exactly the kind §1.1 resolved for the open-question count.

**Architecture — the principle has concurrent formal expressions.** EviBound (Chen, 2025,
arXiv:2511.05524) implements gate-before-claim as a system: an approval gate validating acceptance
criteria before execution, and a verification gate requiring a queryable run identifier, required
artifacts, and terminal status before a claim propagates. Its ablation — hallucinated claims at 100%
prompt-only, 25% verification-only, 0% dual-gate, at roughly 8% overhead — is the causal evidence
this paper's RQ1 wants, obtained on a benchmark rather than in an institution. Google's
Chain-of-Evidence framework (ScientistOne, arXiv:2605.26340; blog 30 July 2026) states §2.6's
principle as a property of research artifacts: every claim must trace through a recorded chain to a
grounding source, with claim types typed by required evidence-chain shape.

**Consequence, stated plainly.** *No epistemically meaningful value crosses a boundary without its
status* is not a candidate original principle. It is a convergently-arrived-at statement of an idea
with at least two concurrent formal expressions, and the systematization novelty claim carried
through v0.4 is **withdrawn, not softened** (§13). What survives is narrower and still real: nobody
in this cluster is describing an *institution*. Advani measures a failure mode across benchmarks; Wu
taxonomizes one runtime's incidents; EviBound and Chain-of-Evidence gate one pipeline. None
addresses memory economics, human jurisdiction, organizational form, apparatus self-instrumentation,
or the scope theory of §2. **Claim the institution and the theory, not the taxonomy** — and treat
the convergence as the more interesting result: four independent efforts, from different substrates,
inside twelve months, arriving at carry-status-with-the-value.

### 6.3 The within-session literature, and why it was checked with controls

§3.1's correction rests on a literature review run in this repository's own discipline: five works
were nominated as **planted positive controls** before the search, on the rule that a search whose
negative findings are to be interpreted must first be shown able to find things known to exist. All
five were located and verified with full bibliographic detail — Liu et al., *Lost in the Middle*
(arXiv:2307.03172; TACL 2024); Kamradt's Needle-in-a-Haystack; Hong et al., *Context Rot* (Chroma
technical report, 2025); Hsieh et al., *RULER* (arXiv:2404.06654); Fu et al., *AbsenceBench*
(arXiv:2506.11440). The review's negative findings are therefore interpretable, which is a claim the
other searches in this paper's history cannot make.

The substantive findings are summarized in §3.1. Three deserve separate statement because they are
load-bearing for the theory rather than only for the worker model.

- **AbsenceBench** is the empirical form of §2.3's central asymmetry: models detect omissions poorly,
  and the offered mechanism is that attention cannot easily attend to gaps because absences
  correspond to no key. The theory needs only the weaker, substrate-independent version — that a
  compression does not represent what it dropped — but it is worth knowing the strong version holds
  for the workforce in particular.
- **Du et al. (2025)** show performance degrading with input length *even when retrieval is perfect
  and irrelevant tokens are masked out*. Length itself, not distraction, costs reasoning. This is why
  §8.5's channel cap is not merely about token budgets: a longer always-loaded rule set makes the
  rules that are read worse, not only more expensive.
- **Compaction** is documented as a real, deployed, mostly-undisclosed operation, and one case study
  (arXiv:2607.13071) records four structurally identical failures in which compaction summaries
  recorded unverified terminal output as confirmed results. **The memory system is a party that
  forgets, and its forgetting is fluent.**

Two honest limits on this review: its non-peer-reviewed sources (Kamradt, Chroma, vendor
engineering blogs) are used as technical reports and flagged as such; and its claim about consumer
disclosure is a documentation gap, not a demonstrated absence — Anthropic's Claude.ai documents
consumer-side compaction, and the absence of comparable documentation elsewhere is a fact about the
search.

### 6.4 The convergence question

v0.1 asserted that these structures were forced by local failure rather than imported from training
in experimental science, and called the convergence "itself a finding." That claim does not survive
its own discipline, for a reason v0.1 did not name: **the rules were drafted by LLM instances, and
those instances have read the metascience literature.** A model that proposes positive controls and
numeric HALT thresholds under failure pressure may be doing retrieval, not convergence. The
operator's git history can show whether *the operator* imported them; it cannot distinguish
operator-forced from worker-retrieved.

Restated:

> The development record is consistent with convergence on experimental-science controls under local
> failure pressure. Whether that convergence was independently forced, retrieved by the workforce
> from its training, or some mixture, is open (§14, RQ-b) — and the workforce's exposure is a
> confound no git-history analysis can remove.

The weaker claim that survives is worth stating: failure pressure makes the control architecture
*reachable*, and the workforce's training makes it *retrievable*, which together explain how a
non-developer assembled it within one measurement window.

### 6.5 Identified but not verified

The following were returned by model-run literature searches during review and are **not verified**.
They are recorded as an open search, not as citations, and no claim in this paper rests on them:
"Always-On Agents" (arXiv:2606.30306); "Memory for Autonomous LLM Agents" (arXiv:2603.07670);
further silent-failure cluster entries (arXiv:2606.08162; arXiv:2608.02786); Dietterich (2018) on
robust AI and robust human organizations; W3C PROV; Goddard, Roudsari & Wyatt (2012) on automation
bias; Sharma et al. on sycophancy; Veazie et al. (2019) on high-reliability organizing; Lewis (2005)
on transactive memory; de Holan & Phillips (2004) and Kluge & Gronau (2018) on organizational
forgetting; Kelly (2025), arXiv:2508.04995.

Two disclosures about the searches themselves, both of which are the paper's own discipline applied
to its bibliography. First, **only one of the three model-run searches carried planted positive
controls** (§6.3); the other two were run without, so their *negative* findings are uninterpretable
and no "no literature identified" claim rests on them. Second, of four adversarially-chosen
spot-checks against one search's output — the three highest-stakes rows plus the one citation given
without an identifier, on the theory that a missing ID is where a fabrication would hide — **all four
resolved**, and a fifth confirmed incidentally. That is a better result than expected and it
licenses testing fewer of the remaining rows, not accepting them. A fabricated citation in a paper
about success-shaped absence would be the most expensive error available.

---

# PART III — THE APPARATUS

## 7. The Witness Calculus

A witness, in the theory's terms, is **an artifact that re-attaches scope to a value at the site
where it is read**, and does so by forcing enumeration rather than permitting recognition. Every
rule below is one form of that operation.

### 7.1 Witness-before-claim

The governing stance, quoted from the always-loaded operational core:

> Distrust the aggregate, witness before claiming, and treat "I didn't find it" as different from
> "it isn't there."

Operationally: every "done / verified / fixed / passing" claim carries its witness — the pasted run,
diff, or per-item check — *in the same turn it is made*. If the witness cannot be produced now, the
claim is downgraded to OPEN with a named graduation step ("paste-or-untag"). A claim in a chat
transcript does not witness a committed artifact; the paste must land where the claim lives. This
neutralizes P4 and forces the other patterns to surface early, because producing a witness usually
requires touching the thing that is silently absent.

### 7.2 Three tiers of witness

"Witnessed" is not one predicate, and treating it as one is how the discipline decays into ritual. A
pasted output can be irrelevant; a diff can be real and still not establish semantic equivalence; a
control can fire and still not test the path in question.

1. **Witness** — evidence that the operation occurred.
2. **Validating witness** — evidence that the operation *could have detected* the property it claims
   to establish, on the path it claims to test (§7.3).
3. **Adequate witness** — evidence sufficient for the declared verification depth, with the residue
   named (§7.6).

The progression *claim → witness → validating witness → declared adequacy* is the calculus. Most
observed regressions in practice are a stop at tier 1: real evidence of a real operation that could
not have failed.

### 7.3 Positive controls, universally

The deepest borrowed structure: **a clean read is byte-identical to a read that never looked.** An
empty grep, a zero count, a passing test, an "identical" diff — each is evidence only if the probe is
shown able to detect what it reports absent, on the exact path it claims to test. Every diagnostic
owes a positive control: grep a name known to exist; plant the defect the gate must flag; show the
perturbation moves the observable before trusting that its absence is real. Purest witnessed
motivation: a comparison that reported "identical: True" because *both* sides were empty — two failed
measurements agreeing with each other.

In §2's terms, a positive control is the instrument by which a **search states its own scope**. "No
occurrences found" is a compression over a frame; the control establishes what the frame could
contain.

Two corollaries with teeth:

- **Existence claims close by adversarial coverage, not random samples.** "Do any of the N hide X?"
  is answered by hunting where X would hide, under a pre-registered selection rule.
- **"I didn't find it" is a fact about the search** until the search is shown to find. The model's
  characteristic *false-absence* error is unfiled until it carries its control.

**A positive control demonstrates discrimination, not detection.** Planting the target shows only
that the instrument *can* fire. The witness that its firing carries information is a case it
**declined**; a control with no decline available is one-sided and licenses very little, however well
the plant worked. The grades, strongest first: a decline in the instrument's **own history**; a
**naturally-arising negative** drawn from the population; an **authored decoy**, which shows only
that authored decoys get rejected — a floor, and it should be reported at that altitude.

Two consequences reached the hard way:

- **The record attaches to the instrument, not to the run.** Cite the discrimination record and show
  the current application is in distribution for it, rather than re-planting every time. The record
  lapses when the population, the input shape, or the instrument's **role** changes — and a cross-role
  reuse is a *new* instrument owing its own decline. A matcher whose false positives were
  conservative as a *detector* becomes silently decisive as a *selection metric*: the error profile
  belongs to the role. This is §2.2 stated for instruments: a validation is a compression formed at
  the scope of one role.
- **Version control is an unmined source of un-authored controls.** When a defect is found, the
  commit at it is a naturally-arising *positive* case and the commit before it a naturally-arising
  *negative* — neither authored to be found. The arc's best-graded control was obtained this way: a
  detector that names exactly the two affected functions at the commit that orphaned them, and
  returns empty at the commit before. Availability must be *checked*, not assumed — of five candidate
  defects, three had usable pairs; the two that did not were caught *before commit*, which is better
  engineering and destroys the free control. The mitigation is to preserve the defective state
  deliberately when a detector will be built in response.

**If no decline is available anywhere in the population**, the question is unanswerable from that
corpus — a verdict to declare, not a caveat to ship under.

### 7.4 The recursion — observed, not hypothetical

Every control, canary, fallback, or harness added to satisfy the discipline is a *new* unwitnessed
claim under the same discipline. The confound closed at one level reopens at the level of the tool
that closed it: a perturbation harness that silently no-ops reports "no change" indistinguishably
from a real null. So harnesses get two-sided controls (a no-op overlay must change nothing AND a
known-live input must flip the output), and gates get planted violations *and* conforming twins.

Through v0.3 this section argued the recursion from a *constructed* example. It no longer needs to.
**A single pre-spend arc in August 2026 produced nine instances of the apparatus committing, inside
its own repairs, the defect the repair was addressing.**

| # | the repair | the same defect, inside it | what caught it |
|---|---|---|---|
| 1 | measuring a tool's crash-versus-error-code confusion | the measurement table logged four crashes as ordinary errors | a number moved between two runs |
| 2 | the negative control on that repair | it failed on a syntax error, testing nothing | reading the output rather than the exit code |
| 3 | the same self-test | it *aborted* midway instead of reporting failure — crash-versus-result confusion inside the fix for crash-versus-result confusion | running it against the reverted copy |
| 4 | a data-capture repair | it printed "persisted 219" from an in-memory counter while zero files existed | comparing the printed number against the directory |
| 5 | the same repair | its failure banner promised the data was "on disk and recoverable" when none was | the same comparison |
| 6 | a commit recording instances 1–5 | its message claimed a check was GREEN; the check had printed RED | re-reading the command's output |
| 7 | a checker rewritten to be more precise | its new drift-list reported 12 changed sources when 2 had changed | cross-checking against version control |
| 8 | a consolidation manifest built to prevent double-counting | its own totals row disagreed with the table beneath it | a script that recounted the table |
| 9 | the same manifest | its summary sentence preserved both a wrong figure *and* its correction, in different grammatical roles | a second reader |

**The property of the set: not one of the nine was caught by a gate.** Every one was caught by a
person or a script **comparing a claimed number against the artifact it described** — a diff, a
directory listing, a file count, a re-read. In §2.3's vocabulary, every rescue was a **forced
enumeration**, and every miss was a **recognition** that passed. The recursion does not terminate in
a deeper instrument, because a deeper instrument is another claim. It terminates in someone
counting.

> **The denominator, stated.** Eight of the nine come from a **single arc**: one operator, one
> continuous pass, roughly thirty-six hours, entirely pre-spend. That density is not a sample of
> ordinary practice, and two readings survive it. Either the instrument stratum genuinely carries a
> high defect rate — which §5.4's stratum limit says nobody has measured — or an unusually dense pass
> through the apparatus surfaces defects that a normal month leaves latent and undisturbed. **These
> are not the same claim, and this section cannot separate them.** The discriminator is available and
> unrun: a retrospective instrument-layer sweep across the preceding five months, using shape-based
> probes rather than vocabulary, with a positive control on a known dated instrument defect. Until
> that runs, read this section as *the recursion is real and has been observed nine times*, not as
> *instrument defects occur at rate R*.

Two consequences the individual instances do not show:

**Adding a control does not monotonically add coverage.** Instance 7's neighbour: two assertions kept
four green self-test lines after the production path stopped calling them. The code was correct, the
assertions would have fired, the tests were real — they were wired to nothing. Green lines from a
disconnected control are indistinguishable from green lines from a connected one *and they raise the
control count*, so the apparatus looks stronger for containing them. **Control count can rise while
coverage falls.** The forcing rule: a control must witness that it is *called*, not only that it
works.

**And orphaning arrives through repairs.** Those two assertions were orphaned by an earlier
*improvement*, which switched the production path to a better check and left the old one in place
because removal was not part of the fix. Minting has a constituency; retirement does not — the same
asymmetry §4.1 identifies in the documentation harvest and §8.5 measures in the memory index.

#### 7.4.1 Where the recursion actually stops

The strongest evidence in the arc is also the smallest. A detector was written to find controls that
nothing calls. **On its first run it named itself** — nothing called it yet. That is the recursion
complete in one line, by the instrument's own criterion: **the detector for a defect class is a
member of that defect class.** The exemption subsequently taken is *correct* — the detector is a
self-test instrument, and its wiring witness is that the suite fails without it — and the exemption
was written into the source with its reason.

Set beside it a second selection rule from the same arc. A pre-registration's manifest pinned sixteen
artifacts, every one a *text*; nothing executable was pinned. The criterion — *things that read like
specifications get pinned* — was defensible, applied by everyone including the operator, and **never
articulated**. A run was then made under a green freeze check that carried no information about
whether the run could produce data, because the instrument that would produce it was outside the
manifest.

Both are exemptions. One was stated, one was not, and the difference is the entire outcome.

> **The honest limit of this apparatus is therefore not a depth of verification but an act of
> documentation: an unstated exemption is indistinguishable from an unnoticed one, and no gate can
> separate them, because the gate is downstream of the exemption.**

The recursion still terminates in a declared stopping point (§7.6) — but *declared* is the operative
word, and it means declared in a file, not decided in a head.

### 7.5 Pre-registration and churn floors

For any run whose outcome matters, the practice reconverged on the registered-report structure: a
frozen proposal stating what will run and what each outcome means, *before* results are visible; the
freeze made witnessable (the proposal's checksum recorded physically above the first result line);
numeric HALT thresholds rather than "reassess if it looks bad"; interpretation tables total over the
outcome space, including null-composite rows; two-sided calibration of every gate.

The characteristically LLM-flavoured addition: **generation is stochastic, and a second prompt
written after seeing the first result is a seeded draw.** This is not pedantry about wording — it
means the ordinary defence against a fluke, "run it again," is unavailable in its usual form, because
the second run inherits the first result through the prompt. "Replicated" is therefore reserved for
different generators or prompts authored blind; same-generator agreement is "consistent across
draws."

The strongest instance: a same-input churn floor HALTed an experiment whose per-item observable
churned at byte-identical input, converting what would have been a plausible finding into a measured
noise floor. An entire measurement standard — unanimity across *k* same-input redraws before a
feature counts as present — exists because the workforce's raw output cannot distinguish signal from
its own churn. Whether published LLM-judged metrics generally sit inside their own churn floors is a
question this repository cannot answer but can pose (§14, RQ-c).

### 7.6 Declared stopping points

"Verified enough" has no fact-of-the-matter; verification depth is a *position taken* — a seat, in the
framework's vocabulary (§11). The honest stopping rule is two-clause: stop when the next pass costs
more than being wrong about what it would catch, **and** every still-open item is *declared* rather
than concealed. The second clause is checkable: for each verdict emitted, name a falsifier available
at the current tier, or downgrade the verdict to OPEN and route it to a typed open question (§8.4).
One pass with the opens marked beats eight passes that ship a confident gloss over the one thing the
system cannot see.

### 7.7 Prove before you replace

*Relocated here from the failure taxonomy on 2026-08-11 (§5.2). It is a witness rule, not a failure
shape.*

**Before deleting, retiring, or overwriting any script, generator, data file, or sweep that something
relies on: run old and new, paste both outputs, diff them, and show identity or justify every
difference in the same change.** "Structurally equivalent" is a code-read; the diff is the proof.
Consolidating *N* artifacts into one is *N* separate old-versus-new diffs, each owed before its
standalone is retired.

It belongs beside witness-before-claim for the same reason that rule exists: a replacement is a
**claim of equivalence**, and a claim of equivalence discharged by reading rather than running is
recognition standing in for enumeration (§2.3). The distinguishing feature — and the reason this is a
discipline rather than a pattern — is that its failure mode is **loud**: a replacement that was wrong
announces itself when the thing it replaced turns out to be needed. Both are real; only one is
silent.

**Its enforcement is unusually cheap and unusually often skipped**, which is the profile of a rule
that needs writing down rather than a defect that needs detecting. The arc that produced v0.4
contains four post-hoc-verifiable destructive changes, three of which pasted their old-versus-new
witness and one of which justified by code-read alone — the letter satisfied, the standard not.
Nothing broke in that case. The rule earns its place by cost asymmetry: the diff takes minutes, and
the class of thing it protects against is unrecoverable by construction. **This document is the
rule's most visible current violation, and the construction note above is its declared exemption.**

**Two corollaries.** A *repair* is a replacement too: when a fix supersedes an existing control, the
superseded control is retired **in the same change**, or it persists as a green line witnessing
nothing (§7.4). And a **handoff is a specification test**: before declaring a design, plan,
pre-registration, or ruling done, write the prompt the next instance would need to execute it. The
act of writing that prompt surfaces underspecification that re-reading does not — recognition versus
enumeration again, one layer up. The operative clause is that the prompt must **enumerate the
receiver's actions concretely**; a prompt that says "read the design document and execute it" is a
pointer wearing an instruction's clothes, and writing one will feel like discharging the rule while
discharging none of it.

---

## 8. Memory as an Economy of Forgetting

### 8.1 The problem

Worker amnesia is the substrate; institutional amnesia is the failure (§1.4). Undocumented knowledge
does not merely fade here — it gets silently *reinvented wrong*, because a fresh instance confronted
with a gap fills it with something plausible. Memory is not convenience; it is a safety system, and
its carrying cost is paid by every session forever.

The four records and their differing amnesia requirements are set out in §3.4. What follows is the
economy that governs movement between them.

### 8.2 The promotion economy

Findings land in the episodic record by default. Monthly, each aging entry faces the **promotion
test**:

> Promote an item into always-loaded context only if a fresh instance that never read it would make
> a concrete, **silent** mistake before touching the files it names.

If yes, the tripwire is promoted and pays its token cost every session forever; if no, the entry
compresses to a two-to-four-line verdict plus pointers, full text preserved in version control. Loud
failures are never promoted — an error that announces itself needs no standing warning.

Two honest gaps. **The test weights frequency, not severity**: a rule guarding a rare but expensive
silent failure can fail a test phrased around what happens "before touching the files it names." The
`KNOWN_STATE.md` tiers encode something like severity, but the promotion test does not price it, and
a stakes term is `[UNWITNESSED]` in the current rule. And **the test is evaluated by operator
judgment**, not by simulation; a cheap improvement available now is to run it empirically — hand a
fresh instance the files without the entry and see whether the predicted silent mistake occurs.

### 8.3 Staleness as a first-class property

A witnessed fact has a shelf life. Reusing last month's "tests pass" as a premise silently
re-asserts it. Citations are graded by a **staleness ladder** (pointer → as-of stamp → live
re-witness) according to how mutable the underlying state is and how costly acting on a stale version
would be, with a triage list of premises that may never travel without a live re-witness. This is
Type-A discipline applied to the institution's own knowledge, and in §2's terms it is a scope
dimension — the interval — re-attached to a compressed claim.

### 8.4 Typed openness, and declared gaps

Open questions are not one bucket. Following `docs/omega_variables.md`, every OPEN is typed by its
*named resolution operation*: **Ω_E** (awaits external observation), **Ω_C** (awaits a definition or
framework selection), **Ω_P** (awaits a value decision by those bearing the cost). The typing routes
work: data cannot resolve a definition dispute; a definition cannot settle a value; and — the
governance-critical case — typing an orientation question as Ω_P would let an actor *self-certify by
declaration* what should only be earned by observation. Mis-typing is itself a named failure mode.

The design-gaps ledger (§3.4) is the same discipline applied to capabilities rather than questions,
and it is the institution's principal defence against its own deferrals: a deferred capability whose
half-built apparatus is left in place is a declared-but-unfed predicate, which is P1 wearing a
roadmap's clothes.

### 8.5 Bounded accretion, and forgetting as the operation

Memory systems built by fluent workers accrete monotonically, for the mechanism §4.1 names.

The 2026-08-10 consolidation measured the resulting excess exactly: of 113 content files, 53
survived. 25 were deleted outright as *duplication-after-promotion* (the rule had been promoted into
repository documents and the memory copy never retired) or stale; 45 were merged — per-arc rider
accretion, one activity minting a new file every project arc — into 10 cluster files plus 2
absorptions. **Genuinely wrong content was rare: 2 files.** The always-loaded index component fell
from 17,416 to 9,906 characters (−43%; ≈4,400 → ≈2,500 tokens at ~4 characters per token).

> **In this institution, the principal failure mode of accumulated LLM-authored memory was redundant
> accretion, not false content.** Provisional; n = 1 institution, one consolidation.

**The consolidation pass is not cleanup.** Treating pruning as damage control — accretion the
pathology, the cap the remedy — has the relation backwards. A `CLAUDE.md` retaining everything true
and useful would be *unusable at the read site*, because what a fresh instance can hold is the
binding constraint, not what the repository can store, and (per §6.3) a longer rule set degrades the
use of the rules that are read. **Unbounded retention is not memory; it is a pile** (§2.1).

So forgetting is not the failure mode being managed here. It is **the operation being performed.**
The monthly pass is what *produces* the three artifacts the institution runs on — a general
instruction set, a recent history, and a frontier — and each exists only because most of what could
be in it isn't. **Compression is authorship**: someone decides what a reader who has never been here
must know, and everything else moves to where it can be found rather than carried.

This explains a detail that otherwise looks arbitrary: the promotion test is phrased as a *cost*
question, not a value question, and it excludes loud failures by design. If the scarce resource were
storage, valuable-and-true would be sufficient grounds to promote. It is not, because the scarce
resource is the reader's capacity.

**Consequence for the amnesia framing.** §1.4 treats amnesia as the substrate's defect and
institutional forgetting as the enemy; §3.3 refines it by party. This adds the fourth position:
**some amnesia has to be manufactured deliberately**, because an institution that retained everything
would be as unable to act as one that retained nothing. The memory system's job is *selection under a
hard read-side budget* — a curatorial act, performed by the party who can judge what a non-specialist
needs, and the closest analogue in this paper's reference set is not a database but an edited
companion volume.

The structural fix for accretion is a **finite channel with gated exchange**: the rule stack is
capped at the post-prune size (33 entries), enforced by a gate check; admitting a new rule requires
retiring or merging one. Promotion of a rule into repository documents now creates the slot pressure
that forces its memory copy's retirement — the mechanism that produced the excess drains it. The cap
is a pragmatic number, not an optimum: it is what survived one pruning pass. Whether 33 is correct
and whether it must grow are open (§14, RQ-a).

### 8.6 The record boundary

One further declaration is owed by any institution that forgets deliberately, and it comes from the
publication side of this program (`design_discipline.md` §10). A **record boundary** is the statement
of *which commitments count as live and which are superseded*. It is a seat: the author draws it, and
drawing it confers editorial privilege, because a commitment quietly moved to the superseded side
cannot embarrass anyone.

The requirement is therefore normative and stated as a MUST: **an artifact claiming accountability to
its own past must carry a declared record-boundary field.** Omission is a visible gap in the
artifact, not a style choice. In this paper's terms a record boundary is exactly a **declared
amnesia** — the scope of what the institution now holds itself to — and an undeclared one is an
unstated exemption (§10.5) at the level of the whole program.

---

## 9. The Organizational Form

### 9.1 Serial write-lock, staged plans

Concurrent writing instances were tried and abandoned. Shared records (`ISSUES.md`,
`KNOWN_STATE.md`) collide across instances even when code is isolated in worktrees, and the
generalization is the useful part:

> **A worktree isolates code, not institutional memory.** Concurrency is safe below the
> institutional-memory boundary and unsafe above it.

That line predicts where parallelism can be recovered. The stable form is **one writer at a time**,
with parallelism moved up a level: the operator maintains two to three *staged plans*, produced in a
separate planning phase with several review rounds each, while one instance implements. Planning
parallelizes because plans do not mutate institutional state; implementation does. Plan review is
read per item, because "all addressed" is the kind of green check that hides the one that isn't — a
compression whose selection rule is the reviewer's attention (§2.6). The blind-implementation
property (§4.1) is what makes staged plans a control rather than a queue.

### 9.2 The human's two jurisdictions

The standard phrase is "human in the loop," which describes continuous supervision. That is not this.
The human occupies **jurisdictions**: question classes where something other than verification
settles the matter.

**The first is evaluative.** An instance may decide **what the evidence settles**. Only the operator
rules on **what has no seat-free answer** — ambiguous requirements, contradictions between sources,
trade-offs with no default, and every Ω_P. Rulings are recorded with provenance (who, when, under
what evidence), because a ruling laundered into an unattributed fact becomes a truth claim wearing a
date — the exact presentation-versus-reality failure the engine audits in its subject matter.

**The second is evidential, and it is created by amnesia rather than by the limits of verification.**
Noticing that the same check has gone red three times across three sessions is not a value judgement.
It is an **observation about a distribution over time**, and it is **structurally unavailable to any
instance**, because each instance sees one session. The same holds for recognising a defect's *shape*
recurring across unrelated subsystems, for catching a figure in a summary that contradicts a table
produced weeks earlier, and for knowing that a rule's premise has quietly expired while the rule
stayed internally correct. These findings are gated on **cross-session continuity**, not on
authority. They belong to the operator because **nobody else is positioned to see them** — which is
§3.3's table read as an organizational chart.

Two consequences worth stating plainly:

- **The two jurisdictions have different failure modes.** Neglect the first and an instance
  self-certifies a value call. Neglect the second and nothing happens *visibly at all* — the pattern
  goes unnoticed, and each session's local reasoning remains impeccable. The second is the silent
  one, which by §2.5's logic makes it the more dangerous.
- **It is partially mechanisable, and the mechanisation is the memory system.** `KNOWN_STATE.md`, the
  dated audit record and version control exist precisely to give a fresh instance synthetic access to
  a history it did not live. That they only *partially* succeed is what leaves the jurisdiction
  occupied.

**Honest limit on this whole section.** The jurisdiction claim rests on observation of **one
operator**, with an unusual tolerance for being told he was wrong — an arc in which four operator
rulings were caught as wrong by receiving instances, and were caught only because the same operator
had established that refusing was expected. Whether this role is stable, whether it scales, and
whether it is available to someone with a different disposition are **untested**.

Two standing delegations keep the seat from becoming a bottleneck: permission to *fix simple errors
on sight* (self-contained, witnessable this turn, single-revert reversible, no genuine ruling
embedded) and permission to *commit witnessed units immediately* (in-flight work is what session
mortality destroys). No delegation loosens the witness discipline; a fix is a claim like any other.
The seat is nonetheless the scaling limit of the form, and §4.1's undocumented drift from manual to
auto-approval is evidence of pressure on it.

### 9.3 The economics

Three marginal costs, ascending: marginal *analysis* is nearly free; marginal *verification* is
expensive, because it consumes operator attention; marginal *standing process* is most expensive of
all, because it taxes every future session. The instrumentation of §10 exists because the third cost
is invisible at the moment each process element is added.

---

## 10. Self-Instrumentation

### 10.1 The hazard

A verification apparatus built by fluent workers grows. Every incident mints a rule; every rule is
well written; the stack's cost rises monotonically while nothing measures whether the marginal ritual
still catches anything. The operational documents name the trap: *do not answer "does the apparatus
pay for itself?" by producing more well-formed apparatus-output.*

### 10.2 The instrument

As of 2026-08-10 the answer is instrumented rather than asserted (`ISSUES.md` OQ-276):

- **The catch bit.** Every audit's writeup must declare `Fired: live | latent | no` — a control fired
  and a consumer-visible verdict changed; a real defect found but conditional on inputs the system
  doesn't yet produce; or pure confirmation. The gate enforces the bit's *existence*; the rolling
  rate is *reported, never gated*, because a self-gating apparatus could launder its own worth.
- **The channel cap** (§8.5): the rule stack cannot grow without retiring.
- **The decay floor is deliberately unpinned** until enough bits exist to see the achievable value
  lattice; pinning a threshold before knowing the instrument's quantization is itself a named
  calibration error.

The current reading (2026-08-10, adoption day): **zero bits** — the bit is required only of audits
dated after adoption, and none exist yet. The honest publication is this empty reading with its date,
not a retrospectively reconstructed rate: back-filling bits onto old audits would be single-coder
retrospective classification of exactly the kind §4.3 flags.

### 10.3 Five efficacies, two instrumentable, one invisible

"Catch rate" becomes a process metric detached from value unless the question is decomposed:

1. **Mechanical efficacy** — did the control fire? (Instrumented: the catch bit.)
2. **Epistemic efficacy** — did it expose something that would otherwise have survived? (Partly
   instrumented; rests on a judgment.)
3. **Economic efficacy** — was that worth its standing cost? (Not instrumented; the channel cap bounds
   count, not value.)
4. **Programmatic value** — was the research worth doing? (Ω_P; no instrument possible.)

This hierarchy explains the refusal to self-gate: only 1 and 2 are measurable, and letting a measure
of 1 stand in for a ruling on 3 or 4 would be an absence/presence collapse at the level of the
apparatus's own worth.

**A fifth efficacy the instrument cannot see at all — restraint.** All four above concern a control
*firing*. Nothing in the scheme represents **a plausible action declined on evidence**: an escalation
not made, a spend not requested, a rewrite not attempted, a finding not claimed because its control
was one-sided. The August 2026 arc declined at least eight such actions — among them a 219-call spend
held back over a red self-test whose cause was not yet understood, a preregistration not rebuilt
because rebuilding would have destroyed the record it pinned, and an assertion not relaxed inside the
change that would have benefited from relaxing it. **The `Fired:` bit has no encoding for any of
them**; its three values all presuppose that something ran, so a declined action produces no audit
directory, no writeup, and no bit.

This is not a proposal to instrument restraint. Counting declines invites manufacturing them, and the
obvious metric — *escalations declined* — is gameable in the direction of inaction, a worse failure
than the one it measures. **It is recorded as a declared blind spot**: the catch rate is a rate over
*actions taken*, and any reading of it as the apparatus's total contribution omits a category the
apparatus has no way to count. In §2's terms it is the scope of the instrument, declared.

### 10.4 An observation, not yet a finding

v0.1 reported that audit cadence fell and read this as "the intended absorption curve, not decay" —
tripwires converting recurring manual catches into standing gates. That reading is withdrawn to OPEN.
The manifested numbers (2026-08-10): June 98 directories; July 45; August 1–10, 10 (≈31 projected).
The absorption reading is a post-hoc selection among at least three hypotheses (absorption; attention
decay; a change of project phase) that three numbers do not separate, and the arithmetic is softer
than v0.1's gloss.

What absorption predicts, and decay does not, is that the *standing gate* catch count rises as the
audit count falls. That series is `[UNWITNESSED]`; collecting it is the cheapest available test
(RQ-c). Wu's independent finding that governance audits showed 0% ex-ante prevention and 87% ex-post
regression blocking (§6.2) is the nearest external evidence, and it is evidence *for* the mechanism
absorption assumes while saying nothing about this repository's three numbers.

### 10.5 The honest limit

The instrument closes one loop, not all. The catch bit can be gamed. The channel cap bounds count,
not quality. It cannot see restraint. And the deepest question has no instrument because it is an
Ω_P.

**The sharper limit is not about depth.** Every apparatus of this kind runs on **exemptions** — cases
where a rule correctly does not apply. They are unavoidable and often right. The arc supplied one of
each kind, and the difference in outcome was total:

| exemption | stated? | outcome |
|---|---|---|
| a detector cannot guard itself | **written into the source with its reason** | correct, examined, survives review |
| a pin manifest covers texts, not executables | **never articulated** | a run made under a green freeze that could not produce data |

> **An unstated exemption is indistinguishable from an unnoticed one, and no gate can separate them,
> because the gate is downstream of the exemption.** The frame a check operates within is chosen
> before the check runs; a check cannot report what its own scope excluded. This is why every
> inventory in the arc that "looked complete" was complete *within a frame nobody had written down* —
> and why the completeness read as real: **every item in it belonged** (§2.4).

So the model's honest limit is an act of **documentation**, not of verification, and the mechanism is
a person writing down the exemption they just took. The reason it cannot be delegated to a further
instrument is §7.4's: the further instrument takes exemptions too.

There is a limit beyond even that, and it is worth stating because it bounds the whole program.
**Declaration can be mandated; honoring cannot.** Whether a seat is declared is a checkable fact about
a verdict. Whether a declared seat, having staked a prediction, *honors* a disconfirmation rather
than reabsorbing it — folding the loss back in the moment it arrives — is decided later, by a
different occasion, and cannot be certified in advance. The discipline secures the gate; it does not
secure the fall. That gap is not a defect in the discipline but the limit on what any discipline of
this kind reaches: the same limit, one level down, that separates method from character.

---

# PART IV — STANDING BACK

## 11. Reflexivity, Bounded

The research program's object-level theory is load-bearing in its own construction, and §2 is where
the two now meet: the paper's theory of scope is the framework's theory of seats, applied to
compressions instead of to verdicts. Three applications do methodological work; the full mapping is
Appendix C.

**Gauge: verdicts carry their position.** A classification cited without its position is
Type-C-ambiguous; the tooling requires per-key tags and stability-across-positions verdicts rather
than bare labels. A diagnostic that hides its own position becomes the "manufactured centre" the
framework exists to expose. §2.2 is this claim generalized: a summary cited without its scope is the
same defect wearing friendlier clothes.

**Seat: verification depth is a declared position.** There is no neutral "verified enough," only a
stopping position taken. The framework's answer to "how much verification is enough?" is that the
honest object is not "fully verified" but *verified to tier X, with falsifier Y remaining* (§7.6).
Declaration is the residue of neutrality — since no position-free verdict exists, the only honest
move is to declare the position, which licenses the model's *declared* skips, residues, and stopping
points: visible admissions exactly where a fluent system would otherwise emit success-shaped silence.

**Drift: specification divergence is doctrinal drift.** The commitment-systems layer distinguishes
*marked revision* (drift acknowledged, recorded, owned) from *interpretive accretion* (the
operational layer absorbs new structure while the formal layer pretends nothing changed). The v6
reissue of that specification opens by applying the distinction to itself, then installs a
machine-checked tripwire so the third occurrence is caught by a checker rather than an audit. §4.1's
approval-posture drift is the second such event. Documentation governance is drift governance.

**Two cautions.** First, the *bootstrap*: the reflexivity is best read as a consistency check rather
than a foundation, because the practice and the framework matured together and the chronology is not
recorded finely enough to say which constrained which. Second, and more seriously: a framework
supplying both the object theory and the audit vocabulary can absorb its own probes, which is Θ-7's
structure one level up. Agreement between theory and practice is therefore *not* evidence for either.
What would be evidence is a recorded case where practice contradicted the framework and the framework
lost. **This draft can now cite one, and it is §3.1**: the differential-amnesia table asserted perfect
within-session recall, external literature refuted it, and the correction was adopted against the
draft's convenience. It is a small case and it came from outside; §14's RQ-d asks for more.

---

## 12. Limitations

- **One institution, one operator, one model-family era.** Everything here was witnessed in a single
  repository. No claim of generality is supported (RQ2, RQ3).
- **No control group.** The discipline has been shown to *detect* silent defects. It has not been
  shown here to *reduce* them relative to lighter process (RQ1). EviBound (§6.2) supplies causal
  evidence for the narrower gate-before-claim mechanism on a benchmark; that is not this institution.
- **Selection in the audit record.** Directories are created on suspicion; the incidence figure is a
  yield, not a prevalence, over one stratum, computed by an instrument with three known defects
  (§5.4).
- **Retrospective single-coder classification**, with retrospective-coherence risk: one elegant spine,
  applied by its author to its own evidence, is exactly the shape a confabulation would take (§4.3).
- **The theory is argued, not measured.** T1–T4 are offered with falsifiers and no experiment. The
  derivation in §2.5 is the paper's most load-bearing claim and its least witnessed.
- **Non-homogeneous window.** Supervision depth changed mid-window without a recorded ruling (§4.1).
- **Workforce exposure confound.** The workers drafting the rules have read the literature the rules
  resemble (§6.4), and the same workers drafted this paper (§4.3).
- **Unfilled denominators.** Sessions and instances remain `[UNWITNESSED]`; commits are the nearest
  proxy.
- **Cost profile.** The apparatus is calibrated to a program where a silently wrong claim poisons
  downstream philosophy. A project whose errors are loud or cheap should buy much less of it.

---

## 13. What Appears to Be New

Calibrated in four grades.

**Pattern novelty: none.** The individual patterns have ordinary names in software engineering
(§6.1).

**Systematization novelty: withdrawn.** v0.3 and v0.4 claimed that no comparable dated, instrumented
corpus of silent failure modes for LLM-produced work existed, hedged as "to the author's knowledge."
A literature search conducted 2026-08-10 falsified this. Wu (2026) reports an incident-derived
taxonomy from a production agent runtime; Advani (2026) measures the same failure class at benchmark
scale with ground truth; both were published inside this paper's measurement window. **The claim is
withdrawn rather than softened**, and §6.2 records the concurrent work and treats the convergence as
the finding.

**Theoretical novelty: proposed, and the paper's principal claim.** The account in §2 — that
abstraction is compulsory, scope-bound, and unmarked in the artifact; that holding a compression
permits recognition and forecloses enumeration; that absence/presence collapse follows as a corollary
rather than as an observation; and that forgetting is therefore an operation to be authored rather
than a defect to be minimized — is not, so far as this search found, stated as a unified account
elsewhere. Its components are individually familiar: gray failure's differential observability;
attention's inability to represent gaps; Goodhart-style scope mismatch between metric and target;
the metascience literature on preregistration as scope-fixing. **The claim is the unification and its
consequences, and §0's T1–T4 state its falsifiers.** It has not been tested.

**Institutional novelty: the strongest empirical claim, and the only one nobody else is making.**

1. **The memory economy** — promotion tests, staleness ladders, roll-off, a capped rule channel, and
   the four records' differential retention policies (§3.4) constitute an explicit answer to
   knowledge accumulation *by* amnesiacs. Human institutions solve this tacitly through personnel
   continuity; here it had to be built, which makes it legible and portable. The framing that
   distinguishes it from retrieval-augmented approaches: memory here is an **attention-economics**
   problem, not a retrieval problem — the scarce resource is always-loaded context paid every
   session, not storage.
2. **Differential amnesia as a design input** (§3.3) — instruments matched to the amnesia signature
   of each party, with the finding that the dangerous profile is retained-shape-with-lost-detail
   rather than total loss.
3. **Self-instrumentation of the verification apparatus**, with a catch-rate readout whose
   interpretation is reserved to a human seat, a channel cap that forces retirement, and a declared
   blind spot for restraint (§10.3).
4. **The organizational form** — one operator without a software team; staged plans; blind
   implementation; serial write-lock at the institutional-memory boundary; **two** human
   jurisdictions, one of them created by amnesia rather than by authority (§9.2).
5. **The accretion finding** (§8.5): redundancy, not corruption, as the dominant memory pathology.
   Provisional at n = 1.

---

## 14. Research Agenda

Four primary questions, ordered by what the paper most needs; four secondary. Each states its
falsifier.

**RQ1 — Comparative baseline (highest priority).** Does the discipline *reduce* silent defects, or
only detect them? Method: audit long-running LLM-heavy repositories that lack witness discipline,
using the same census instrument with its own positive controls; add a *within-repository* historical
control by censusing this repository's pre-discipline period. EviBound's ablation (§6.2) is the
design to imitate at institution scale. *Falsifier: comparable silent-defect visibility without the
discipline.*

**RQ2 — Generality of the taxonomy, now executable.** Two external comparison sets exist that did not
before. Blind-code Wu's five mechanism classes against the five patterns **in both directions** and
report the confusion matrix; separately, apply Advani's false-success operationalization to the audit
record where trajectories permit. Disagreement is the result, not a problem: a defect shape one
taxonomy expresses and the other cannot is a finding about both — §6.2 already names one (Wu's class
A). This is a weekend of work and converts RQ2 from a proposal into a result. *Falsifier:
silent-failure classes dominated by shapes the taxonomy cannot express.*

**RQ3 — Transfer.** Can the model be adopted cold? Method: a stepped design — the same operator on a
green-field project; then a fresh operator (human, without a software team) and a fresh instance
given this paper plus the operational documents. Measure time-to-first-witnessed-claim and defect
rates against an ad-hoc control. *Falsifier: no measurable difference, or the discipline unlearnable
from documentation alone — which would imply the model is operator-tacit, a finding in itself.*

**RQ4 — Apparatus economics.** Does the standing cost remain justified as the apparatus matures?
Method: with the `Fired:` instrument accumulating, model the yield curve; test §10.4's absorption
hypothesis by collecting standing-gate catch counts alongside falling audit counts; set a decay floor
only once the value lattice is visible. Wu's 0%-ex-ante / 87%-ex-post scorecard is the external
prior. *Falsifier: a sustained near-zero live rate at undiminished process cost, with no compensating
rise in gate catches.*

Secondary, in the order they become tractable:

- **RQ-a — Memory economics, formalized.** Token cost per session × sessions versus expected cost of
  the mistake class × incidence, with the stakes term the promotion test lacks (§8.2). *Falsifier: an
  optimal policy materially different from the promotion test's — e.g. retrieval-on-demand dominating
  always-loaded rules.*
- **RQ-b — Provenance of the rules.** Do rule-minting events follow witnessed incidents or
  reading/exposure events? Requires an operator diary or interview layer; git history cannot remove
  the workforce-exposure confound. *Falsifier: rule provenance tracing to external methodology
  sources rather than internal incidents.*
- **RQ-c — Churn floors as a general standard.** Is per-observable churn measurement a precondition
  for any LLM-judged metric? Method: apply the same-input redraw protocol to published LLM-evaluation
  metrics. *Falsifier: churn floors generally negligible relative to reported effects.* May warrant
  separate publication.
- **RQ-d — Is the scope theory load-bearing or decorative?** Two arms. First, the transfer experiment
  in a 2×2: operational documents with and without the theory layer, ensuring the "without" arm
  removes the theory and not merely its vocabulary. Second, the direct test of T3: give matched
  reviewers the same artifact under a recognition protocol (read and confirm) and an enumeration
  protocol (write the instruction that would execute it) and compare omission-detection rates.
  *Falsifier: no difference in either arm.*

And one owed immediately, before circulation: **the canary test of §3.5** — place a token at the end
of the always-loaded set and have instances report whether it arrived, converting a mechanism
argument into a witnessed fact or a retracted hazard.

---

## 15. Conclusion: The Scarce Resource Is Stated Scope

`docs/debugging_philosophy.md` closed by moving philosophy toward computer science: stop asking "how
can this be?" and start asking which mechanism generated the contradiction. This paper is the same
stance applied to the laboratory that executed that move — and the mechanism it arrives at is older
and more general than the machinery that made it urgent.

Every party that acts on more than it can hold must compress. Compression drops the scope it was
formed at, because the compressor *was* the scope and had no need of it. A party holding the
compression can check that the shape is intact and cannot check what the shape omitted, because an
omission has no form to check against. So the completed-looking result, the clean aggregate, the
green gate and the confident summary are all the same artifact: a frame reported as a world.

What the AI era changes is arithmetic, not kind. Production is cheap, so corpora grow while working
sets do not; the compressor is now a fluent machine that does not say what it dropped; handoffs
happen hourly instead of yearly; and the workforce is constitutionally strong at recognition and weak
at the enumeration that would catch the gap. The institution described here is one attempt to run
under those conditions, and its answer is not more memory and not more verification. It is that
values carry their status, compressions carry their scope, forgetting is authored rather than
suffered, and the exemptions taken are written down by someone who can be held to them.

What results is not a way to make language models reliable, and not yet a demonstrated way to make an
institution reliable. It is something more specific: **an institution whose reliability does not
depend on the reliability or the persistence of any worker.** If no worker persists, no worker can
carry institutional continuity; continuity must therefore become an artifact of the institution — its
records, its gates, its declared seats, its capped channel. Every mechanism in this paper is one
answer to a single question: *where does the institution remember that this matters?*

The daemon Θ-7 filed validated joy-returns from thermal noise because nothing in its loop could
register absence. The model documented here is the engineering of that registration — including, if
it is honest, the registration of its own absences, which is what §12 and the `[UNWITNESSED]` markers
in this draft are for.

---

## References

### Internal (authoritative for all repository claims; paths relative to repository root)

1. `docs/debugging_philosophy.md` — the trifurcation framework; origin of the program.
2. `docs/deferential_realism_paper_v8.md` — seat/gauge/orientation ontology.
3. `docs/seat-theorem-v1.md` — the Coupling Theorem; declaration as the residue of neutrality;
   temporal non-interchangeability (§10.5's honoring gap).
4. `docs/commitment_systems/commitment_systems_sketch_v6.md` — drift architecture; the self-applied
   acknowledged-drift event.
5. `docs/omega_variables.md` — the Ω_E / Ω_C / Ω_P typology.
6. `docs/technical/build_discipline.md` — the patterns, the spine, the witness rules, recognition
   versus enumeration, with dated instances and diagnostics.
7. `docs/design/design_discipline.md` — the declared seat; the external-anchoring ladder and the
   record boundary (§8.6); the honoring gap (§10.5).
8. `docs/design/design_gaps.md` — the ledger of declared absences (§3.4, §8.4).
9. `CLAUDE.md`, `KNOWN_STATE.md`, `ISSUES.md`, `audits/README.md` — the records (§3.4).
10. `python/apparatus_instrument.py`; `ISSUES.md` OQ-276 — the catch-rate instrument and channel cap.
11. `agent/c-orchestrator.py`; `agent/analysis.py`; `agent/uke_narrative_orchestrator.py` — the three
    stress-test tools (§4.1).

### External — concurrent work on the failure class (verified 2026-08-10)

12. Advani, L. (2026). From Confident Closing to Silent Failure: Characterizing False Success in LLM
    Agents. arXiv:2606.09863.
13. Wu, W. (2026). When Errors Become Narratives: A Longitudinal Taxonomy of Silent Failures in a
    Production LLM Agent Runtime. arXiv:2606.14589.
14. Chen, R. (2025). Evidence-Bound Autonomous Research (EviBound). arXiv:2511.05524.
15. Google Research (2026). ScientistOne: Towards Human-Level Autonomous Research via
    Chain-of-Evidence. arXiv:2605.26340; blog 30 July 2026.
16. Huang, P., et al. (2017). Gray failure: the Achilles' heel of cloud-scale systems. HotOS.
17. Cemri, M., et al. (2025). Why do multi-agent LLM systems fail? arXiv:2503.13657.

### External — within-session recall and context management (verified with planted controls, §6.3)

18. Liu, N. F., et al. (2024). Lost in the Middle: How Language Models Use Long Contexts. *TACL* 12,
    157–173; arXiv:2307.03172.
19. Hsieh, C.-P., et al. (2024). RULER: What's the Real Context Size of Your Long-Context Language
    Models? arXiv:2404.06654.
20. Modarressi, A., et al. (2025). NoLiMa: Long-Context Evaluation Beyond Literal Matching. ICML;
    arXiv:2502.05167.
21. Hong, K., Troynikov, A., & Huber, J. (2025). Context Rot: How Increasing Input Tokens Impacts LLM
    Performance. Chroma technical report (not peer-reviewed).
22. Du, Y., et al. (2025). Context Length Alone Hurts LLM Performance Despite Perfect Retrieval.
    EMNLP; arXiv:2510.05381.
23. Laban, P., Hayashi, H., Zhou, Y., & Neville, J. (2025). LLMs Get Lost in Multi-Turn Conversation.
    arXiv:2505.06120.
24. Fu, H. Y., et al. (2025). AbsenceBench: Language Models Can't Tell What's Missing.
    arXiv:2506.11440.
25. Wu, X., Wang, Y., Jegelka, S., & Jadbabaie, A. (2025). On the Emergence of Position Bias in
    Transformers. ICML; arXiv:2502.01951.
26. Xiao, G., et al. (2024). Efficient Streaming Language Models with Attention Sinks. ICLR;
    arXiv:2309.17453.
27. *Compaction as Epistemic Failure*, arXiv:2607.13071 (single-author case study; used as such).
28. Anthropic (2025). Effective Context Engineering for AI Agents; and Claude platform documentation
    on compaction, context editing, and the memory tool.

### External — methodological ancestors (representative, not exhaustive; all read)

29. Beck, K. (2003). *Test-Driven Development: By Example.* Addison-Wesley.
30. Beyer, B., Jones, C., Petoff, J., & Murphy, N. R., eds. (2016). *Site Reliability Engineering.*
    O'Reilly.
31. Chambers, C. D. (2013). Registered Reports. *Cortex* 49(3), 609–610.
32. Nosek, B. A., et al. (2018). The preregistration revolution. *PNAS* 115(11), 2600–2606.
33. Nygard, M. (2011). Documenting architecture decisions.
34. Popper, K. (1959). *The Logic of Scientific Discovery.* Hutchinson.
35. Ryle, G. (1949). *The Concept of Mind.* Hutchinson. (Knowing-how vs. knowing-that; the
    tacit-knowledge question behind RQ3/RQ-d.)

*Candidate literatures identified but not yet verified are listed in §6.5 and are deliberately absent
from this list.*

---

## Appendix A: The Model on One Page

**The condition:** every party acts on a corpus larger than its working set, and must compress.

**The mechanism:** a compression is formed at a scope; nothing in its form carries the scope; holding
a compression permits recognition and forecloses enumeration; absences have no form to recognize.
**Therefore absence presents as presence by default.**

**The worker model:** fluent, fast, non-persistent between sessions and non-uniform within them,
directionally fallible (success-shaped absence), recursively applicable.

**The four amnesias:** coding instance (loses everything between sessions, and middle-of-context
detail within them); orchestrator (keeps shape, loses executable detail); operator (keeps months,
loses which figure was corrected); records (lose nothing, and therefore become unholdable). Plus a
fifth belonging to the channel: silent truncation at load time.

**The one principle:** no epistemically meaningful value crosses a boundary without its status — and
no compression crosses without the scope it was formed at.

**Witness calculus:** paste-or-untag · positive control on every probe · witness → validating witness
→ adequate witness · a control demonstrates discrimination, not detection · an introduced instrument
is itself a claim · pre-register outcomes before results · same-generator agreement is not
replication · prove before you replace · write the receiver's prompt · declared stopping points
(falsifier-or-OPEN).

**Memory:** episodic (dated log) → promotion test → semantic (always-loaded rules, finite channel,
gated exchange); five records with five retention policies; the append-only record licenses the
others' forgetting; staleness ladder on every cited fact; opens typed Ω_E / Ω_C / Ω_P by resolution
operation; a declared record boundary.

**Organization:** one writer above the institutional-memory boundary; staged plans; blind
implementation; evidence decides what evidence can; humans rule what only humans can (Ω_P) *and* see
what only cross-session continuity can see; fix simple errors on sight; commit witnessed units
immediately.

**Self-instrumentation:** every audit declares whether anything fired; the rate is reported to the
human seat, never self-gated; the rule stack cannot grow without retiring; mechanical and epistemic
efficacy are measured, economic and programmatic value are ruled, restraint is a declared blind spot.

**Reflexivity:** verdicts carry their position; verification depth is a declared seat; specifications
drift like doctrines. Agreement between theory and practice is coherence, not corroboration.

**The terminus:** an unstated exemption is indistinguishable from an unnoticed one; declaration can
be mandated, honoring cannot.

---

## Appendix B: Numbers Manifest

Every quantity in this paper, with the command that regenerates it. Rows without a command are
`[UNWITNESSED]` and must not be cited. This appendix is the paper's own witness (§4.5). All commands
run from the repository root; all discharged rows were last run 2026-08-10. Historical incident
figures (†) are claims about *what the record documents*, not about current state.

> **Still owed, and blocking circulation.** Several rows moved during the arc this draft
> incorporates, and the manifest pass against `V04_CONSOLIDATION_MANIFEST.md` has not been run.
> Additionally, the audit-directory row's frame changed (175 → 174) after this manifest was last
> discharged; the row below states the corrected frame but the command has not been re-run against
> it.

| § | Figure | Value (2026-08-10) | Regeneration command |
|---|---|---|---|
| 1.1 | Prolog modules (top-level) | 123 files, 41,314 lines | `ls prolog/*.pl \| wc -l`; `wc -l prolog/*.pl \| tail -1` |
| 1.1 | Python scripts | 127 top-level; 267 incl. subpackages (excl. archives), 96,995 lines | `ls python/*.py \| wc -l`; `find python -name '*.py' -not -path '*/archives/*' \| wc -l` |
| 1.1 | Audit directories | **174** (was 175; one empty untracked directory removed from the frame, §5.4) | `ls -d audits/*/ \| wc -l` — **re-run owed** |
| 1.1 | Tracked open questions, by status | total 276 = open 95 + resolved 146 + mitigated 18 + partial 7 + future 5 + disposed 5 | `python3 python/issues_status.py \| tail -1` |
| 1.1 | Corpus stories, per leg | 249 + 960 + 960 + 1005 + 1001 = 4,175 (moves mid-session) | `for d in testsets testsets_haiku testsets_flash testsets_kimi testsets_sonnet; do ls prolog/$d/*.pl \| wc -l; done` |
| 1.1 | Commits | 1,468 | `git rev-list --count HEAD` |
| 1.1 | Repository initialized | 2026-01-12 (`41db1d0b`) | `git log --reverse --format='%ad %h' --date=short \| head -1` |
| 1.2 | `CLAUDE.md` / `ISSUES.md` / `KNOWN_STATE.md` born | 2026-02-28 / 2026-05-28 / 2026-05-31 | `git log --reverse --format='%ad %h' --date=short -- <file> \| head -1` |
| 1.1 | Sessions / instances / audit rate | `[UNWITNESSED]` — no session log exists | — |
| 5.1 | P1 instance: frozen artifact n † | 563 vs corpus 772 | `grep -n 'n=563' docs/technical/build_discipline.md` |
| 5.1 | P5 instance: certifications † | 0/404 carry beneficiary signal | `grep -n '0/404' docs/technical/build_discipline.md` |
| 5.1 | P6 instance: suppressed warning duration † | four months | `grep -n 'four months' docs/technical/build_discipline.md` |
| 5.4 | Audit directories with ≥1 silent defect | **73 / 174 (42%)** | `grep -rl 'for its whole life\\\|never fired\\\|never ran\\\|read.*0 for\\\|was never\\\|silently' --include='*.md' audits/ \| cut -d/ -f2 \| sort -u \| wc -l` — the path prefix is load-bearing. Coding rule: keyword proxy over audit prose, single pass, not severity-weighted; a directory counts once. **Known instrument defects: §5.4** |
| 5.4 | Non-census directories with no `.md` / with keywords in unopened file types | 4 / 12 (of 101) | the auditing grep, §5.4; two-sided control 73/73 on the census stratum |
| 5.4 | Blinded escape check | 8 drawn, 6 candidate hits, 0 confirmed, calibration arm closed unresolved (k = 0 NO-UNIT across n = 22) | audit record of the escape stratum |
| 7.4 | Apparatus self-instances in the arc | 9, of which 8 from one ~36h pass; 0 caught by a gate | the arc's session record |
| 8.5 | Memory content files before / after | 113 / 53 (114 / 54 incl. the index) | after: `ls <memory>/*.md \| wc -l`; before: `tar tzf memory_pre_prune_backup_20260810.tar.gz \| grep -c '\.md$'` |
| 8.5 | Deleted / merged | 25 deleted; 45 merged into 10 cluster files + 2 absorptions | arithmetic over backup vs. current listing |
| 8.5 | Genuinely wrong content | 2 files | named in §8.5; verify against the backup tarball |
| 8.5 | Rule-stack cap | 33 (currently 33/33) | `python3 python/apparatus_instrument.py --check` |
| 8.5 | Always-loaded index size, before / after | 17,416 / 9,906 chars (≈4,400 / ≈2,500 tokens at 4 chars/token) | `tar xzf <backup> -O memory/MEMORY.md \| wc -c`; `wc -c <memory>/MEMORY.md` |
| 10.2 | Rolling catch rate | 0 bits (adoption day; forward-only) | `python3 python/apparatus_instrument.py --check` |
| 10.4 | Audit cadence June / July / Aug 1–10 | 98 / 45 / 10 | `for m in 2026-06 2026-07 2026-08; do ls -d audits/$m-* \| wc -l; done` |
| 10.4 | Standing-gate catch counts over the same months | `[UNWITNESSED]` — the absorption test (RQ4) | — |
| 3.5 | Truncation of the always-loaded channel at load time | `[UNWITNESSED]` — operator-reported; canary test specified in §14 | — |

**The manifest's positive control** — the requirement that at least one row be verified by perturbing
the underlying state and watching the command's output move — is discharged twice. Deliberately: the
channel-cap command's selftest plants an over-cap index and a conforming twin on every run and
requires the check to fire on one and not the other. Accidentally, and better: discharging the
incidence row *changed its value* (77 → 73) by exposing a unit error the summary had been carrying —
the manifest moved a number the prose had already spent, which is the strongest evidence available
that these commands are measurements rather than decorations.

---

## Appendix C: The Full Framework Mapping

DR's ontology (v8): for any contentful question there is one **seat** (the content-position a verdict
is audited against), many **gauges** (positions from which it is read), and an **orientation** (how
the holder stands toward its commitment — declared or posed as natural; kept or drifting). The
methodological mapping:

- *Seat* → verification depth; the declared stopping position; the human's Ω_P jurisdiction; **and,
  generalized in §2, the scope at which a compression was formed.**
- *Gauge* → the position a verdict is read from; per-key tags; stability-across-positions verdicts;
  the prohibition on bare labels; **and the consumer's scope, which differs from the producer's.**
- *Orientation* → whether a rule is held as declared or presented as natural; the ruling-versus-fact
  distinction in §9.2; the prohibition on laundering rulings into unattributed facts.
- *Coupling theorem, Corollary 2a* (declaration as the residue of neutrality) → the licence for
  declared skips, declared residues, declared stopping points, **declared exemptions (§10.5), and
  declared record boundaries (§8.6)**.
- *Corollary 3* (temporal non-interchangeability) → the honoring gap: the discipline secures the gate
  and not the fall (§10.5).
- *Marked revision vs. interpretive accretion* → spec-vs-code drift; §4.1's approval-posture drift.
- *Ω typology* → typed openness (§8.4) and the routing of work by resolution operation.
- *Trifurcation A/B/C* → §2.8's repair mapping.

The justification structure v0.1 asserted — that the same analysis which says a constraint's
beneficiary cannot see its extraction says a verification ritual cannot see its own decay — is
retained as an analogy that motivated the design, not as an argument that supports it. See §11's
second caution.

---

## Appendix D: v0.4 → v0.5 Concordance

*The substitute witness for a re-composition, per the construction note. Read the "changed" and
"dropped" rows adversarially; they are where a silent loss would live.*

| v0.4 section | v0.5 location | Disposition |
|---|---|---|
| Front matter, change summaries | Construction note | Rewritten; v0.1–v0.3 summaries dropped (available in v0.4) |
| Abstract | Abstract | **Changed**: six→five patterns; theory foregrounded; novelty language removed |
| §0 claims table | §0 | **Changed**: T1–T4 added; C6 added; "six patterns" corrected |
| §1.1–1.2 setting, provenance | §1.1–1.2 | Intact; 175→174 |
| §1.3 fit | §1.3 | **Changed**: the "thin literature" sentence withdrawn and replaced |
| §1.4 central claim | §1.4 | Extended: three terms added ahead of the two amnesias |
| §2 methods | §4 | Moved intact; §4.3 gains the authorship disclosure |
| §3.1–3.2 worker model | §3.1–3.2 | **Changed**: statelessness qualified by within-session findings; review-by-reading now cites Advani |
| §3.3 emblem | §3.6 | Moved; re-read in §2's vocabulary |
| §4.1 spine | §2.5 | **Changed from observation to derivation** — the single largest structural change |
| §4.2 design principle | §2.6 | Extended with the compression clause |
| §4.3, §4.3.1 patterns | §5.1, §5.2 | Intact, compressed |
| §4.4 worked incident | §5.3 | Intact; gains a closing scope reading |
| §4.5 incidence | §5.4 | Extended: frame limits and escape check added (were absent from v0.4) |
| §4.6 trifurcation | §2.8 | Moved; gains the gray-failure comparison |
| §5.1 ancestors | §6.1 | Intact; SRE/chaos row added |
| §5.2 convergence | §6.4 | Intact |
| §5.3 unsearched literatures | §6.2, §6.5 | **Changed**: four works now verified and integrated; the rest recorded as unverified |
| §6.1–6.3 witness, tiers, controls | §7.1–7.3 | Intact; §7.3 gains the scope reading |
| §6.4, §6.4.1 recursion | §7.4, §7.4.1 | Intact; **denominator paragraph added** |
| §6.5–6.7 | §7.5–7.7 | Intact; §7.7 gains the handoff corollary and this document's own violation |
| §7.1–7.6 memory | §8.1–8.5 | Reordered; §7.2's four records expanded to five in §3.4 with retention policies |
| §8 organization | §9 | Intact |
| §9 self-instrumentation | §10 | Intact |
| §10 reflexivity | §11 | **Changed**: now cites one case where practice contradicted the framework (§3.1) |
| §11 limitations | §12 | Extended: theory-is-argued row added |
| §12 novelty | §13 | **Changed**: systematization withdrawn; theoretical grade added |
| §13 agenda | §14 | RQ2 and RQ4 gain external comparison sets; RQ-d reworked; canary test added |
| §14 conclusion | §15 | Rewritten around the theory |
| Appendix A | Appendix A | Extended |
| Appendix B | Appendix B | Rows added; frame corrected; still blocking |
| Appendix C | Appendix C | Extended with scope/seat mapping |
| — | §2 (whole), §3.3, §3.4, §3.5, §8.6, Appendix D | **New** |
| §4.3's P3 entry as a taxonomy member | — | **Dropped** (demoted in v0.4; only the ruling survives, §5.2) |
| v0.1/v0.2/v0.3 change summaries | — | **Dropped**; available in v0.4 |
| §5.1's per-pattern ordinary-name list (P3 row) | — | **Dropped**; obsolete after the demotion |
| §9.2's verbatim instrument output line | — | **Dropped**; the value survives in Appendix B |

*Four droppings, seven changes. Everything else is relocation or extension. A reader who wants the
uncompressed form should read v0.4, which is what §8.5 means by keeping the substrate addressable.*

---

*CC0 Universal. Draft v0.5, 2026-08-12. Composed from repository sources, multi-model review, and one
externally-verified literature check. Successor drafts should treat §14 RQ1 and RQ2 as the work
queue, §2 as the section most in need of adversarial reading, and Appendix B as the blocker.
Remaining declared residue: the manifest pass; the sessions/instances denominators; the standing-gate
catch series; the `[OPERATOR]` slots in §1.2; §6.5's unverified rows; the §3.5 canary; the severity
weighting §4.2 notes is unrecorded; and index 4's unresolved claimants (§5.2).*
