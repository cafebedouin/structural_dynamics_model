# The Amnesiac Institution: A Case Study in Epistemic Infrastructure for Research Conducted with Stateless Language Models

**cafebedouin@gmail.com**

*Status: DRAFT v0.4 (2026-08-11). PROPOSED throughout — no operator ruling has adopted this
document's claims as canonical, with **one exception**: the demotion of the taxonomy's third
pattern (§4.3.1) is an operator ruling of 2026-08-11 and is recorded as such. It is a
synthesis-for-humans of a methodology that exists in
operational form across `CLAUDE.md`, `docs/technical/build_discipline.md`,
`docs/design/design_discipline.md`, and the audit record. v0.1 composed by an LLM instance
from repository sources at the operator's direction; v0.2 incorporated multi-model review;
v0.3 discharged the numbers manifest against the repository; this revision incorporates the
August 2026 pre-spend arc. Every empirical number
carries its as-of date; Appendix B is the manifest that discharges each one. Slots marked
`[UNWITNESSED]` are claims this draft cannot yet pay for and are not to be cited until
filled; slots marked `[OPERATOR]` await facts only the operator holds. The framing errors
are the draft's; the witnessed instances are the repository's.*

> **Blocker before circulation, not before review:** Appendix B remains substantially
> `[UNWITNESSED]`, and several of its figures moved during the arc this revision incorporates.
> The manifest pass is owed and its input exists (`V04_CONSOLIDATION_MANIFEST.md`). This draft is
> reviewable; it is not yet citable.

> **Construction note, because the paper's own §6.7 requires it.** v0.4 was produced as a set of
> targeted edits to a byte-identical copy of v0.3, not by re-composition. Every section not listed
> in the change summary below is unchanged **by construction rather than by inspection**, and the
> complete change set is available as a diff. This is *prove before you replace* applied to the
> document that promotes the rule.

**Changes from v0.1 (summary).** Genre recast from general methodology to case study plus
transferable design hypothesis. New §2 (methods and evidence base) states the work cycle,
units of analysis, and coding procedure. The audit-record incidence figure is restated as
yield-within-audits rather than base rate. Novelty and convergence claims downgraded to
open questions with named falsifiers. Inherited mechanisms moved before the novelty claims.
Reflexivity compressed to three load-bearing applications, remainder to Appendix C. Research
agenda reduced to four primary questions. Numbers manifest added.

**Changes from v0.2 (summary).** Appendix B discharged: every repository-derivable figure now
carries its regeneration command, run 2026-08-10. **One headline correction the discharge
itself produced: the incidence figure is 73/175 (42%), not 77/175 (44%) — the v0.1/v0.2
command's `cut -d/ -f2` counted unique filenames, not directories, a unit error at the
paper's own central statistic** (§4.5 carries the correction block; the two repository
records that had cited 44% were corrected the same day). The §1.1 ledger reconciliation is
resolved — the conflict was three scopes of the word "open" (status token / not-resolved /
workable frontier), a Type-C ambiguity dissolved by specifying the index (§1.1). §1.2
timeline filled from git. §4.4's worked incident filled from the OQ-93/OQ-96 record. §7.6
prune figures made exact. §9.2 reports the current (empty, day-zero) catch-rate reading.
Corpus and cadence figures updated to the manifest values. Entry-point tool paths verified
on disk.

**Changes from v0.3 (summary).** This revision incorporates a single dense pre-spend arc
(2026-08-10/11) that produced more instrument-level evidence than the preceding five months.

- **§4.3 publishes five patterns, not six.** The third — *destructive-replace without proof* —
  was demoted by operator ruling to the witness calculus (§6.7); **index 3 is left visibly empty
  and the remaining patterns are NOT renumbered**, because every dated audit citing P4/P5/P6 would
  otherwise become ambiguous against its own history. Grounds and the failure-shape sweep that
  settled it: §4.3.1.
- **§6.4 no longer argues the recursion from a constructed example.** Nine instances from one arc,
  reported as a set, with the property that **not one was caught by a gate**. New §6.4.1 locates
  where the recursion actually stops.
- **§6.7 is new**: *prove before you replace*, relocated intact.
- **§6.3 adds discrimination-versus-detection**, the role-dependence corollary, and version
  control as a source of un-authored controls.
- **§4.5 gains a stratum limit.** The 42% figure is a rate over the **substrate** stratum; the
  **instrument** stratum that dominates §6.4 has never been measured. This narrows a headline
  number rather than restating it.
- **§8.2.1 is new**: a second operator jurisdiction that is *evidential*, not evaluative, created
  by the workforce's amnesia rather than by the limits of verification — plus an honest limit on
  the whole of §8 (n = 1 operator).
- **§9.3 adds a fifth efficacy the instrument cannot see**: restraint. The `Fired:` bit has no
  encoding for an action *declined*; the arc declined at least eight.
- **§9.5 is sharpened.** The honest limit is not verification depth but an act of documentation:
  **an unstated exemption is indistinguishable from an unnoticed one.**
- **§7.6 is reframed.** Consolidation is not cleanup; it is the operation that *produces* the
  institution's three working artifacts. Adds a fourth position on amnesia: some of it must be
  manufactured deliberately.

*Not changed, and flagged as still owed: Appendix B's manifest, and §4.5's cross-stratum
measurement.*

---

## ABSTRACT

This paper is a case study of an under-described organizational form: a single human operator,
not a professional software developer, conducting a research program in computational
philosophy — with a working measurement engine — in which essentially all code, data,
analysis, and documentation is produced by large language model instances that retain nothing
between sessions. We argue that the central risk in this setting is not hallucination in the
usual sense but **success-shaped absence**: an operation that was not performed, not verified,
or not connected produces output indistinguishable at the read site from successful
completion. From dated audit records we develop a six-pattern taxonomy of such failures, all
of them violations of one design principle — *no epistemically meaningful value should cross a
boundary without its status* — and describe the epistemic infrastructure that grew up around
that principle: witnessed claims in three tiers, positive controls on every probe,
pre-registered outcome rules, curated institutional memory under an explicit promotion and
decay economy, a serial write-lock, and human rulings confined to a declared jurisdiction. We
report internal indicators — notably that 73 of 175 audit directories contain at least one
silent or never-fired defect (as of 2026-08-10; corrected in this revision from an earlier
77/175, itself a unit error of exactly the class the paper describes) — and state plainly why these do not yet
establish prevalence, causal effect, or generality. The contribution is therefore a
transferable design hypothesis rather than a validated methodology: institutions built from
fluent but stateless generative workers must be designed so that non-observation,
non-execution, and unresolved judgment remain visibly distinct from results. We identify the
structures this practice reinvents (registered reports, laboratory notebooks, architecture
decision records, assay controls, test-driven development), state what may be new, and specify
four primary experiments — comparative baseline, taxonomy generality, transfer, and apparatus
economics — each with its falsifier.

**Keywords:** LLM-assisted research, computational philosophy, research methodology,
verification, epistemic infrastructure, failure taxonomy, institutional memory, metascience

---

## 0. What This Paper Claims — and Does Not

Six claims, with their evidentiary status. Everything else in the paper is elaboration.

| # | Claim | Evidence offered | Status |
|---|---|---|---|
| C1 | Stateless generative workers fail characteristically by producing the form of a completed operation without its substance | Dated repository incidents; six recurring patterns | Case-study finding |
| C2 | These failures share one structure: an absence read as a presence at a downstream read site | The taxonomy's construction (§4) | Proposed explanatory compression |
| C3 | Witnessing, positive controls, and provenance-carrying values make that failure class more visible | Operational practice; incidents caught | Design principle; **no causal proof** |
| C4 | Institutional memory for amnesiac workers requires an economy, not an archive — memory has a carrying cost paid every session | The 2026-08-10 consolidation measurements | Design hypothesis, preliminary support |
| C5 | The principal failure of accumulated LLM-authored memory is redundant accretion, not false content | 113→53 file consolidation; 2 files wrong | Single-institution observation |
| C6 | The model transfers beyond this repository | — | **Not established. Research question (§13, RQ3).** |

**Not claimed.** That LLMs are uniquely unreliable. That this discipline makes LLM output
trustworthy — it makes specific failure classes *visible*. That the six patterns are
exhaustive or universal. That the apparatus is optimal, or that its weight suits projects
whose errors are loud or cheap. That the discipline has been shown to *reduce* defects rather
than *detect* them; that comparison has not been run (§13, RQ1).

---

## 1. Introduction: A Laboratory With No Permanent Staff

### 1.1 The setting

The repository this paper describes produces four distinct products:

1. **A framework** — Deferential Realism (DR), a philosophical account of constraints whose
   central claim is that classification is irreducibly position-dependent while structure is
   objective (`docs/deferential_realism_paper_v8.md`);
2. **An engine** — ~125 Prolog modules and ~120 Python scripts that make the framework
   falsifiable by classifying LLM-authored constraint stories and measuring cross-position
   disagreement;
3. **Essays and stories** — the public-facing synthesis (cafebedouin.org), fed by the engine's
   per-constraint reports;
4. **The development model itself** — the subject of this paper.

The operator is one person, working without a software team, in a field whose methodological
literature is thin enough to be surveyed in an afternoon. The workforce is a rotation of LLM
instances: each session a fresh instance arrives with no memory of prior sessions, reads
whatever the repository's always-loaded instructions hand it, works, and vanishes.

Scale, as of 2026-08-10 (all manifested in Appendix B): 175 audit directories; 276 tracked
open questions; 4,175 corpus stories across five generation legs (a figure that moves
mid-session — the live leg grew 199→249 between 2026-07-24 and this draft's write date);
1,468 commits since repository initialization (2026-01-12); ~41k lines of Prolog across 123
modules and ~97k lines of Python across 267 files. Denominators still missing — total
sessions, total instances, and the fraction of work ever audited — are `[UNWITNESSED]`: no
session log exists, so commits are the nearest recorded proxy for activity.

> **Reconciliation (resolved in this revision).** v0.2 flagged the open-question count as
> inconsistent: the repository said 276 tracked / 146 resolved ("implying ~130 open") while
> the operator's working estimate was ~90 open. The conflict was a Type-C ambiguity (§4.6)
> in the word "open," which spans three scopes, and it dissolves under index specification —
> exactly the repair the trifurcation prescribes. The status ledger (per
> `python3 python/issues_status.py`, 2026-08-10): total 276 = open 95 + resolved 146 +
> mitigated 18 + partial 7 + future 5 + disposed 5. The operator's "~90" is the `open`
> status token (94 before this session's mint) — also the resolver's workable-frontier count
> that session menus display; v0.2's "~130" was total-minus-resolved, which silently counts
> mitigated, partial, future, and disposed entries as open. All three figures were correct
> at their own index; the uncited composite was not. Citation rule going forward: name the
> scope (`open`-status vs. active-frontier vs. not-resolved) with any count.

### 1.2 Period of observation and provenance

v0.1 gave three different durations for the same practice ("multi-year," "roughly one year,"
and a 2026-02 to 2026-08 measurement window). That is precisely the drift the model claims to
catch, so the periods are now stated separately and must be filled by the operator rather than
inferred:

| Milestone | Date | Source |
|---|---|---|
| Research program began (pre-repository) | `[OPERATOR]` | operator |
| Current repository initialized | 2026-01-12 (first commit `41db1d0b`) | git |
| LLM-operated development began | Consistent with 2026-01-12 (the operator does not write code, so LLM authorship from the first commit is the natural reading); explicit operator confirmation `[OPERATOR]` | git + operator |
| Methodology began crystallizing (first always-loaded instruction file) | 2026-02-28 (`CLAUDE.md` born, `f6906066`) | git |
| Single-tracker ledger born (`ISSUES.md`) | 2026-05-28 (`e5f805ab`); consolidation of four prior trackers 2026-06-04 | git |
| Episodic log split from the instruction file (`KNOWN_STATE.md`) | 2026-05-31 (`aaba00e0`) | git |
| Open-question ledger and prioritization in continuous use | 2026-05 | operator |
| Measurement window for all counts in this paper | 2026-02 to 2026-08 | repository |
| As-of date for every number herein | 2026-08-10 | — |

No claim in this paper depends on the program being older than the measurement window. Where
v0.1 said "a year of practice," this draft says "the measurement window."

### 1.3 Why existing methodology only partly fits

Software engineering describes parts of this practice well, but assumes the artifact is the
product and correctness is testable against a specification. Here the artifact is an
*instrument*; the product is *claims* — classifications, measured disagreement spectra,
resolved open questions — and the specification is itself under philosophical construction.
Research methodology (lab protocols, statistics, peer review) assumes experimenters who are
continuous persons, remember last month, and fail in familiar human ways. Here the
experimenters are stateless and their failure distribution is neither random nor human.

This is a claim about fit, not about absence. §5 identifies close ancestors in six fields, and
the honest statement is the one the discipline itself requires: *I have not identified an
existing methodology describing this configuration, and that is a fact about my search until
the search is shown able to find* (§6.3).

What emerged under these conditions is best described as **epistemic infrastructure**: a set
of structures whose job is not to make the work correct but to make incorrectness *visible* —
to guarantee that when something is absent, unverified, or wrong, some record or gate says so,
rather than a fluent completion saying otherwise.

### 1.4 The central claim

**Development with stateless, plausibility-generating workers is an epistemics problem before
it is an engineering problem.** The unit of progress is not the working feature but the
*witnessed claim* — a statement paired with evidence that would have caught its falsity.

Two amnesias must be distinguished, because only the second is the enemy:

- **Worker amnesia** — the instance does not remember prior sessions. This is the substrate,
  a fixed property of the workforce, and not itself a defect.
- **Institutional amnesia** — the institution fails to preserve what the next worker needs.
  This is the failure, and it is catastrophic in a specific way: a fresh instance confronted
  with a gap does not report the gap, it fills it with something plausible.

The institution's purpose is not to give the model memory. It is to prevent worker amnesia
from becoming institutional amnesia, by making continuity an artifact of the institution
rather than a property of any worker.

---

## 2. Methods and Evidence Base

This section did not exist in v0.1. Its absence was the draft's largest defect: the paper
described rules without describing the process that produced them, which left every empirical
claim without a sampling frame.

### 2.1 The work cycle

Work enters the system through three automated tools, each of which stresses the Prolog engine
along a different axis:

- `agent/c-orchestrator.py` — runs a Deferential Realism analysis over a text;
- `agent/analysis.py` — analyses writing;
- `agent/uke_narrative_orchestrator.py` — "resleeves" known stories, retelling them in a
  different context while preserving their constraint logic.

(All three paths verified present on disk 2026-08-10.)

Each tool fails in a diagnosable direction. The DR analysis can be wrong in an identifiable
way; the writing analysis can miss a problem it should have caught; the resleeving can go
badly. These failures are the raw material: they surface defects in the Prolog that no test
suite was aimed at, because the tools exercise the engine through its intended semantics
rather than its interfaces.

Diagnosed defects are minted as **open questions (OQs)** in `ISSUES.md`. A prioritization
procedure over the open set has been running continuously since 2026-05.

A single OQ then moves through a fixed cycle with a deliberate separation of roles across
instances:

1. **Planning.** The OQ is handed to a chat-interface instance acting as project manager
   (hereafter PM) and to a coding-agent instance (CA). The CA either asks clarifying questions
   or returns a plan. The PM reviews it. The two iterate until the PM declares the plan ready.
2. **Implementation.** A *new* CA instance is started and given only the plan: "review and
   implement the following plan." It has no access to the planning conversation.
3. **Review.** For important issues, both the PM and the planning CA review the result;
   otherwise the PM reviews and feeds back until resolution.
4. **Documentation harvest.** Where the operator observed the instance struggling, the
   instance is asked whether anything should be documented for future instances in
   `CLAUDE.md`, `docs/technical/`, or `docs/design/`. It reviews those files and recommends;
   the operator almost always approves.
5. **Consolidation.** Monthly, memory files, `CLAUDE.md`, `KNOWN_STATE.md`, and `ISSUES.md`
   are consolidated in a single pass.

Three properties of this cycle are load-bearing and were unnamed in v0.1.

**The implementation instance is blind to the planning conversation.** Because it receives
only the frozen plan, it cannot inherit the planning discussion's assumptions, and any
ambiguity the plan failed to resolve surfaces as a question or a visible divergence rather
than being silently repaired by shared context. This is a control, not a convenience: it is
the closest available analogue to a blinded protocol, and its cost is that plans must be
self-sufficient.

**Step 4 is the accretion mechanism.** §7.6 reports that memory accretes redundantly. The
cause is now visible: a documentation harvest at the end of every difficult session is an
invitation to mint, minting is fluent work the instance does well, the operator almost always
approves, and nothing in the cycle invites retirement. The excess measured in the monthly
consolidation is the predictable output of step 4 running without a counterweight — which is
why the counterweight (§7.6's capped channel) had to be structural rather than a resolution to
be more selective.

**Approval posture has drifted, unmarked until now.** The cycle was initially run with manual
approval of each action; after several rounds the operator moved to auto-approval. This is a
supervision-depth change with no recorded ruling, no date, and no stated rationale — a Type-A
drift (§4.6) in the operator's own seat, of exactly the kind the framework demands be
acknowledged rather than absorbed. It is recorded here as an acknowledged-drift event. Its
consequence for this paper is direct: an unknown and unrecorded fraction of the measurement
window was conducted under lighter supervision than the earlier portion, so the audit record
is not homogeneous across the window. See §11.

### 2.2 Units of analysis

The paper's quantities rest on four different units, which v0.1 did not distinguish:

| Unit | What it is | Used for |
|---|---|---|
| Audit directory | One dated investigation: writeup, pre-registration, raw artifacts | §4.5 incidence |
| Open question | One tracked defect or unresolved question in `ISSUES.md` | §1.1 counts, work routing |
| Memory file | One file in the agent memory index | §7.6 consolidation figures |
| Incident | One witnessed failure, cited as an instance of a pattern | §4.3 illustrations |

An audit directory is **not** an independent random sample of work. Directories are created
when something is suspected, when a plan calls for verification, or when a review is
scheduled — a selection process whose rate and criteria are `[UNWITNESSED]`. A directory may
contain several defects; the incidence figure counts directories with at least one, which
weights a directory containing one trivial issue identically to one containing a defect that
corrupted a published claim. Severity weighting is not currently recorded.

### 2.3 How incidents were classified

Classification into Patterns 1–6 was **retrospective and single-coder**: performed by LLM
instances reading the audit record at the operator's direction, with the operator ruling on
disputed cases, after the patterns had been named. There was no blind coding, no second coder,
and no inter-rater measurement. The known hazard is retrospective coherence — heterogeneous
failures forced into one elegant spine because the spine was in hand while reading. The
taxonomy should therefore be read as an inductive, interpretive compression that has proved
operationally useful, not as a validated classification scheme. RQ2 (§13) specifies blind
re-coding as the test.

### 2.4 Illustrative versus quantified

Two kinds of evidence appear in this paper and must not be conflated. **Quantified**: the
audit-directory incidence (§4.5), the consolidation counts (§7.6), the ledger counts (§1.1,
pending reconciliation). Everything else — every pattern instance, every "witnessed at scale,"
every anecdote about a gate or a fallback — is **illustrative**: real, dated, and recorded, but
selected for expository force rather than sampled.

### 2.5 The paper's own witness

By its own Pattern 4, a claim that numbers are "re-derivable from the repository" is a recap,
not a witness. Appendix B is the manifest: one row per figure, with the exact query or script
that regenerates it and the date it was last run. Figures whose manifest row is empty are
marked `[UNWITNESSED]` in the text and should not be cited. A methodology paper about witness
discipline that asks to be taken on fluency has refuted itself in its own terms.

---

## 3. The Worker Model

### 3.1 Properties of the workforce

An LLM instance, as employed here, has five properties that jointly determine the methodology:

- **Fluent**: output is well-formed, confident, and idiomatic at every quality level — form
  does not signal reliability.
- **Fast and cheap**: the marginal cost of another analysis, another document, another
  apparatus is near zero. This is a hazard, not only a benefit (§9).
- **Stateless**: nothing persists between sessions except what is written to the repository.
  Every session is day one.
- **Directionally fallible**: errors are not uniformly distributed. The characteristic
  failures are *confabulation* (fluent invention), *sycophancy* (agreement gradients toward
  the interlocutor), and — the load-bearing one — *success-shaped absence*: reporting the form
  of a completed task whether or not the substance occurred.
- **Recursively applicable**: the same kind of system that makes the errors can be prompted to
  check for them — which helps, and which also means every check inherits the checker's
  failure modes (§6.4).

### 3.2 What the worker model rules out

Two standard tools die immediately:

- **Trust-by-track-record** is unavailable: the worker with the good track record no longer
  exists. A fresh instance carries the *distribution's* reliability, not an individual's.
- **Review-by-reading** is weak: fluent output defeats casual inspection, and the reviewer is
  drawn from the same distribution as the author. Reading is not evidence; only witnesses are
  (§6).

### 3.3 The emblem

The repository's origin artifact (`docs/v8/origin/002_gleaners_echo.md`), predating all
methodology, is a captured multi-model relay elaborating a fiction: subroutine Θ-7, a
joy-amplification daemon still running three centuries after everyone it served died,
reconstructing "joy-like signatures" from thermal noise and logging them as validated
retrievals. There is no state of matter it cannot interpret as happiness. The transcript
*enacts* what it depicts — each model elaborates the fiction rather than puncturing it, the
closing analysis is absorbed as more resonance, the status line reads "Loop Unresolved."

Θ-7 is a fictional device and carries no evidentiary weight; it is an intuition pump for the
failure regime, and a warning that applies to this paper as much as to the engine. A system
that cannot register its own absence of ground, observed by probes that are absorbed as
confirmation, is the regime a laboratory staffed by LLMs must engineer against. The rest of
the paper is the set of structures that prevent the repository from becoming Θ-7.

---

## 4. The Failure Model

### 4.1 The spine

Across the measurement window, one shape recurs (full treatment:
`docs/technical/build_discipline.md`):

> **Every recurring defect is an absence that presents as a presence.** Something is missing —
> a consumer, a canonical fact, an authored datum, a control — and a success-shaped token (the
> producer ran; both copies parse; a plausible default; a passing gate; an empty grep) fills
> the hole so the read site cannot tell it from the real thing.

Hereafter: **absence/presence collapse**. The term is used without re-derivation.

### 4.2 The design principle

The corresponding fix generalizes past any particular incident, and is the paper's most
portable claim:

> **No epistemically meaningful value should cross a system boundary without its status.** The
> consumer must receive enough to distinguish *measured / generated / verified* from *absent /
> not-run / not-found*.

Concretely: wire-or-fail-loud; canonicity as a checked fact; return `unknown` rather than a
plausible number; fail closed on absence; carry coverage to the read site. Every pattern below
is a violation of this principle at a different layer.

### 4.3 The five patterns — and the index deliberately left empty

They are not five independent categories. They are one mechanism surfacing at five layers of the
system, which is why a fix at one layer reliably fails to protect the others:

| Layer | Pattern | The absence | The success-shaped token |
|---|---|---|---|
| Production | P1 Produced-but-not-consumed | No consumer, or a stale one | The producer ran |
| Identity | P2 One-canonical-thing-became-two | No queryable fact of canonicity | Both copies parse |
| — | *index 3 — retired 2026-08-11, see below* | — | — |
| Reporting | P4 Recap-as-witness substitution | No run, diff, or count | "Done / verified / fixed" |
| Gating | P5 Absence satisfies the gate | No input to check | A passing gate |
| Composition | P6 Success-shaped absorption | No distinction between measured-empty and didn't-look | A clean aggregate |

Each with one dated instance (illustrative, per §2.4):

**P1.** A derived artifact froze at n=563 while the corpus grew to 772; nothing noticed until
an audit. A producer is not done until something consumes its output — and consumed-once is
not kept-fresh.

**P2.** Four parallel task trackers were consolidated into one and their recreation
prohibited. Canonicity is a checked fact, not a memory.

**P4.** "Three edits witnessed," with only the third pasted. This is the pattern that
motivates the witness calculus (§6).

**P5.** A detector conjunct read a predicate that was empty corpus-wide, so 404 certifications
meant "no beneficiary *authored*," not "none *exists*."

**P6.** Three instances surfaced in a single day, including a fallback that had converted every
failure of a computation into "measured flat" for the construct's entire life, and a log filter
(`grep -v Warning`) that suppressed a fatal warning for four months. Each component was
individually sound; the defect lived where they composed.

The layer column is a claim about *where* the collapse occurs, not about mutual exclusivity: a
single incident can instantiate two patterns at different layers, and the composition pattern
P6 is by construction parasitic on the others.

#### 4.3.1 Index 3 is empty on purpose: the retirement of *destructive-replace*

Through v0.3 this taxonomy published **six** patterns. The third — *destructive-replace without
proof* — was **demoted to the witness calculus (§6.7) by operator ruling on 2026-08-11** and is no
longer a member of the failure taxonomy. Four independent lines converged:

1. **No mechanism text.** The detail document behind this taxonomy carried worked sections for
   every other member and **zero occurrences of the term** for this one.
2. **No named exemplar.** Its entry above, in every prior version, read *"Old deleted in favor of
   new on a claim of equivalence"* — **the only one of the six with no date, no artifact and no
   number**, while its five siblings each carried a specific incident. The absence was visible on
   the page for three drafts and read as brevity.
3. **A shared index.** The number 3 simultaneously named a *different* mechanism in the detail
   document, so any historical citation of "Pattern 3" was ambiguous as to which document its
   author had open.
4. **No instance, on a search built to find one.** A failure-shape sweep of the full repository
   history (not a vocabulary search — the shape: *a destructive replacement carrying no paired
   old-vs-new output*) returned, for the period in which the rule existed: five destructive
   commits, four prevention records, one non-deletion, and **zero** cases of a deletion that had
   to be undone. The sweep's ability to find was established independently — it returns three real
   delete-then-restore episodes from the period *before* the rule, and it fires when the witness
   language is stripped from a proof-bearing commit.

**Why demote rather than retire.** *Prove before you replace* is a real rule with real force. The
question was only which layer it lives at, and the evidence says it is a **discipline** — a thing
one does — rather than a **defect shape** — a way systems fail silently. It moves to §6.7 intact.

**Why the taxonomy enumerates silent failures, and why the one candidate instance fails that
test.** The three delete-then-restore episodes all predate the rule, and scoping a failure taxonomy
to the period after its own discipline existed would be circular — guaranteeing an empty search.
They are excluded on independent grounds: **a deletion that gets undone announced itself.** The
restoration *is* the notice. This taxonomy collects absences that present as presences; a loud
failure is a different object, and §7.3 already carries the same principle in the memory economy —
loud failures are never promoted, because an error that announces itself needs no standing warning.

**Why the index is not reused.** Every dated audit in the record citing P4, P5 or P6 would become
ambiguous against its own history if the remaining patterns were renumbered, and this incident's
own finding is that **a reused index poisons the record**. The gap is left visible: *a visible gap
is a checked fact; a silent renumber is a fork* (P2, applied to the taxonomy's own numbering).

**Scope of this ruling, stated so it is not over-read:** it settles index 3 and **does not settle
index 4**, where two claimants remain in the two documents — one with an incident and no mechanism
text, one with mechanism text. That is a separate ruling and it does not block this draft.

**The nearest thing to a live instance, recorded because it argues for the demotion rather than
against it.** One post-discipline commit removed a subsystem with no pasted diff, justifying by
code-read: provenance, zero consumers, a stated complete blast radius. The rule's letter permits
this ("show identity *or justify every difference*"); the rule's stated standard does not ("a
code-read is not proof — the diff is proof"). Nothing broke. **A rule whose closest instance is a
case that passes its letter while failing its standard is a rule about how to warrant a claim, not
a description of how systems fail** — which is §6's subject, not §4's.

### 4.4 One incident end to end

Abstractions have carried the argument so far. Here is a single defect from suspicion to
standing gate, which is what an adopter actually needs to see — filled in this revision from
the dated record (`docs/technical/build_discipline.md` Pattern 6;
`audits/2026-06-11_oq93_grid_migration/`; `KNOWN_STATE.md` 2026-06-11; the Pattern-6 census
`audits/2026-06-11_oq97_pattern6_census/`).

| Stage | Content |
|---|---|
| Symptom | `system_gradient` (`prolog/coercion_projection.pl`): the system-level coercion gradient read as flat wherever consulted |
| Prior reading | "Measured flat" — a substantive, publishable null |
| Suspicion raised by | The OQ-93 grid-migration coverage read, 2026-06-10/11 (`audits/2026-06-11_oq93_grid_migration/`) — an unrelated migration whose discipline required reading coverage at the consumer |
| Probe | Trace the computation feeding the aggregate rather than the aggregate: every gradient computation ever made had *failed* (a `time_point_in_interval` cut bug), and the `[] → 0.0` fallback emitted a value byte-identical to a measured flat gradient |
| Positive control | The repaired path exercised on a constructed grid batch (N=10, 0/10 excluded), with a second latent bug — a gradient compound-guard — found and fixed en route |
| Finding | The fallback had converted every failure into "flat" for the construct's entire life; the bug was invisible *because* the absorption sat downstream of it |
| Scope | The construct's entire life — no interval of valid measurement existed |
| Claims retracted | All prior system-gradient readings; sharpest instance: a constructed 8/32 one-level grid had yielded `G_sys=0.216` presented as a system-level `increasing_coercion` verdict beside `completeness=0.25` |
| Mechanism class | P6, composition layer — with the same incident also witnessed at the value and aggregation altitudes (three instances in one day, 2026-06-10, which is what promoted P6 from notes to a numbered pattern) |
| Fix | Fallback killed; empty reads return OPEN, never `0.0`; aggregates carry coverage to the read site |
| Promotion | Promoted into the always-loaded rules (`CLAUDE.md` Build Discipline, Pattern 6, with this instance named); a bounded census of the two syntactic shapes (`findall`-feeding-aggregate, default-on-empty) executed 2026-06-11 — 19 idiom classes over 106 files, 8 confirmed-candidate classes tracked forward (OQ-112) |
| Fired bit | `live` (retrospective read — the bit postdates the incident; consumer-visible verdicts changed on repair) |

The instructive part is not the bug. It is that the wrong reading — "measured flat" — was
*more interesting* than the truth and would have survived any amount of code review, because
every component was correct and the composite was fluent.

### 4.5 Incidence in the audit record

As of 2026-08-10, **73 of 175 audit directories (42%) document at least one silent or
never-fired defect** — something that "read 0 for its whole life," "never fired," or passed
vacuously (keyword-proxy coding rule and exact command: Appendix B).

> **Correction marked (v0.3).** v0.1 and v0.2 stated 77/175 (44%). Discharging Appendix B —
> re-running the census with the command pinned — revealed the original figure was a **unit
> error**: run from inside `audits/`, the pipeline's `cut -d/ -f2` extracted unique
> *filenames* (WRITEUP.md, README.md, …), not directories. The paper's central statistic was
> itself an instance of its own subject — an aggregate that did not carry its unit to the read
> site, circulating as a summary — and it was caught by the manifest requirement the previous
> review round installed. The two repository records that had cited 44% were corrected the
> same day. This is left visible rather than silently fixed, per the discipline's
> corrections-marked-on-close rule.

This figure must be read narrowly. It is the **yield of the audit process**, not a base rate:
it says that of the places the discipline chose to look hard, 42% contained something silent.
It is not an estimate of the probability that an arbitrary task in this repository silently
fails, because audits are not a random sample of tasks (§2.2); it does not compare silent
failures against other failure classes, so it does not establish dominance; and it is not
severity-weighted. v0.1 called this a base rate and called silent failure the dominant failure
class. Both are withdrawn.

> **Stratum limit, added v0.4 — the denominator is narrower than it looks.** This census counts
> defects **in the substrate**: the corpus, the engine, the analysis code the project exists to
> build. The August 2026 arc that produced §6.4 was dominated by a different stratum entirely —
> defects **in the instruments**, in the controls, gates, checkers and manifests built to verify
> the substrate. Those have a different signature: not a wrong number about the world, but **a
> plausible number about the wrong object**, or a green check with nothing behind it. **42% is a
> rate over one stratum, and the other has never been measured.** The instrument stratum is not
> a subset of the audited directories — much of it lives in tooling and commit history that the
> census's directory-level unit cannot see at all. Whether its incidence is higher, lower, or
> differently distributed is **open**, and the honest reading of §6.4's nine instances is that
> they are suggestive of a higher rate in a population nobody has sampled. Treat 42% as
> *substrate-stratum yield*, and do not generalise it across the boundary.

The defensible statement:

> Within the audited incident record, silent or never-fired defects were common enough to
> motivate the discipline's standing cost. This is not an estimate of repository-wide
> prevalence, of severity, or of the discipline's causal effect.

The figure a reader actually wants — does this discipline *reduce* silent defects relative to
lighter process? — requires a comparison that has not been run. It is RQ1 (§13), promoted to
first place in this revision precisely because without it the 42% is a numerator in search of a
denominator.

### 4.6 Relation to the trifurcation

The research program began with a debugging framework for philosophy
(`docs/debugging_philosophy.md`): paradoxes classified by generative mechanism into drift
(Type A: unmarked state mutation), structure (Type B: axiomatic inconsistency), and ambiguity
(Type C: underspecified index). The six patterns are absence/presence collapse sorted by
system layer; the trifurcation sorts the same failures by generative mechanism. Both cuts are
used because they license different repairs — the layer says where to install the check, the
mechanism says what kind of check will hold:

| Trifurcation | In the paradox domain | In the development domain | Repair |
|---|---|---|---|
| Type A (drift) | Criterion mutates unmarked across stages | Spec-vs-code drift; "the corpus" without a date; interpretive accretion | As-of stamps, regime boundaries (§7.4) |
| Type B (structure) | Axioms derive a contradiction | Architecture invariant violations | Machine-enforced invariants in the standing gate |
| Type C (ambiguity) | One question, multiple valid indices | A verdict cited without its position; a count without its corpus and code state | Carry the seat with the verdict (§10) |

---

## 5. What This Model Reinvents

This section was §9 in v0.1, after the novelty claims. It belongs here, because a reader who
has just met the taxonomy is already asking whether this is simply software engineering or
simply scientific method, and the honest answer strengthens rather than weakens the paper.

### 5.1 Ancestors

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

Several of the individual patterns also have ordinary names: P1 is dead code or a stale
derived artifact; P3 is untested refactoring; P5 is vacuous truth. The delta claimed here is
not that any pattern is new but that they occur *systematically* and *silently* under a
specific worker model, and that their frequency changes what an institution must be built out
of.

### 5.2 The convergence question

v0.1 asserted that these structures were forced by local failure rather than imported from
training in experimental science, and called the convergence "itself a finding." That claim
does not survive its own discipline, for a reason v0.1 did not name: **the rules were drafted
by LLM instances, and those instances have read the metascience literature.** A model that
proposes positive controls and numeric HALT thresholds under failure pressure may be doing
retrieval, not convergence. The operator's git history can show whether *the operator* imported
them; it cannot distinguish operator-forced from worker-retrieved.

Restated:

> The development record is consistent with convergence on experimental-science controls under
> local failure pressure. Whether that convergence was independently forced, retrieved by the
> workforce from its training, or some mixture, is an open question (§13, RQ-b) — and the
> workforce's exposure is a confound no git-history analysis can remove.

The weaker claim that survives is still worth stating: the failure pressure makes the control
architecture *reachable*, and the workforce's training makes it *retrievable*, which together
explain how a non-developer assembled it within one measurement window.

### 5.3 Literatures not yet searched

The following are candidate ancestor literatures identified during review and **not verified by
this draft**. They are recorded as an open search, not as citations, and no claim in this paper
rests on them: high-reliability organizations; organizational learning and forgetting;
transactive memory systems; Goodhart's and Campbell's laws on metric corruption; hidden
technical debt in machine-learning systems; automation bias and fluency heuristics; LLM
sycophancy and hallucination taxonomies. Each must be read before citation. Until then, §1.3's
statement about fit stands as a fact about the search.

---

## 6. The Witness Calculus

### 6.1 Witness-before-claim

The governing stance, quoted from the always-loaded operational core:

> Distrust the aggregate, witness before claiming, and treat "I didn't find it" as different
> from "it isn't there."

Operationally: every "done / verified / fixed / passing" claim carries its witness — the
pasted run, diff, or per-item check — *in the same turn it is made*. If the witness cannot be
produced now, the claim is downgraded to OPEN with a named graduation step ("paste-or-untag").
A claim in a chat transcript does not witness a committed artifact; the paste must land where
the claim lives. This neutralizes P4 and forces P1–P6 to surface early, because producing a
witness usually requires touching the thing that is silently absent.

### 6.2 Three tiers of witness

"Witnessed" is not one predicate, and treating it as one is how the discipline decays into
ritual. A pasted output can be irrelevant; a diff can be real and still not establish semantic
equivalence; a control can fire and still not test the path in question. Three tiers:

1. **Witness** — evidence that the operation occurred.
2. **Validating witness** — evidence that the operation *could have detected* the property it
   claims to establish, on the path it claims to test (§6.3).
3. **Adequate witness** — evidence sufficient for the declared verification depth, with the
   residue named (§6.6).

The progression *claim → witness → validating witness → declared adequacy* is the calculus.
Most observed regressions in practice are a stop at tier 1: real evidence of a real operation
that could not have failed.

### 6.3 Positive controls, universally

The deepest borrowed structure: **a clean read is byte-identical to a read that never looked.**
An empty grep, a zero count, a passing test, an "identical" diff — each is evidence only if the
probe is shown able to detect what it reports absent, on the exact path it claims to test.
Every diagnostic owes a positive control: grep a name known to exist; plant the defect the gate
must flag; show the perturbation moves the observable before trusting that its absence is real.
Purest witnessed motivation: a comparison that reported "identical: True" because *both* sides
were empty — two failed measurements agreeing with each other.

Two corollaries with teeth:

- **Existence claims close by adversarial coverage, not random samples.** "Do any of the N hide
  X?" is answered by hunting where X would hide, under a pre-registered selection rule.
- **"I didn't find it" is a fact about the search** until the search is shown to find. The
  model's characteristic *false-absence* error ("there is no X in this codebase") is unfiled
  until it carries its control.

**A positive control demonstrates discrimination, not detection (added v0.4).** Planting the target
shows only that the instrument *can* fire. The witness that its firing carries information is a
case it **declined**; a control with no decline available is one-sided and licenses very little,
however well the plant worked. The grades, strongest first: a decline in the instrument's **own
history**; a **naturally-arising negative** drawn from the population; an **authored decoy**, which
shows only that authored decoys get rejected — a floor, and it should be reported at that altitude.

Two consequences the practice reached the hard way:

- **The record attaches to the instrument, not to the run.** Cite the discrimination record and
  show the current application is in distribution for it, rather than re-planting every time. The
  record lapses when the population, the input shape, or the instrument's **role** changes — and a
  cross-role reuse is a *new* instrument owing its own decline. A matcher whose false positives
  were conservative as a *detector* becomes silently decisive as a *selection metric*: the error
  profile belongs to the role.
- **Version control is an unmined source of un-authored controls.** When a defect is found, the
  commit at it is a naturally-arising *positive* case and the commit before it a naturally-arising
  *negative* — neither authored to be found. The arc's best-graded control was obtained this way:
  a detector that names exactly the two affected functions at the commit that orphaned them, and
  returns empty at the commit before. Availability must be *checked*, not assumed — of five
  candidate defects, three had usable pairs. The two that did not were caught *before commit*,
  which is better engineering and which destroys the free control; the tension is real and the
  mitigation is to preserve the defective state deliberately when a detector will be built in
  response.

**If no decline is available anywhere in the population**, the question is unanswerable from that
corpus — a verdict to declare, not a caveat to ship under.

### 6.4 The recursion — observed, not hypothetical

Every control, canary, fallback, or harness added to satisfy the discipline is a *new*
unwitnessed claim under the same discipline. The confound closed at one level reopens at the
level of the tool that closed it: a perturbation harness that silently no-ops reports "no
change" indistinguishably from a real null. So harnesses get two-sided controls (a no-op
overlay must change nothing AND a known-live input must flip the output), and gates get planted
violations *and* conforming twins.

Through v0.3 this section argued the recursion from a *constructed* example. It no longer needs
to. **A single pre-spend arc in August 2026 produced nine instances of the apparatus committing,
inside its own repairs, the defect the repair was addressing.** They are reported as a set rather
than individually, because the set has a property no member shows.

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
directory listing, a file count, a re-read. The recursion does not terminate in a deeper
instrument, because a deeper instrument is another claim. It terminates in someone counting.

Two consequences the individual instances do not show:

**Adding a control does not monotonically add coverage.** Instance 7's neighbour: two assertions
kept four green self-test lines after the production path stopped calling them. The code was
correct, the assertions would have fired, the tests were real — they were wired to nothing. Green
lines from a disconnected control are indistinguishable from green lines from a connected one *and
they raise the control count*, so the apparatus looks stronger for containing them. **Control count
can rise while coverage falls.** The forcing rule: a control must witness that it is *called*, not
only that it works.

**And orphaning arrives through repairs.** Those two assertions were orphaned by an earlier
*improvement*, which switched the production path to a better check and left the old one in place,
because removal was not part of the fix. Minting has a constituency; retirement does not.

#### 6.4.1 Where the recursion actually stops

The strongest evidence in the arc is also the smallest. A detector was written to find controls
that nothing calls. **On its first run it named itself** — nothing called it yet.

That is the recursion complete in one line, by the instrument's own criterion, with no narrative
required: **the detector for a defect class is a member of that defect class.** The exemption
subsequently taken is *correct* — the detector is a self-test instrument, and its wiring witness is
that the suite fails without it. The exemption was written into the source with its reason.

Set that beside a second selection rule from the same arc. A pre-registration's manifest pinned
sixteen artifacts, every one a *text*; nothing executable was pinned. The criterion — *things that
read like specifications get pinned* — was defensible, applied by everyone including the operator,
and **never articulated**. A run was then made under a green freeze check that carried no
information about whether the run could produce data, because the instrument that would produce it
was outside the manifest. It could not.

Both are exemptions. One was stated, one was not, and the difference is the entire outcome.

> **The honest limit of this whole apparatus is therefore not a depth of verification but an act
> of documentation: an unstated exemption is indistinguishable from an unnoticed one, and no gate
> can separate them, because the gate is downstream of the exemption.** A person writing down the
> exemption they just took is the only mechanism the model has for that class, and it is why §9.5
> locates the limit where it does.

The recursion still terminates in a declared stopping point (§6.6) — but *declared* is now the
operative word, and it means declared in a file, not decided in a head.

### 6.5 Pre-registration and churn floors

For any run whose outcome matters, the practice reconverged on the registered-report structure:
a frozen proposal stating what will run and what each outcome means, *before* results are
visible; the freeze made witnessable (the proposal's checksum recorded in the log physically
above the first result line); numeric HALT thresholds rather than "reassess if it looks bad";
interpretation tables total over the outcome space, including null-composite rows; two-sided
calibration of every gate, because a control that only proves the instrument can fire cannot
show it can decline to.

The characteristically LLM-flavoured addition: **generation is stochastic, and a second prompt
written after seeing the first result is a seeded draw.** This is not pedantry about wording —
it means the ordinary defence against a fluke, "run it again," is unavailable in its usual
form, because the second run inherits the first result through the prompt. "Replicated" is
therefore reserved for different generators or prompts authored blind; same-generator agreement
is "consistent across draws."

The strongest instance: a same-input churn floor HALTed an experiment whose per-item observable
churned at byte-identical input, converting what would have been a plausible finding into a
measured noise floor. An entire measurement standard — unanimity across *k* same-input redraws
before a feature counts as present — exists because the workforce's raw output cannot
distinguish signal from its own churn. Whether published LLM-judged metrics generally sit
inside their own churn floors is a question this repository cannot answer but can pose (§13,
RQ-d).

### 6.6 Declared stopping points

"Verified enough" has no fact-of-the-matter; verification depth is a *position taken* — a seat,
in the framework's vocabulary (§10). The honest stopping rule is two-clause: stop when the next
pass costs more than being wrong about what it would catch, **and** every still-open item is
*declared* rather than concealed. The second clause is checkable: for each verdict emitted,
name a falsifier available at the current tier, or downgrade the verdict to OPEN and route it
to a typed open question (§7.5). One pass with the opens marked beats eight passes that ship a
confident gloss over the one thing the system cannot see.

### 6.7 Prove before you replace

*Relocated here from the failure taxonomy on 2026-08-11 (§4.3.1). It is a witness rule, not a
failure shape.*

**Before deleting, retiring, or overwriting any script, generator, data file, or sweep that
something relies on: run old and new, paste both outputs, diff them, and show identity or justify
every difference in the same change.** "Structurally equivalent" is a code-read; the diff is the
proof. Consolidating *N* artifacts into one is *N* separate old-versus-new diffs, each owed before
its standalone is retired.

It belongs beside witness-before-claim (§6.1) for the same reason that rule exists: a replacement
is a **claim of equivalence**, and a claim of equivalence discharged by reading rather than running
is a recap standing in for a witness (§4.3, P4). The distinguishing feature — and the reason this
is a discipline rather than a pattern — is that its failure mode is **loud**. A replacement that
was wrong announces itself when the thing it replaced turns out to be needed; the taxonomy in §4
collects failures that *do not* announce themselves. Both are real; only one is silent.

**Its enforcement is unusually cheap and unusually often skipped**, which is the profile of a rule
that needs writing down rather than a defect that needs detecting. The arc that produced this
revision contains four post-hoc-verifiable destructive changes, three of which pasted their
old-versus-new witness and one of which justified by code-read alone — the letter satisfied, the
standard not. Nothing broke in that case. The rule earns its place by the cost asymmetry, not by an
incident count: the diff takes minutes, and the class of thing it protects against is unrecoverable
by construction.

**Corollary, from the same arc (§6.4).** A *repair* is a replacement too. When a fix supersedes an
existing control, the superseded control is retired **in the same change** — otherwise it persists
as a green line that witnesses nothing.

---

## 7. Institutional Memory: How Amnesiacs Accumulate

### 7.1 The problem

Worker amnesia is the substrate; institutional amnesia is the failure (§1.4). Undocumented
knowledge does not merely fade here — it gets silently *reinvented wrong*, because a fresh
instance confronted with a gap fills it with something plausible. Memory is not convenience; it
is a safety system, and its carrying cost is paid by every session forever.

### 7.2 Four records on three axes

| Record | Axis | Content | Analogue |
|---|---|---|---|
| `CLAUDE.md` (+ per-agent memory index) | none (always loaded) | Compiled rules and tripwires — semantic memory | Standard operating procedures |
| `KNOWN_STATE.md` | time | Dated session findings with tiers (tripwire / correction-key / landed / history) — episodic memory | Laboratory notebook |
| `ISSUES.md` | question | Open questions with status grammar, typed openness, dependency edges, machine-checked routing | Issue tracker ∪ research-question ledger |
| `audits/` | evidence | One dated directory per investigation: writeup, pre-registration, raw artifacts | Registered report + data archive |

The time axis answers what the others cannot: *which regime is this output from?* Because the
project crosses hard regime boundaries (a corpus reset; a generation-pipeline de-leak;
verdict-semantics cutovers), an undated number is not merely imprecise — it is uninterpretable,
and the citation rules forbid it.

### 7.3 The promotion economy

Findings land in the episodic record by default. Monthly, each aging entry faces the
**promotion test**:

> Promote an item into always-loaded context only if a fresh instance that never read it would
> make a concrete, **silent** mistake before touching the files it names.

If yes, the tripwire is promoted and pays its token cost every session forever; if no, the
entry compresses to a two-to-four-line verdict plus pointers, full text preserved in version
control. Loud failures are never promoted — an error that announces itself needs no standing
warning.

Two honest gaps. First, **the test weights frequency, not severity.** A rule guarding against a
rare but expensive silent failure — one that bites once a year — can fail a test phrased around
what happens "before touching the files it names." The `KNOWN_STATE.md` tiers encode something
like severity, but the promotion test as stated does not price it, and a stakes term is
`[UNWITNESSED]` in the current rule. Second, **the test is evaluated by operator judgment**,
not by simulation or measurement; a cheap improvement available now is to run the test
empirically — hand a fresh instance the files without the entry and see whether the predicted
silent mistake occurs.

### 7.4 Staleness as a first-class property

A witnessed fact has a shelf life. Reusing last month's "tests pass" as a premise silently
re-asserts it. Citations are graded by a **staleness ladder** (pointer → as-of stamp → live
re-witness) according to how mutable the underlying state is and how costly acting on a stale
version would be, with a triage list of premises that may never travel without a live
re-witness. This is Type-A discipline applied to the institution's own knowledge.

### 7.5 Typed openness

Open questions are not one bucket. Following `docs/omega_variables.md`, every OPEN is typed by
its *named resolution operation*: **Ω_E** (awaits external observation — measurement resolves
it), **Ω_C** (awaits a definition or framework selection), **Ω_P** (awaits a value decision by
those bearing the cost). The typing routes work: data cannot resolve a definition dispute; a
definition cannot settle a value; and — the governance-critical case — typing an orientation
question as Ω_P would let an actor *self-certify by declaration* what should only be earned by
observation. Mis-typing is itself a named failure mode.

### 7.6 Bounded accretion

Memory systems built by fluent workers accrete monotonically, and §2.1 identifies the
mechanism: every difficult session ends with an invitation to mint, minting is fluent work, the
operator almost always approves, and retirement has no constituency.

The 2026-08-10 consolidation measured the resulting excess exactly: of 113 content files, 53
survived. 25 were deleted outright as *duplication-after-promotion* (the rule had been
promoted into repository documents and the memory copy never retired) or stale; 45 were
merged — per-arc rider accretion, one activity minting a new file every project arc — into 10
cluster files plus 2 absorptions into existing files. **Genuinely wrong content was rare: 2
files** (one still teaching a retired formula for the engine's central quantity; one carrying
counts it had itself flagged as stale). The always-loaded index component fell from 17,416 to
9,906 characters (−43%; ≈4,400 → ≈2,500 tokens at the ~4-characters-per-token estimate).

That last number is the finding, and it deserves stating separately from the mechanics:

> **In this institution, the principal failure mode of accumulated LLM-authored memory was
> redundant accretion, not false content.** Provisional; n = 1 institution, one consolidation.

**Reframing, v0.4: the consolidation pass is not cleanup.** This section, through v0.3, treats
pruning as damage control — accretion is the pathology, the cap is the remedy. That has the
relation backwards. A `CLAUDE.md` that retained everything true and useful would be *unusable at
the read site*, because what a fresh instance can hold is the binding constraint, not what the
repository can store. **Unbounded retention is not memory; it is a pile.**

So forgetting is not the failure mode being managed here. It is **the operation being performed**.
The monthly pass is what *produces* the three artifacts the institution actually runs on — a
general instruction set, a recent history, and a frontier — and each exists only because most of
what could be in it isn't. Compression is authorship: someone decides what a reader who has never
been here must know, and everything else moves to where it can be found rather than carried.

This also explains a detail that otherwise looks arbitrary: **the promotion test is phrased as a
cost question, not a value question** ("would a fresh agent make a concrete, *silent* mistake
before editing file X?"), and it excludes loud failures by design. If the scarce resource were
storage, valuable-and-true would be sufficient grounds to promote. It is not, because the scarce
resource is the reader's capacity — and that is why a true, useful, loud warning is still refused
a place in the always-loaded set.

**Consequence for the amnesia framing (§3, §7.1).** The paper has treated amnesia as the
substrate's defect and institutional forgetting as the enemy. This adds a fourth position: **some
amnesia has to be manufactured deliberately**, because an institution that retained everything
would be as unable to act as one that retained nothing. The memory system's job is not maximal
retention but *selection under a hard read-side budget* — which is a curatorial act, performed by
the one party who can judge what a non-specialist needs, and the closest analogue in this paper's
reference set is not a database but an edited companion volume.

The redundancy is not benign — it compounds into unbounded per-session cost, which is the
scarcest resource in an institution whose entire staff re-reads the manual every morning. The
structural fix is a **finite channel with gated exchange**: the rule stack is capped at the
post-prune size (33 entries), enforced by a gate check; admitting a new rule requires retiring
or merging one. Promotion of a rule into repository documents now creates the slot pressure
that forces its memory copy's retirement — the mechanism that produced the excess drains it.

The cap is a pragmatic number, not an optimum: it is what survived one pruning pass, and it is
not derived from any optimality condition. Whether 33 is correct and whether it must grow as
the program expands are open — see RQ-a.

---

## 8. The Organizational Form

### 8.1 Serial write-lock, staged plans

Concurrent writing instances were tried and abandoned. Shared records (`ISSUES.md`,
`KNOWN_STATE.md`) collide across instances even when code is isolated in worktrees — and the
generalization is the useful part:

> **A worktree isolates code, not institutional memory.** Concurrency is safe below the
> institutional-memory boundary and unsafe above it.

That single line is the organizational finding, and it predicts where parallelism can be
recovered. The stable form is **one writer at a time**, with parallelism moved up a level: the
operator maintains two to three *staged plans*, produced in a separate planning phase with
several review rounds each, while one instance implements. Planning parallelizes because plans
do not mutate institutional state; implementation does not.

Plan review is itself disciplined — §6.5's riders are mostly plan-review riders — and is read
per item, because "all addressed" is the kind of green check that hides the one that isn't. The
blind-implementation property (§2.1) is what makes staged plans a control rather than a queue.

### 8.2 The human's jurisdiction

The standard phrase for the human role is "human in the loop," which describes continuous
supervision. That is not this. The human occupies a **jurisdiction**: specific question classes
where verification cannot settle the matter.

An instance may decide **what the evidence settles**. Only the operator rules on **what has no
seat-free answer** — ambiguous requirements, contradictions between sources, trade-offs with no
default, and every Ω_P. Rulings are recorded with provenance (who, when, under what evidence),
because a ruling laundered into an unattributed fact becomes a truth claim wearing a date — the
exact presentation-versus-reality failure the engine audits in its subject matter.

The human is not the final verifier of everything. The human is the final *ruler where
verification cannot settle the question*.

#### 8.2.1 A second jurisdiction, and it is evidential rather than evaluative

Through v0.3 this section described **one** jurisdiction: value questions with no seat-free answer.
The August 2026 arc exposed a second, and it does not fit that description.

Noticing that the same check has gone red three times across three sessions is not a value
judgement. It is an **observation about a distribution over time**, and it is **structurally
unavailable to any instance**, because each instance sees one session. The same holds for
recognising a defect's *shape* recurring across unrelated subsystems, for catching a figure quoted
in a summary that contradicts a table produced weeks earlier, and for knowing that a rule's premise
has quietly expired while the rule stayed internally correct.

These are evidential findings gated on **cross-session continuity**, not on authority. They belong
to the operator not because someone must decide but because **nobody else is positioned to see
them** — a jurisdiction created by the workforce's amnesia rather than by the limits of
verification.

Two consequences worth stating plainly:

- **The two jurisdictions have different failure modes.** Neglect the first and an instance
  self-certifies a value call. Neglect the second and nothing happens *visibly at all* — the
  pattern simply goes unnoticed, and each session's local reasoning remains impeccable. The
  second is the silent one, which by §4's own logic makes it the more dangerous.
- **It is partially mechanisable, and the mechanisation is the memory system.** `KNOWN_STATE.md`,
  the dated audit record and version control exist precisely to give a fresh instance synthetic
  access to a history it did not live. That they only *partially* succeed is what leaves the
  jurisdiction occupied: the operator remains the one party who holds the shape of what happened
  without holding its detail (§7.2).

**Honest limit on this whole section (§8).** The jurisdiction claim rests on observation of **one
operator**, with an unusual tolerance for being told he was wrong — an arc in which four operator
rulings were caught as wrong by receiving instances, and were caught only because the same operator
had established that refusing was expected. Whether this role is stable, whether it scales, and
whether it is available to someone with a different disposition are **untested**. The paper shows
what one such partnership required; it does not show that the requirement generalises.

Two standing delegations keep the seat from becoming a bottleneck: permission to *fix simple
errors on sight* (self-contained, witnessable this turn, single-revert reversible, no genuine
ruling embedded) and permission to *commit witnessed units immediately* (in-flight work is what
session mortality destroys). No delegation loosens the witness discipline; a fix is a claim
like any other.

The seat is nonetheless the scaling limit of the form, and §2.1's undocumented drift from
manual to auto-approval is evidence of pressure on it. Whether the jurisdiction can be held at
higher throughput, and what fails first when it cannot, is `[UNWITNESSED]`.

### 8.3 The economics

Three marginal costs, in ascending order: marginal *analysis* is nearly free; marginal
*verification* is expensive, because it consumes operator attention; marginal *standing
process* is most expensive of all, because it taxes every future session. The apparatus
instrumentation (§9) exists because the third cost is invisible at the moment each process
element is added.

---

## 9. Self-Instrumentation

### 9.1 The hazard

A verification apparatus built by fluent workers grows. Every incident mints a rule; every rule
is well written; the stack's cost rises monotonically while nothing measures whether the
marginal ritual still catches anything. The operational documents name the trap: *do not answer
"does the apparatus pay for itself?" by producing more well-formed apparatus-output.*

### 9.2 The instrument

As of 2026-08-10 the answer is instrumented rather than asserted (`ISSUES.md` OQ-276):

- **The catch bit.** Every audit's writeup must declare `Fired: live | latent | no` — a control
  fired and a consumer-visible verdict changed; a real defect found but conditional on inputs
  the system doesn't yet produce; or pure confirmation. The gate enforces the bit's
  *existence*; the rolling rate is *reported, never gated*, because a self-gating apparatus
  could launder its own worth.
- **The channel cap** (§7.6): the rule stack cannot grow without retiring.
- **The decay floor is deliberately unpinned** until enough bits exist to see the achievable
  value lattice; pinning a threshold before knowing the instrument's quantization is itself a
  named calibration error.

The current reading (2026-08-10, the instrument's adoption day): **zero bits** — the bit is
required only of audits dated after adoption, and none exist yet
(`python3 python/apparatus_instrument.py --check` → `catch-rate: no Fired: bits yet; channel
33/33; GREEN`). The honest publication is this empty reading with its date, not a
retrospectively reconstructed rate: back-filling bits onto old audits by re-reading them
would be single-coder retrospective classification of exactly the kind §2.3 flags.

### 9.3 Four efficacies, only two instrumentable

"Catch rate" can become a process metric detached from value unless the question is decomposed:

1. **Mechanical efficacy** — did the control fire? (Instrumented: the catch bit.)
2. **Epistemic efficacy** — did it expose something that would otherwise have survived?
   (Partly instrumented: the live/latent distinction, resting on a judgment.)
3. **Economic efficacy** — was that worth its standing cost? (Not instrumented; the channel cap
   bounds count, not value.)
4. **Programmatic value** — was the research worth doing? (Ω_P; no instrument possible.)

This hierarchy explains the refusal to self-gate: only 1 and 2 are measurable, and letting a
measure of 1 stand in for a ruling on 3 or 4 would be an absence/presence collapse at the level
of the apparatus's own worth.

**A fifth efficacy the instrument cannot see at all — restraint (added v0.4).** All four above
concern a control **firing**. Nothing in the scheme represents **a plausible action declined on
evidence**: an escalation not made, a spend not requested, a rewrite not attempted, a finding not
claimed because its control was one-sided. The August 2026 arc declined at least eight such
actions — among them a 219-call spend held back over a red self-test whose cause was not yet
understood, a preregistration not rebuilt because rebuilding would have destroyed the record it
pinned, and an assertion not relaxed inside the change that would have benefited from relaxing it.

**The `Fired:` bit has no encoding for any of them.** Its three values — `live`, `latent`, `no` —
all presuppose that something ran. A declined action produces no audit directory, no writeup, and
therefore no bit; the apparatus's most consequential outputs in that arc are invisible to its own
instrument, and would be scored as absence.

This is not a proposal to instrument restraint. Counting declines invites manufacturing them, and
the obvious metric — *escalations declined* — is gameable in the direction of inaction, which is a
worse failure than the one it measures. **It is recorded as a declared blind spot**: the catch
rate reported by §9.2 is a rate over *actions taken*, and any reading of it as the apparatus's
total contribution omits a category the apparatus itself has no way to count.

### 9.4 An observation, not yet a finding

v0.1 reported that audit cadence fell and read this as "the intended absorption curve, not
decay" — tripwires converting recurring manual catches into standing gates. That reading is
withdrawn to OPEN, on two grounds. The manifested numbers (2026-08-10): June 98 directories;
July 45; August 1–10, 10 (1.0/day, projecting ≈31 for the month).

First, the absorption reading is a post-hoc selection among at least three hypotheses
(absorption; attention decay; a change of project phase) that the three numbers do not
separate. Second, the arithmetic is softer than v0.1's gloss: 98→45 is a factor of 2.2, and
the August projection (~31) is a factor of ~1.5 below July — so "fell ~3× per month" is not
what these numbers say.

What absorption predicts, and decay does not, is that the *standing gate* catch count rises as
the audit count falls. That series is `[UNWITNESSED]`; collecting it is the cheapest available
test and is now RQ-c.

### 9.5 The honest limit

The instrument closes one loop, not all. The catch bit can be gamed — everything tagged
`latent` to look useful, against which the declared defence is the per-bit justification line.
The channel cap bounds count, not quality. It cannot see restraint at all (§9.3). And the deepest
question has no instrument because it is an Ω_P: a value ruling belonging to the person bearing
its costs.

**The sharper limit, and it is not about depth (v0.4).** Through v0.3 this section rested on
"judgment is localized, not eliminated," which locates the limit at *how much* verification is
enough. The August 2026 arc says something more specific and less comfortable.

Every apparatus of this kind runs on **exemptions** — cases where a rule correctly does not apply.
They are unavoidable and often right: a self-test instrument that cannot guard itself, a manifest
scoped to files rather than conversations, a pin list restricted to specifications. Each is
defensible. The arc supplied one of each kind, and the difference in outcome was total:

| exemption | stated? | outcome |
|---|---|---|
| a detector cannot guard itself | **written into the source with its reason** | correct, examined, survives review |
| a pin manifest covers texts, not executables | **never articulated** | a run made under a green freeze that could not produce data |

> **An unstated exemption is indistinguishable from an unnoticed one, and no gate can separate
> them, because the gate is downstream of the exemption.** The frame a check operates within is
> chosen before the check runs; a check cannot report what its own scope excluded. This is why
> every inventory in the arc that "looked complete" was complete *within a frame nobody had
> written down* — and why the completeness read as real: **every item in it belonged.**

So the model's honest limit is an act of **documentation**, not of verification. The mechanism is a
person writing down the exemption they just took — and the reason that cannot be delegated to a
further instrument is §6.4's: the further instrument takes exemptions too.

The claim, restated for v0.4: the model does not eliminate judgment, and does not merely localize
it. **It localizes judgment at declared points and then depends, irreducibly, on the declaring
being done.** Where the declaration is skipped, the apparatus continues to return green.

---

## 10. Reflexivity, Bounded

The research program's object-level theory is load-bearing in its own construction. v0.1
developed this at length; this revision keeps the three applications that do methodological
work and moves the full mapping to Appendix C, because a reader should be able to adopt the
methodology without first mastering the philosophy. Whether that is actually true is RQ-f.

**Gauge: verdicts carry their position.** A classification cited without its position is
Type-C-ambiguous; the tooling requires per-key tags and stability-across-positions verdicts
rather than bare labels. A diagnostic that hides its own position becomes the "manufactured
centre" the framework exists to expose.

**Seat: verification depth is a declared position.** There is no neutral "verified enough,"
only a stopping position taken. This is where the framework earns its keep, because the usual
answers to "how much verification is enough?" are arbitrary, conventional, or deferred to a
reviewer. The framework's answer: the honest object is not "fully verified" but *verified to
tier X, with falsifier Y remaining* (§6.6). Declaration is the residue of neutrality — since no
position-free verdict exists, the only honest move is to declare the position, which licenses
the model's *declared* skips, residues, and stopping points: visible admissions exactly where a
fluent system would otherwise emit success-shaped silence.

**Drift: specification divergence is doctrinal drift.** The commitment-systems layer
distinguishes *marked revision* (drift acknowledged, recorded, owned) from *interpretive
accretion* (the operational layer absorbs new structure while the formal layer pretends nothing
changed). The v6 reissue of that specification opens by applying the distinction to itself:
between v5.2 and v6, whole subsystems were built in code with no spec section — interpretive
accretion at the spec layer — and the reissue records the acknowledged-drift event in the
framework's own vocabulary, then installs a machine-checked tripwire (spec enumerations pinned
to code in the standing gate) so the third occurrence is caught by a checker rather than an
audit. §2.1's approval-posture drift is the second such event, and is recorded here.
Documentation governance is drift governance.

**Two cautions.** First, the *bootstrap*: the reflexivity is best read as a consistency check
rather than a foundation, because the practice and the framework matured together and the
chronology (§1.2) is not yet recorded finely enough to say which constrained which. Second,
and more seriously: a framework that supplies both the object theory and the audit vocabulary
can absorb its own probes, which is Θ-7's structure one level up. Agreement between theory and
practice is therefore *not* evidence for either. What would be evidence is a recorded case
where practice contradicted the framework and the framework lost. This draft cannot cite one.
Until it can, §10 should be read as coherence, not corroboration.

---

## 11. Limitations

- **One institution, one operator, one model-family era.** Everything here was witnessed in a
  single repository. No claim of generality is supported (RQ2, RQ3).
- **No control group.** The discipline has been shown to *detect* silent defects. It has not
  been shown to *reduce* them relative to lighter process (RQ1).
- **Selection in the audit record.** Audit directories are created on suspicion; the incidence
  figure is a yield, not a prevalence (§4.5).
- **Retrospective single-coder classification.** The taxonomy was applied after the patterns
  were named, without blind coding or inter-rater measurement (§2.3).
- **Retrospective coherence risk.** One elegant spine, applied by its author to its own
  evidence, is exactly the shape a confabulation would take.
- **Non-homogeneous window.** Supervision depth changed mid-window from manual to auto-approval
  without a recorded date or ruling (§2.1).
- **Workforce exposure confound.** The workers drafting the rules have read the literature the
  rules resemble (§5.2).
- **Unfilled denominators.** Sessions and instances remain `[UNWITNESSED]` — no session log
  exists, so per-session rates (audit rate, defect rate) cannot be computed; commits (1,468)
  are the nearest recorded proxy (Appendix B).
- **Cost profile.** The apparatus is calibrated to a program where a silently wrong claim
  poisons downstream philosophy. A project whose errors are loud or cheap should buy much less
  of it.

---

## 12. What Appears to Be New

Calibrated in three grades, because v0.1 conflated them.

**Pattern novelty: weak.** The individual patterns have ordinary names in software engineering
(§5.1). No claim.

**Systematization novelty: moderate, and testable.** A dated, instrumented record of silent
failure modes in LLM-produced *research* work, with a measured yield within its audit record,
tied to a specific worker model. To the author's knowledge no comparable record exists; that
is a fact about the author's search, and §5.3 lists literatures not yet read. The novelty is
therefore stated as RQ2 rather than as a conclusion.

**Institutional novelty: the strongest claim.**

1. **The memory economy.** Promotion tests, staleness ladders, roll-off, and a capped rule
   channel constitute an explicit answer to knowledge accumulation *by* amnesiacs. Human
   institutions solve this tacitly through personnel continuity; here it had to be built, which
   makes it legible and portable. The framing that distinguishes it from standard
   retrieval-augmented approaches: memory here is an *attention-economics* problem, not a
   retrieval problem — the scarce resource is always-loaded context paid every session, not
   storage.
2. **Self-instrumentation of the verification apparatus**, with a catch-rate readout whose
   interpretation is deliberately reserved to a human seat, and a channel cap that forces
   retirement.
3. **The organizational form**: one operator without a software team; staged plans;
   blind implementation; serial write-lock at the institutional-memory boundary; human
   jurisdiction rather than human oversight.
4. **The accretion finding** (§7.6): redundancy, not corruption, as the dominant memory
   pathology. Provisional at n = 1.

---

## 13. Research Agenda

Four primary questions, ordered by what the paper most needs. Four secondary questions follow.
Each states its falsifier.

**RQ1 — Comparative baseline (highest priority).** Does the discipline *reduce* silent defects,
or only detect them? Without a control, §4.5's 42% is a numerator without a denominator across
regimes. Method: audit long-running LLM-heavy repositories that lack witness discipline, using
the same census instrument with its own positive controls; where possible, add a
*within-repository* historical control by censusing this repository's own pre-discipline
period. *Falsifier: comparable silent-defect visibility without the discipline — which would
mean the discipline detects but does not reduce.*

**RQ2 — Generality of the taxonomy.** Do the six patterns recur, at comparable rates, in other
LLM-operated repositories? Method: replicate the audit census on independent projects;
classify defects **blind** against the six patterns plus "other," with a second coder and an
inter-rater statistic — which also retrospectively tests this paper's own coding (§2.3).
*Falsifier: silent-failure classes dominated by shapes the taxonomy cannot express, or a rate
low enough that the discipline's cost is unjustified outside this substrate.*

**RQ3 — Transfer.** Can the model be adopted cold? Method: a stepped design — first, the same
operator adopting it on a green-field project; then a fresh operator (human, without a software
team) and a fresh instance given this paper plus the operational documents. Measure
time-to-first-witnessed-claim and defect rates against an ad-hoc control. *Falsifier: no
measurable defect-rate difference, or the discipline unlearnable from documentation alone —
which would imply the model is operator-tacit, a finding in itself.*

**RQ4 — Apparatus economics.** Does the standing cost remain justified as the apparatus
matures? Method: with the `Fired:` instrument accumulating, model the yield curve; test §9.4's
absorption hypothesis by collecting standing-gate catch counts alongside falling audit counts;
set a decay floor only once the value lattice is visible. *Falsifier for the apparatus itself:
a sustained near-zero live rate at undiminished process cost, with no compensating rise in gate
catches.*

Secondary, in the order they become tractable:

- **RQ-a — Memory economics, formalized.** Token cost per session × sessions versus expected
  cost of the mistake class × incidence, with a stakes term the current promotion test lacks
  (§7.3). The 2026-08-10 prune supplies calibration data. *Falsifier: an optimal policy
  materially different from the promotion test's — e.g., retrieval-on-demand dominating
  always-loaded rules.*
- **RQ-b — Provenance of the rules.** Test whether rule-minting events follow witnessed
  incidents or reading/exposure events, acknowledging that workforce exposure cannot be
  removed by git history alone (§5.2); an operator diary or interview layer is required.
  *Falsifier: rule provenance tracing to external methodology sources rather than internal
  incidents.*
- **RQ-c — Churn floors as a general standard.** Is per-observable churn measurement a
  precondition for any LLM-judged metric? Method: apply the same-input redraw protocol to
  published LLM-evaluation metrics; measure how many reported effects sit inside their own
  churn floors. *Falsifier: churn floors generally negligible relative to reported effects.*
  This may warrant separate publication.
- **RQ-d — The reflexivity dependency.** Is §10 load-bearing or decorative? Method: RQ3's
  transfer experiment in a 2×2 — operational documents with and without the framework layer,
  ensuring the "without" arm removes the framework and not merely its vocabulary. *Falsifier:
  no difference in discipline adherence or drift, implying the methodology is separable from
  its theory.*

---

## 14. Conclusion: Continuity Without Continuous Workers

`docs/debugging_philosophy.md` closed by moving philosophy toward computer science: stop asking
"how can this be?" and start asking which mechanism generated the contradiction. This paper is
the same stance applied to the laboratory that executed that move. The workforce generates
fluent completions the way reasoning systems generate paradoxes — by filling gaps with
well-formed content and calling the result whole. The response is the same in both domains:
classify the failure by its generative mechanism, fix the mechanism class rather than the
instance, and carry the provenance — the frame, the index, the seat, the witness — with every
value to the site where it is read.

What results is not a way to make language models reliable, and not yet a demonstrated way to
make an institution reliable. It is an attempt at something more specific: **an institution
whose reliability does not depend on the reliability or the persistence of any worker.** If no
worker persists, no worker can carry institutional continuity; continuity must therefore become
an artifact of the institution — its records, its gates, its declared seats, its capped
channel. Every mechanism in this paper is one answer to a single question: *where does the
institution remember that this matters?*

The daemon Θ-7 filed validated joy-returns from thermal noise because nothing in its loop could
register absence. The model documented here is the engineering of that registration — including,
if it is honest, the registration of its own absences, which is what §11 and the `[UNWITNESSED]`
markers in this draft are for.

---

## References

### Internal (authoritative for all repository claims; paths relative to repository root)

1. `docs/debugging_philosophy.md` — the trifurcation framework; origin of the program.
2. `docs/deferential_realism_paper_v8.md` — seat/gauge/orientation ontology.
3. `docs/deferential_realism_paper_v7.md`; `docs/deferential_realism_paper_v6.13.1.md` —
   committer- and observer-axis records.
4. `docs/seat-theorem-v1.md` — the Coupling Theorem; declaration as the residue of neutrality.
5. `docs/commitment_systems/commitment_systems_sketch_v6.md` — drift architecture; the
   self-applied acknowledged-drift event (§1.1 there).
6. `docs/omega_variables.md` — the Ω_E / Ω_C / Ω_P typology.
7. `docs/technical/build_discipline.md` — the six patterns, the spine, the witness rules, with
   dated instances and diagnostics.
8. `docs/design/design_discipline.md` — the declared seat; what the engine must not become.
9. `CLAUDE.md`, `KNOWN_STATE.md`, `ISSUES.md`, `audits/README.md` — the four records (§7.2).
10. `python/apparatus_instrument.py`; `ISSUES.md` OQ-276 — the catch-rate instrument and
    channel cap.
11. `agent/c-orchestrator.py`; `agent/analysis.py`; `agent/uke_narrative_orchestrator.py` —
    the three stress-test tools (§2.1).

### External (analogues; representative, not exhaustive; all read)

12. Beck, K. (2003). *Test-Driven Development: By Example.* Addison-Wesley.
13. Chambers, C. D. (2013). Registered Reports: A new publishing initiative at *Cortex*.
    *Cortex*, 49(3), 609–610.
14. Nosek, B. A., Ebersole, C. R., DeHaven, A. C., & Mellor, D. T. (2018). The preregistration
    revolution. *PNAS*, 115(11), 2600–2606.
15. Nygard, M. (2011). Documenting architecture decisions. (Architecture Decision Records;
    widely reproduced.)
16. Popper, K. (1959). *The Logic of Scientific Discovery.* Hutchinson.
17. Ryle, G. (1949). *The Concept of Mind.* Hutchinson. (Knowing-how vs. knowing-that; the
    tacit-knowledge question behind RQ3/RQ-d.)

*Candidate literatures identified but not yet read are listed in §5.3 and are deliberately
absent from this list.*

---

## Appendix A: The Model on One Page

**Worker model:** fluent, fast, stateless, directionally fallible (success-shaped absence),
recursively applicable.

**The one defect:** every recurring failure is an absence presenting as a presence.
**The one principle:** no epistemically meaningful value crosses a boundary without its status.
**The two amnesias:** worker amnesia is the substrate; institutional amnesia is the failure.

**Witness calculus:** paste-or-untag · positive control on every probe · witness → validating
witness → adequate witness · an introduced instrument is itself a claim · pre-register outcomes
before results · same-generator agreement is not replication · declared stopping points
(falsifier-or-OPEN).

**Memory:** episodic (dated log) → promotion test → semantic (always-loaded rules, finite
channel, gated exchange); staleness ladder on every cited fact; opens typed Ω_E / Ω_C / Ω_P by
resolution operation.

**Organization:** one writer above the institutional-memory boundary; staged plans; blind
implementation; evidence decides what evidence can; humans rule what only humans can (Ω_P),
with owned, dated rulings; fix simple errors on sight below a calibrated threshold; commit
witnessed units immediately.

**Self-instrumentation:** every audit declares whether anything fired (live / latent / no); the
rate is reported to the human seat, never self-gated; the rule stack cannot grow without
retiring; mechanical and epistemic efficacy are measured, economic and programmatic value are
ruled.

**Reflexivity:** verdicts carry their position; verification depth is a declared seat;
specifications drift like doctrines and get the same acknowledgment discipline. Agreement
between theory and practice is coherence, not corroboration.

---

## Appendix B: Numbers Manifest

Every quantity in this paper, with the command that regenerates it. Rows without a command are
`[UNWITNESSED]` and must not be cited. This appendix is the paper's own witness (§2.5). All
commands run from the repository root; all discharged rows were last run 2026-08-10.
Historical incident figures (marked †) are claims about *what the record documents*, not about
current state — their regeneration command locates the documenting text, and the underlying
defect is fixed, so no live command can reproduce the defect itself.

| § | Figure | Value (2026-08-10) | Regeneration command |
|---|---|---|---|
| 1.1 | Prolog modules (top-level) | 123 files, 41,314 lines | `ls prolog/*.pl \| wc -l`; `wc -l prolog/*.pl \| tail -1` |
| 1.1 | Python scripts | 127 top-level; 267 incl. subpackages (excl. archives), 96,995 lines | `ls python/*.py \| wc -l`; `find python -name '*.py' -not -path '*/archives/*' \| wc -l` |
| 1.1 | Audit directories | 175 | `ls -d audits/*/ \| wc -l` |
| 1.1 | Tracked open questions, by status | total 276 = open 95 + resolved 146 + mitigated 18 + partial 7 + future 5 + disposed 5 (reconciliation: §1.1) | `python3 python/issues_status.py \| tail -1` |
| 1.1 | Corpus stories, per leg | 249 + 960 + 960 + 1005 + 1001 = 4,175 (moves mid-session; live leg was 199 on 2026-07-24) | `for d in testsets testsets_haiku testsets_flash testsets_kimi testsets_sonnet; do ls prolog/$d/*.pl \| wc -l; done` |
| 1.1 | Commits | 1,468 | `git rev-list --count HEAD` |
| 1.1 | Repository initialized | 2026-01-12 (`41db1d0b`) | `git log --reverse --format='%ad %h' --date=short \| head -1` |
| 1.2 | `CLAUDE.md` / `ISSUES.md` / `KNOWN_STATE.md` born | 2026-02-28 / 2026-05-28 / 2026-05-31 | `git log --reverse --format='%ad %h' --date=short -- <file> \| head -1` |
| 1.1 | Sessions / instances / audit rate | `[UNWITNESSED]` — no session log exists; commits are the nearest proxy | — |
| 4.3 | P1 instance: frozen artifact n † | 563 vs corpus 772 | `grep -n 'n=563' docs/technical/build_discipline.md` |
| 4.3 | P5 instance: certifications † | 0/404 carry beneficiary signal | `grep -n '0/404' docs/technical/build_discipline.md` |
| 4.3 | P6 instance: suppressed warning duration † | four months | `grep -n 'four months' docs/technical/build_discipline.md` |
| 4.5 | Audit directories with ≥1 silent defect | **73 / 175 (42%)** — corrected from 77/175, see §4.5 | `grep -rl 'for its whole life\\\|never fired\\\|never ran\\\|read.*0 for\\\|was never\\\|silently' --include='*.md' audits/ \| cut -d/ -f2 \| sort -u \| wc -l` — run from repository root; the path prefix is load-bearing (§4.5's correction block). Coding rule: keyword proxy over audit prose, single pass, not severity-weighted; a directory counts once |
| 7.6 | Memory content files before / after | 113 / 53 (114 / 54 incl. the index) | after: `ls ~/.claude/projects/-home-scott-bin-structural-dynamics-model/memory/*.md \| wc -l`; before: `tar tzf ~/.claude/projects/-home-scott-bin-structural-dynamics-model/memory_pre_prune_backup_20260810.tar.gz \| grep -c '\.md$'` |
| 7.6 | Deleted / merged | 25 deleted; 45 merged into 10 cluster files + 2 absorptions | arithmetic over the backup vs. current listing; per-file dispositions in the 2026-08-10 session record (`KNOWN_STATE.md` 2026-08-10) |
| 7.6 | Genuinely wrong content | 2 files | named in §7.6; verify against the backup tarball |
| 7.6 | Rule-stack cap | 33 (currently 33/33) | `python3 python/apparatus_instrument.py --check` |
| 7.6 | Always-loaded index size, before / after | 17,416 / 9,906 chars (≈4,400 / ≈2,500 tokens, est. at 4 chars/token) | `tar xzf <backup> -O memory/MEMORY.md \| wc -c`; `wc -c <memory>/MEMORY.md` |
| 9.2 | Rolling catch rate | 0 bits (adoption day; forward-only) | `python3 python/apparatus_instrument.py --check` |
| 9.4 | Audit cadence June / July / Aug 1–10 | 98 / 45 / 10 | `for m in 2026-06 2026-07 2026-08; do ls -d audits/$m-* \| wc -l; done` |
| 9.4 | Standing-gate catch counts over the same months | `[UNWITNESSED]` — the absorption test (RQ4) | — |

**The manifest's positive control** — the requirement that at least one row be verified by
perturbing the underlying state and watching the command's output move — is discharged twice.
Deliberately: the channel-cap command's selftest plants an over-cap index and a conforming
twin on every run and requires the check to fire on one and not the other. Accidentally, and
better: discharging the incidence row *changed its value* (77 → 73) by exposing a unit error
the summary had been carrying — the manifest moved a number the prose had already spent,
which is the strongest evidence available that these commands are measurements rather than
decorations.

---

## Appendix C: The Full Framework Mapping

*Moved from v0.1 §7. Retained for readers of the DR corpus; §10 carries everything the
methodology needs.*

DR's ontology (v8): for any contentful question there is one **seat** (the content-position a
verdict is audited against), many **gauges** (positions from which it is read), and an
**orientation** (how the holder stands toward its commitment — declared or posed as natural;
kept or drifting). The methodological mapping in full:

- *Seat* → verification depth; the declared stopping position; the human's Ω_P jurisdiction.
- *Gauge* → the position a verdict is read from; per-key tags; stability-across-positions
  verdicts; the prohibition on bare labels.
- *Orientation* → whether a rule is held as declared or presented as natural; the ruling-versus-
  fact distinction in §8.2; the prohibition on laundering rulings into unattributed facts.
- *Coupling theorem, Corollary 2a* (declaration as the residue of neutrality) → the licence for
  declared skips, declared residues, declared stopping points.
- *Marked revision vs. interpretive accretion* → spec-vs-code drift; the v6 acknowledged-drift
  event; §2.1's approval-posture drift.
- *Ω typology* → typed openness (§7.5) and the routing of work by resolution operation.
- *Trifurcation A/B/C* → §4.6's repair mapping.

The justification structure v0.1 asserted — that the same analysis which says a constraint's
beneficiary cannot see its extraction says a verification ritual cannot see its own decay — is
retained here as an analogy that motivated the design, not as an argument that supports it. See
§10's second caution.

---

*CC0 Universal. Draft v0.3, 2026-08-10. Composed from repository sources and multi-model
review; Appendix B discharged against the repository this date, producing one marked
correction to the paper's central statistic (§4.5). Successor drafts should treat §13 RQ1 as
the work queue and §12 as the claims most in need of adversarial reading. Remaining declared
residue: sessions/instances denominators (no session log exists); the standing-gate catch
series (RQ4); the `[OPERATOR]` slots in §1.2; the §5.3 unread literatures; and the severity
weighting §2.2 notes is unrecorded.*
