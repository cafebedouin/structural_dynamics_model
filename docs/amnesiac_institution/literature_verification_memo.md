# Literature Verification Memo — for v0.4 of *The Amnesiac Institution*

**Date:** 2026-08-10. **Scope:** verification of citations returned by three model literature
searches (Gemini, Perplexity, Grok) against the live web. **Status of this memo:** the
verified/unverified split below is itself the witness; rows marked UNCHECKED must not be cited
until someone runs the check.

---

## 1. Verification results

### 1.1 What I checked and confirmed

Four spot-checks, chosen adversarially — the three highest-stakes claims plus the one citation
Grok gave *without* an identifier, on the theory that a missing ID is where a fabrication would
hide. All four resolved.

| Claim under test | Result | Source |
|---|---|---|
| Advani (2026), "From Confident Closing to Silent Failure: Characterizing False Success in LLM Agents," arXiv:2606.09863 | **CONFIRMED.** Single author Laksh Advani, submitted 1 Jun 2026, cs.LG. Abstract, PDF, and third-party citations all located. Reported figures match Grok's summary. | arxiv.org/abs/2606.09863 |
| Wu (2026), "When Errors Become Narratives: A Longitudinal Taxonomy of Silent Failures in a Production LLM Agent Runtime," arXiv:2606.14589 | **CONFIRMED.** Wei Wu, independent researcher, submitted 12 Jun 2026, cs.SE. Five-class taxonomy and the *fail-plausible* term are as described. | arxiv.org/abs/2606.14589 |
| Chen (2025), "Evidence-Bound Autonomous Research (EviBound)," arXiv:2511.05524 | **CONFIRMED.** Ruiying Chen, submitted 28 Oct 2025, cs.AI, 27 pp. Dual-gate architecture and the 100% → 25% → 0% ablation are as described. Cited by several subsequent papers. | arxiv.org/abs/2511.05524 |
| "Science One / ScientistOne + Chain-of-Evidence," Google Research, no ID given | **CONFIRMED.** Google Research blog post dated 30 Jul 2026; underlying paper "ScientistOne: Towards Human-Level Autonomous Research via Chain-of-Evidence," arXiv:2605.26340. | research.google/blog/science-one-framework-... |

A fifth confirmation arrived incidentally: "Confident and Wrong: Silent Semantic Failures in
Coding Agents," arXiv:2603.25764, which Grok listed in its secondary cluster.

**Verdict on the reviews.** Grok's search is reliable on the sample tested — 5/5, including the
riskiest row. This is the opposite of what I expected and it should update how you weight that
source. It does *not* license accepting the untested rows; it licenses testing fewer of them.

### 1.2 What I did not check — UNCHECKED, do not cite

Everything below is plausible and none of it is verified by me:

- AbsenceBench (Fu et al., 2025), arXiv:2506.11440
- "Always-On Agents" survey, arXiv:2606.30306; "Memory for Autonomous LLM Agents," arXiv:2603.07670
- Silent-failure cluster: arXiv:2606.08162, arXiv:2608.02786
- Dietterich (2018), "Robust Artificial Intelligence and Robust Human Organizations," arXiv:1811.10840
- All of Perplexity's rows: W3C PROV-Overview; Goddard, Roudsari & Wyatt (2012) on automation
  bias; Sharma et al. (2023/2025) on sycophancy; Veazie et al. (2019) HRO evidence brief;
  Lewis (2005) on transactive memory; de Holan & Phillips (2004); Kluge & Gronau (2018)
- Gemini's single return: Kelly (2025), arXiv:2508.04995

**Positive-control gap.** Neither Perplexity nor Grok was given seeded positives, so neither
review's *negative* findings are interpretable. Nobody has yet demonstrated that these searches
can find a work known to exist and then correctly report failing to find one that doesn't.
Before §1.3's "no literature identified" claim is finalized, one search must be run with
planted controls.

---

## 2. Damage assessment

The confirmed literature costs the paper more than either review said. Ranked by severity.

### 2.1 SEVERE — the novelty claim in §12 no longer stands as written

§12 currently says, of the systematization claim: *"To the author's knowledge no comparable
record exists."* That is now false, and the closest competitor is very close indeed.

**Wu (arXiv:2606.14589) is structurally the same paper.** A single independent researcher, a
single long-running LLM system, an eight-week window, 22 incidents with root-cause postmortems,
a five-class mechanism-oriented taxonomy of silent failure, and one named meta-pattern — a
failure whose error signal never reaches a human in actionable form. Wu even names the LLM-
specific escalation (*fail-plausible*: the model converts the error into fluent narrative
delivered to the user), which is the Amnesiac Institution's P4 and P6 arriving from an
independent direction. Wu was posted 12 June 2026 — inside your measurement window.

**Advani (arXiv:2606.09863) supplies the denominator you lack** and does it with text-
independent ground truth: 9,876 tau2-bench and 1,879 AppWorld trajectories, false success at
45–48% of failures in single-control domains and 75.8% among self-assessing coding-agent
trajectories. It also independently establishes that LLM judges cannot detect the failure —
no judge configuration exceeded 0.65 AUROC, and judges anchored on confident closing language
rather than verified state change. That is §3.2's "review-by-reading is weak" and §6.4's
recursion problem, measured.

**Consequence.** The systematization novelty claim must be withdrawn, not softened. What
survives is narrower and still real: nobody in this cluster is describing an *institution*.
Advani measures a failure mode across benchmarks; Wu taxonomizes one runtime's incidents;
neither addresses memory economics, human jurisdiction, organizational form, or apparatus
self-instrumentation. Claim the institution, not the taxonomy.

### 2.2 SEVERE — C3 has concurrent architectural realizations

EviBound (arXiv:2511.05524) implements the witness calculus as a system: a pre-execution
approval gate on acceptance-criteria schemas, a post-execution verification gate requiring a
queryable run ID, required artifacts, and FINISHED status; claims propagate only when backed by
machine-checkable evidence. Its ablation — 100% hallucinated claims prompt-only, 25%
verification-only, 0% dual-gate, at ~8% overhead — is the causal evidence your RQ1 wants,
obtained on a benchmark rather than an institution.

Google's Chain-of-Evidence (arXiv:2605.26340, blog 30 Jul 2026) states your §4.2 design
principle almost exactly: every claim must trace through a recorded chain of supporting claims
and evidence to a grounding source, with claim types typed by required evidence-chain shape,
and an audit that cross-checks bibliography entries against scholarly APIs to catch phantom
references.

**Consequence.** "No epistemically meaningful value crosses a boundary without its status" is
no longer a candidate original principle. It is a convergently-arrived-at statement of an idea
with at least two concurrent formal expressions. Cite them and claim the convergence, which is
more interesting than priority: three independent efforts, working from different substrates in
the same twelve months, arrived at gate-before-claim architectures.

### 2.3 MODERATE — §1.3's fit claim needs narrowing

"A field whose methodological literature is thin enough to be surveyed in an afternoon" is not
survivable. Perplexity's proposed replacement is close to right: the defensible claim is about
the *configuration*, not the literature. See §3.2 below for drop-in text.

### 2.4 MODERATE — the incidence figure must not be compared to Advani's

There will be a temptation to write "Advani finds 45–75%, we find 42%, consistent." Resist it.
Advani's denominator is *failures*; yours is *audit directories*. His numerator is trajectories
where the agent asserted completion against contradicting ground truth; yours is directories
containing at least one silent or never-fired defect. The numbers are not commensurable and
juxtaposing them would be a Type-C error of exactly the kind §1.1 just resolved for the
open-question count.

### 2.5 OPPORTUNITY — RQ2 just became executable

RQ2 (taxonomy generality, blind re-coding) previously had no external comparison set. It now
has two: Wu's five mechanism classes and Advani's false-success operationalization. Blind-code
Wu's 22 incidents against your six patterns and your audit sample against Wu's five classes,
and report the confusion matrix. That is a weekend of work and it converts RQ2 from a proposal
into a result.

---

## 3. Drop-in revisions

### 3.1 Replace §5.3 ("Literatures not yet searched") with a new §5.3 and §5.4

**§5.3 Concurrent literature (2025–2026).**

> The failure class this paper names became an active research front during the measurement
> window, which both corroborates the diagnosis and removes any claim of priority. Three lines
> are directly relevant.
>
> *Measurement.* Advani (2026) characterizes *false success* — an agent asserting completion
> while environment state contradicts it — across 9,876 tau2-bench and 1,879 AppWorld
> trajectories with text-independent ground truth, finding it in 45–48% of failures in
> single-control domains and 75.8% of self-assessing coding-agent trajectories. The same work
> shows LLM judges cannot detect it, with no configuration exceeding 0.65 AUROC and judges
> anchoring on confident closing language rather than verified state change. This is
> independent, better-controlled support for §3.2's rejection of review-by-reading and for the
> §6.4 recursion problem. Its denominators are trajectories, not audit directories, and its
> rates are therefore not comparable to §4.5's yield.
>
> *Taxonomy.* Wu (2026) reports a longitudinal study of one production agent runtime — 22
> incidents with root-cause postmortems over eight weeks — and derives a five-class,
> mechanism-oriented taxonomy of silent failure, identifying as its meta-pattern a failure whose
> error signal never reaches a human in actionable form. Wu's class D, *fail-plausible* — the
> model transforms an error into fluent narrative delivered to the user — is this paper's P4 and
> P6 reached independently. Wu's study and this one are near-twins in method: single system,
> single observer, incident-derived taxonomy. Their convergence is the strongest available
> evidence for C2, and their independence makes the comparison in RQ2 immediately executable.
>
> *Architecture.* EviBound (Chen, 2025) implements gate-before-claim as a system: an approval
> gate validating acceptance criteria before execution and a verification gate requiring a
> queryable run identifier, required artifacts, and terminal status before a claim propagates,
> reducing hallucinated claims from 100% (prompt-only) to 0% (dual gates) at roughly 8%
> overhead. Google's Chain-of-Evidence framework (2026) states the same requirement as a
> property of research artifacts: every claim must trace, through a recorded chain, to a
> grounding source, with claim types carrying required evidence-chain shapes.
>
> The paper's §4.2 principle is therefore not original. Three efforts working from different
> substrates — benchmark evaluation, production runtime, autonomous research systems, and this
> repository — converged on carrying status with the value within twelve months. That
> convergence is a stronger result than priority would have been, and it is offered as such.

**§5.4 Literatures identified but not yet verified.** Keep the existing list, retitled, with the
addition that Perplexity's and Grok's candidate rows (§1.2 of the verification memo) are
recorded as unverified and are not cited anywhere in this paper.

### 3.2 Replace §1.3's fit sentence

Delete: *"in a field whose methodological literature is thin enough to be surveyed in an
afternoon."*

Insert: *"working in a configuration for which this search identified no existing methodology:
a single-operator research institution whose code, analysis, and documentation are produced
principally by stateless LLM instances under an explicit witness-and-memory discipline. Adjacent
literatures are dense and are surveyed in §5 — silent-failure measurement, evidence-gated agent
architectures, agent memory, high-reliability organizing. The claim is about the configuration,
not about the absence of relevant work, and it remains a fact about the search until §5.4's
unverified rows are read."*

### 3.3 Rewrite §12's systematization paragraph

Delete the paragraph beginning *"Systematization novelty: moderate, and testable."*

Insert:

> **Systematization novelty: withdrawn.** v0.3 claimed that no comparable dated, instrumented
> corpus of silent failure modes for LLM-produced work existed, hedged as "to the author's
> knowledge." A literature search conducted 2026-08-10 falsified this. Wu (2026) reports an
> incident-derived taxonomy from a production agent runtime, and Advani (2026) measures the same
> failure class at benchmark scale with ground truth. Both were published inside this paper's
> measurement window. The claim is withdrawn rather than softened; §5.3 records the concurrent
> work and treats the convergence as the finding.

Then amend the institutional-novelty preamble to read: *"Institutional novelty: the only
surviving claim, and correspondingly the one on which the paper now rests."*

### 3.4 Amend RQ2

Add to the method: *"Two external comparison sets now exist. Blind-code Wu's five mechanism
classes against the six patterns in both directions, and report the confusion matrix; separately,
apply Advani's false-success operationalization to the audit record where trajectories permit.
Disagreement is the result, not a problem: a defect shape one taxonomy expresses and the other
cannot is a finding about both."*

---

## 4. What to do next, in order

1. **Read Wu (arXiv:2606.14589) in full.** It is the nearest neighbour and the paper cannot be
   circulated without engaging it. Budget: one sitting.
2. **Read Advani (arXiv:2606.09863) for the judge-failure section**, which bears directly on
   §6.4 and is better evidence than anything in the repository.
3. **Skim EviBound and the ScientistOne CoE paper (arXiv:2605.26340)** for §5.3's architecture
   paragraph. Do not over-invest; the delta is that neither is an institution.
4. **Run one search with planted positive controls** before §1.3 is finalized, so the "no
   methodology identified" claim carries a validating witness rather than a witness (§6.2).
5. **Do the RQ2 blind re-coding.** It is now the cheapest available conversion of an open
   question into a result.
6. **Leave the unverified rows in §5.4 unverified and uncited** until read. The paper's own
   §6.3 rule applies to its own bibliography, and a fabricated citation in a paper about
   success-shaped absence would be the most expensive error available.

---

*All URLs above were resolved on 2026-08-10. Rows in §1.2 were not. This memo carries the same
staleness ladder as the paper: a confirmed citation is a live re-witness on its date, and an
arXiv listing can be withdrawn.*
