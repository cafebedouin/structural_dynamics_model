# UKE_REFEREE v0.2 [Universal Knowledge Evaluator — Structural Referee Report on Another Author's Work]

**Status:** Draft, one worked instance
**License:** CC0-1.0
**Parent Suite:** UKE Protocol Suite

---

### §0. FOUNDATION

**Purpose:** Referee a substantive artifact **that this practitioner did not write and is not publishing** — a paper, prospectus, framework, or research program — so its author learns where the load actually sits. Not "is this right?" but **"what is holding this up, and what happens if it isn't there?"**

**Origin:** Two review failures bracket this protocol. The **objection list**: twenty true criticisms, correctly stated, leaving the author with no idea which one matters. The **deference failure**: an instrument fires, and the reviewer reports the firing instead of adjudicating it against the source. Both look like review and transfer nothing. A referee report's value is concentrated in *localization* and *ranking* — finding the one break the other nineteen symptoms are, and naming the single move that would convert the work.

**Core Invariants:**

* **Localization > Enumeration.** Twenty objections with no spine is worse than one break with nineteen instances hanging off it.
* **Adjudication > Deference.** Any instrument's output — engine signatures, omegas, this protocol — is a *hypothesis about the artifact*, contestable by reading the artifact.
* **Convertibility > Completeness.** Rank asks by what each converts from narrative into result. "Do all seven steps" is not a recommendation.
* **Verified skeleton > Assumed skeleton.** Recompute what is recomputable. It decides whether the weak points are structural or computational, and those get different reports.
* **Declared incapacity > Bluffed expertise.** State what you cannot supply. A report that names its ceiling is usable; one that conceals it is a hazard. (This is **F34 Epistemic Trespass**, and it is the referee's characteristic failure.)

**The Core Discipline:**

Referee the work the way `docs/technical/build_discipline.md` audits a build. Absence presents as presence. A gate that passes on missing input has checked nothing. A protective assumption is itself a claim and inherits the burden. Find the place where the artifact could have come out wrong and did not — and if there is no such place, that is the finding.

---

### §0.1 PIPELINE POSITION — READ THIS BEFORE USING

This protocol is **outside** the UKE publication pipeline, and the distinction is the reason it exists separately:

```
own work:      Draft → UKE_G → UKE_E → UKE_D → UKE_REALITY → UKE_A → UKE_R → publish
another's work:            [source artifact] + [optional engine output] → UKE_REFEREE → letter to author
```

| | operates on | asks | terminates in |
|---|---|---|---|
| **UKE_A** (`analysis/uke_audit.md`) | *our* artifact + its metadata block | did the generator do what it claimed? | a compliance verdict + Ω routing |
| **UKE_R** (`analysis/uke_review.md`) | *our* artifacts + audit reports | promote / salvage / archive / contain? | a governance decision |
| **UKE_REFEREE** (this) | *someone else's* artifact | what is load-bearing, and what would convert it? | a report addressed to its author |

**Do not merge these.** UKE_A requires a UKE_G metadata block and audits protocol adherence; this protocol has no metadata block to check and audits an argument's structure. UKE_A's independence rule bars participation in *generation*; here the practitioner never had any. The overlap is real but it is in the **shared fracture vocabulary**, not in the object.

**Shared vocabulary, not a second copy.** Findings route to the **F01–F36 fracture codes and the Fracture ↔ Omega matrix in `agent/analysis/uke_audit.md` Appendix A**, which is canonical. This protocol mints **no new F-codes**; §11 lists only the *referee-specific* patterns that the matrix does not name, and maps each to its nearest matrix entry. A second fracture taxonomy would be Pattern 2 committed inside a review protocol.

---

### §1. INTAKE

#### §1.1 The two inputs, and their standing

You may receive **(a)** the source artifact and **(b)** instrument output about it — `enhanced_report.py` signatures, omegas, classifications, or another reader's notes.

> **(a) is evidence. (b) is a hypothesis.** Read the source. Where a firing does not survive contact with it, say so, name the section that refutes it, and rule against it.

The instrument earns its place by *directing attention*, not by supplying verdicts. Its highest-value output is often the firing that turns out to be wrong, because adjudicating it forces a reading nothing else would have prompted.

**Independence, defined as in UKE_A §0:** independence means no participation in producing the artifact — **not** information isolation. The referee should read the source, the cited literature, and the generation context if available. A referee who withholds reading in the name of independence has confused the two.

#### §1.2 The two-loci rule

When an instrument over-fires, the defect has **at least two locations**, and a source-vs-report comparison cannot separate them:

* **Instrument defect** — the signature does not discriminate, and would misfire on a faithful representation too.
* **Upstream defect** — the representation the instrument read had already dropped what the source contains, and the instrument classified it correctly.

**If the intermediate artifact exists, put it in the comparison. If it does not, declare the ambiguity rather than resolving it by assumption.**

#### §1.3 Instrument ledger

Record per finding: `upheld` / `overruled` / `not reached`, with the deciding source location and, where overruled, the locus per §1.2. At least one row must be a finding **the instrument did not surface** — the positive control that you read the artifact rather than the report. Attested reading with no per-finding disposition is **F19 Protocol Skip**.

---

### §2. CREDIT FIRST — AND IT DECIDES THE GENRE

Establish what works **before** the critique, and not for politeness. The credit determines what kind of report this is.

* **The skeleton.** Recompute what is recomputable — arithmetic, units, dates, internal cross-references, cited values. Report either way. *Sound skeleton ⇒ the weak points are structural and the report must not read as "too speculative." Broken skeleton ⇒ that is the report, and the structural analysis waits.* (Failure to run this is **F35 Faux Rigor**, detected rather than committed.)
* **The disciplines the author already keeps.** A status table separating established from conjectural, an explicit kill list, a declared scope, a question answered in advance. **An artifact that flags its own weakness has pre-empted the objection that names it**, and raising it anyway is reporting a firing rather than reading.
* **The genre.** What is the artifact *for*? A prospectus asking whether an idea is worth pursuing is not a submission claiming a result. **Analytic vocabularies carry unstated preconditions** — an extraction vocabulary presupposes a program with something to defend; a reproducibility vocabulary presupposes a claimed result. Applying one whose precondition the artifact does not meet is a defect *in the report* (nearest matrix entry: **F20 Specification Drift**, applied to the reviewer).

---

### §3. THE SPINE: ONE BREAK, LOCALIZED

**§3.1 Build the dependency chain.** Write the artifact's own chain of dependence in its order, using its section numbers: what must be established for the next thing to mean anything.

**§3.2 Find the break and state it in one sentence.** Locate the earliest link asserted rather than established. Compress it to a sentence a reader could repeat — *"the document specifies consequences before it specifies a theory"* — then show the remaining weaknesses as that break localized, section by section. **If you cannot compress it, you have a list, not a spine. Keep reading.**

**§3.3 The unestablished-object test.** Distinguish an **unknown value** — a quantity whose number we await — from an **unestablished object** — a thing whose existence, sign, or type decides whether the downstream discussion describes anything at all. An artifact treating the second as the first reads as incomplete while being structurally empty. Name every parameter whose *sign or type*, not magnitude, gates the argument.

**§3.4 Sequencing is a finding.** A list of parallel next steps usually is not parallel. Name the **gate** — the item whose absence makes the others uninstantiable.

---

### §4. THE ABSENCE BATTERY

Each is a way to pass a check never taken. Run all; report which were run.

* **Survival-condition test.** A constraint derived from the fact that nothing has gone wrong yet is **not a result about the mechanism** — it states the condition under which the model would survive, and supplies no evidence until the mechanism is computed. (`build_discipline.md` Pattern 5.)
* **Free-function test.** If every difficulty has an available answer of the same shape, something uncalculated is absorbing all of them. Name it, then ask: **can one parameter set satisfy every consumer at once?** List each consumer's demand and check the overlap. *If the windows do not overlap, breadth is a tension, not a virtue* (**F04 Cherry-Picking** at the level of the whole argument).
* **Could-it-come-out-wrong test.** Find one operation that could have failed and did not. If there is none, the finding is that the artifact is not yet the kind of thing that can be wrong — stated as a description of stage, not of quality.
* **Open-window test.** Not being excluded is not evidence. Second edge: a window is often open *because* objects there are hard to detect, which fights using them to produce visible effects. **The same property cannot be load-bearing for both invisibility and visibility.**
* **Uncalibrated-template test.** A borrowed functional form works at home because coefficients were fit to a measured corpus. Imported where neither fitting data nor a first-principles route exists, it is not an under-determined parameterization — it is a template with nothing behind it (**F25 Arbitrary Threshold**).
* **Analogy-load test.** When a neighbouring formalism is cited as evidence of tractability, check what makes *that* formalism tractable. If the enabling structure has no analogue here, the citation reads as a route to calculability and is not one.

---

### §5. PROTECTIVE POSTULATES

> **The protective mechanism must be derived from the same microphysics that generates the effect.**

When one uncalculated quantity must be *large* for the phenomenology and *small* for safety or survival, the artifact has stated one requirement twice in opposite directions about something nobody has computed.

**Method: find the comparison class.** Most fields have a prior episode where an ambitious proposal met the same demand, and the standard the community actually applied beats the referee's opinion. Report what that field did — which condition was closed by *calculation*, which empirical reassurance was later shown logically incomplete, and in what order.

---

### §6. THE FIX/BOUNDARY SPLIT AND THE ASKS

**§6.1 Route every finding** (adopted from UKE_A §5). For each, decide:

* **Error** — fixable within the artifact's own frame → `route_to_fix`.
* **Boundary** — a missing constraint or unestablished object → `elevate_to_omega`, using the matrix formulation from `uke_audit.md` Appendix A where an F-code applies, or an Ω_E/Ω_C/Ω_P typing where none does.

A referee report that routes everything to `fix` has not found the structure. One that routes everything to `omega` has not done the reading.

**§6.2 What would change my assessment.** Close with **three to five ranked single moves**, each converting one piece from narrative into result.

* Each is one move, not a program. If it reads like a research agenda, it belongs in the body.
* Each says what it converts — not "compute X" but "computing the sign of X moves it from postulate to result."
* **At least one must be cheap.** A model-independent number checkable in an afternoon beats the perfect experiment, because it will actually get done.
* Rank by conversion, not by difficulty or by your interest.

---

### §7. WHAT THE REFEREE MAY NOT CLAIM

* **State incapacity up front, concretely.** *"You asked for a proof of inconsistency. I can't give you one. What I can give you is where the proof would have to start, and why the attempt would stall in a specific place — which is itself information."* This tells the author how to weight everything after it.
* **Scope every claim to what your evidence licenses.** Where a conclusion depends on choices the artifact has not made, say so. Resist the strong form; hand the burden back as an unmet obligation rather than a defeat.
* **Correct yourself in place, marked, mid-document.** If a point turns out wrong, write the correction where the point would have gone and say why. A report showing one self-correction is more trustworthy on all its other claims.
* **Separate verified from assessed.** Claim exactly the arithmetic you checked and the structure you audited, and no domain authority beyond it.

---

### §8. CONTROL ARTIFACTS AND THE REFEREE'S OWN CALIBRATION

Adopted from UKE_A §4, because a referee protocol with no positive control is an instrument that has never been shown able to fire.

**Before a formal or contested report**, run the referee against:

* **Type 1 — deliberate flaws.** A version of the artifact (or a comparable one) with a known structural defect planted. *The report must find it.*
* **Type 2 — known clean.** A comparable artifact whose structure is sound. *The report must decline* — and this is the load-bearing half. A referee that produces a spine for everything has produced no information, since the spine is the deliverable.

```
[CALIBRATION]
detection: [found planted defect: yes/no]
decline:   [declined on clean artifact: yes/no/not run]
sensitivity: [high | medium | low]
```

**Grades, strongest first** (`build_discipline.md` → *A positive control demonstrates DISCRIMINATION, not detection*): a decline in the referee's **own history** > a **naturally-arising** clean artifact > an **authored** clean one. A calibration with no decline available is one-sided and licenses nothing.

---

### §9. SELF-APPLICATION, AND WHERE RECURSION STOPS

**Recursion termination** (as UKE_A §0): referee reports are not themselves refereed by this protocol. The chain terminates at the author's and practitioner's judgment. §9 is a **pre-flight check on the referee**, not a recursive audit:

* **Am I reporting firings?** If the spine is the instrument's output rather than the artifact's structure, restart at §1.1.
* **Is my spine a spine, or a list with a heading?** Remove the first section — do the others still read as instances of the same break?
* **Is my most confident objection the one I can least verify?** That correlation is the tell for **F34**. Re-scope (§7) or cut.
* **Have I told the author which single thing to do?** If §6.2 is unranked or longer than five, the ranking has been offloaded onto the person who needed it done.

---

### §10. OUTPUT FORMAT

```
[UKE_META]
protocol: UKE_REFEREE v0.2
artifact: [title, date, genre — prospectus / submission / framework / program]
inputs: [source read: full/partial] [instrument output: source + version, or none]
referee_position: [verified vs assessed; domain expertise claimed: none/partial/full]
spine: [the break, in one sentence]

[VERIFICATION-LIMITS]
{what could not be checked and why — sources unavailable, domain competence, access}

[REPORT BODY]
{Addressed to the author, second person:
 1. What holds, and what kind of report this therefore is (§2)
 2. The dependency chain and where it breaks (§3)
 3. The break localized, section by section
 4. Protective postulates and comparison class (§5)
 5. Constraints to confront, ordered (§3.4)
 6. Smaller points, labelled as such
 7. What would change my assessment (§6.2)}

[INSTRUMENT LEDGER]
finding → upheld | overruled | not reached — deciding location — [locus: instrument / upstream / ambiguous]
(≥1 row must be a finding the instrument did not surface)

[ROUTING]
{per finding} → route_to_fix | elevate_to_omega
Ω: [Label] — [Question] (Source: FXX from uke_audit.md Appendix A, or Ω_E/Ω_C/Ω_P)

[CALIBRATION]
{from §8, or "not run" with reason}

[QUALITY GATES]
Adjudication / Two loci / Skeleton / Credit / Genre / Spine / Objects / Battery /
One-spectrum / Protection / Sequencing / Routing / Asks / Incapacity: [Pass/Fail each]
```

---

### §11. REFEREE-SPECIFIC PATTERNS

**Only patterns the Appendix A matrix does not name.** Everything else routes to F01–F36.

**F-OBJECTION-LIST.** N true criticisms with no spine; the author cannot tell which matters. *Nearest matrix entry: none — the matrix types defects in an artifact, not in a report about one.* Fix: §3.

**F-ENGINE-DEFERENCE.** Reporting an instrument's firings as findings. Fix: §1.1. *Related: F08 Appeal to Authority, with the instrument as the authority.*

**F-LOCUS-CONFLATION.** Concluding "the instrument over-fires" from a comparison that skipped the intermediate artifact. Fix: §1.2. *Related: F05 Correlation/Causation.*

**F-PREEMPTION-BLINDNESS.** Raising an objection the artifact already states about itself. Fix: §2 — read the status table, kill list and scope declaration *first*; they retire whole classes of objection. *Related: F10 Straw Man.*

**F-UNRANKED-ASKS.** Closing with the artifact's own to-do list rather than ranked converting moves. Fix: §6.2.

**F-CONTEMPT.** Treating stage as quality — "this is not yet a research program" written as a verdict on the author rather than a description of where the work sits. Fix: the finding is structural, and belongs in the author's own vocabulary wherever they supplied one. If they already wrote the sentence, quote them and hold them to it.

---

### §12. VERSION NOTES

**v0.2 — Suite reconciliation (2026-08-12)**

Written as `uke_audit_architecture.md` and **renamed**, because `agent/analysis/uke_audit.md` v1.4 already holds the name `UKE_AUDIT` — a different protocol (protocol-adherence and grounding verification on *our own* artifacts, requiring a UKE_G metadata block). Two documents named UKE_AUDIT with different referents is the index collision this repository tracks as OQ-278, and minting it inside a review protocol would have been the joke telling itself. Renamed to UKE_REFEREE: refereeing another author's manuscript, which is what it does.

**Absorbed from `analysis/uke_audit.md` v1.4:** the F01–F36 fracture matrix as the **canonical** vocabulary (v0.1 minted a rival set — Pattern 2, now retired: §11 keeps only referee-specific patterns and maps each to its nearest matrix entry); the `route_to_fix` / `elevate_to_omega` split (§6.1), which is sharper than v0.1's flat Ω typing; **control artifacts (§8)**, the largest gap in v0.1 — a referee protocol with no positive control is an instrument never shown able to fire, and the *decline* half is load-bearing; the structured `[VERIFICATION-LIMITS]` block, replacing v0.1's prose incapacity statement; the independence definition (no participation in generation ≠ information isolation); and recursion termination, which v0.1's self-application section contradicted and §9 now reconciles.

**v0.1 — Extraction (2026-08-12)**

Formalized from a worked instance: a review of a speculative physics prospectus produced by feeding `enhanced_report.py` output plus the source to a model outside the engine's framing, whose result a domain physicist engaged with. The protocol is the *architecture* of that review, not its physics.

Provenance, recorded because it is the claim to check: the review's moves are this repository's existing disciplines pointed at a foreign domain. The survival-condition test is Pattern 5; the same-microphysics rule is *an introduced instrument is itself a claim*; the open-window test is the governing stance (*"I didn't find it" is a fact about the search*); could-it-come-out-wrong is *a check that cannot fail witnesses nothing*. **Nothing here is new discipline — it is the build discipline pointed outward at someone else's artifact.**

**Known gaps, declared:** n = 1 domain, 1 referee, 1 author's judgment of value. §1.2's two-loci rule has never been exercised with the intermediate artifact in hand. §5's comparison-class method assumes a field with documented precedent. §8's calibration has **never been run** — the protocol currently has detection evidence and no decline. §2's genre-precondition premise is asserted, not established.
