# UKE_REFEREE v0.3 [Universal Knowledge Evaluator — Structural Referee Report on Another Author's Work]

**Status:** Working draft, under active revision
**License:** CC0-1.0
**Parent Suite:** UKE Protocol Suite

---

### §0. FOUNDATION

**Purpose:** Referee a substantive artifact **that you did not write and are not publishing** — a paper, prospectus, framework, or research program — so its author learns where the load sits and what to do next. Not "is this right?" but **"what is holding this up, what happens if it isn't there, and which single move settles it fastest?"**

**Core Invariants:**

* **Localization > Enumeration.** Twenty true objections with no spine is worse than one break with nineteen instances hanging off it. Assume the author has already reached most of your objections independently — **the ranking is the deliverable, not the list.**
* **Sequencing > Comprehensiveness.** The most valuable thing a referee produces is usually *what to do first and what not to spend on yet.* A cheap question that forecloses an expensive one is worth more than a complete critique.
* **Adjudication > Deference.** Any instrument's output — engine signatures, omegas, this protocol — is a *hypothesis about the artifact*, contestable by reading the artifact.
* **Verified skeleton > Assumed skeleton.** Recompute what is recomputable. It decides whether the weak points are structural or computational, and those get different reports.
* **Declared incapacity > Bluffed expertise.** State what you cannot supply. A report naming its ceiling is usable; one concealing it is a hazard. (**F34 Epistemic Trespass** — the referee's characteristic failure.)

**The Core Discipline:**

Referee the work the way `docs/technical/build_discipline.md` audits a build. Absence presents as presence. A gate that passes on missing input has checked nothing. A protective assumption is itself a claim and inherits the burden. Find the place where the artifact could have come out wrong — and if there is none, that is the finding.

---

### §0.1 PIPELINE POSITION

This protocol is **outside** the UKE publication pipeline, which is the reason it exists separately:

```
own work:      Draft → UKE_G → UKE_E → UKE_D → UKE_REALITY → UKE_A → UKE_R → publish
another's work:          [source artifact] + [optional instrument output] → UKE_REFEREE → letter to author
```

| | operates on | asks | ends in |
|---|---|---|---|
| **UKE_A** (`analysis/uke_audit.md`) | *our* artifact + metadata block | did the generator do what it claimed? | compliance verdict + Ω routing |
| **UKE_R** (`analysis/uke_review.md`) | *our* artifacts + audit reports | promote / salvage / archive / contain? | governance decision |
| **UKE_REFEREE** | *someone else's* artifact | what is load-bearing, what converts it? | report addressed to its author |

**Shared vocabulary, not a second copy.** Findings route to the F01–F36 codes and the Fracture ↔ Omega matrix in `agent/analysis/uke_audit.md` Appendix A, which is canonical. This protocol mints no new F-codes; §11 lists only *referee-specific* patterns the matrix does not name. **If that file is unavailable, say so and route by Ω type instead** — do not invent codes.

---

### §1. INTAKE

#### §1.1 Two inputs, two standings

**(a) the source artifact** — evidence. **(b) instrument output about it** (engine signatures, omegas, another reader's notes) — a *hypothesis*. Read the source. Where a firing does not survive contact with it, say so, name the section that refutes it, and rule against it.

**Independence** means no participation in producing the artifact — **not** information isolation. Read the source, the cited literature, and the generation context if available.

#### §1.2 No-instrument mode (the common case)

When (b) is absent, **say so in one line and skip §1.3–§1.4 entirely.** Do not emit empty ledger blocks. Substitute this control, which is what §1.4 exists to provide:

> **Two-sided skeleton check.** Recompute what is recomputable (§2) and report both the values that held and the values that did not. A referee who reports only failures has not shown the check could pass; one who reports only successes has not shown it could fail.

#### §1.3 The two-loci rule *(instrument runs only)*

When an instrument over-fires the defect has at least two locations, and a source-vs-report comparison cannot separate them:

* **Instrument defect** — the signature does not discriminate, and would misfire on a faithful representation too.
* **Upstream defect** — the representation the instrument read had already dropped what the source contains.

Put the intermediate artifact in the comparison if it exists; declare the ambiguity if it does not.

#### §1.4 Instrument ledger *(instrument runs only)*

Per finding: `upheld` / `overruled` / `not reached`, with the deciding source location and, where overruled, the locus. **At least one row must be a finding the instrument did not surface** — the control that you read the artifact rather than the report.

---

### §2. CREDIT, GENRE, AND THE AUTHOR'S OWN STANDARDS

Do this before the critique. It decides what kind of report this is.

**§2.1 Verify the skeleton.** Recompute the recomputable — arithmetic, units, dates, internal cross-references, cited values. **Report three states, not two:** *sound where checkable* (say what fraction was checkable), *broken*, or *unverifiable*. Sound ⇒ the weak points are structural and the report must not read as "too speculative." Broken ⇒ that is the report, and the structural analysis waits. **Give the successful checks a home** — they are usually the most load-bearing evidence you have.

**§2.2 Check citation integrity.** References listed but never cited; citations to works that do not carry what they are invoked for; a bound attributed to a source the artifact never actually consults. For work whose warrant is "I have read the literature," this is first-order.

**§2.3 Honour pre-emptions.** An artifact that flags its own weakness has **pre-empted the objection that names it.** Read the status table, the kill list, and the scope declaration *first*. Raising a pre-empted objection is reporting a firing rather than reading.

**§2.4 Harvest the author's own standards.** Find where the artifact states how it should be judged — a stated criterion, a declared discipline, a sentence like *"this should be judged by whether one parameter set survives all five constraints."* **Grade against that first.** It is the highest-yield move in the protocol: it makes the central finding structural rather than adversarial, because the standard is theirs. If they wrote the sentence, quote it and hold them to it.

**§2.5 Identify the genre.** A prospectus asking whether an idea is worth pursuing is not a submission claiming a result. **Analytic vocabularies carry unstated preconditions** — extraction language presupposes a program with something to defend; reproducibility language presupposes a claimed result. Applying one whose precondition the artifact does not meet is a defect *in the report*.

---

### §3. THE SPINE

**§3.1 Build the dependency chain.** Write the artifact's own chain of dependence, in its order, using its section numbers: what must be established for the next thing to mean anything.

**§3.2 Find the break; state it in one sentence.** Two candidate criteria exist and they can point at different links: the **earliest** link asserted rather than established, and the **load-bearing** one — the link whose failure explains the most other symptoms. **Prefer load-bearing, and name the earliest separately if they differ.** Compress the break to a sentence a reader could repeat, then show the remaining weaknesses as that break localized. *If you cannot compress it, you have a list. Keep reading.*

**§3.3 Unknown value vs unestablished object.** Distinguish a quantity whose *number* we await from a thing whose *existence, sign, or type* decides whether the downstream discussion describes anything at all. An artifact treating the second as the first reads as incomplete while being structurally empty. Name every parameter whose sign or type — not magnitude — gates the argument.

---

### §4. SEQUENCING — WHAT TO DO FIRST, AND WHAT NOT TO SPEND ON

**This section is the protocol's highest-yield output. Treat it as the point of the report, not as a closing remark.**

A list of "next steps" is almost never parallel. The author cannot see this from inside, because each item looks independently tractable.

**Method:**

1. **Take the artifact's own program** — its future-work list, its §12, its open problems.
2. **For each item, ask what it presupposes.** An item requiring a well-defined object that another item is supposed to establish is *downstream*, not parallel.
3. **Name the gate** — the item whose absence makes the others uninstantiable. State it flatly: *"nobody can put the two-body spectrum on a lattice before you've said what theory is going on the lattice."*
4. **Say what not to spend on yet.** This is the part authors act on. An expensive computation deferred until a cheap question returns is a concrete saving, and it is frequently the whole value of the report.
5. **Check for the cheap decisive question.** Is there a question answerable in days whose answer could foreclose months? If yes, it leads §6.2 regardless of where it sits in the artifact's own ordering.

---

### §5. THE ABSENCE BATTERY

Each is a way to pass a check never taken. Run all; **report which fired and which did not** — a battery item that finds nothing is a result, not an omission.

* **Imported-phenomenology test.** When an artifact explores *the same mechanism in a new domain*, separate what is **derived** from what is **presupposed to carry over**. Shared laws do not license shared emergent behavior. List the phenomena the artifact expects by analogy — bound states, saturation density, stable composites, a spectrum, an equilibrium — and for each ask whether anything establishes that *this* domain produces them at all. **The tell is a rich phenomenology available before the theory that would generate it.** This sits upstream of §3.3: not merely "is the object established" but "is the object's *kind* imported."
* **Survival-condition test.** A constraint derived from the fact that nothing has gone wrong yet is **not a result about the mechanism** — it states the condition under which the model would survive, and supplies no evidence until the mechanism is computed.
* **Free-function test.** If every difficulty has an available answer of the same shape, something uncalculated is absorbing all of them. Name it, then ask whether **one parameter set can satisfy every consumer at once.** List each consumer's demand and check the overlap — and **distinguish two outcomes that license different reports**: *the windows do not overlap* (a finding against the artifact) versus *the windows cannot be located* (a finding about its stage).
* **Could-it-come-out-wrong test.** Look for an operation the artifact performs that **could have come out either way**. An operation that came out *against* the author and was reported anyway is the strongest form — credit it. If no such operation exists anywhere, the finding is that the artifact is not yet the kind of thing that can be wrong: a description of stage, not of quality.
* **Failure-propagation test.** Where the artifact *does* record a negative result, trace whether the sections downstream of it were revised. A concession in §2 that §4 proceeds past unchanged is a local wrong with no propagation mechanism — and it is invisible to every other item here.
* **Open-window test.** Not being excluded is not evidence. Second edge: a window is often open *because* objects there are hard to detect, which fights using them to produce visible effects. **The same property cannot be load-bearing for both invisibility and visibility.**
* **Uncalibrated-template test.** A borrowed functional form works at home because its coefficients were fitted to a measured corpus. Imported where neither fitting data nor a first-principles route exists, it is a template with nothing behind it. **This test cannot report its own failure to fire** — if you do not recognize the borrowed form, it returns silence indistinguishable from a clean result. Name the templates you checked.
* **Analogy-load test.** When a neighbouring formalism is cited as evidence of tractability, check what makes *that* formalism tractable. If the enabling structure has no analogue here, the citation reads as a route to calculability and is not one.

---

### §6. PROTECTIVE POSTULATES, ROUTING, AND THE ASKS

**§6.1 Protective postulates.** *The protective mechanism must be derived from the same microphysics that generates the effect.* When one uncalculated quantity must be *large* for the phenomenology and *small* for safety or survival, the artifact has stated one requirement twice in opposite directions about something nobody has computed.

**Find the comparison class.** Most fields have a prior episode where an ambitious proposal met the same demand, and the standard the community actually applied beats your opinion. Report which condition was closed by *calculation* and which empirical reassurance was later shown incomplete. **If you cannot name a comparison class, say which of two things is true** — *"I don't know of one"* or *"the field has no precedent."* They are different claims and only the second is a finding.

**§6.2 Route every finding.** **Error** — fixable within the artifact's frame → `route_to_fix`. **Boundary** — a missing constraint or unestablished object → `elevate_to_omega`. A report routing everything to fix has not found the structure; one routing everything to omega has not done the reading. Number your findings so routing can reference them.

**§6.3 What would change my assessment.** Three to five **ranked single moves**, each converting one piece from narrative into result.

* One move, not a program. If it reads like a research agenda it belongs in the body.
* Say what it converts — not "compute X" but "computing the sign of X moves it from postulate to result."
* **State a rough cost** for each. The author cannot sequence without it.
* **At least one must be cheap.** An afternoon-sized check beats the perfect experiment, because it will actually get done.
* Rank by conversion, not by difficulty or by your interest.

**§6.4 Answer the question the author asked.** If the artifact states what it wants decided — *inconsistent, calculable, or fertile?* — **give the verdict, or say precisely why you cannot and what would settle it.** A report that produces a spine and never answers the author's own question has done analysis instead of refereeing.

---

### §7. WHAT THE REFEREE MAY NOT CLAIM

* **State incapacity up front, concretely.** *"You asked for a proof of inconsistency. I can't give you one. What I can give you is where the proof would have to start, and why the attempt would stall in a specific place."* This tells the author how to weight everything after it.
* **Scope claims to what your evidence licenses.** Where a conclusion depends on choices the artifact has not made, say so, and hand the burden back as an unmet obligation rather than a defeat.
* **Separate verified from assessed.** Claim the arithmetic you checked and the structure you audited, and no domain authority beyond it.
* **Correct yourself in place, marked.** If a point turns out wrong, write the correction where the point would have gone. Do not manufacture one for credibility; do not silently drop one that occurred.
* **Keep the report shorter than the artifact.** A referee report that outruns what it reviews has failed its reader regardless of content.

---

### §8. BEFORE YOU SEND

**Recursion terminates here.** Referee reports are not themselves refereed. These are pre-flight checks on you, not a recursive audit.

* **Am I reporting firings?** If the spine is the instrument's output rather than the artifact's structure, restart at §1.1.
* **Is my spine a spine, or a list with a heading?** Remove the first section — do the others still read as instances of the same break?
* **Is my most confident objection the one I can least verify?** That correlation is the tell for F34. Re-scope or cut.
* **Have I said what to do first and what not to spend on?** (§4.) If not, the report is a critique, not a referee report.
* **What would have made me decline?** Write one paragraph naming the artifact that would have gotten a clean report from you. **A referee who can produce a spine for anything has produced no information** — the spine is the deliverable, so it has to be withholdable. If you cannot describe the declining case, say so; that is a fact about this report's weight.

---

### §9. OUTPUT FORMAT

Adapt freely — the blocks below are what the author needs, not a schema to satisfy. **Omit any block that would be empty and say why in one line.**

```
[UKE_META]
protocol: UKE_REFEREE v0.3
artifact: [title, date, genre]
inputs: [source: full/partial] [instrument output: none | source + version]
verified: [what you actually recomputed, and the fraction checkable]
assessed: [what you judged structurally]
expertise_claimed: [none | partial | full]
spine: [the break, in one sentence]

[WHAT HOLDS]
{skeleton verification incl. successful checks; disciplines the author keeps;
 pre-empted objections not raised; the author's own standard, quoted}

[THE BREAK, LOCALIZED]
{numbered findings F1..Fn, each an instance of the spine}

[SEQUENCING]
{the gate; what not to spend on yet; the cheap decisive question}

[BATTERY]
{per item: fired / did not fire / could not run — one line each}

[ROUTING]
F1 → route_to_fix | elevate_to_omega — Ω: [Label] — [Question]

[WHAT WOULD CHANGE MY ASSESSMENT]
{3–5 ranked moves, each with what it converts and a rough cost, ≥1 cheap}

[THE AUTHOR'S QUESTION]
{their question, answered — or why it cannot be, and what would settle it}

[LIMITS]
{what could not be checked and why; what would have made me decline}
```

---

### §10. REFEREE-SPECIFIC PATTERNS

Only patterns the Appendix A matrix does not name — it types defects in artifacts, not in reports about them.

**F-OBJECTION-LIST.** N true criticisms, no spine; the author cannot tell which matters. Fix: §3.

**F-NO-SEQUENCE.** A complete critique that never says what to do first. The most common way a technically correct report delivers nothing. Fix: §4.

**F-ENGINE-DEFERENCE.** Reporting an instrument's firings as findings. Fix: §1.1.

**F-LOCUS-CONFLATION.** "The instrument over-fires," concluded from a comparison that skipped the intermediate artifact. Fix: §1.3.

**F-PREEMPTION-BLINDNESS.** Raising an objection the artifact already states about itself. Fix: §2.3.

**F-CONTEMPT.** Treating stage as quality — "this is not yet a research program" as a verdict on the author rather than a description of where the work sits. Fix: §2.4. Grade against their standard, in their words.

---

### §11. VERSION NOTES

Kept short and forward-facing: what changed, and where the protocol is still thin. This document is expected to churn with use.

**v0.3.** Sequencing promoted from a subsection to §4, its own section with a method — it is what authors act on, and a complete critique that omits it delivers little. Battery gains **imported-phenomenology** (shared laws do not license shared emergent behavior; the tell is a rich phenomenology available before the theory that would generate it) and **failure-propagation** (a recorded negative result whose downstream sections were never revised). *Could-it-come-out-wrong* was worded backwards — it rewarded a passing check when an operation that came out **against** the author and was reported anyway is the stronger signal. §2 gains the author's-own-standards harvest (highest-yield single move), citation integrity, and a third skeleton state. §6 gains cost estimates, the comparison-class fallback, and **answer the author's question**. No-instrument mode added: §1.3–§1.4 are skipped rather than emitted empty, since a control satisfied by absence is Pattern 5. Calibration block replaced by the *what would have made me decline* paragraph — always available, and it carries the same information.

**Still thin.** §5's comparison class depends on referee knowledge the protocol cannot check for and offers only a declaration when it is missing. The uncalibrated-template test cannot report its own failure to fire. §1.3's two-loci rule has never been exercised with the intermediate artifact in hand. The genre-precondition premise in §2.5 is asserted, not established.
