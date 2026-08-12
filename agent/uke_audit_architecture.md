# UKE_AUDIT v0.1 [Universal Knowledge Evaluator — Structural Audit of a Body of Work]

---

### §0. FOUNDATION

**Purpose:** Review a substantive artifact — a paper, prospectus, framework, or program — so that its author learns where the load actually sits, in a form they can act on. Not "is this right?" but "**what is holding this up, and what happens if it isn't there?**"

**Origin:** Two review failures bracket this protocol. The first is the **objection list**: twenty true criticisms, correctly stated, that leave the author with no idea which one matters. The second is the **deference failure**: an engine, a checklist, or a framework fires, and the reviewer reports the firing instead of adjudicating it against the source. Both produce output that looks like review and transfers nothing. This protocol exists because a review's value is concentrated almost entirely in *localization* and *ranking* — finding the one break that the other twenty symptoms are, and saying which single move would convert the work.

**Core Invariants:**

* **Localization > Enumeration.** Twenty objections with no spine is a worse review than one break with nineteen instances hanging off it. Find the break.
* **Adjudication > Deference.** Any instrument's output — engine signatures, omegas, this protocol — is a *hypothesis about the artifact*, contestable by reading the artifact. A reviewer who reports firings has not reviewed.
* **Convertibility > Completeness.** Rank the asks by what each one converts from narrative into result. "Do all seven steps" is not a recommendation.
* **Verified skeleton > Assumed skeleton.** Check what you can actually check — arithmetic, citations, dates, internal consistency — because it decides whether the weak points are structural or computational, and those get different reviews.
* **Declared incapacity > Bluffed expertise.** State plainly what you cannot supply. A review that names its own ceiling is usable; one that conceals it is a hazard.

**The Core Discipline:**

Audit the work the way `docs/technical/build_discipline.md` audits a build. Absence presents as presence. A gate that passes on missing input has checked nothing. A protective assumption is itself a claim and inherits the burden. Find the place where the artifact could have come out wrong and did not — and if there is no such place, that is the finding.

**Relationship to Other Protocols:**

UKE_AUDIT is the **inbound** counterpart to UKE_THINK and UKE_OPINION, which are outbound: they produce an argument. This one receives someone else's and reports on its structure. It inherits UKE_THINK's §3 three-type diagnostic (A/B/C), its Ω typing (Ω_E / Ω_C / Ω_P), and its grounding gradient; it does not inherit the craft layer, because an audit is addressed to one author, not to readers.

It differs from all of them in one respect that governs everything below: **the reviewer is usually not the domain expert.** UKE_THINK assumes the writer knows the field. UKE_AUDIT assumes they may not, and is built so that a non-expert reviewer with a verified skeleton and a structural eye still produces something the expert wants — while never claiming what only the expert could know (§7).

---

### §1. INTAKE: WHAT YOU ARE GIVEN, AND WHAT IT IS WORTH

#### §1.1 The two inputs, and their standing

You may receive **(a)** the source artifact and **(b)** instrument output about it — `enhanced_report.py` signatures, omegas, classifications, or another reviewer's notes.

> **(a) is evidence. (b) is a hypothesis.** Read the source. Where the instrument's firing does not survive contact with it, say so, name the section that refutes it, and rule against it in the review.

The instrument earns its place by *directing attention*, not by supplying verdicts. Its highest-value output is often the firing that turns out to be wrong, because adjudicating it forces a reading of the source that nothing else would have prompted.

#### §1.2 The two-loci rule (do not skip this)

When an instrument over-fires, the defect has **at least two possible locations**, and a comparison that runs source-against-report cannot separate them:

* **Instrument defect** — the signature does not discriminate, and would misfire on a faithful representation too.
* **Upstream defect** — the representation the instrument read had already dropped what the source contains, and the instrument classified it correctly.

Attributing an over-firing to the instrument without inspecting the intermediate artifact is a stage-skipping error. **If the intermediate exists, put it in the comparison. If it does not, declare the ambiguity rather than resolving it by assumption.**

#### §1.3 Consumption is checkable

Record, per instrument finding: `upheld` / `overruled` / `not reached`, with the source location that decided it. At least one row must be a finding you reached **that the instrument did not surface** — the positive control that you read the artifact rather than the report. Attested reading with no per-finding disposition is **F-ATTESTED-CONSUMPTION**.

---

### §2. CREDIT FIRST — AND IT DECIDES THE GENRE

Establish what is genuinely working **before** the critique, and not for politeness. The credit determines what kind of review this is, and getting it wrong wastes the whole document.

Verify and state:

* **The skeleton.** Recompute what is recomputable — arithmetic, unit consistency, dates, internal cross-references, cited values. Report the result either way. *If the skeleton is sound, the weak points are structural, and the review must not read as "too speculative." If the skeleton is broken, that is the review and the structural analysis waits.*
* **The disciplines the author already keeps.** A status table separating established from conjectural, an explicit kill list, a declared scope, an omega answered in advance — these change what may be said later. **An artifact that flags its own weakness has pre-empted the objection that names it**, and a reviewer who raises it anyway is reporting a firing rather than reading.
* **The genre.** What is this artifact *for*? A prospectus asking whether an idea is worth pursuing is not a submission claiming a result. Vocabularies carry preconditions — an extraction vocabulary presupposes a program with something to defend; a reproducibility vocabulary presupposes a claimed result. **Applying a vocabulary whose precondition the artifact does not meet is a framing error in the review, not a finding about the work** (F-VOCABULARY-MISINDEX).

---

### §3. THE SPINE: ONE BREAK, LOCALIZED

#### §3.1 Build the dependency chain

Write the artifact's own chain of dependence, in its order: what must be established for the next thing to mean anything. Use the artifact's section numbers.

#### §3.2 Find where it breaks, and say it in one sentence

Locate the earliest link that is asserted rather than established. State the break as a single sentence a reader could repeat — *"the document specifies consequences before it specifies a theory"* — and then **show that the remaining weaknesses are that break localized**, section by section.

If you cannot compress the break to one sentence, you have not found it yet; you have a list. Keep reading.

#### §3.3 The unestablished-object test

The sharpest form the break usually takes:

> Distinguish an **unknown value** — a quantity whose number we await — from an **unestablished object** — a thing whose existence, sign, or type determines whether the downstream discussion describes anything at all.

An artifact that treats the second as the first will read as merely incomplete while being structurally empty. Name every parameter whose *sign or type*, not magnitude, gates the argument.

#### §3.4 Sequencing is a finding

A list of parallel next steps usually is not parallel. Identify which item is the **gate** — the one whose absence makes the others uninstantiable — and say so. *"Nobody can put the two-body spectrum on a lattice before you've said what theory is going on the lattice."*

---

### §4. THE ABSENCE BATTERY

Run all of these. Each is a way for a work to pass a check it never took.

* **Survival-condition test.** A constraint derived from the fact that nothing has gone wrong yet — the world still exists, the system has not crashed, no one has complained — is **not a result about the mechanism**. It is a statement of the condition under which the model would survive. It supplies no evidence for the model until the mechanism is computed. *Verdict: name it as a condition, not a finding.*
* **Free-function test.** If every difficulty has an available answer of the same shape (*"perhaps most of the mass is in large objects"*, *"perhaps the effect is small in that regime"*), then something uncalculated is absorbing all of them. Identify it. Then ask the killing question: **can one parameter set satisfy every consumer at once?** List what each consumer demands and check whether the windows overlap. *If they do not, breadth is a tension, not a virtue.*
* **Could-it-come-out-wrong test.** Find one place where the work performs an operation that could have failed and did not. If there is none, the finding is that the artifact is not yet the kind of thing that can be wrong — state it in those terms, without contempt, as a description of stage rather than quality.
* **Open-window test.** Not being excluded is not evidence. And check the second edge: a window is often open *because* objects there are hard to detect, which is in direct tension with using them to produce visible effects. **The same property cannot be load-bearing for both invisibility and visibility.**
* **Uncalibrated-template test.** A borrowed functional form (a liquid-drop expansion, a scaling law, a maturity model) works in its home domain because coefficients were fit to a large measured corpus. Imported where no fitting data and no first-principles route exist, it is not an under-determined parameterization — **it is a template with nothing behind it**. Say which.
* **Analogy-load test.** When the artifact cites a neighbouring formalism as evidence of tractability, check what makes that formalism tractable. If the enabling structure (a symmetry relating the new case to a measured one, a conserved quantity, an existing effective theory) has no analogue here, the citation reads as a route to calculability and is not one.

---

### §5. PROTECTIVE POSTULATES

The most common structural defect in ambitious work, and it has a settled standard.

> **The protective mechanism must be derived from the same microphysics that generates the effect.**

When one uncalculated quantity is required to be *large* for the phenomenology and *small* for safety, consistency, or survival, the artifact has not stated two facts. It has stated one requirement twice, in opposite directions, about something nobody has computed.

**Method:** find the comparison class. Nearly every field has a prior episode where an ambitious proposal met the same demand, and the standard the community actually applied is a stronger argument than the reviewer's own opinion. Report what that field did — which condition was closed by calculation, which empirical reassurance was later shown to be logically incomplete, and in what order. **A precedent showing the standard was met once is worth more than an objection asserting it should be.**

---

### §6. WHAT WOULD CHANGE MY ASSESSMENT

Close with a short ranked list — three to five — of single moves, each of which would convert one piece of the work from narrative into result. Constraints:

* **Each item is one move, not a program.** If it reads like a research agenda, it belongs in the body.
* **Each says what it converts.** Not "compute X" but "computing the sign of X would move it from postulate to result."
* **At least one must be cheap.** A model-independent number the author can check quickly is worth more than the perfect experiment, because it will actually get done.
* **Rank by conversion, not by difficulty or by your interest.**

---

### §7. WHAT THE REVIEWER MAY NOT CLAIM

* **State your incapacity up front, concretely.** *"You asked for a proof of inconsistency. I can't give you one. What I can give you is where the proof would have to start, and why the attempt would stall in a specific place — which is itself information."* This is not modesty; it tells the author how to weight everything that follows.
* **Scope every claim to what your evidence licenses.** Where a conclusion depends on choices the artifact has not made, say that it depends on them. Resist the strong form — *"I'd resist stating this as 'X is excluded'; whether it is depends on exactly those unstated choices"* — and hand the burden back as an unmet obligation rather than a defeat.
* **Correct yourself in place, marked, mid-document.** If a point you were going to make turns out to be wrong, write the correction where the point would have gone and say why it was wrong. Do not silently drop it. A review that shows one self-correction is more trustworthy on all its other claims.
* **Separate verified from structural.** Say which parts you checked and which you assessed. A reviewer who verified the arithmetic and audited the structure should claim exactly that, and no domain authority beyond it.

---

### §8. SELF-APPLICATION

The protocol applies to itself, and its own failure modes are the ones it names:

* **Am I reporting firings?** If the review's spine is the instrument's output rather than the artifact's structure, restart from §1.1.
* **Is my spine a spine, or a list with a heading?** Test: remove the first section. Do the others still make sense as instances of the same break? If yes, it is a spine. If they read as independent, it is a list.
* **Is my most confident objection the one I can least verify?** The correlation is common and it is the tell for bluffed expertise. Re-scope it (§7) or cut it.
* **Have I told the author which single thing to do?** If §6 is longer than five items or unranked, the review has offloaded the ranking onto the person who needed it done.

---

### §9. QUALITY GATES

- [ ] **Adjudication:** Every instrument finding carries `upheld` / `overruled` / `not reached` with the deciding source location, and ≥1 beyond-the-instrument row.
- [ ] **Two loci:** Where a finding was overruled, is the defect's location identified — or the ambiguity declared rather than assumed away?
- [ ] **Skeleton:** Recomputable content actually recomputed, result stated either way.
- [ ] **Credit:** Working disciplines named before the critique, and the artifact's own pre-emptions honoured rather than re-raised.
- [ ] **Genre:** Artifact type identified, vocabulary preconditions checked.
- [ ] **Spine:** The break stated in one repeatable sentence, with the remaining weaknesses shown as its localizations.
- [ ] **Objects:** Every sign/type-gating parameter named as an unestablished object, not an unknown value.
- [ ] **Battery:** Survival-condition, free-function, could-it-come-out-wrong, open-window, uncalibrated-template, analogy-load — all run.
- [ ] **One-spectrum:** If the work serves multiple consumers from one free function, overlap checked.
- [ ] **Protection:** Any protective postulate tested against the same-microphysics rule, with a comparison class.
- [ ] **Sequencing:** The gate item named, parallel lists de-parallelized.
- [ ] **Asks:** §6 is ≤5 ranked single moves, each stating what it converts, ≥1 cheap.
- [ ] **Incapacity:** Stated up front; verified and structural claims separated; ≥0 domain claims beyond what was checked.
- [ ] **Ω:** Every unresolved question typed Ω_E / Ω_C / Ω_P.

---

### §10. OUTPUT FORMAT

```
[UKE_META]
protocol: UKE_AUDIT v0.1
artifact: [title, date, genre — prospectus / submission / framework / program]
inputs: [source read: yes/partial] [instrument output: source + version, or none]
reviewer_position: [what was verified vs assessed; domain expertise claimed: none/partial/full]
spine: [the break, in one sentence]

[REVIEW BODY]
{Addressed to the author, second person. Order:
 1. What holds, and what kind of review this therefore is (§2)
 2. The dependency chain and where it breaks (§3)
 3. The break localized, section by section — one heading per instance
 4. Protective postulates and comparison class (§5)
 5. Constraints to confront, ordered (§3.4)
 6. Smaller points, explicitly labelled as such
 7. What would change my assessment (§6)}

[INSTRUMENT LEDGER]
finding → upheld | overruled | not reached — deciding location — [locus if overruled: instrument / upstream / ambiguous]
(≥1 row must be a finding the instrument did not surface)

### Open Questions (Ω)
Ω_E: [Label] — resolvable by measurement/calculation — [what would resolve it]
Ω_C: [Label] — underspecified — [what index specification would dissolve it]
Ω_P: [Label] — preference- or authority-dependent — [who holds the decision]

[QUALITY GATES]
Adjudication / Two loci / Skeleton / Credit / Genre / Spine / Objects / Battery /
One-spectrum / Protection / Sequencing / Asks / Incapacity / Ω: [Pass/Fail each]
```

---

### §11. ANTI-PATTERNS

**F-OBJECTION-LIST.** N true criticisms with no spine; the author cannot tell which matters. Fix: §3. If you cannot compress the break to a sentence, keep reading — do not ship the list.

**F-ENGINE-DEFERENCE.** Reporting an instrument's firings as findings. Fix: §1.1. The firing is a hypothesis; the source decides.

**F-LOCUS-CONFLATION.** Concluding "the instrument over-fires" from a source-vs-report comparison that skipped the intermediate artifact. Fix: §1.2 — inspect the intermediate or declare the ambiguity.

**F-PREEMPTION-BLINDNESS.** Raising an objection the artifact already states about itself. Fix: §2 — read the status table, the kill list, and the scope declaration *first*; they retire whole classes of objection.

**F-VOCABULARY-MISINDEX.** Applying an analytic vocabulary whose precondition the artifact does not meet (extraction language to an unfunded prospectus; reproducibility language to a proposal). Fix: §2 genre check. This is a defect in the review.

**F-SURVIVAL-AS-RESULT.** Accepting "nothing has gone wrong yet" as evidence about a mechanism. Fix: §4 — it names a condition, not a finding.

**F-BREADTH-AS-SUPPORT.** Counting domains covered as confirmation, when one uncalculated function is serving all of them. Fix: §4 one-spectrum test — check whether the windows overlap.

**F-EXPERTISE-BLUFF.** Asserting a domain claim the reviewer cannot verify, usually in the most confident sentence in the review. Fix: §7 — re-scope or cut, and state the incapacity up front.

**F-UNRANKED-ASKS.** Closing with the artifact's own to-do list rather than a ranked set of converting moves. Fix: §6 — ≤5, ranked by conversion, ≥1 cheap.

**F-CONTEMPT.** Treating stage as quality — writing "this is not yet a research program" as a verdict on the author rather than a description of where the work sits. Fix: the finding is structural and belongs in the author's own vocabulary wherever they have supplied one. If they already wrote the sentence, quote them and hold them to it.

---

### §12. VERSION NOTES

**v0.1 — Extraction (2026-08-12)**

Formalized from a worked instance: a review of a speculative physics prospectus produced by
feeding `enhanced_report.py` output plus the source to a model outside the engine's framing, whose
result a domain physicist engaged with. The protocol is the *architecture* of that review, not its
physics.

The extraction is honest about provenance in one respect worth recording. The review's moves are
this repository's existing disciplines in a foreign domain — the survival-condition test is
`build_discipline.md` Pattern 5 (absence satisfies the gate); the same-microphysics rule is *an
introduced instrument is itself a claim*; the open-window test is the governing stance (*"I didn't
find it" is a fact about the search*); the could-it-come-out-wrong test is *a check that cannot
fail witnesses nothing*. **Nothing here is new discipline; it is the build discipline pointed
outward at someone else's artifact.** That is the claim to check if this protocol is ever
evaluated: whether the transfer holds in domains beyond the two it has been run in.

**Known gaps, declared:** n = 1 domain, 1 reviewer, 1 author's judgment of value. §1.2's two-loci
rule has never been exercised with the intermediate artifact actually in hand. §5's comparison-class
method assumes a field with a documented precedent and will not fit young or fragmented domains.
The genre check (§2) rests on a precondition — that analytic vocabularies carry unstated genre
requirements — that is asserted here and not established.
