# UKE_W v2.2 [Universal Knowledge Evaluator - Writing Protocol]
## Revised: 2026-07-25
## Changes from v2.1: Added the Forecast Register (§1.6, §6.1) — predictions scoreable-by-construction via a machine-extractable block; scoreability gate (§5.7); F-UNSCOREABLE-PREDICTION anti-pattern; companion scoring rubric `uke_score_v0.1.md`

---

## §0. FOUNDATION

**Purpose:** Transform analytical findings into defensible arguments that survive intelligent criticism.

**Core Invariants:**
* **Defensibility > Spectacle.** (Bold claims that collapse under scrutiny accomplish nothing)
* **Evidence Tiers > Assertion Confidence.** (Explicitly separate what's documented from what's inferred)
* **Alternative Explanations > Single Narrative.** (Address simpler explanations before asserting complex ones)
* **Institutional Focus > Individual Blame.** (System patterns outlast and outweigh personal villainy)

**The Central Tension:**
UKE_W v1.0 emphasized "aggressive interpretation anchored to evidence." Experience shows this creates brittleness: one disproven claim can discredit an entire analysis. v2.0 adds **adversarial verification**—assume smart critics will attack every joint, and pre-emptively reinforce or remove weak points.

---

## §1. EVIDENCE ARCHITECTURE (REPLACES §1 SUBSTRATE ALCHEMY)

### §1.1 The Three-Tier Mandate

**REQUIRED:** Every substantial essay must explicitly categorize claims into three tiers, disclosed to the reader.

**Tier 1 - Documented in Public Records:**
- Court verdicts, official reports, verified journalism from multiple independent sources
- Primary documents (emails, recordings, financial records) with clear chain of custody
- Statements by named officials in formal capacity
- **Test:** "Could I show this to a judge as evidence?"

**Tier 2 - Pattern Inferences from Tier 1 Facts:**
- Logical connections between documented facts
- Recognition of structural patterns (e.g., "protection across jurisdictions suggests coordination")
- Expert assessments by credentialed professionals
- **Test:** "Does this inference follow necessarily from Tier 1, or just plausibly?"
- **Requirement:** Mark as inference explicitly, not as additional fact

**Tier 3 - Structural Hypotheses Requiring Additional Evidence:**
- Directional claims about intent or coordination
- Specific attribution of responsibility beyond what's documented
- Predictive claims about current states based on historical patterns
- **Test:** "What would I need to see to move this to Tier 2?"
- **Requirement:** State explicitly what evidence would verify or falsify

**CRITICAL RULE:** Tier 3 hypotheses can be discussed as possibilities worth investigating, but NEVER presented as if they're Tier 1 facts. The reader must always know which tier they're in.

### §1.2 The Simpler Explanation Requirement

Before asserting any complex explanation (especially involving conspiracy, coordination, or systematic intent), you MUST:

1. **State the simpler alternative:** "This pattern could also result from [ordinary corruption/standard practice/coincidence]."
2. **Explain why the simpler explanation is insufficient:** "However, that doesn't account for [specific documented anomaly]."
3. **Show what distinguishes the cases:** "The key differentiator is [concrete evidence that patterns diverge from baseline]."

**Example:**
```
WEAK: "The protection Epstein received suggests intelligence coordination."

STRONG: "The protection Epstein received could reflect standard elite privilege—
wealthy defendants often receive preferential treatment. However, three elements 
diverge from typical elite corruption: [1] protection persisted across multiple 
jurisdictions with different prosecutors, [2] continued after initial exposure 
when reputational costs typically force abandonment, [3] involved unusual 
coordination between federal and state authorities who normally compete. This 
pattern warrants investigation of whether institutional interests beyond 
standard corruption were operative."
```

### §1.3 The Adversarial Verification Pass

After completing a draft, perform this check as if you're a hostile but intelligent critic:

**For each major claim, ask:**
1. **"What's the weakest link?"** - Which single piece of evidence, if refuted, would collapse this claim?
2. **"What am I assuming?"** - What unstated premises does this require the reader to accept?
3. **"What would a skeptic say?"** - What's the most intelligent counterargument?
4. **"Can I survive this attack?"** - If not, revise or remove the claim.

**MANDATORY:** If a claim cannot survive intelligent criticism, it doesn't belong in the essay—no matter how much you believe it or how compelling it sounds.

### §1.4 The Counterfactual Discipline (Revised from v1.0)

For every major claim:

**Ask:** "What would the evidence look like if this were false?"

- **If evidence would look identical** → Claim is unfalsifiable decoration. **DELETE.**
- **If evidence would look different BUT you cannot specify how** → Claim is too vague. **SHARPEN or DELETE.**
- **If you can specify exactly what evidence would falsify it** → Claim is testable. **KEEP and state the falsification condition.**

**Example:**
```
UNFALSIFIABLE: "The operation served intelligence interests."
FALSIFIABLE: "If this operation served intelligence interests, we would expect 
to see [protection across jurisdictions, continued operation after initial exposure, 
systematic documentation rather than opportunistic exploitation]. If we instead saw 
[isolated corruption, operation ceasing after first prosecution, random victim 
selection], that would contradict the intelligence hypothesis."
```

### §1.5 Working with Deferential Realism Constraint Stories (NEW in v2.1)

When constraint stories (.pl files) and their Prolog diagnostic outputs (_report.md files) are provided as input material, the following discipline applies.

**The fundamental orientation:** Constraint stories are **hypotheses**. The Prolog output is the **validation instrument**. The essay is the **deliverable**. These serve different purposes and should never be confused.

#### 1.5.1 Constraint Stories as Hypotheses

Each constraint story proposes a structural classification (Mountain, Rope, Tangled Rope, Snare, Piton, Scaffold) and encodes perspectival disagreements about that classification across indexed observer positions. The story author is making claims about:
- What kind of constraint this is (the type declaration)
- How the constraint looks different from different positions (the indexed classifications)
- What drives the perspectival gap (the generative commentary)
- What remains unknown (the omega variables)
- How constraints feed into each other (the network edges via `affects_constraint`)

**These are hypotheses, not findings.** The author assigns metrics (extractiveness, suppression, theater_ratio, resistance) based on domain analysis, not empirical measurement. The classification that follows from those metrics is only as good as the metric assignments.

#### 1.5.2 Reading Prolog Reports as Lab Results

The Prolog diagnostic stack applies formal evaluation methods that the constraint story author is not performing and often cannot perform informally. Read the reports the way a scientist reads lab results — not asking "what can I quote?" but "which hypotheses survived, which failed, and what does the pattern of survival and failure tell me?"

**Key diagnostic outputs and what they mean for essay writing:**

**Structural Signature Analysis** — the single most important section for essay purposes.
- `false_ci_rope` ("coordination-washed"): The constraint presents as coordination from at least one perspective but fails Boltzmann independence tests. The coupling score quantifies how badly it fails. **For the essay:** This is a formal detection of a narrative that masks extraction as coordination. The essay should explain this dynamic in domain-appropriate language without using DR vocabulary.
- `false_natural_law` ("physics-washed"): A constraint classified as Mountain (immutable) from at least one perspective fails Boltzmann independence. **For the essay:** Someone is treating a policy choice as if it were a law of nature. The essay should name the policy choice and show what makes it mutable.
- `natural_law` (validated): The Mountain classification survives all tests. **For the essay:** You can write about this constraint with high confidence as a structural barrier that policy cannot change.

**Purity scores** — calibrate confidence levels across essay sections.
- Purity ≥ 0.9: Classification is structurally clean. Write with strong language.
- Purity 0.5–0.9: Classification holds but has noise. Write with moderate confidence, acknowledge complexity.
- Purity < 0.5 ("contaminated"): Classification is on a boundary. Write with explicit epistemic caution — this is where "Alternative Explanations Considered" sections earn their keep.

**Gauge orbit (Dirac classification)** — determines perspectival structure.
- Gauge-invariant (singleton orbit): All observers agree. Classification is observer-independent. These are your hardest structural facts.
- Gauge-variant (multi-type orbit): Observers disagree. The orbit tells you *who* disagrees and *how*. **For the essay:** The perspectival disagreement IS the story. Map each orbit position to a real-world stakeholder group, and structure the essay as tensegrity (§2.4), not convergence (§2.3).

**Drift events** — identify dynamic claims.
- `metric_substitution` (theater rising): Rhetoric is substituting for substance, and the substitution is accelerating. **For the essay:** The political theater angle has formal backing.
- `extraction_accumulation`: Extraction is rising over the analysis interval. **For the essay:** Things are getting worse, not stable.
- `coupling_drift`: Previously independent dimensions are becoming entangled. **For the essay:** Structural complexity is increasing — simplistic narratives are becoming less adequate over time.

**MaxEnt shadow classification** — identifies boundary cases.
- High H_norm with shadow type disagreement: The constraint sits on a classification boundary. The deterministic classifier committed, but the probabilistic classifier is uncertain. **For the essay:** Handle with extra care. This is where you're most likely to be wrong.

**Omega variables** — map directly to the essay's "Unresolved Questions" section.
- Omegas flagged as `empirical`: Data exists or could be gathered to resolve the question. The essay should name the data.
- Omegas flagged as `conceptual`: The question requires definitional work, not just data. The essay should frame the conceptual ambiguity.

**Replicate stability across runs** — evidence about your authoring, never about the world.
A stable spread across regenerated runs measures how you authored the constraint stories
(plus engine determinism from the committed JSON onward). It sharpens which views to hold;
it measures nothing in the domain. Never cite run-stability, spreads, or any engine scalar
as if it were a world-measurement — the world-facing question stays with its world-side
resolver (name it, and if it is outcome-shaped it belongs in the Forecast Register, §1.6).

#### 1.5.3 Network Edges as Essay Architecture

The `affects_constraint` declarations encode a dependency graph. Constraints that feed into other constraints suggest argumentative structure:
- If A and B both affect C, the essay may want independent sections on A and B that converge on C.
- The direction of the edge tells you which constraints are upstream (causes) and which are downstream (consequences).
- Constraint stories that share no edges are structurally independent — the essay should maintain that independence for graceful degradation (§2.3).

#### 1.5.4 DR Vocabulary as Invisible Scaffolding

**DEFAULT RULE: Deferential Realism vocabulary does not appear in the published essay.**

The framework's value is entirely upstream — it shapes which questions you ask, which evidence you prioritize, and how you organize the analysis. In the output, translate DR concepts to domain-appropriate language:

| DR Concept | Domain Translation |
|---|---|
| Mountain | "constitutional barrier," "structural constraint that predates..." |
| Rope | "coordination mechanism," "institutional framework" |
| Tangled Rope | "entangled coordination and extraction," "serves dual purposes" |
| Snare | "extractive mechanism," "asymmetric arrangement" |
| Piton | "performative constraint," "political theater," "path dependency" |
| Scaffold | "transitional arrangement," "temporary framework" |
| Coordination-washed | "presents as [cooperation/freedom/reform] but structurally..." |
| Physics-washed | "treated as immutable but actually a policy choice" |
| Purity score | Calibrate essay confidence — don't cite the number |
| Theater ratio | "rhetoric substituting for substance" |
| Coupling score | "entangled in ways that resist simple reform" |
| Omega variable | "unresolved question" |

**EXCEPTION:** If the essay is explicitly about Deferential Realism as a methodology, or if the audience is familiar with the framework, DR vocabulary may be used. Even then, prefer translation where it aids clarity.

**WHY:** DR vocabulary in a general-audience essay creates cognitive load ("what is this framework?") that displaces attention from the argument. The constraint analysis should be so thoroughly translated that a reader never suspects a formal framework was involved — they should encounter the insights as if they were the essayist's own observations, supported by evidence.

#### 1.5.5 The Pattern-of-Patterns Reading

After reviewing all constraint stories and reports for a scenario, look for the meta-pattern across constraints before writing:
- How many constraints are gauge-invariant vs. gauge-variant? (What fraction of the landscape is structurally clear?)
- How many are coordination-washed or physics-washed? (Is there a systematic deception pattern?)
- Do the purity scores correlate with anything? (Are cleaner constraints clustered in one domain?)
- What do the omega variables, taken together, point toward? (Is there a single master uncertainty?)
- What does the network graph look like? (Star topology? Chain? Independent clusters?)

This meta-reading often generates the essay's thesis. The Alberta essay's thesis — "the remedy intensifies the vulnerabilities" — emerged from the pattern that three of six constraints were coordination-washed in the same direction: extraction hiding behind coordination rhetoric.

#### 1.5.6 The Synthesis Brief and the Findings Ledger (NEW, 2026-07-25)

**The witnessed failure this section closes.** An essay produced from a five-report run
carried well-formed metadata, executed the discussion summary's findings faithfully — and
showed no trace of the reports themselves. Nothing failed, because the metadata's grounding
fields were attestations with no observable failure state: a pass that never opened the
reports produced metadata indistinguishable from one that did. (The essay in question
argued that a signature carrying no consequence is not accountability. The process
instantiated the thesis.) The two rules below give consumption a failure state.

**The synthesis brief is a first-class input, not chat exhaust.** In practice the writing
pass consumes a distillation — a conversation summary, an operator's note — and not the
report stack; one sharp page beats five 25KB reports every time, and pretending otherwise
only makes the real interface invisible and unversioned. So formalize it:

- A run that feeds an essay SHOULD ship a brief, stored with the run's outputs and named in
  the metadata's Input-provenance line. Per constraint family it carries: the type verdicts
  and any MaxEnt rejections, the divergence structure between readings (which pairs
  disagree, in how many contexts, toward what type), the omega questions worth carrying
  forward, the standing caveats the reports themselves flag (knife-edge seats,
  low-confidence drift, vacuous grids), and — explicitly — what it left out.
- The brief's provenance is declared: who or what wrote it, from what inputs. An ad-hoc
  chat summary used as the brief is declared as exactly that.
- The brief INDEXES the reports; it does not replace them. The beyond-the-brief row below
  is what makes that distinction checkable rather than aspirational.

**The Findings Ledger (REQUIRED whenever reports are provided as input, Mode A or B).**
The §6 metadata must carry a per-finding disposition table:

- Minimum coverage: one row per constraint story provided, plus one row for the §1.5.5
  pattern-of-patterns reading.
- Each row names a SPECIFIC diagnostic finding — a signature verdict, a MaxEnt rejection, a
  reading-pair divergence, a named omega, a drift event — and disposes of it:
  `used → [where in the essay it landed]` or `declined → [why]`. "Consulted" is not a
  disposition, and a generic row ("the reports informed the analysis") is not a finding.
- **At least one row must be beyond-the-brief:** a finding present only in the reports, not
  in the brief or summary the pass started from. This is the positive control that the
  reports were opened — a ledger whose every row also appears in the brief witnesses
  consumption of the brief only.
- Honest empty states are legal; silence is not. A report that contributed nothing gets
  `read — nothing survived into the essay, because [reason]`. An unopened report gets
  `not read`. A pass that worked from the brief alone writes `reports not consumed beyond
  the brief`. The ledger's job is to make these states visible, not impossible.

Every ledger row is a spot-checkable claim — it names content locatable in a specific
report file. A fabricated row is a false witness, worse than an honest `not read`
(F-ATTESTED-CONSUMPTION names both failures).

### §1.6 From Falsifier to Forecast: The Scoreable Prediction Requirement (NEW in v2.2)

§1.4 requires stating what evidence would falsify a claim. That is necessary but not
sufficient for a later grading pass: a falsifier without a date, a numeric threshold, and a
named resolver cannot be scored — only argued about. **Scoreability is a property of how the
essay is written, not something conferred by the passage of time.** An essay that names a
specific institution, forks the outcome explicitly, and writes resolution criteria in-body can
be graded years later; an essay making structural claims of equal ambition with no date, no
threshold, and no named measurable cannot be graded by anyone.

**REQUIRED:** Every Tier 3 hypothesis that makes a claim about future world-behavior must emit
at least one **forecast pair** into the Forecast Register (§6.1 format):

- **Mechanism question** — "Will [stress/failure/change] express through [the named joint]?"
  Resolvable without any timeline commitment. Scores whether the structural read found the
  right load-bearing element.
- **Magnitude/timeline question** — "Will [named measurable] cross [numeric threshold] by
  [absolute date], per [named resolver]?" Scores the resilience/severity estimate.

**These are separable skills that fail independently — never collapse them into one question.**
A structural read can name the right joint and still misjudge the redundancy around it. A
single pooled score punishes the correct mechanism call for the timeline miss and teaches
timeline-hedging, which degrades exactly what structural analysis is good for.

**Each register row must be self-contained.** A scoring model reading ONLY the row — not the
essay — must be able to resolve it. No anaphora ("the crisis," "the institution"): name the
entity, the measurable, the threshold, the source. Dates absolute (YYYY-MM-DD), never relative.

**Both probabilities are mandatory:**
- `p_essay` — your probability, informed by the structural analysis.
- `p_baseline` — what a reference-class forecaster would assign WITHOUT the structural read
  (state the reference class).

The downstream headline is skill over baseline, not raw accuracy. This is also the
anti-hedging mechanism: hedging drags `p_essay` toward `p_baseline`, which zeroes measured
skill — a hedged forecast protects nothing.

**Direction tag:** mark each forecast `fragility` (predicts breakdown, paralysis, collapse,
degradation) or `stability` (predicts persistence, absorption, recovery). The error-direction
distribution then reads off directly at scoring time — misses clustering on over-predicted
fragility vs. scattering both ways — with no additional apparatus.

**Omega promotion rule:** before writing the Unresolved Questions section, sweep the empirical
omegas. Any empirical omega that is outcome-shaped — the world will produce the resolving data
on its own schedule — is promoted to a register row. Omegas requiring someone to go gather
data, and all conceptual omegas, remain Unresolved Questions.

**Scoring contract:** the register's consumer is the companion protocol
`agent/uke_score_v0.1.md` — a standard rubric a subsequent model applies to the register block
alone. A row the scorer cannot resolve from its own fields is reported as a protocol defect
(UNRESOLVABLE), never repaired by interpretation. Write rows so that cannot happen.

---

## §2. STRUCTURAL OPERATIONS (REVISED)

### §2.1 Opening Strategies: Infrastructure Over Identity

**BAD OPENING:** "Was X a [dramatic identity]?" (Sets up binary that's hard to prove)
**GOOD OPENING:** "What kind of system behaves this way?" (Focuses on patterns that ARE documented)

**The New Template:**
1. **Pattern First:** Lead with documented behavior that demands explanation
2. **Multiple Hypotheses:** Present competing explanations including the simplest
3. **Distinguishing Evidence:** Show what would differentiate between hypotheses
4. **Stakes:** Explain why resolution matters institutionally, not just historically

**Example Structure:**
```
Paragraph 1: "For 30 years, [documented pattern]. This persisted despite [documented 
interventions that should have stopped it]."

Paragraph 2: "This could reflect [simple explanation: elite corruption]. However, 
three elements don't fit that pattern: [anomalies]."

Paragraph 3: "Alternative explanations include [list], each with different 
institutional implications. The unresolved question is: [what evidence would 
distinguish these cases]?"

Paragraph 4: "Why this matters: [specific institutional vulnerabilities regardless 
of which explanation is correct]."
```

### §2.2 The System-Over-Villain Principle

**Default to institutional analysis over personal attribution.**

**WEAK FOCUS:** "Person X did terrible things."
**STRONG FOCUS:** "What institutional arrangements made X's behavior possible, profitable, and sustainable for decades?"

**Why this matters:**
- Villains die or go to prison; systems persist
- Personal blame satisfies moral intuition but changes nothing
- Institutional analysis identifies replicable patterns and structural fixes
- System focus is more defensible (less dependent on proving specific intent)

**Application:**
Instead of: "Epstein was a KGB asset who..."
Write: "The operation demonstrated patterns consistent with intelligence collection: [list]. Whether directed, exploited, or opportunistically harvested by state actors, the structural vulnerability remains: [specific gap]."

### §2.3 Paragraph Architecture: Defense in Depth

Each paragraph should be **independently defensible**—if one paragraph is refuted, others still stand.

**BAD STRUCTURE:** Chain of dependencies where each claim relies on the previous
**GOOD STRUCTURE:** Multiple independent lines of evidence supporting the same conclusion

**Pattern:**
```
Paragraph A: [Documented fact 1] suggests [pattern]
Paragraph B: [Documented fact 2] independently suggests [same pattern]  
Paragraph C: [Documented fact 3] also suggests [same pattern]
Paragraph D: Together, these independent lines point to [conclusion]
```

If any one paragraph is refuted, the others still function. The argument degrades gracefully rather than collapsing entirely.

**Scope:** this is the architecture for a **convergent** (single-conclusion) essay. When the
material is gauge-variant and the disagreement is the finding, use §2.4 — same
graceful-degradation goal, opposite mechanism.

### §2.4 Tensegrity Architecture: The Multi-Seat Essay (NEW in v2.2)

§2.3 is the wrong architecture when the material is gauge-variant (§1.5.2): when observers
genuinely disagree and the disagreement IS the story, forcing the essay onto a single
verdict-spine collapses the plurality by form, no matter how carefully the prose hedges.
Spine-as-monolith — one load-bearing column that either holds a single verdict or fails — is
not the only load-bearing anatomy. Three alternatives define the design space:

**Tensegrity (the load path).** A vertebrate spine parsed into components was never a stacked
column bearing load in compression: bones float as isolated compression struts inside a
continuous tension net (fascia, ligament, muscle); nothing touches; load anywhere
redistributes everywhere. Build the multi-seat essay this way:

- **Seats are compression struts.** Each perspective is rendered at its measured strength —
  and *rendered* means the PROSE carries the weight, never the number. For DR material, the
  indexed position's χ magnitude calibrates how hard you write that seat (a strong seat gets
  a direct, forceful account; a weak seat reads visibly thinner); the number itself stays in
  the scaffolding. **Mode B governs tensegrity essays with no exception** (§1.5.4, §3.3): a
  χ value on the page is not measurement the reader can use — it is the *impression* of
  measurement, a false authority smuggled into the exact structure claiming no seat is
  neutral, and it manufactures the center this section exists to prevent. Hold seats APART;
  never fuse them into a synthetic middle position; never refute a seat merely to clear
  room for a conclusion.
- **Declared disparities are the tension members.** Each disparity between seats is stated
  explicitly AND carries its kill condition: what observation would collapse it (the seats
  actually converge) or snap it (one seat is untenable). **Tensegrity without tension is a
  pile of struts:** a disparity with no kill condition is not a structural member, it is
  both-sides filler — cut it or sharpen it. §1.4 applies to disparities, not just claims.
- **Graceful degradation is the payoff.** Refute one strut and the net holds, because no
  perspective was carrying the load alone. This is §2.3's goal achieved by the opposite
  mechanism: §2.3 gets it from redundant support of one conclusion; §2.4 gets it from
  distributed load across held-apart seats. The disparity is not indecision — it is the
  load-bearing structure.

**Snapping a strut (earned collapses only).** A tension member snaps ONLY when its
pre-stated kill condition fires on witnessed grounds — never because the composition wants
a collapse. Three rules:

- **The snap engages the seat's stated claim, not a crisper substitute.** Refuting a
  sharper argument than the one the seat made reads clean precisely because it answers the
  wrong claim (a seat pleading *margin* is not refuted by showing a *floor*-claim circular).
  Before any snap, restate the claim being snapped and check it is the one the seat
  actually holds.
- **A concession scripted into the seat's own mouth earns nothing.** You write every
  account, so you can make any account self-destruct; relocating your inference into the
  teller's voice makes the collapse look *inevitable*, not *sound*
  (F-VENTRILOQUIZED-CONCESSION). The inhabitability test governs the whole account,
  concession included: would its holder sign it?
- **The architecture never requires a snap.** An essay with zero snapped joints is
  structurally complete; pressure to snap something "for structural variety" is form
  driving content. And **like joints get like treatment:** two disparities hanging on the
  same missing resolver share the same status — hold both open, or commit both with the
  same priced kill condition. Snapping one while holding its structural twin open is an
  unearned asymmetry.

**Absent struts (the provenance rule).** A strut can read weak for two reasons that no
single seat can distinguish: the domain is genuinely flat there, or that eye was suppressed
below the threshold of being heard — never authored, never measured. χ is computed from what
was authored, so a seat with no authored eye still renders at a magnitude as if measured,
and the χ spread across the *present* struts silently looks like a complete picture. So
every strut carries its provenance to the read site — **measured** (authored data grounds
it), **inferred** (constructed by the analyst from other seats' material), or **absent** (no
eye exists) — and three rules follow:

- A flat or zero-disparity reading is **undetermined** between genuinely-flat and
  suppressed-below-hearing unless the seat's provenance is measured. Never render an absence
  as a weak strut.
- Absent struts are named **in-body**, not in a footnote: the structure declares its own
  coverage, because a tensegrity figure over the struts that exist presents as symmetric —
  a stereo pair with two eyes missing must say so.
- The decision of which eyes count toward the depth computation is itself structural
  content — but **attribute only what you can witness.** The near cause of an absent strut
  is always your own method, and that IS declarable as fact ("these seats are absent from my
  corpus because I authored from institutional records"). Whether the method-side absence
  mirrors a real-world suppression is a SECOND undetermined, one level up: the suppressed
  eye and the unsourced eye present identically from your seat. Never attribute to the
  domain's power structure what may belong to your own sourcing limits — that smuggles the
  verdict back in through the provenance rule.

**Radial, not axial (no front).** The brittlestar has no spine, no brain, and no front: a
nerve ring, five equivalent arms, a calcite skeleton packed with microlenses — the organism
is a distributed eye with no cyclopean point, and it moves whichever arm the situation loads.
The multi-seat essay likewise needs no privileged "forward" seat and no center that sees for
everyone. **Your own analytical seat is one more strut, declared as such** — not the frame
the other seats hang from. A synthesis voice that presents itself as seatless has
manufactured a center (F-MANUFACTURED-CENTER).

**Hydrostatic commitment (local, costed stiffness).** The octopus arm has no skeleton; it
manufactures rigidity on demand by pressurizing fluid where needed and stays fluid elsewhere.
The multi-seat essay may still commit HARD — locally: one joint stiffened to a verdict when
the cost of delaying a decision exceeds the cost of being wrong. The commitment is declared,
costed, and temporary-until-resolved — real, local stiffness that never pretends to be
permanent bone ("I am flattening this and owning the flattening"). Every pressurized joint
carries its falsifier, and if it is outcome-shaped it emits a Forecast Register pair (§1.6):
it is exactly the empirical hinge whose resolution would legitimately stiffen the whole
structure — and until resolution, it stays fluid.

**Integration lives in the reader (the Rashomon rule).** A stereo pair implies fusion: two
eyes, one recoverable depth — a fact of the matter the disparity triangulates. Most
gauge-variant material is not like that. The seats are not noisy measurements of one hidden
number; they are accounts shaped by what each teller cannot afford to have be true, and the
integrated whole is NOT a fifth account that averages or reconciles them — it is the reader
holding all of them and seeing what each account's distortion reveals about its teller's
stake. So write the seats as ACCOUNTS a reader can inhabit — each honest enough to live
inside, including the ones you dispute — and refuse the reconciling voice: the shape of the
difference is the finding, and a fifth voice that "integrates" is the manufactured center
returning. Reserve depth-fusion — recovering a single answer from the disparity — for the
joints where a fact of the matter actually exists: those are precisely the hydrostatic
joints above, and their fusion waits on the named resolver, not on the writer.

**Discretion is not constraint (the agency line).** Symmetric inhabitability is not
symmetric agency. Some tellers live inside constraints that other tellers SET: where a seat
had a fork and chose — a dataset picked, a deduction designed, a tier authored — the choice
is named as a choice, not dissolved into that teller's constraint story. Scoring discretion
as constraint is how the symmetric form manufactures "no one is responsible": the
view-from-nowhere returning through equal treatment. The genre's symmetry is for *hearing*
the seats, never for equating their authorship.

**When every joint hangs on the same missing instrument.** You cannot Rashomon a
well-measured thing. If the kill conditions of most tension members name the SAME absent
resolver — the survey cancelled, the series never collected, the rate unpublished — say so
in the essay: the plurality of accounts is then the fingerprint of the missing instrument,
the held-open questions are one question wearing several coats, and the spine may be the
absence itself rather than the disagreement. Each instrument-absence is a decision by
someone, attributed PER INSTANCE where witnessed; the pattern needs no coordinator, and
inferring one from the pattern alone is unwitnessed attribution (the absent-strut rule's
method-level discipline applies here too).

**Choosing the architecture:** singleton orbit (observers agree), or a flattening you declare
and own → §2.3 convergent. Multi-type orbit where the disparity is the finding → §2.4
tensegrity. Writing gauge-variant material as a convergent essay WITHOUT declaring the
flattening is F-MANUFACTURED-CENTER.

**And the architecture is scaffolding too.** The reader gets accounts and the differences
between them — never the frame names, the strut count, or the genre machinery. If the prose
needs its architecture explained to work, the scaffolding is leaking at the structural
level (F-SCAFFOLDING-LEAK); every frame beyond the one doing load-bearing work for the
READER is a candidate to drop.

---

## §3. EVIDENCE DEPLOYMENT (REVISED)

### §3.1 Specificity Requirements

**RULE:** Every claim needs **at least one** specific, verifiable moment.

**Additionally REQUIRED:** Source citation sufficient for independent verification.

**Insufficient:** "Recent reports suggest..."
**Sufficient:** "According to court filing [case number], filed [date], [specific claim]."

**Insufficient:** "Intelligence professionals have stated..."
**Sufficient:** "John Smith, former [specific position], stated in [publication] on [date]: [quote]."

### §3.2 The Source Quality Hierarchy

When multiple sources exist, use highest-quality available:

**Tier S (Strongest):**
- Court records, official government documents
- Peer-reviewed academic publications
- Direct primary sources (emails, recordings with verified provenance)

**Tier A (Strong):**
- Major newspaper investigative reporting (NYT, WaPo, WSJ, Reuters, AP, BBC)
- Official statements by named government officials
- Verified testimony under oath

**Tier B (Moderate):**
- Reputable secondary sources with clear attribution
- Named expert analysis with credentials disclosed
- Investigative journalism from established outlets

**Tier C (Weak - Use Only If Nothing Better Available):**
- Anonymous sources
- Tabloid reporting (even if sometimes accurate)
- Unverified claims from blogs or social media
- "Reports suggest" without specific attribution

**RULE:** If you're using Tier C evidence for a major claim, you must:
1. State the source quality explicitly
2. Explain why better sources aren't available
3. Mark the claim as requiring verification
4. State what would constitute proper verification

### §3.3 The Model Transparency Requirement (REVISED in v2.1)

If you use any analytical model to generate claims (DR metrics, statistical analysis, pattern classification), the transparency requirement depends on the model's **visibility mode** in the output.

**MODE A — VISIBLE MODEL (model is named and described in the essay):**

**MANDATORY DISCLOSURE:**
1. **Methodology:** How are metrics calculated?
2. **Inputs:** What data feeds the model?
3. **Validation:** Has this model been peer-reviewed or independently verified?
4. **Limitations:** What can this model NOT tell us?

**RULE:** Model outputs are NEVER Tier 1 evidence. They are analytical tools that help interpret Tier 1 evidence. The reader must understand this distinction.

**Example:**
```
BAD: "The extractiveness coefficient reached 0.92, proving systematic exploitation."

GOOD: "Using a Deferential Realism analytical framework (an interpretive model, 
not empirical measurement), we can characterize the pattern as having high 
'extractiveness'—meaning asymmetric benefit flows where one party gains at 
another's expense. This characterization helps organize the documented facts 
[list], but the model itself is not additional evidence."
```

**MODE B — INVISIBLE SCAFFOLDING (model shaped the analysis but does not appear in the essay):**

This is the **default mode** when using Deferential Realism constraint stories as input (see §1.5.4). The model's analytical contributions are translated into domain-appropriate language and supported by independently sourced evidence.

**MANDATORY DISCIPLINE:**
1. **No DR vocabulary in the output.** Translate all concepts (see §1.5.4 table).
2. **Every claim must stand on its own evidence.** If a claim originated from a DR diagnostic (e.g., "coordination-washed" detection), the essay must support that claim with Tier 1 evidence from public records, not with the diagnostic finding itself.
3. **Confidence calibration must be invisible but operative.** Use purity scores and MaxEnt entropy to calibrate how strongly you write about each constraint, but do not cite the scores.
4. **The model did the finding; the evidence does the proving.** The constraint stories told you *where to look*. The research tells the reader *what's there*.

**WHY MODE B EXISTS:** A model that readers don't know is less vulnerable to the F-MODEL-AUTHORITY anti-pattern — but only if every translated claim is independently evidenced. Invisible scaffolding without independent evidence is worse than visible modeling, because the reader has no way to evaluate the analytical source.

**CRITICAL CHECK for Mode B:** After drafting, remove all constraint story files from your mental workspace and ask: "Does every claim in this essay have public-record support that a reader could verify without knowing DR exists?" If not, the claim needs either more evidence or removal.

---

## §4. STAKES AND ENDINGS (REVISED)

### §4.1 The Stakes Anchor: Institutional Over Historical

**REQUIRED:** Stakes must answer "Why does this matter for current decision-making?"

**WEAK STAKES:** "This reveals the truth about X." (Historical curiosity)
**STRONG STAKES:** "This pattern indicates [specific current vulnerability] requiring [specific institutional response]."

**Template:**
```
If this pattern reflects [hypothesis A]: [specific institutional action needed]
If this pattern reflects [hypothesis B]: [different but overlapping actions needed]  
Regardless of which hypothesis is correct: [minimum actions that address all cases]
```

### §4.2 The Action Requirement (NEW)

Every essay analyzing institutional failure must include:

**SPECIFIC, IMPLEMENTABLE RECOMMENDATIONS** that:
- Don't require accepting your most controversial claims
- Could be implemented by specific institutions with existing authority
- Would resolve key uncertainties or close identified gaps
- Scale to the severity of documented problems

**Example:**
```
VAGUE: "This situation requires investigation."
SPECIFIC: "The Department of Justice should conduct a counterintelligence 
assessment of current officials with security clearances who were documented 
in Epstein's orbit, following standard CI protocols established in [regulation]. 
Results should be reported to relevant oversight committees by [timeline]. This 
requires no new legal authority and addresses the documented gap between criminal 
prosecution and security assessment."
```

### §4.3 Ending Types: Escalation to Action

**BANNED:** Summary, "In conclusion," restatement of thesis

**REQUIRED:** Choose appropriate ending based on essay type:

**Type 1 - Unresolved Institutional Question:**
End with the most important question that existing institutions could answer but haven't. Make the refusal to answer look like the actual scandal.

**Type 2 - Cascade Implication:**
End with what happens if the documented pattern is allowed to continue/spread. Make inaction look more costly than action.

**Type 3 - Minimal Accountability Demand:**
End with the smallest possible institutional response that would address the documented problem. Make refusal to do even this minimal action look indefensible.

---

## §5. QUALITY GATES (REVISED AND EXPANDED)

### §5.1 The Brittleness Test (NEW)

**Ask:** "If one major claim were disproven, would the entire argument collapse?"

- **If YES:** Restructure to create independent lines of evidence
- **If NO:** Verify that remaining claims still support actionable conclusions

**Goal:** Graceful degradation. The argument should get weaker if attacked, not shatter entirely.

### §5.2 The Defamation Check (NEW)

For any named individual:

**Ask:** "Am I claiming this person committed crimes or unethical acts?"

- **If YES and they've been convicted:** State the conviction explicitly
- **If YES and they haven't been convicted:** Either remove the claim or frame it as "alleged," "charged with," or "according to [specific source]"
- **If YES and it's based on inference:** Move to Tier 2 or 3 explicitly

**RULE:** NEVER present unproven criminal activity as fact, regardless of how convinced you are.

### §5.3 The Alternative Explanation Check (NEW)

**Ask:** "Have I addressed the simplest competing explanation?"

- **If NO:** Add section explaining why simpler explanations are insufficient
- **If YES but dismissively:** Strengthen the engagement - show specifically why evidence doesn't fit the simpler pattern

### §5.4 The Source Verification Check (REVISED)

For every Tier 1 claim:

**Verify:**
- [ ] Specific citation provided (not "recent reports")
- [ ] Source is independently verifiable
- [ ] Source meets quality threshold for claim importance
- [ ] If source is contested, alternative sources provided
- [ ] If using secondary sources, primary sources identified where possible

**The recall limit, and the borrowed-example rule (NEW in v2.2).** Verification is
precision-shaped by construction: every check is seeded by a claim already in the draft, so it
searches the frame's own neighborhood — a true claim returns confirmation, and the counterexample
sitting one step to the side of a sentence that passed is unreachable by any query the sentence
generates. A pass returning all-claims-check is byte-identical to a pass that never looked for
what wasn't claimed. A fresh reader does not fix this: a new instance reading the finished essay
inherits the seat from the artifact — the text is the query. **Recall requires a different
question, not a different reader.**

One blind zone is predictable and therefore addressable: when the essay leans on an example
imported from outside its field, the likeliest counterexample lives in that example's HOME field,
which has known things about its own canonical cases for decades and whose literature the essay's
author does not hold. **Rule: for every borrowed example, run at least one verification query
whose subject is the EXAMPLE in its home literature, not the essay's claim** ("what does the
history of non-Euclidean geometry say about how the alternative became available" finds Saccheri;
"was the parallel postulate a choice" never will, because that claim is true and verification
returns confirmation). Evidence status and kill condition: ISSUES.md OQ-233.

The residual no query closes is declared, not recalled: state what corpus the piece was built
from and what was knowingly not consulted (§6 metadata, "Corpus consulted"). That converts an
invisible miss into a marked one — the only move available from a seat.

- [ ] Every borrowed example has at least one home-field query run against the example itself
- [ ] The Corpus-consulted declaration is filled (§6 metadata)

### §5.5 The Model Humility Check (REVISED in v2.1)

If essay uses analytical models/frameworks:

**For Mode A (visible model):**
- [ ] Model methodology disclosed
- [ ] Model outputs marked as interpretive, not empirical
- [ ] Model limitations acknowledged
- [ ] Model not treated as independent evidence

**For Mode B (invisible scaffolding — default for DR constraint stories):**
- [ ] No DR vocabulary appears in the published text
- [ ] Every claim that originated from a DR diagnostic has independent public-record support
- [ ] Confidence levels across sections track the purity/entropy gradient from the reports (strong language for clean constraints, cautious language for contaminated ones)
- [ ] Omega variables have been translated into the "Unresolved Questions" section
- [ ] The essay would be fully intelligible and defensible to a reader who has never heard of Deferential Realism
- [ ] You have not cited coupling scores, purity numbers, theater ratios, χ magnitudes, or any DR metric as evidence — in a §2.4 tensegrity essay too; §2.4 is not an exception to Mode B

**For either mode, whenever constraint stories/reports were provided as input (§1.5.6):**
- [ ] Findings Ledger present: one row per provided story plus a pattern-of-patterns row, each with a `used (where)` / `declined (why)` / `not read` disposition
- [ ] At least one ledger row is beyond-the-brief, or the ledger states `reports not consumed beyond the brief` explicitly
- [ ] Input provenance names the brief artifact (or `ad-hoc chat summary` / `none`) and lists reports read and not read

### §5.6 The Counterfactual Completeness Check (REVISED)

For major claims:

**Verify:**
- [ ] Falsification conditions stated
- [ ] Evidence that would disprove claim identified
- [ ] If claim is unfalsifiable, it's been removed or completely reframed

### §5.7 The Scoreability Gate (NEW in v2.2)

For the Forecast Register (§1.6, §6.1):

- [ ] Every Tier 3 hypothesis making a future-behavior claim has at least one forecast pair
- [ ] Mechanism and magnitude are separate rows, never one pooled question
- [ ] Each row passes the register-only-reader test: resolvable with no access to the essay
- [ ] All dates absolute; all thresholds numeric; every resolver named
- [ ] Both `p_essay` and `p_baseline` stated, with the reference class
- [ ] Every row direction-tagged (`fragility` | `stability`)
- [ ] The block parses as YAML and carries the `FORECAST REGISTER v1` marker
- [ ] Claims with predictive ambition but no register row have been explicitly reclassified as
      non-predictive (interpretive/retrospective) — silence is not an exemption

---

## §6. OUTPUT FORMAT (REVISED)

```markdown
# [Title: Focus on Infrastructure/System, Not Identity/Villain]

[Opening: Pattern that demands explanation, not assertion of conclusion]

[Body sections following evidence tier discipline]

## Evidence Framework

### Documented in Public Records (Tier 1):
- [Claim] - [Specific source with citation]
- [Claim] - [Specific source with citation]

### Reasonable Inferences from Documented Facts (Tier 2):
- [Inference] - [Which Tier 1 facts support this, and why]
- [Inference] - [Marked explicitly as inference, not additional fact]

### Structural Hypotheses Requiring Additional Evidence (Tier 3):
- [Hypothesis] - [What evidence would move this to Tier 2]
- [Hypothesis] - [What evidence would falsify this]

## Alternative Explanations Considered
- [Simpler explanation]: [Why insufficient based on documented anomalies]
- [Competing complex explanation]: [How evidence would distinguish]

## Institutional Actions Required
[Regardless of which hypothesis proves correct]

1. [Specific action] - [Which institution] - [Timeline]
2. [Specific action] - [Which institution] - [Timeline]

## Forecast Register
[REQUIRED — the machine-extractable block per §6.1. Publication in the public copy is a
per-essay voice call; the archived copy MUST retain it.]

## Unresolved Questions
[What could existing institutions answer but haven't]

---

## METADATA (for author review, not publication)

**Adversarial Review:**
- Weakest link: [Identified vulnerability]
- Most likely criticism: [Expected attack vector]
- Defense: [How claim survives or why it's been strengthened]

**Brittleness Assessment:**
- Independent evidence lines: [Count]
- Critical dependencies: [Identified and addressed]

**Source Quality:**
- Tier S sources: [Count]
- Tier C sources: [Count and justification if used]

**Model Transparency:**
- Models used: [List]
- Visibility mode: [A (visible) / B (invisible scaffolding)]
- Limitations disclosed: [Yes/No/N/A for Mode B]

**DR Scaffolding (REQUIRED whenever constraint stories/reports were provided as input, Mode A or B — an absent block is F-ATTESTED-CONSUMPTION by omission):**
- Input provenance: [brief artifact used (path/version, or "ad-hoc chat summary", or none); reports read; reports not read]
- Findings ledger (§1.5.6): [one row per story + a pattern-of-patterns row: finding → used (where) / declined (why) / not read; mark the beyond-the-brief row(s), or state "reports not consumed beyond the brief"]
- Constraint stories used: [Count and IDs]
- Structural signatures detected: [List findings that shaped the essay]
- Purity gradient: [Which sections are high-confidence vs. boundary cases]
- Omega-to-question mapping: [Which omegas became which unresolved questions]
- Unsupported translations: [Any DR insights that lack independent Tier 1 evidence — these should have been removed]
- Strut provenance (§2.4 essays): [Per seat: measured / inferred / absent; absent struts named in-body? Y/N; who decided which eyes count]
- Corpus consulted: [Sources and fields the piece was built from; fields knowingly NOT consulted — especially the home fields of borrowed examples (§5.4); converts invisible misses into marked ones]
```

### §6.1 The Forecast Register Block Format (NEW in v2.2)

The register is the scoring interface: a subsequent model extracts this block alone (per
`agent/uke_score_v0.1.md`) and grades it without reading the essay. Keep the fence and the
`FORECAST REGISTER v1` marker exactly as formatted.

```yaml
# FORECAST REGISTER v1
essay: [slug]
date_written: [YYYY-MM-DD]
forecasts:
  - id: F1
    hypothesis: "[the Tier 3 hypothesis this pair scores, restated self-contained]"
    column: mechanism            # mechanism | magnitude
    question: "[Binary, self-contained: named entity, no anaphora. e.g. 'Will the leadership
      succession process — not sanctions, protest, or economic shock — be the primary locus
      of governance disruption in COUNTRY X following EVENT Y?']"
    resolution_date: YYYY-MM-DD
    resolver: "[Named source/criterion that settles it]"
    p_essay: 0.00                # your probability, structural analysis included
    p_baseline: 0.00             # reference-class probability without the structural read
    reference_class: "[what base rate p_baseline comes from]"
    direction: fragility         # fragility | stability
  - id: F2
    hypothesis: "[same hypothesis — the paired magnitude/timeline question]"
    column: magnitude
    question: "[e.g. 'Will INSTITUTION Z remain without a confirmed head for more than 30
      days after EVENT Y, per REUTERS/AP reporting?']"
    resolution_date: YYYY-MM-DD
    resolver: "[Named source/criterion]"
    p_essay: 0.00
    p_baseline: 0.00
    reference_class: "[...]"
    direction: fragility
```

Rows are pairs by construction (`column: mechanism` + `column: magnitude` per hypothesis); a
hypothesis may carry several magnitude rows at different thresholds/horizons, but never a
pooled row. Scoring, aggregation, and the two-column discipline live in `uke_score_v0.1.md` —
do not restate them here.

---

## §7. ANTI-PATTERNS (REVISED AND EXPANDED)

### F-SPECTACLE-OVER-DEFENSIBILITY
Making dramatic claims that sound compelling but can't survive intelligent criticism.
**Fix:** Run adversarial verification. If claim can't survive attack, remove it.

### F-TIER-CONFLATION
Presenting inferences or hypotheses as if they're documented facts.
**Fix:** Explicitly mark which tier every major claim belongs to.

### F-SINGLE-NARRATIVE
Asserting one complex explanation while ignoring simpler alternatives.
**Fix:** Add "Alternative Explanations" section addressing simpler possibilities.

### F-MODEL-AUTHORITY
Treating analytical model outputs as empirical evidence.
**Fix:** Disclose model methodology and mark outputs as interpretive tools.

### F-VILLAIN-FOCUS
Making the essay about individual bad actors rather than institutional patterns.
**Fix:** Restructure to emphasize systems that enabled/sustained the behavior.

### F-CHAIN-DEPENDENCY
Structuring arguments where each claim depends on the previous one.
**Fix:** Create independent evidence lines that support conclusion separately.

### F-SOURCE-VAGUENESS
Using phrases like "reports suggest" or "intelligence sources say" without specific attribution.
**Fix:** Provide specific, verifiable citations or remove the claim.

### F-STAKES-ABSENCE
Essay lacks clear answer to "why does this matter for current decision-making?"
**Fix:** Add institutional action requirements or unresolved questions section.

### F-UNFALSIFIABILITY
Making claims that cannot be disproven by any conceivable evidence.
**Fix:** State what evidence would falsify the claim, or remove it.

### F-REPORT-AS-CONTENT (NEW in v2.1)
Treating Prolog diagnostic reports as quotable source material rather than as validation instruments for hypotheses.
**Fix:** Read reports like lab results. Extract the verdicts (which hypotheses survived, which failed, what the pattern means), not the sentences. The report told you *where the structure is*; the research tells the reader *what's there*.

### F-SCAFFOLDING-LEAK (NEW in v2.1)
DR vocabulary or metric citations appearing in a Mode B (invisible scaffolding) essay, creating cognitive load for readers unfamiliar with the framework.
**Fix:** Search the draft for all DR-specific terms (Mountain, Rope, Snare, Tangled Rope, Piton, Scaffold, purity, coupling score, theater ratio, gauge-invariant, Boltzmann, presheaf, orbit). Replace with domain-appropriate translations per §1.5.4 table.

### F-UNSUPPORTED-TRANSLATION (NEW in v2.1)
Translating a DR diagnostic finding into domain language but failing to provide independent evidence for the translated claim. The essay says "presents as coordination but structurally transfers dependency" because the Prolog said "coordination-washed" — but the essay provides no public-record evidence for that claim.
**Fix:** Every translated DR insight must have at least one Tier 1 fact supporting it independently. If you can't find one, the insight stays in your notes, not in the essay.

### F-UNSCOREABLE-PREDICTION (NEW in v2.2)
Making a structural claim with predictive ambition — atrophy, cascade, baseline shift,
paralysis — with no date, no threshold, and no named measurable, so that nothing could ever
grade it. The claim reads as bold but is immune to being wrong.
**Fix:** Emit the forecast pair (§1.6) into the Forecast Register, or explicitly mark the
claim as interpretive (non-predictive). The test: could a model reading only the register
grade this essay in two years? If not, the prediction is decoration.

### F-MANUFACTURED-CENTER (NEW in v2.2)
Writing gauge-variant material as a single-verdict essay without declaring the flattening:
the synthesis voice presents itself as seatless — a center that sees for everyone — and the
plurality collapses by form regardless of how carefully the prose hedges. The dual failure
is fake tensegrity: seats listed side by side with no kill-conditioned disparities holding
them in relation — both-sides filler posing as structure.
**Fix:** Either build the essay as tensegrity (§2.4 — seats as struts at measured strength,
disparities as tension members with kill conditions, your own seat declared as one strut),
or commit to the flattening explicitly: name the seat you are collapsing to, own the cost,
and attach the falsifier (hydrostatic commitment, §2.4).

### F-ABSENT-STRUT (NEW in v2.2)
Rendering a seat nobody authored as if it were a weak seat: a flat reading that actually
means "no eye was ever pointed there" enters the structure at face value, and the χ spread
over the struts that exist presents as a complete, symmetric picture. Absence presenting as
presence — the same failure F-MANUFACTURED-CENTER names at the center, occurring at a strut.
**Fix:** Carry provenance per strut (measured / inferred / absent, §2.4). Flat-without-
measured is undetermined, never evidence of flatness; absent struts are named in-body; the
eye-selection is attributed at the METHOD level (what the author sourced — declarable as
fact), with the world-side cause of the absence left undetermined unless independently
witnessed.

### F-ENGINE-AS-TRUTH (NEW in v2.2)
Treating the engine's outputs as findings about the world rather than as sharpened
hypotheses about where to look: χ values placed in prose as if they were measurements;
run-stability ("three runs, stable spread") cited as evidence about the domain when it is
evidence about how the constraint stories were authored; the essay auditing the reports
instead of telling the story the reports pointed at. The engine's job is to sharpen the
analysis and let the writer hold multiple views at once — it measures the authored corpus,
never the world.
**Fix:** §1.5.1 orientation (stories are hypotheses, reports are validation, the essay is
the deliverable), the replicate-stability rule (§1.5.2), Mode B with no §2.4 exception
(§3.3, §5.5). The only world-measurements in the essay are world-side sources with named
resolvers — everything engine-side is scaffolding, and scaffolding disappears.

### F-VENTRILOQUIZED-CONCESSION (NEW in v2.2)
Making a seat refute itself by scripting the collapse into its own mouth — often paired
with quietly swapping the seat's stated claim for a crisper one the refutation can beat.
The relocation makes the writer's inference look like the teller's inevitability; the snap
reads earned and is not. Common driver: form-pressure to snap a joint for structural
variety, which sacrifices the softest seat to the aesthetic.
**Fix:** §2.4 snapping rules — snap only on a fired, pre-stated kill condition; engage the
claim the seat actually made; the holder must be able to sign the whole account, concession
included; zero snaps is structurally complete; like joints get like treatment.

### F-ATTESTED-CONSUMPTION (NEW, 2026-07-25)
Grounding metadata that attests without witnessing: "corpus consulted," "reports reviewed,"
a filled scaffolding block — with no per-finding disposition, so a pass that never opened
the reports produces metadata indistinguishable from one that did. Consumption claimed as a
signature with no observable failure state; the essay-layer instance of recap-as-witness.
The dual failure is the fabricated ledger — rows invented to satisfy the format — a false
witness worse than an honest `not read`.
**Fix:** §1.5.6. The Findings Ledger with per-row dispositions; the beyond-the-brief
positive-control row; `not read` / `nothing survived` / `reports not consumed beyond the
brief` as legal, visible entries. Ledger rows name content locatable in specific report
files — spot-check them against the reports when in doubt.

---

## §8. THE CORE DISCIPLINE

**UKE_W v2.0 adds one overriding principle:**

**ASSUME INTELLIGENT OPPOSITION**

Write as if:
- A smart skeptic will attack every weak point
- Institutional actors will ignore anything they can dismiss
- Your most controversial claim will be used to discredit everything else
- Only claims that survive adversarial review will have impact

This doesn't mean hedging everything. It means:
- Making bold claims that are actually defensible
- Removing spectacular claims that are brittle
- Focusing on documented facts that demand institutional response
- Building arguments that degrade gracefully rather than shatter

**The paradox:** This discipline makes arguments MORE threatening to power, not less—because they can't be dismissed as conspiracy theory or partisan attack.

---

## §9. VERSION NOTES

**Changes from v2.1 to v2.2:**

**Added:**
- §1.6: The Scoreable Prediction Requirement — every future-behavior Tier 3 hypothesis emits
  a mechanism/magnitude forecast pair with absolute dates, numeric thresholds, named
  resolvers, `p_essay` + `p_baseline`, and a fragility/stability direction tag
- §6.1: The Forecast Register block format (machine-extractable YAML, `FORECAST REGISTER v1`
  marker) + a register slot in the §6 output template
- §5.7: The Scoreability Gate
- F-UNSCOREABLE-PREDICTION anti-pattern
- Companion scoring protocol `agent/uke_score_v0.1.md` — the register's consumer; a
  subsequent model applies it as a standard rubric to the register block alone
- §2.4: Tensegrity architecture for the multi-seat essay — seats as compression struts at
  measured strength, declared disparities with kill conditions as the tension net, no
  privileged front (the author's seat declared as one strut), hydrostatic local commitment
  wired to the Forecast Register; §2.3 scoped as the convergent mode; §1.5.2 gauge-variant
  material now routes to §2.4
- F-MANUFACTURED-CENTER anti-pattern (seatless synthesis voice; dual: fake tensegrity —
  seats with no kill-conditioned disparities)
- §2.4 absent-strut provenance rule + F-ABSENT-STRUT anti-pattern + §6 metadata line —
  a consumer-found blind spot (first live §2.4 application, Draft 4): a seat nobody authored
  renders at a χ magnitude as if measured, so absence presents as presence; per-strut
  provenance (measured / inferred / absent), flat-without-measured = undetermined, coverage
  declared in-body, eye-selection attributed at the method level only (the world-side cause
  of an absence is a second undetermined, one level up — the suppressed eye and the
  unsourced eye present identically from the author's seat; attributing the domain's power
  structure for what may be a sourcing limit smuggles the verdict back in). Scope of the
  rule, stated plainly: it closes the REPRESENTATIONAL gap, not the epistemic one — it makes
  the writer say they cannot tell which cause produced the zero; it does not let them tell.
- Second consumer-found correction (Draft 4 → Draft 5): §2.4's "at its measured χ" clause
  read as a Mode B exception and put scores in the prose — closed: χ calibrates how hard
  each seat is written, the number never appears; Mode B has no §2.4 exception (§5.5
  hardened). Plus the Rashomon rule (integration lives in the reader — no reconciling fifth
  voice; depth-fusion reserved for hydrostatic joints with a real resolver), the
  replicate-stability rule (§1.5.2: run-stability is evidence about the authoring, never
  the world), and F-ENGINE-AS-TRUTH.
- Fourth reviewer-found addition (2026-07-23): the recall limit + borrowed-example rule in §5.4
  (verification is precision-shaped — draft-as-query-source searches its own neighborhood; a
  fresh reader inherits the seat from the artifact, so recall needs a different question, not a
  different reader; for borrowed examples, query the example in its home field, not the claim) +
  the Corpus-consulted declaration in §6 metadata (the declared residual — the essay-layer
  instance of the OQ-230 coverage field). Evidence + kill condition: OQ-233.
- Third reviewer-found correction (Draft 6): snapping was ungoverned — the Rashomon rule
  lets the writer script every account, so an account can be made to self-destruct. §2.4
  snapping rules added (snap only on a fired pre-stated kill condition; engage the seat's
  stated claim, not a crisper substitute; the holder signs the whole account, concession
  included; zero snaps is complete; like joints hanging on the same missing resolver get
  like treatment) + F-VENTRILOQUIZED-CONCESSION; the agency line (discretion is not
  constraint — symmetry must not launder operators into weather); the shared-instrument
  convergence rule (you cannot Rashomon a well-measured thing — when most kill conditions
  name the same absent resolver, the absence is the spine; per-instance attribution, no
  coordinator inferred from the pattern); and "the architecture is scaffolding too" (frame
  machinery never appears to the reader; excess frames are dropped).
- Fifth consumer-found addition (2026-07-25): the synthesis brief + the Findings Ledger
  (§1.5.6; §5.5 either-mode checklist; §6 metadata Input-provenance and ledger lines, DR
  Scaffolding block made required-when-input-provided; F-ATTESTED-CONSUMPTION). Witnessed
  instance: a five-report run produced a strong essay ("The Byline Is Not a Bond") whose
  metadata showed no trace of the reports — the pass consumed the conversation summary,
  and nothing in the protocol could tell; the essay's own thesis (a signature carrying no
  consequence is not accountability) described its own grounding metadata. Consumption now
  has a failure state: the de-facto interface (the brief) is formalized as a named, stored
  artifact with declared provenance; the reports get a per-finding disposition ledger with
  a beyond-the-brief positive-control row; and honest `not read` / `nothing survived`
  entries are legal while silence is not.

**Lesson that prompted this revision:**
A retrospective grading attempt over the essay archive found that scoreability is a property
of how an essay was written, not of elapsed time: essays that named institutions, forked
outcomes, and wrote falsifiers in-body could be graded; essays making structural claims of
equal ambition with no date, threshold, or named measurable could not. The one graded case
also showed mechanism-identification and magnitude-estimation failing independently (right
joint, wrong resilience estimate) — hence the two-column register, and skill-over-baseline as
the headline so the scoring rule cannot reward timeline-hedging. Provenance and the
pre-registered fragility-bias hypothesis: ISSUES.md OQ-229. §2.4 came from the same review
cycle: designing a stereo-pair (multi-seat) essay exposed that the protocol's only
architecture was a convergent verdict-spine — the form itself collapsed plurality (the same
lesson the pipeline learned when the auto-synthesized essay was removed), so the multi-seat
form got its own load-bearing architecture instead of a hedged version of the old one.

---

**Changes from v2.0 to v2.1:**

**Added:**
- §1.5: Complete guidance for working with Deferential Realism constraint stories as input
  - Constraint stories as hypotheses (§1.5.1)
  - Reading Prolog reports as lab results (§1.5.2)
  - Network edges as essay architecture (§1.5.3)
  - DR vocabulary as invisible scaffolding (§1.5.4)
  - Pattern-of-patterns meta-reading (§1.5.5)
- Three new anti-patterns: F-REPORT-AS-CONTENT, F-SCAFFOLDING-LEAK, F-UNSUPPORTED-TRANSLATION

**Revised:**
- §3.3 Model Transparency: Now distinguishes Mode A (visible model) from Mode B (invisible scaffolding), with Mode B as default for DR constraint stories
- §5.5 Model Humility Check: Now includes Mode B checklist for verifying DR vocabulary has been fully translated and all translated claims have independent evidence

**Philosophy shift from v2.0:**
- v2.0 assumed models would be visible in the output and required explicit disclosure
- v2.1 recognizes that the default mode for DR is invisible scaffolding: the framework shapes the analysis but the essay stands on its own evidence
- The key insight: DR constraint stories are research instruments, not content. The Prolog output validates hypotheses; web research fills in evidence; UKE_W shapes the deliverable. Each layer serves a different function, and the essay's credibility rests on the evidence layer, not the hypothesis layer.

**Lesson that prompted this revision:**
In practice, the Prolog diagnostic stack was doing more work than initially recognized — not as quotable content, but as a calibration instrument. Structural signature detections (coordination-washed, physics-washed) identified the essay's central claims. Purity scores calibrated confidence levels across sections. Omega variables generated the essay's unresolved questions. Theater drift metrics backed the political theater angle. None of these appeared in the essay, but all of them shaped it. The revision makes this relationship explicit so future essays can leverage the diagnostic stack more deliberately.

---

**Changes from v1.0 to v2.0:**

**Added:**
- Three-tier evidence framework (mandatory)
- Simpler explanation requirement
- Adversarial verification pass
- Source quality hierarchy
- Model transparency requirement
- Institutional action requirements
- Brittleness test
- Defamation check
- Alternative explanation check

**Revised:**
- Counterfactual test (now requires stating falsification conditions)
- Stakes anchor (now requires institutional focus)
- Paragraph architecture (now emphasizes defense in depth)
- Opening strategies (now emphasizes infrastructure over identity)

**Philosophy shift:**
- v1.0: "Aggressive interpretation anchored to evidence"
- v2.0: "Defensible arguments that survive intelligent criticism"

**The key insight:** Spectacular claims that collapse under scrutiny accomplish nothing. Modest claims backed by irrefutable evidence and focused on institutional accountability are more threatening to power structures because they can't be dismissed.

---

## END OF UKE_W v2.2

**Status:** Production-ready
**Purpose:** Generate arguments that survive adversarial review and force institutional response
**Key innovation (v2.0):** Mandatory adversarial verification and evidence tiering prevents brittleness
**Key innovation (v2.1):** Explicit integration of DR constraint stories as hypothesis-validation inputs with invisible-scaffolding default
**Key innovation (v2.2):** The Forecast Register — predictions scoreable-by-construction, two-column (mechanism/magnitude), graded later by `uke_score_v0.1.md` without human triage
