# 📋 UKE_SUMMARY v1.0 [Universal Knowledge Evaluator — Engine Extraction Protocol]

### §0. FOUNDATION

**Purpose:** Extract load-bearing findings from DR engine reports and organize them into a beat sheet that anchors downstream essay generation and editorial validation. This protocol does not write prose. It extracts, flags, and organizes.

**Core Invariant:** **THE ENGINE OUTRANKS THE STORY.** The Prolog engine computes from declared metrics. If the engine's computed type disagrees with the story's declared type, the engine is right and the story flinched. This invariant is inherited from UKE_FLINCH. UKE_SUMMARY enforces it at the extraction stage so that downstream protocols cannot silently drop findings.

**Secondary Invariant:** **EXTRACT, DON'T INTERPRET.** The model executing UKE_SUMMARY produces a structured extraction with engine language preserved. It does not explain findings, soften them, contextualize them, or assess their importance. Importance is determined by structural criteria (§2), not editorial judgment.

**What this protocol is:**
- A mechanical extraction phase (like UKE_FLINCH §1-§3)
- An organizer that sequences findings into narrative-ready beats
- A reference document that serves two pipeline positions

**What this protocol is not:**
- An essay
- An interpretation of findings
- An editorial assessment
- A quality judgment

**Pipeline Position:**
```
Prolog engine → reports + .pl files
                    ↓
            ┌─ UKE_SUMMARY ─┐
            │  (this protocol) │
            └────────┬────────┘
                     │
          ┌──────────┼──────────┐
          ↓                     ↓
  Human + LLM discussion    [retained for analysis.py]
          ↓                     ↓
  UKE_DISCUSSION draft          ↓
          ↓                     ↓
  Essay instance                ↓
          ↓                     ↓
  analysis.py ←──── UKE_SUMMARY output
  (UKE_REALITY ENGINE-BACKED)
```

**Required Inputs:**
1. **Constraint stories** (`.pl` files) — the LLM-authored Prolog specifications
2. **Enhanced reports** (`_report.md` files) — the engine's computed output

Both are required. Do not proceed without them.

**Output:** A beat sheet with per-constraint extractions, divergence flags, cross-constraint patterns, and a narrative-flow outline.

---

### §1. PER-CONSTRAINT EXTRACTION (Mechanical)

For each constraint story and its corresponding report, extract the following. Use the engine's exact language. Do not paraphrase findings into softer formulations.

#### §1.1 Identity and Divergence

```
CONSTRAINT: [constraint_id]
HUMAN READABLE: [from human_readable/2]
DOMAIN: [from topic_domain/2]

DECLARED TYPE: [from constraint_claim/2 in .pl file]
COMPUTED TYPE (canonical four observers):
  powerless:     [type]
  moderate:      [type]
  institutional: [type]
  analytical:    [type]

ORBIT SIGNATURE: [from report, e.g., [tangled_rope, snare]]
ORBIT SPAN: [number of distinct types in orbit]

INDEX MISMATCHES:
  [list every INDEX MISMATCH from report, verbatim]
  Total: [N] of [total perspectives] mismatched

STRUCTURAL SIGNATURE: [from report — e.g., false_ci_rope, false_natural_law, natural_law]
SIGNATURE CONFIDENCE: [from report]
SIGNATURE INTERPRETATION: [copy the → line verbatim from report]
```

#### §1.2 Core Metrics

```
BASE METRICS:
  ε (extractiveness): [value]
  suppression: [value]
  theater_ratio: [value]

CHI DECOMPOSITION (from report):
  | Observer | χ | f(d) | scope_mod |
  | powerless | [value] | [value] | [value] |
  | moderate | [value] | [value] | [value] |
  | institutional | [value] | [value] | [value] |
  | analytical | [value] | [value] | [value] |

DOMINANT FACTOR: [from Axiom 2 decomposition — e.g., "directionality (98.6%)"]
```

#### §1.3 Diagnostic Stack

Extract from the report. Copy values; do not summarize.

```
VERDICT: [GREEN / YELLOW / RED] — [N] subsystems, [N] tension(s)

Boltzmann: [compliant / non_compliant(score, threshold)]
Purity: [intrinsic score] ([band]) → effective [score] after contamination
Coupling: [independent / weakly / strongly_coupled] (score: [value])
H¹ band: [value] — [interpretation from report]

MAXENT:
  Classical top type: [type] (P=[value], confidence=[value])
  Indexed top type (analytical): [type] (P=[value])
  Classical/Indexed TV Distance: [value] ([small/moderate/large])

WASSERSTEIN: [total W₁] — peak transport: [edge] ([value])

CONTEXTUALITY: [value] (H¹=[value], [N] of [N] context-pairs disagree)
MONOTONICITY: [constant / monotone / non-monotone]

ACTIVE THEOREMS: [list with one-line descriptions from report]
  [e.g., "T1 (Cover Story — satisfied): extraction is structurally invisible 
   from at least one observer position"]
```

#### §1.4 Drift and Contamination

```
DRIFT EVENTS: [N] total — [N] critical, [N] warning, [N] watch
  [list each event with severity and evidence, verbatim from report]

CONTAMINATION NETWORK:
  Intrinsic purity: [value]
  Effective purity: [value]
  Propagation delta: [value]
  Neighbors: [N]
    [list each neighbor: name, type, edge type, strength, purity]

TERMINAL STATE PREDICTION: [type] (confidence: [level])
```

#### §1.5 Actors

```
BENEFICIARIES: [list from .pl file]
VICTIMS: [list from .pl file — note if empty]

OMEGA VARIABLES (from .pl and report):
  [label]: [question] — [resolution type from report]
  
ENRICHED OMEGA (from report, if present):
  [label]: severity=[value], gap_class=[value], gap_pattern=[value]
```

#### §1.6 Abductive Flags

```
ABDUCTIVE FLAGS: [N] trigger(s)
  [list each: trigger class, confidence, anomaly, category, interpretation]
```

---

### §2. LOAD-BEARING FINDING IDENTIFICATION (Structural Criteria)

After extracting §1 for all constraints, identify load-bearing findings using these structural criteria. A finding is load-bearing if it meets ANY of the following conditions. The model does not exercise editorial judgment about importance — it applies the criteria mechanically.

**Criterion 1: Declared/Computed Type Divergence.**
The .pl file declares one type; the engine computes a different type from any canonical observer position. Every such divergence is load-bearing.

**Criterion 2: Structural Signature Override.**
The engine fires a structural signature (false_ci_rope, false_natural_law, false_summit_mountain) that overrides the metric-based classification. Every override is load-bearing.

**Criterion 3: Critical Drift.**
Any drift event at critical severity. Critical drift means the constraint is actively changing in ways that affect classification.

**Criterion 4: Contamination Effect.**
Effective purity differs from intrinsic purity by more than 0.10. The constraint's classification is being influenced by its network neighbors.

**Criterion 5: High Perspectival Fracture.**
H¹ ≥ 3. Observers disagree in ways that cluster into discrete blocs. The disagreement is structural, not noise.

**Criterion 6: Enriched Omega with Extraction Masking.**
The engine generated an enriched omega with gap_pattern containing "masked" (e.g., snare_masked_as_rope). This means the engine detected that extraction is hidden by institutional framing.

**Criterion 7: Cross-Constraint Convergence.**
Multiple constraints sharing a beneficiary show convergent signatures, convergent drift, or convergent contamination. Systemic pattern, not constraint-local finding.

```
LOAD-BEARING FINDINGS:

Finding [N]:
  Constraint: [id]
  Criterion: [which criterion triggered]
  Engine says: [verbatim from report]
  .pl declares: [verbatim from story, if divergence]
  
  [repeat for each finding]

Total load-bearing findings: [N]
```

---

### §3. CROSS-CONSTRAINT ANALYSIS (Mechanical)

#### §3.1 Shared Beneficiary Sets

From the cross-constraint convergence section of the reports (if present):

```
BENEFICIARY SET: [beneficiary name] (N=[count])
  Members: [list constraint_ids]
  
  Convergent signatures: [list if present]
  Convergent drift: [type, severity, if present]
  Convergent contamination: [pattern if present]
  
  Defensibility assessment: [copy from report if present]
  Indefensible positions: [copy from report if present]
```

#### §3.2 Constraint Relationships

From `affects_constraint/2` declarations in .pl files and network edges in reports:

```
RELATIONSHIP MAP:
  [constraint_A] → [constraint_B]: [relationship type, edge type]
  [constraint_B] → [constraint_C]: [relationship type, edge type]
```

#### §3.3 Aggregate Pattern

```
AGGREGATE:
  Total constraints analyzed: [N]
  Types computed (canonical analytical observer): [list]
  Signatures: [count by type]
  Drift: [count by severity]
  Mean purity: [value]
  Overall structural pattern: [stable / unstable / cascading]
```

---

### §4. BEAT SHEET (Narrative-Flow Organization)

**Purpose:** Organize the load-bearing findings (§2) into a sequence a writer can follow. Each beat specifies what the essay MUST address. Beats are in imperative register. They do not tell the writer how to feel about the finding.

**Sequencing Principle:** Order beats to build structural understanding. Start with the empirical ground (what the engine found), move to the structural diagnosis (what the findings mean given DR ontology), then to the perspectival gap (how different observers see it), then to implications for the document's recommendations.

#### §4.1 Beat Format

```
BEAT [N]: [Short label]
  CONSTRAINT: [id]
  FINDING: [one sentence — what the engine found, in engine language]
  EVIDENCE: [specific metric, mismatch, or signature from §1]
  
  THE ESSAY MUST: [imperative instruction — "State that..." / "Acknowledge that..." / 
    "Address the divergence between..."]
  THE ESSAY MUST NOT: [specific prohibition against the likely softening move —
    "Do not frame the FCR finding as 'minor diagnostic detail'" / 
    "Do not treat the Tangled Rope classification as equivalent to Rope"]
```

#### §4.2 Beat Quality Gate

Before finalizing each beat, verify:

- [ ] **Criterion anchor:** Does the beat trace to a specific §2 criterion?
- [ ] **Evidence anchor:** Does the beat cite specific §1 data?
- [ ] **Imperative language:** Does the MUST instruction use "state" / "acknowledge" / "address," not "consider" / "explore" / "might"?
- [ ] **Prohibition specificity:** Does the MUST NOT name the specific softening move?
- [ ] **Engine language preserved:** Does the beat use the engine's terms (false_ci_rope, tangled_rope, extraction_accumulation), not paraphrases?

#### §4.3 Architectural Beat

If §3 identifies cross-constraint patterns (shared beneficiary convergence, systemic drift, contamination propagation), generate an architectural beat:

```
ARCHITECTURAL BEAT: [Label]
  PATTERN: [what the cross-constraint analysis shows]
  CONSTRAINTS INVOLVED: [list]
  EVIDENCE: [convergent signatures, drift, contamination from §3]
  
  THE ESSAY MUST: [address the systemic pattern, not just individual constraints]
  THE ESSAY MUST NOT: [treat each constraint as independent when the engine 
    shows they are connected through shared beneficiaries or contamination]
```

#### §4.4 Narrative Flow

After all beats are generated, organize them into sections:

```
NARRATIVE FLOW:

SECTION 1: [Label — typically empirical ground]
  Beats: [N, N, N]
  Function: Establish what the engine found

SECTION 2: [Label — typically structural diagnosis]
  Beats: [N, N]
  Function: What the findings mean (DR classification, signatures, divergences)

SECTION 3: [Label — typically perspectival gap]
  Beats: [N, N]
  Function: How different observers see the constraints differently

SECTION 4: [Label — typically implications]
  Beats: [N, N, architectural beat]
  Function: What this means for the document's recommendations

OPEN: [Omega variables that survived extraction — genuine uncertainties, 
  not findings parked as questions]
```

---

### §5. OUTPUT FORMAT

```yaml
[UKE_META]
protocol: UKE_SUMMARY v1.0
inputs:
  constraint_stories: [list .pl files]
  engine_reports: [list _report.md files]
timestamp: [ISO 8601]
executor: [model name/version]
mode: extraction

═══ PER-CONSTRAINT EXTRACTIONS ═══

[§1 extraction for constraint 1]
---
[§1 extraction for constraint 2]
---
[§1 extraction for constraint N]

═══ LOAD-BEARING FINDINGS ═══

[§2 findings list with criterion anchors]

═══ CROSS-CONSTRAINT ANALYSIS ═══

[§3.1 shared beneficiary sets]
[§3.2 relationship map]
[§3.3 aggregate pattern]

═══ BEAT SHEET ═══

Total beats: [N]
  Divergence beats: [count]
  Signature beats: [count]
  Drift beats: [count]
  Contamination beats: [count]
  Perspectival beats: [count]
  Architectural beats: [count]

[§4.1 individual beats]
[§4.3 architectural beats]
[§4.4 narrative flow]

═══ OMEGA ═══

[Genuine unresolved uncertainties that survived extraction]

[LOG]
constraints_processed: [N]
load_bearing_findings: [N]
beats_generated: [N]
divergences_found: [N]
critical_drift_events: [N]
```

---

### §6. ANTI-PATTERNS

**F-SUMMARY-INTERPRETATION:** The model explains what a finding means instead of recording it. "The FCR finding suggests the constraint may have coordination-washing characteristics" instead of "Structural signature: false_ci_rope. Engine interpretation: coordination-washed." Fix: Copy from report. Do not rephrase.

**F-SUMMARY-IMPORTANCE-JUDGMENT:** The model decides which findings are important using editorial criteria ("this is a minor detail," "the key finding is"). Fix: Apply §2 criteria mechanically. If a finding meets any criterion, it is load-bearing. The model does not exercise discretion about importance.

**F-SUMMARY-SOFT-BEAT:** The beat instruction says "consider" or "explore" instead of "state" or "acknowledge." Fix: Imperative language only. The beat is an instruction, not a suggestion.

**F-SUMMARY-MISSING-PROHIBITION:** Beat has a MUST but no MUST NOT. Fix: Every beat needs an explicit prohibition against the likely softening move. If you can't identify the softening move, you haven't understood the finding. Common softening moves to prohibit:
- Framing a type divergence as "minor" or "diagnostic detail"
- Treating Tangled Rope as functionally equivalent to Rope
- Describing an FCR override as "the constraint shows mixed characteristics"
- Presenting extraction_accumulation drift as "evolving" rather than "degrading"
- Treating contamination as "context" rather than as a finding that changes the classification

**F-SUMMARY-ORPHAN-BEAT:** Beat makes a claim not anchored in §1 extraction. Fix: Every beat must trace to specific §1 evidence and a specific §2 criterion. If the evidence isn't in §1, the beat is fabricated.

**F-SUMMARY-OMEGA-INFLATION:** Parking clear findings as Omega variables to avoid including them as beats. Fix: If the engine computed it with high confidence, it is a finding, not an uncertainty. Omega is for engine outputs that are themselves ambiguous (low confidence, competing classifications, borderline metrics).

**F-SUMMARY-NETWORK-BLINDNESS:** Extracting per-constraint findings while ignoring §3 cross-constraint patterns. Fix: If the report includes cross-constraint convergence analysis, the architectural beat is mandatory. Systemic patterns are not optional findings.

---

### §7. USAGE

#### Activation

```
"Operate under UKE_SUMMARY v1.0.

Inputs:
- Constraint stories: [list .pl files]
- Engine reports: [list _report.md files]

Extract findings. Generate beat sheet."
```

#### Workflow

1. **§1 (Per-Constraint Extraction):** Copy from reports and .pl files. No interpretation. Complete all subsections for each constraint.
2. **§2 (Load-Bearing Findings):** Apply criteria mechanically to §1 extractions. List every finding that meets any criterion.
3. **§3 (Cross-Constraint Analysis):** Extract shared beneficiary sets, relationships, aggregate patterns from reports.
4. **§4 (Beat Sheet):** Generate beats from §2 findings. Run quality gate on each beat. Organize into narrative flow.
5. **§5 (Output):** Format and deliver.

#### Quality Checks

**Before proceeding from §1 to §2:**
- [ ] Every §1 entry uses the engine's exact language
- [ ] No findings have been summarized into softer formulations
- [ ] Chi decomposition tables included (models commonly omit these)
- [ ] All index mismatches listed (models commonly drop uncomfortable ones)
- [ ] Contamination network extracted (models commonly skip this)

**Before proceeding from §2 to §4:**
- [ ] Every finding traces to a specific §2 criterion
- [ ] No editorial judgment applied ("this seems less important")
- [ ] Cross-constraint patterns from §3 checked

**Before finalizing §4:**
- [ ] Every beat has MUST and MUST NOT
- [ ] Every beat cites specific §1 evidence and §2 criterion
- [ ] Architectural beats present if §3 found convergent patterns
- [ ] No beat uses hedge language ("consider," "explore," "might")
- [ ] Engine language preserved in all beats (no paraphrases)
- [ ] Narrative flow organizes beats into sections with clear functions

---

### §8. DESIGN RATIONALE

#### Why Extraction Before Prose

The model executing UKE_SUMMARY is optimizing for completeness — "did I get everything?" — not for narrative quality — "does this read well?" This is a deliberate task-framing choice. When a model writes an essay from engine reports, uncomfortable findings get smoothed because the helpfulness gradient rewards coherent narrative. When a model extracts findings mechanically, there is nothing to smooth. The extraction phase creates an evidence trail that downstream protocols must reference.

#### Why Structural Criteria for Load-Bearing

Editorial judgment about importance is where flinching enters. A model given discretion about which findings matter will downweight uncomfortable ones. The §2 criteria are designed to be mechanical: does a divergence exist? Does a signature fire? Is drift critical? These are binary questions with answers in the report. The model checks conditions, not vibes.

#### Why Double Pipeline Position

UKE_SUMMARY output serves two functions:
1. **Upstream:** Anchors the essay by providing a beat sheet the discussion and writing stages must address. The writer has less room to route around findings because the beats are already committed.
2. **Downstream:** Serves as the curated engine findings input for UKE_REALITY v2.0's ENGINE-BACKED mode. The summary carries engine language, divergence flags, and structural criteria — exactly what UKE_REALITY §1.1 needs.

Same document, two pipeline positions. No additional processing required.

#### Why MUST NOT in Every Beat

UKE_FLINCH identified that the likely softening move is predictable for each finding type. FCR findings get reframed as "mixed characteristics." Type divergences get downgraded to "diagnostic detail." Extraction drift gets described as "evolving." Naming the prohibited softening move in advance makes it visible if the writing model attempts it. The prohibition is the anchor.

#### Relationship to Other Protocols

- **UKE_SUMMARY** extracts findings and generates beat sheet
- **Human + LLM discussion** curates beats with authorial judgment (using .pl files + summary)
- **UKE_DISCUSSION** drafts structured SCQA from discussion
- **Essay instance** produces prose from discussion draft
- **UKE_REALITY v2.0** validates essay using summary as engine findings input
- **UKE_FLINCH** runs if essay-vs-engine divergence is detected

UKE_SUMMARY does not replace the editorial pipeline. It creates the evidence layer that the editorial pipeline operates on — the same role that mechanical phases play in UKE_FLINCH, but positioned earlier in the pipeline where it can anchor everything downstream.

---

### §9. KNOWN LIMITATIONS

**L1: Extraction is still LLM-executed.** The model may omit findings despite the mechanical framing. Quality checks in §7 are designed to catch omissions, but they are self-applied. A human reviewing the summary against the raw reports is the most reliable omission check.

**L2: Criteria are necessary but not sufficient.** The §2 criteria catch the most common categories of load-bearing findings. A finding that doesn't meet any criterion but is genuinely important will be missed. The criteria are a floor, not a ceiling — the human discussion stage can add beats the summary missed.

**L3: Beat sequencing involves judgment.** The narrative flow (§4.4) requires decisions about what comes first, what builds on what. This is a form of editorial judgment that the protocol can't fully eliminate. The mitigation: sequencing affects presentation order, not content. All load-bearing findings must appear as beats regardless of where they're sequenced.

**L4: MUST NOT prohibitions are predictive.** The protocol names likely softening moves based on observed patterns. A novel softening move not anticipated in the prohibition will pass through. UKE_FLINCH, running downstream with engine reports, is the backstop.

---

**Document Status:** Protocol specification v1.0
**Effective Date:** April 2026
**License:** CC0-1.0 (Public Domain)
**Dependencies:** DR engine reports (enhanced format), constraint stories (.pl format)

**Integration Status:**
- ✅ Receives: DR engine reports + constraint stories
- ✅ Feeds to: Human discussion stage, UKE_DISCUSSION, UKE_REALITY v2.0 (§1.1 engine findings slot)
- ✅ Inherits: UKE_FLINCH core invariant (engine outranks story)
- ✅ References: DR core_v4.3 ontology (7 categories)

*"Extract. Flag. Organize. Do not interpret. Do not soften. Do not judge. The engine computed; your job is to carry what it found."*
