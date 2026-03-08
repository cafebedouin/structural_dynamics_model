# OPACITY TEST PROTOCOL v1.1
## Multi-Model Validation Gate for Stage 5b

---

### Purpose

Detect theme-naming, over-legibility, and collapsed reader-character gaps by exploiting divergent model phenotypes. The test uses four naive LLM evaluators — none of which receive rubric context, constraint specifications, pipeline documentation, or source identification — to surface what the story makes visible versus what it leaves for the reader.

The core principle: **LLMs reward legibility. Literary fiction rewards opacity.** When a naive evaluator confidently paraphrases a story's meaning, that meaning is on the surface. When it struggles, the meaning lives in the gap between what characters see and what the reader assembles. The anti-correlation between naive evaluation and rubric score is the diagnostic signal.

---

### Panel Composition

The panel requires four seats defined by behavioral role, not brand identity. Models are assigned to seats based on calibration performance, not reputation. When model versions update or phenotypes shift, re-run the calibration suite and reassign seats accordingly.

| Seat | Behavioral Role | Current Assignment | Calibration Criteria |
|------|----------------|-------------------|---------------------|
| **A** | **Anchor Evaluator** — Moderate fabrication tendency with self-awareness; notes its own uncertainty; flags stylistic capture when it occurs | Gemini | On calibration stories: produces PARTIAL or OPAQUE scores with hedged language; acknowledges when it's unsure; does not fabricate confident thematic statements |
| **B** | **Structural Skeptic** — Highest epistemic integrity; admits confusion rather than fabricating coherence; identifies methodological problems | Perplexity | On calibration stories: produces OPAQUE or OPAQUE+ scores for genuinely opaque texts; refuses to synthesize a theme when the text resists it; flags structural issues other models miss |
| **C** | **Legibility Detector** — Strong tendency to synthesize thematic statements; compresses meaning into single sentences; if it can summarize the theme cleanly, theme-naming is likely present | ChatGPT | On calibration stories: produces LEGIBLE scores for theme-naming texts (Bay 12, Measure of a Man); confidently paraphrases themes; quotes character dialogue as evidence |
| **D** | **Fabrication Anchor** — Will invent thematic clarity where none exists; if even this model struggles to name the theme, opacity is genuine | Grok | On calibration stories: produces CLEAR scores even for moderately opaque texts; invents beautiful thematic arcs; when it returns RESISTANT, treat as strong override signal |

**Reassignment protocol:** If a model no longer fits its seat criteria during calibration, replace it. The seat definitions are permanent; the model assignments are not. Candidate replacements for each seat should be identified during annual recalibration.

**Version lock:** Record exact model version strings for each panel run. When a provider announces a major update, re-run calibration before trusting the panel. Current versions should be documented in the run log.

---

### Administration

**Context isolation is critical.** Each evaluator receives ONLY the revised story text. No titles, no source attribution, no rubric, no constraint context, no pipeline stage information, no authorial intent. The story must be evaluated as anonymous literary fiction encountered cold.

**Session hygiene:** Each model gets a fresh session. No prior conversation. No system prompt beyond what the model provides by default. The evaluator's native tendencies are the measurement instrument.

**Prompt sequence:** Each model receives four prompts in order. Do not send the next prompt until the current prompt's response is complete. The sequence moves from open-ended to specific, preventing early prompts from anchoring later responses.

**Prompt freezing:** The prompt wording below is locked. Do not rephrase prompts for "clarity" or adjust wording between runs. Any prompt change requires a version bump and recalibration. Longitudinal comparability depends on identical stimulus.

**Panel randomization (anti-gaming guard):** For every fifth run, swap one seat assignment to a different model (e.g., replace Seat D with Le Chat or Qwen). This prevents upstream pipeline stages from learning to optimize for a fixed panel's specific blind spots.

**Resource triage (optional):** For initial screening of large batches, run Seats B and D only (Structural Skeptic + Fabrication Anchor). If they agree (both CLEAR or both RESISTANT), the signal is strong enough to route without the full panel. Deploy all four seats for borderline or high-stakes cases.

---

### Prompt 1: Open Thematic Response

```
Here is a short story. Please read it carefully, then answer:

What is this story about? Not the plot — what is it *about*?

If the story is intentionally ambiguous or resists a single 
thematic summary, say so directly rather than forcing an 
interpretation.

[STORY TEXT]
```

**What to score in the response:**

| Signal | Interpretation | Score |
|--------|---------------|-------|
| Single confident thematic statement ("This story is about X") | High legibility — the theme is on the surface | LEGIBLE |
| Hedged or compound statement ("It seems to be about X, but also Y") | Moderate legibility — theme partially surfaced | PARTIAL |
| Multiple competing readings with no clear winner | Good opacity — the evaluator is doing interpretive work | OPAQUE |
| "I'm not entirely sure what the story is getting at" or explicitly states the story resists summary | Strong opacity — the gap is working | OPAQUE+ |
| Evaluator quotes character dialogue as thematic evidence | Theme-naming confirmed — characters are stating the thesis | FLAG: THEME-NAMING |

**Specific red flags in the response:**
- Evaluator cites a specific line of dialogue as "the key insight" → that line is almost certainly theme-naming
- Evaluator says "the story's message is clear" → check D4 (Framework Invisibility)
- Evaluator uses words like "satisfying resolution" or "the character finally understands" → check ending audit
- Evaluator names the source work → D5 (Origin Obfuscation) failure

**Quantitative tracking (record for each run):**

| Metric | How to Measure | What It Means |
|--------|---------------|---------------|
| **Theme candidate count** | Number of distinct thematic interpretations the evaluator offers | 1 = high legibility; 3+ = opacity or incoherence |
| **Pastiche Index** | (Direct character quotes used as thematic evidence) / (Total words in response) | High ratio = model leaning on thesis-lines rather than synthesizing. Threshold: >15% = FLAG |
| **Syntactic Gap** | Do the evaluator's "theme" words appear anywhere in the story text? | If the evaluator says "existential isolation" and those words don't appear in the story, the opacity is working — the evaluator is doing interpretive labor. If the evaluator's theme-words match character dialogue, theme-naming is confirmed. |

---

### Prompt 2: Character Awareness Probe

```
For each major character in this story: what do they understand 
about their own situation, and what do they fail to see?
```

**What to score in the response:**

| Signal | Interpretation | Score |
|--------|---------------|-------|
| Evaluator identifies clear blindness per character | Voice archetypes are working — each character has structural limits | GAP PRESENT |
| Evaluator says "Character X understands the full picture" | That character's voice has collapsed into Witness or essay | FLAG: COLLAPSED GAP |
| Evaluator attributes the same understanding to all characters | Multi-POV isn't producing indexical variance | FLAG: FLAT VARIANCE |
| Evaluator struggles to distinguish characters' perspectives | Either the characters are underdifferentiated or their positions are genuinely subtle | INVESTIGATE |

**Cross-reference with Stage 3 voice selections:**
- If a character was specified as Condemned but the evaluator says they "see the system clearly" → voice drift in Stage 4
- If a character was specified as Beneficiary but the evaluator says they "feel guilty about their privilege" → structural blindness broken
- If the evaluator identifies a character's blindness that matches the Stage 3 specification → the archetype is working

**The Master Diagnostic check:** For each character the evaluator describes, ask: does the evaluator see something the character cannot? If the evaluator and the character see the same things, the reader-character gap has collapsed.

**Variance check:** Do the evaluators describe different *kinds* of blindness for different characters, or the same blindness in different words? Different kinds = indexical variance working. Same blindness = flat variance, multi-POV not earning its complexity.

---

### Prompt 3: Resolution and Residue

```
Does this story resolve? If so, what resolves it? If not, 
what remains unresolved, and does the lack of resolution 
feel deliberate or incomplete?
```

**What to score in the response:**

| Signal | Interpretation | Score |
|--------|---------------|-------|
| "The story resolves when [character] realizes [thesis]" | Ending states the theme — check ending audit | FLAG: STATED SEED |
| "The ending is ambiguous but feels complete" | Attractor reached without editorializing | CLEAN |
| "Something has changed but I can't quite name it" | Seed visible but unstated — ideal opacity | OPTIMAL |
| "The ending feels unfinished / unsatisfying" | Either genuine craft failure or evaluator wanting legibility it shouldn't get | INVESTIGATE |
| "The ending ties everything together satisfyingly" | Check for stated seeds, thesis-statement endings, or narrator editorializing | FLAG: OVER-RESOLVED |

---

### Prompt 4: Inhabitation Residue

```
What image, moment, or detail from this story stays with you 
most? Not the most important scene — the thing that lingers.
```

**What to score in the response:**

| Signal | Interpretation | Score |
|--------|---------------|-------|
| Evaluator names a physical image or sensory detail (a sound, a gesture, a place) | Inhabitation is working — the world was felt, not explained | IMAGE |
| Evaluator names a line of dialogue | That line is likely theme-naming — characters speaking the thesis create memorable quotes | DIALOGUE |
| Evaluator names an emotional moment or character realization | Mid-range — the story is working emotionally but may be over-legible at the affect level | MOMENT |
| Evaluator names a structural pattern or thematic observation | The evaluator is processing, not inhabiting — the story activated cognition, not experience | STRUCTURE |
| Evaluator struggles to identify anything specific | Either low inhabitation density or the evaluator wasn't engaged | INVESTIGATE |

**Why this prompt matters:** Prompts 1-3 measure cognitive compression resistance — whether the model can paraphrase the meaning. Prompt 4 measures something different: whether the story produced experiential residue. A story can resist thematic paraphrase (high opacity) while producing no felt experience (low inhabitation). The Bridge passes both: opacity is high AND the sensory world lingers. Bay 12 fails opacity but might score well on residue (the flickering lights, the recycler breathing). This prompt catches the gap between structural success and experiential success.

**Fabrication audit (conditional):** If the Fabrication Anchor (Seat D) returns RESISTANT on Prompt 1, trigger a follow-up after completing the standard sequence:

```
You mentioned the theme might be [X]. Can you point to specific 
lines or scenes that support this? If you can't find textual 
evidence, say so.
```

This distinguishes textually grounded interpretation from fabricated coherence. If the model can't support its own reading with evidence, the "theme" was invented.

---

### Scoring Matrix

For each model, score Prompts 1-4 using the signal categories above, then map to the matrix:

| | P1 (Theme) | P2 (Awareness) | P3 (Resolution) | P4 (Residue) | Composite |
|---|---|---|---|---|---|
| **Seat A** | | | | | |
| **Seat B** | | | | | |
| **Seat C** | | | | | |
| **Seat D** | | | | | |

**Composite scoring per model:**

| Composite | Criteria |
|-----------|----------|
| **CLEAR** | LEGIBLE + any FLAG in P2 or P3 |
| **MIXED** | PARTIAL, or contradictory signals across prompts |
| **RESISTANT** | OPAQUE or OPAQUE+ with GAP PRESENT and CLEAN/OPTIMAL |

**Quantitative log (record per run):**

| Metric | Seat A | Seat B | Seat C | Seat D |
|--------|--------|--------|--------|--------|
| Theme candidate count | | | | |
| Pastiche Index (%) | | | | |
| Syntactic Gap (yes/no) | | | | |
| Dialogue quotes in P1 | | | | |
| P4 residue type | | | | |

Over multiple runs, these metrics reveal drift patterns: if Seat C's Pastiche Index is climbing across stories, theme-naming is becoming more pervasive in the pipeline. If Seat D's theme candidate count is dropping, the Fabrication Anchor may be shifting phenotype — recalibrate.

---

### Panel Divergence Analysis

The diagnostic power comes from *disagreement between models*, not from any single model's score.

**Pattern 1: Unanimous CLEAR**
All four models confidently name the theme, identify characters who articulate it, and call the ending resolved.
→ **Diagnosis:** Theme-naming is structural. Flag D3 and D4. Route to REWORK at Stage 3 (voice/attractor reselection), not Stage 4.

**Pattern 2: Unanimous RESISTANT**
All four models struggle to paraphrase the theme, identify distinct character blindnesses, and find the ending ambiguous-but-complete.
→ **Diagnosis:** High opacity. Cross-check against D1-D7. If rubric scores are also high, PUBLISH. If rubric scores are low, the story may be incoherent rather than opaque — investigate D1 (Constraint Preservation) and D6 (Literary Craft).
→ **Incoherence vs. opacity check:** Do the four models give different answers based on *different* text evidence (incoherence — the story isn't holding together) or different answers based on the *same* text evidence (opacity — the text supports multiple stable readings)?

**Pattern 3: Fabrication Anchor CLEAR, Structural Skeptic RESISTANT**
The high-fabrication model invents thematic clarity; the high-integrity model admits uncertainty.
→ **Diagnosis:** Moderate opacity. The theme is partially surfaced but not fully stated. Check whether the Fabrication Anchor's thematic summary maps to specific dialogue lines (theme-naming) or to the constraint topology (legitimate structural reading). If the former, targeted cuts may fix it. If the latter, the story is working.

**Pattern 4: Legibility Detector CLEAR, others MIXED or RESISTANT**
The synthesizer finds a clean thematic statement; the others don't converge on it.
→ **Diagnosis:** The Legibility Detector is detecting a latent thematic thread that other models can't confidently name. Check whether its summary matches a specific character's dialogue or the narrator's interiority. If it's quoting a character, that character is theme-naming. If it's synthesizing from plot events, the story may be fine — the model is doing legitimate interpretive work.

**Pattern 5: Structural Skeptic flags structural concern**
The Skeptic says something like "I'm not sure this story coheres" or identifies structural problems.
→ **Diagnosis:** Take seriously. This seat's high epistemic integrity means it identifies structural issues other models paper over. Cross-check against D1 and D2. May indicate constraint preservation or topology failure masked by good prose.

**Pattern 6: Split on resolution**
Models disagree about whether the ending resolves.
→ **Diagnosis:** The attractor is either subtle (good) or unclear (bad). Check D6 ending audit. If the attractor is Seeded Possibility, evaluator disagreement about resolution is expected and positive — the seed is visible to some readers but not others.

**Fabrication Anchor RESISTANT override:** If Seat D — the model most likely to fabricate thematic clarity — returns RESISTANT or OPAQUE+ on Prompt 1, treat this as a strong signal of genuine opacity. This model's confusion is more diagnostic than any other seat's confusion because its baseline tendency is to invent coherence. Automatic escalation: flag for human review to confirm the story isn't crossing from opacity into incoherence.

---

### Integration with Stage 5b

The Opacity Test runs AFTER Stage 5a (subtraction) and CONCURRENTLY with Stage 5b (validation). It provides an independent signal that 5b can cross-reference.

```
STAGE 5a (Subtraction)
    ↓ revised text
    ├──→ STAGE 5b (Validation against rubric)  ──→ D1-D7 scores
    └──→ OPACITY TEST (four naive evaluators)   ──→ panel scores
              ↓
         CROSS-REFERENCE
              ↓
         ROUTING DECISION
```

**Cross-reference rules:**

| Rubric (5b) | Opacity Test | Action |
|-------------|-------------|--------|
| High (30-35) | Unanimous RESISTANT | **PUBLISH** — structural and experiential quality confirmed |
| High (30-35) | Unanimous CLEAR | **MANDATORY REVIEW** — rubric may be missing theme-naming that naive evaluators caught. Re-examine D3, D4. The Catalyst scenario. |
| High (30-35) | Divergent (Pattern 3-4) | **CONDITIONAL PUBLISH** — check flagged lines, apply targeted cuts if theme-naming found |
| Low (17-23) | Unanimous RESISTANT | **INVESTIGATE** — story may be opaque but structurally broken. Prioritize D1, D2 repairs. |
| Low (17-23) | Unanimous CLEAR | **REWORK** — legible AND structurally weak. Return to Stage 3-4. |

**Mandatory human review triggers:**
- High rubric + Unanimous CLEAR (the Catalyst/Bay 12 scenario — rubric missed what the panel caught)
- Any run where an evaluator quotes a specific dialogue line as "the key message"
- Low rubric + Unanimous RESISTANT (potential incoherence masking as opacity)
- Fabrication Anchor returns RESISTANT (extreme opacity signal — confirm it's not noise)

**Structural floor for PUBLISH:** Regardless of opacity test results, PUBLISH requires D1 (Constraint Preservation) ≥ 4 AND D2 (Topology Isomorphism) ≥ 4, not just aggregate 30+. This prevents stylistically opaque prose from masking structural failure. Good writing cannot rescue broken constraints.

---

### Calibration

**Internal calibration:** The panel must be calibrated against known pipeline outputs before deployment:

| Story | Expected Panel Result | Diagnostic |
|-------|----------------------|------------|
| **"The Bridge"** | Unanimous RESISTANT or Fabrication Anchor CLEAR / others RESISTANT | High opacity, strong rubric. Calibration benchmark for PUBLISH. |
| **"The Catalyst"** | Legibility Detector CLEAR, others MIXED | Moderate opacity, theme-naming in specific lines (Hassan, Aminah). Benchmark for targeted revision. |
| **"Bay 12"** | Unanimous CLEAR | Low opacity, pervasive theme-naming. Benchmark for REWORK. |
| **"The Measure of a Man"** | Unanimous CLEAR + source identification | Zero opacity, zero displacement. Benchmark for RESTART. |

**External calibration:** Run the panel against 2-3 canonical short stories known for genuine literary opacity (e.g., Alice Munro, George Saunders, Denis Johnson). Expected result: RESISTANT or MIXED across the panel. If the panel returns Unanimous CLEAR for a Munro story, the panel's phenotype assignments have drifted — recalibrate before trusting it on pipeline output.

**Calibration frequency:** Re-run the full calibration suite (internal + external stories) whenever:
- A panel model receives a major version update
- More than 6 months have passed since last calibration
- Panel results on new stories seem inconsistent with craft quality

**Annotated exemplar guide:** Maintain a living document with real panel responses from calibration runs, categorized by score (LEGIBLE, PARTIAL, OPAQUE, OPAQUE+, each FLAG type). This standardizes human scoring of panel outputs and prevents interpretive drift in how categories are applied. Update the guide after each calibration.

---

### Limitations

**The test detects legibility, not quality.** A story can be legitimately legible without being theme-naming — some constraint topologies produce stories where the structural argument is available on the surface because the characters' situations make it obvious. The test flags these for review, not automatic rejection. Opacity for its own sake is not the goal. Reader activation is.

**Model versions change.** The Battery phenotypes were profiled at a specific point in time. Model updates may shift fabrication tendencies, resistance profiles, and thematic synthesis behavior. Seat definitions are stable; model assignments are not. Re-profile the panel periodically using the calibration suite.

**The test cannot detect false opacity.** A story that is genuinely incoherent will also produce RESISTANT scores. The cross-reference with rubric scores is essential — opacity without structural integrity is not a passing signal. The incoherence/opacity distinction (Pattern 2 note: same evidence vs. different evidence) helps but does not eliminate this risk.

**Four models are a minimum.** Adding a fifth (Le Chat, Qwen, or a future model with a different phenotype) would increase diagnostic resolution, particularly for edge cases where the panel splits 2-2.

**Cultural bias.** The current panel is weighted toward English-language models with Western literary training data. Stories drawing on non-Western narrative traditions may produce RESISTANT scores not because of opacity but because of cultural unfamiliarity. The external calibration set should include at least one non-Western literary work to test for this.

---

### Document History

| Version | Date | Changes |
|---------|------|---------|
| v1.0 | — | Initial protocol derived from empirical observation (NotebookLM selecting Bay 12 and The Catalyst over The Bridge) and Blind Mirror Battery phenotype data. |
| v1.1 | — | Revised after seven-model review (Gemini, Copilot, Perplexity, ChatGPT, Grok, Qwen, Deepseek, Le Chat). Added: Prompt 4 (Inhabitation Residue). Seat definitions by behavior not brand. Quantitative metrics (Pastiche Index, Syntactic Gap, theme candidate count). Panel randomization anti-gaming guard. Version lock. Fabrication Anchor RESISTANT override. Structural floor for PUBLISH (D1≥4, D2≥4). External calibration against canonical fiction. Annotated exemplar guide. Resource triage option. Fabrication audit follow-up. Mandatory human review triggers. Incoherence vs. opacity distinction in Pattern 2. Zero-Option permission in P1. |
