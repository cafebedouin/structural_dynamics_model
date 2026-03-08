# Pipeline Reconfiguration: Logic Split

## Problem

`logic_narrative_v4.1.md` combined formal specification with narrative guidance, and was routed to every stage that received `dr_logic`. This caused:

1. **Stage 1 produced narrative output** — `<experience>`, `<dialogue_markers>`, `<narrative>` sections, source vocabulary in constraint descriptions — because the reference document told it to think narratively ("metrics as routing," "narrative use" annotations on every element).

2. **Anonymization failed structurally** — Stage 1 output contained 27+ occurrences of source domain vocabulary (fishing, boats, sharks, marlin, lucky boat) embedded in natural-language descriptions. The `_anonymize_stage_1()` method could only strip character names and titles, not domain vocabulary woven into prose.

3. **Stage 4 had framework vocabulary available** — through Stage 1's narrative annotations, characters could articulate extraction/coordination concepts. Theme-naming dialogue, framework residue, and interpretive endings traced back to Stage 1's pre-digested natural-language descriptions.

4. **Origin recognition** — source-specific occupations, relationships, and imagery survived the pipeline because Stage 1 encoded them in `<experience>` and `<observable_actions>` fields.

## Solution

Split `logic_narrative_v4.1.md` into two documents, each routed only where needed:

### New Documents

| Document | Purpose | Routed to |
|----------|---------|-----------|
| `logic_symbolic.md` | Pure formal specification: types, predicates, thresholds, error taxonomy, lifecycle, Boltzmann test, variable naming convention | Stage 0, Stage 1, Stage 5 |
| `logic_narrative_translation.md` | How constraint types feel, indexical variance as dramatic engine, structural physics as plot architecture, routing principle, translation checklist | Stage 2 only |

### Updated Stage Protocols

| Protocol | Key Changes |
|----------|-------------|
| `stage0.md` | References `logic_symbolic.md` instead of `logic_narrative.md`. `<experience>` field removed from output format — output contains classifications only. "Routing toward drama" instruction removed — ε reflects source structure. `<character_experiences>` renamed to `<character_classifications>`. Added `identity_locked` and `regional` to index options. |
| `stage1.md` | Variable naming (X₁, X₂, Xₙ). Prohibited fields list (`<experience>`, `<dialogue_markers>`, `<narrative>`). Abstract-only trigger descriptions. Source vocabulary banned from output. |
| `stage2.md` | References narrative translation guide. Explicitly describes receiving symbolic-only input. Adds constraint reference table (Cₙ → naturalized form). |
| `stage4.md` | Explicitly receives NO logic reference. Only recieves stage 2 and stage 3. Adds theme-naming dialogue prohibition. |
| `stage5.md` | References symbolic logic. Includes 7-dimension scoring rubric (30-35 PUBLISH, 24-29 REVISE, 17-23 REWORK, <17 RESTART). Automatic overrides. Seven subtractive operations (adds voice audit, ending audit, origin obfuscation check). |

## Document Routing

### Before (all stages received `dr_logic` = logic_narrative)

```python
STAGE_INPUTS = {
    "narrative": {
        "stage_0": ["source", "dr_logic"],
        "stage_1": ["stage_0", "dr_logic"],
        "stage_2": ["stage_1_anon", "dr_logic"],
        "stage_3": ["stage_1_anon", "stage_2", "dr_logic"],
        "stage_4": ["stage_2", "stage_3", "constraint_reports"],
        "stage_5": ["stage_4", "stage_1_anon"],
    },
}
```

### After (two logic documents, each routed specifically)

```python
STAGE_INPUTS = {
    "narrative": {
        "stage_0": ["source", "dr_logic_symbolic"],                               # Symbolic logic for classification during extraction
        "stage_1": ["stage_0", "dr_logic_symbolic"],                              # Symbolic logic only
        "stage_2": ["stage_1_anon", "dr_logic_narrative"],                        # Narrative translation guide only
        "stage_3": ["stage_1_anon", "stage_2"],                                   # No logic ref — works from upstream outputs
        "stage_4": ["stage_1_anon", "stage_2", "stage_3", "constraint_reports"],  # NO logic ref — air gap preserved
        "stage_5": ["stage_4", "stage_1_anon", "dr_logic_symbolic"],              # Symbolic logic for verification
    },
}
```

### Key routing changes:

- **Stage 0:** `dr_logic` → `dr_logic_symbolic`. Gemini needs thresholds and formulas for classification during extraction, but not narrative annotations. `<experience>` field removed from output format. "Routing toward drama" instruction removed — ε reflects source structure, not dramatic optimization.
- **Stage 1:** `dr_logic` → `dr_logic_symbolic`. Receives only formal specification.
- **Stage 2:** `dr_logic` → `dr_logic_narrative`. Receives only narrative translation guide.
- **Stage 3:** Removed `dr_logic`. Voice archetypes and attractor matrix are embedded in Stage 1 output and Stage 3's own protocol.
- **Stage 4:** Unchanged — already didn't receive logic reference.
- **Stage 5:** Added `dr_logic_symbolic`. Needs formal specification to verify constraint preservation.

## Anonymization Simplification

### Before

`_anonymize_stage_1()` needed to:
1. Replace character names from Stage 0 XML `<character name="X">` regex
2. Strip source title from headers
3. Strip hardcoded author names
4. **[FAILING]** Catch domain vocabulary (fishing, boats, sharks, marlin, etc.) — 27+ occurrences missed

### After

Stage 1 output contains NO domain vocabulary by construction. Anonymization reduces to:
1. Strip the variable mapping table from header (X₁ ← Santiago, etc.)
2. Confirm no source title in headers (already working)
3. Confirm no author names (already working)

Domain vocabulary problem **disappears** because Stage 1 never produces it. The output is `C₂: ε=0.70, χ(X₁)=0.84 → Snare` — not "fishing cooperative" or "sharks destroy fish."

## Orchestrator Implementation Changes

### 1. Logic path configuration

```python
# Replace single path:
# LOGIC_NARRATIVE_PATH = NARRATIVE_TRANSFORM_DIR / "logic_narrative_v4.1.md"

# With two paths:
LOGIC_SYMBOLIC_PATH = NARRATIVE_TRANSFORM_DIR / "logic_symbolic.md"
LOGIC_NARRATIVE_PATH = NARRATIVE_TRANSFORM_DIR / "logic_narrative_translation.md"
```

### 2. Pipeline class changes

```python
# In __init__:
# Replace single dr_logic load with two:
self.dr_logic_symbolic = ""
self.dr_logic_narrative = ""

if Path(LOGIC_SYMBOLIC_PATH).exists():
    self.dr_logic_symbolic = _load_context_file(str(LOGIC_SYMBOLIC_PATH))
if Path(LOGIC_NARRATIVE_PATH).exists():
    self.dr_logic_narrative = _load_context_file(str(LOGIC_NARRATIVE_PATH))
```

### 3. Stage runner prompt assembly

The `_run_stage()` method assembles prompts from `STAGE_INPUTS` keys. Need to handle the two new keys:

```python
# In prompt assembly, map input keys to loaded content:
# "dr_logic_symbolic"  → self.dr_logic_symbolic
# "dr_logic_narrative" → self.dr_logic_narrative
```

### 4. Anonymization method

```python
def _anonymize_stage_1(self, stage_1_text: str) -> str:
    """Anonymize Stage 1 output.
    
    With symbolic Stage 1 format, anonymization is minimal:
    1. Strip variable mapping table (X₁ ← source_name lines)
    2. Strip any residual source title from headers
    3. Confirm no author names
    
    Domain vocabulary stripping is no longer needed because
    Stage 1 output contains only symbolic objects.
    """
    lines = stage_1_text.splitlines()
    anonymized = []
    in_mapping_table = False
    
    for line in lines:
        # Strip variable mapping table
        if re.match(r'^X[₁₂₃₄₅₆₇₈₉₀ₙ]\s*←', line):
            in_mapping_table = True
            continue
        if in_mapping_table and not line.strip():
            in_mapping_table = False
            continue
        if in_mapping_table:
            continue
            
        # Strip source title references (existing logic)
        # Strip author names (existing logic)
        
        anonymized.append(line)
    
    return '\n'.join(anonymized)
```

## File Inventory

### New files (in outputs):
- `logic_symbolic.md` — Symbolic logic reference (Stage 0, Stage 1, Stage 5)
- `logic_narrative_translation.md` — Narrative translation guide (Stage 2)
- `stage0.md` — Updated Stage 0 protocol
- `stage1.md` — Updated Stage 1 protocol
- `stage2.md` — Updated Stage 2 protocol
- `stage4.md` — Updated Stage 4 protocol
- `stage5.md` — Updated Stage 5 protocol

### Unchanged:
- `stage3.md` — No protocol changes needed (receives upstream outputs, not logic reference)
- `logic.md` — Full specification, unchanged (source of truth)

### Superseded:
- `logic_narrative_v4_1.md` — Replaced by logic_symbolic.md + logic_narrative_translation.md

## Verification

To confirm the reconfiguration works, run Stage 0 → Stage 1 on the Old Man and the Sea with the new protocols and symbolic reference.

**Stage 0 output should contain:**
- Source character names (Santiago, Manolin) — these are natural at this stage
- χ calculations with type classifications
- NO `<experience>` fields — classifications only
- ε values reflecting the source's actual structure (not adjusted for drama)

**Stage 1 output should contain:**
- Variable names (X₁, X₂) — no "Santiago," "Manolin," "Agent_A"
- χ calculations with threshold checks — no "fishing," "boats," "sharks"
- Abstract transformation triggers — no "form fishing cooperative"
- Structural error observables — no "continues fishing alone"
- Zero natural-language experience descriptions
