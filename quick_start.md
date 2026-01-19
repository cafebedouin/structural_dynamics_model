# Quick Start

## Prerequisites

- **SWI-Prolog** (for Layer 2 validation)
- **Gemini API access** (for Layer 1 generation, optional)
- **Claude API access** (for Layer 3 synthesis, optional)

## Running Layer 2 Audit Only

Test the validation system on existing models:

```bash
cd prolog
swipl

?- [validation_suite].
?- run_scenario('./testsets/columbia_2026_elections.pl', columbia_2026).
```

**Output:**
```
[CONSTRAINT INVENTORY: REALITY AUDIT]
  Constraint | Claimed | Actual | Action
  ----------------------------------------------------------------------
  c1_term_limit | mountain | mountain | navigate
  c2_runoff | rope | noose | replace
  c3_coalition | rope | tangled_rope | reform

[STRUCTURAL SIGNATURE ANALYSIS]
  c1_term_limit: natural_law (confidence: high)
    → No alternatives ever existed, universal inaccessibility
  
  c2_runoff: constructed_constraint (confidence: high)
    → Active enforcement detected (suppression=0.55, resistance=0.65)
    → Alternatives existed (single-round, ranked-choice)

[PERSPECTIVAL GAP ANALYSIS]
  c2_runoff:
    - Individual (powerless): mountain
    - Institutional (manager): rope
    → GAP: Same structure experienced as unchangeable by powerless,
            as coordination tool by powerful
```

## Generating New Models (Layer 1)

**Manual Process:**

1. Copy `prompts/constraint_story_generation_prompt.md`
2. Provide domain description to Gemini:
   ```
   Analyze the constraint topology of [domain description].
   Generate a complete Prolog constraint story file.
   ```
3. Save output as `testsets/[domain_name].pl`
4. Run Layer 2 validation (see above)

**What Layer 1 generates:**

- Base measurements (extractiveness 0.0-1.0, suppression 0.0-1.0)
- 3+ indexed perspectives (different WHO/WHEN/WHERE/HOW MUCH)
- Test suite proving indexical variance
- Beneficiary/victim asymmetry markers
- Model commentary explaining reasoning

**Validation:** Layer 2 auto-repairs syntax errors, checks schema compliance

## Synthesizing Analysis (Layer 3)

**Manual Process:**

1. Run Layer 2 audit, capture executive summary
2. Perform web search for empirical evidence (Perplexity)
3. Provide Claude with UKE_W protocol:
   ```
   Execute UKE_W v1.0 protocol.
   
   Substrate: [Layer 2 audit summary]
   Evidence: [Web search results]
   Voice: [System Architect | Critical Essayist]
   Temperature: [hot | warm | cool]
   
   Generate constraint story essay.
   ```
4. Claude outputs essay with metadata blocks

**What Layer 3 produces:**

- Evidence-backed narrative (all claims trace to substrate)
- Collapsed Omegas (uncertainties → defensible assertions)
- Falsifiable predictions (explicit success/failure criteria)
- Stakes anchor (why this matters beyond being true)
- Quality gate validation (substrate fidelity, counterfactuals, etc.)


