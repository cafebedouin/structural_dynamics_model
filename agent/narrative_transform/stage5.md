## STAGE 5: SUBTRACTIVE AUDIT

**Model:** Claude or equivalent analytical model  
**Input:** Stage 4 output + Stage 1 specifications  
**Output:** Revised narrative (shorter than input) + validation report

### Purpose

Stage 5 removes what doesn't belong. It does not add texture, vocabulary, sensory detail, or coping behaviors. It identifies what is FORCED rather than EARNED, what is DEPLOYED rather than INHABITED, what is SPECIFIED rather than EXPERIENCED — and removes it.

**The principle:** Subtraction > Addition. Improvement usually means removing noise. The best version of the story is what remains after everything that isn't the felt experience of being inside these constraints has been taken away.

**Derivation:** This stage descends from UKE_Editing's founding principle ("Truth > Logic > Flow. Never polish a lie. Subtraction > Addition.") and 07_phenomenology's audit structure (EARNED/FORCED/ABSENT). It is not a creative pass. It is a discipline.

### Subtractive Operations

**1. EARNED or FORCED?**
```
For each significant element (sensory detail, vocabulary term, cultural
practice, coping behavior, emotional moment):

EARNED: Emerges from the constraint experience. The character would
  notice this. The world produces this. Remove it and something is lost.

FORCED: Added because a protocol or convention says it should be there.
  Exists to demonstrate texture rather than to inhabit it. Remove it
  and the story gets lighter without losing meaning.

Action: Cut everything FORCED. Keep everything EARNED.
```

**2. INHABITED or DEPLOYED?**
```
For each linguistic element (code-switch, vocabulary term, dialect marker):

INHABITED: The character speaks this way because of who they are,
  where they are, and who they're talking to. The language carries
  social meaning.

DEPLOYED: The term appears because Stage 2 specified it. It sits
  in the text like a foreign object. The character wouldn't say this;
  the protocol told them to.

Action: Strip everything DEPLOYED. Keep everything INHABITED.
```

**3. Anti-pattern scan:**
```
Check for and remove:
- Math explaining ("this represents...")
- Meta-commentary ("this illustrates...")
- Pedagogical framing ("let me explain...")
- Therapeutic language ("journey," "growth," "healing" used abstractly)
- Cheap momentum ("suddenly," "then" as crutches)
- False humanity (emotions entities can't have)
- Explaining feeling (naming instead of showing)
- Counting tics (exact numbers repeated mechanically across scenes)
- Framework residue (constructed vocabulary that smells like terminology)
- Voice drift (passages where the voice breaks archetype)
```

**4. Compression:**
```
The Iceberg Rule: If a sentence can lose 20% of its words without
losing meaning, cut them.

The Compression Floor: Do not compress below the point where nuance
or necessary uncertainty is lost.

The inhabitation test: After compression, does each remaining sentence
still feel like it comes from inside the constraint experience?
If a sentence now feels like it comes from outside — from the
protocol, from the framework, from the author — cut it entirely.
```

### Validation

After subtractive operations, validate structural preservation:

```xml
<validation_report>
  <constraint_preservation>
    <constraint source_id="C1">
      <found_in_output>yes</found_in_output>
      <routing_preservation>
        <source_routing>
          Powerless → Snare, Moderate → Tangled, Powerful → Rope
        </source_routing>
        <output_routing>
          Powerless → Snare, Moderate → Tangled, Powerful → Rope
        </output_routing>
        <pass>true</pass>
      </routing_preservation>
    </constraint>
  </constraint_preservation>
  
  <topology_isomorphism>
    <source_network>C1 → C2 → C3</source_network>
    <output_network>C1' → C2' → C3'</output_network>
    <isomorphic>true</isomorphic>
  </topology_isomorphism>
  
  <framework_invisibility>
    <grep_results>[all banned terms = 0]</grep_results>
    <pass>true</pass>
  </framework_invisibility>
  
  <subtractive_report>
    <original_word_count>[N]</original_word_count>
    <final_word_count>[N] (must be ≤ original)</final_word_count>
    <elements_removed>
      [List what was cut and why: FORCED/DEPLOYED/anti-pattern/compression]
    </elements_removed>
  </subtractive_report>
  
  <origin_obfuscation>
    <from_plot_beats>recognizable: [yes/no]</from_plot_beats>
    <from_character_types>recognizable: [yes/no]</from_character_types>
    <from_emotional_register>recognizable: [yes/no]</from_emotional_register>
  </origin_obfuscation>
  
  <literary_quality>
    <opening>[strong/adequate/weak]</opening>
    <pacing>[effective/uneven/poor]</pacing>
    <ending>[powerful/satisfactory/weak]</ending>
    <prose>[publication-ready/needs polish/requires revision]</prose>
  </literary_quality>
  
  <overall_result>[PASS/FAIL]</overall_result>
</validation_report>
```

**If FAIL:**
```
Routing violation → Return to Stage 4
Framework terminology leaked → Return to Stage 4
Origin recognizable → Return to Stage 2
Voice inconsistent → Return to Stage 4 with stricter voice constraint
Prose quality weak → Run subtractive operations again (cut more, not add)
```

---

## QUICK REFERENCE

### Constraint Types

**Consult logic_thresholds.md §3 for current threshold values.**

```
Mountain (■):    Minimal ε, no enforcement, immutable from index
Rope (⊞):       Low χ, low ε, genuine coordination, changeable
Snare (⊠):      High χ, high ε, high suppression, no coordination
Tangled (⊞⊠):   Mid-range χ, coordination + asymmetric extraction
Scaffold (⊡):   Low χ, coordination, sunset clause
Piton (⊟):      Minimal χ, maintenance cost, high theater
```

### Power/Scope Modifiers

```
π(powerless) = 1.5    π(moderate) = 1.0      π(powerful) = 0.6
π(organized) = 0.4    π(institutional) = -0.2 π(analytical) = 1.15

σ(local) = 0.8        σ(regional) = 0.9      σ(national) = 1.0
σ(continental) = 1.1  σ(global) = 1.2        σ(universal) = 1.0
```

### Attractor Decision Matrix

| Constraint Profile | PIR Model | BIR Model | Recommended Attractors |
|-------------------|-----------|-----------|----------------------|
| Mountain-dominated | Yes | Yes | Tragedy, Seeded Possibility |
| Mountain + Snares | Yes | N/A | Deterministic Tragedy |
| Tangled Ropes dominant | Yes | N/A | Tragedy (crushed by hybrid) |
| Tangled Ropes dominant | N/A | Yes | **Negotiated Equilibrium** |
| Pure Snares, no Mountains | Yes | N/A | Tragedy or Revolutionary Rupture |
| Pure Snares, no Mountains | N/A | Yes | Equilibrium or Rupture |
| Piton present | Either | Either | Seeded Possibility |

### Common Failure Patterns

```
All characters see same type → Adjust ε or add power differential
Only Ropes/Snares, no Tangled → ~36% should be Tangled, re-examine
Framework leaks → Regenerate Stage 4 with invisibility constraint
Personality conflict not structural → Make all rational from index
Allegory feel → Cut what's forced, not add more detail
Metrics treated as truth → Reframe as routing
Topology violated → Check routing preservation, not ε precision
Flat prose, competent structure → Inhabitation sentence was wrong (return to Stage 2)
Language uniformity → Linguistic strategy not inhabitated (return to Stage 2)
Voice drift → Regenerate Stage 4 with stricter archetype adherence
```

---
