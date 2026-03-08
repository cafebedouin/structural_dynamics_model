## STAGE 1: FORMALIZATION

**Model:** Claude or equivalent specification model
**Input:** Stage 0 constraint map + Symbolic Logic Reference (logic_symbolic.md)
**Output:** Testable formal constraint network — symbolic objects only

### Purpose

Stage 1 translates a natural-language constraint map into a formal constraint network. It produces ONLY symbolic objects: indices, χ calculations, classifications, transformation rules, error observables, and attractor selection.

Stage 1 does NOT describe how constraints feel, what characters experience, how the source story works, or what narrative function constraints serve. Those are Stage 2's job. Stage 1 is a formalization step — it converts observations into testable formal specifications.

### Variable Naming Convention

All characters and entities receive variable names:

```
Characters:    X₁, X₂, X₃, ... Xₙ
Groups:        G₁, G₂, ... Gₙ
Institutions:  I₁, I₂, ... Iₙ  (where context distinguishes from Index)
Constraints:   C₁, C₂, ... Cₙ
```

No source names, no descriptive labels, no aliases. Stage 0 says "Santiago" or "the old man" — Stage 1 says X₁. Stage 0 says "the boy" — Stage 1 says X₂. The mapping is recorded once in a header table and never repeated.

### Instructions

1. **Record variable mapping** (header only — not repeated in body):
   ```
   X₁ ← [Stage 0 character reference]
   X₂ ← [Stage 0 character reference]
   C₁ ← [Stage 0 constraint reference]
   ```
   This mapping enables traceability. It appears ONCE at the top and is stripped during anonymization.

2. **Formalize each constraint** with indexed classifications showing χ calculations:
   ```xml
   <constraint id="C₁">
     <properties>
       <epsilon>[value]</epsilon>
       <suppression>[value]</suppression>
       <coordination>[true/false]</coordination>
       <asymmetry>[true/false]</asymmetry>
     </properties>

     <agent ref="X₁">
       <index>
         <power>[position]</power>       <!-- π = [value] -->
         <time>[horizon]</time>
         <exit>[option]</exit>
         <scope>[level]</scope>           <!-- σ = [value] -->
       </index>
       <chi>ε × π × σ = [calculation] = [result]</chi>
       <type>[classification]</type>
       <threshold_check>[which threshold, pass/fail]</threshold_check>
       <rationale>[structural terms only — no source vocabulary]</rationale>
     </agent>

     <agent ref="X₂">
       <!-- same structure -->
     </agent>

     <indexical_variance>
       C₁: X₁ χ=[value] → [type], X₂ χ=[value] → [type]
       [Divergence description in structural terms]
     </indexical_variance>

     <boltzmann_test>[PASS/FAIL with reasoning]</boltzmann_test>
   </constraint>
   ```

3. **Formalize transformation rules** (IF-THEN format with mechanical χ recalculation):
   ```xml
   <transformation_rule id="TR₁">
     <trigger>
       <condition>[abstract action description]</condition>
       <target>C₁</target>
       <agent>X₁</agent>
     </trigger>
     <index_change>
       <from>P = [position] (π=[value])</from>
       <to>P = [position] (π=[value])</to>
     </index_change>
     <chi_recalculation>
       <before>χ = ε × π × σ = [value] → [type]</before>
       <after>χ = ε × π × σ = [value] → [type]</after>
     </chi_recalculation>
     <type_change>[Type_before] → [Type_after]</type_change>
     <preconditions>[required states]</preconditions>
     <blocked_by>[constraints that prevent]</blocked_by>
   </transformation_rule>
   ```

   **Trigger descriptions must be abstract:** "organize_collective," "establish_alternative_system," "exit_constraint" — NOT source-specific actions like "form fishing cooperative" or "refuse to go to sea."

4. **Formalize error manifestations:**
   ```xml
   <error id="E₁">
     <type>[I–VI from taxonomy]</type>
     <agent>X₁</agent>
     <constraint>C₁</constraint>
     <actual_type>[classification from agent's index]</actual_type>
     <perceived_type>[misclassification]</perceived_type>
     <observable>[testable condition — NOT source-specific action]</observable>
     <correction_trigger>[what would change classification]</correction_trigger>
   </error>
   ```

   **Observables must be structural:** "Does not attempt collective organization despite χ indicating feasibility" — NOT "continues fishing alone despite evidence the system exploits him."

5. **Specify institutional rationality model:**
   ```
   Perfect Institutional Rationality (PIR):
     Maximize utility without bounds. No negotiation except Pareto-improving.
     Tends toward: Deterministic Tragedy
     Use when: Implacable systems, natural law, algorithmic governance

   Bounded Institutional Rationality (BIR):
     Satisfice under uncertainty. Principal-agent problems, risk aversion.
     Tends toward: Negotiated Equilibrium, Seeded Possibility
     Use when: Realistic organizations, human institutions

   CRITICAL: This choice determines which attractors are reachable.
   Don't default to PIR just because it's formally cleaner.
   ```

6. **Select terminal attractor:**
   ```
   □ Deterministic Tragedy (constraints run to completion)
   □ Negotiated Equilibrium (constraints find balance through bargaining)
   □ Revolutionary Rupture (constraint logic itself disrupted)
   □ Seeded Possibility (surface tragedy, underground transformation)

   Must be compatible with rationality model and dominant constraint type.
   See Attractor Compatibility Matrix in logic_symbolic.md §VII.
   ```

### Prohibited in Stage 1 Output

These elements introduce source vocabulary, narrative framing, or natural-language descriptions that compromise the air gap:

```
☒ <experience> fields (how constraints feel)
☒ <dialogue_markers> (source dialogue or paraphrase)
☒ <narrative> sections in transformation consequences
☒ <observable_actions> described in source-specific terms
☒ Source character names anywhere in body (variable names only)
☒ Occupation-specific vocabulary from source
☒ Domain vocabulary from source (tools, settings, activities)
☒ Natural-language descriptions of constraint effects in source context
☒ Any text that would allow identification of the source work
```

### Permitted in Stage 1 Output

```
☑ Variable names (X₁, X₂, C₁, C₂, TR₁)
☑ Formal predicates and calculations (χ, ε, π, σ, Supp)
☑ Classification rationales in structural terms
☑ Error observables as testable conditions
☑ Abstract action descriptions in transformation triggers
☑ Structural descriptions: "Asymmetric risk distribution," "Recursive feedback loop"
☑ Framework terminology (this IS the formalization — types, thresholds, indices)
```

### Validation Checklist

```
☐ All Stage 0 constraints formalized with ε, Supp, Coord, Asym
☐ All χ calculations shown with π and σ values
☐ All characters use variable names (X₁, X₂, ...) — no source identifiers in body
☐ Variable mapping table present in header (for traceability before anonymization)
☐ No source occupation, setting, or domain vocabulary anywhere in output
☐ No <experience>, <dialogue_markers>, or <narrative> fields
☐ Transformation rules use abstract trigger descriptions
☐ Error observables are testable structural conditions
☐ Institutional rationality model specified (PIR/BIR) with justification
☐ Terminal attractor selected, justified, and compatible
☐ Indexical variance explicitly preserved across agents
☐ Boltzmann test run for each constraint claiming Mountain status
☐ No ambiguity in specifications
☐ Output is pure symbolic network — a reader cannot identify the source work
```

---
