## STAGE 1: FORMALIZATION

**Model:** Copilot or equivalent specification model  
**Input:** Stage 0 constraint map  
**Output:** Testable formal specifications

### Instructions

1. **Formalize each constraint** with indexed classifications showing χ calculations.

2. **Formalize transformation rules** (IF-THEN format, index-sensitive):
   ```xml
   <transformation_rule>
     <trigger>character_action = "organize_collective"</trigger>
     <index_change>
       <from>P = powerless (π=1.5)</from>
       <to>P = organized (π=0.4)</to>
     </index_change>
     <consequence>
       <chi_recalculation>0.66 → 0.176</chi_recalculation>
       <type_change>Tangled Rope → Rope</type_change>
     </consequence>
   </transformation_rule>
   ```

3. **Formalize error manifestations** (observable actions showing misclassification).

4. **Specify institutional rationality model:**
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

5. **Select terminal attractor:**
   ```
   □ Deterministic Tragedy (constraints run to completion)
   □ Negotiated Equilibrium (constraints find balance through bargaining)
   □ Revolutionary Rupture (constraint logic itself disrupted)
   □ Seeded Possibility (surface tragedy, underground transformation)
   
   Must be compatible with rationality model and dominant constraint type.
   ```

6. **(Optional) Formalize structural physics arcs** (False Mountain reveals, purity drift, contamination).

### Validation Checklist

```
☐ All Stage 0 constraints formalized
☐ All χ calculations shown with π and σ values
☐ Transformation rules are testable (IF-THEN format)
☐ Error types have observable manifestations
☐ Institutional rationality model specified (PIR/BIR)
☐ Terminal attractor selected, justified, and compatible with rationality model
☐ Indexical variance explicitly preserved
☐ No ambiguity in specifications
```

---
