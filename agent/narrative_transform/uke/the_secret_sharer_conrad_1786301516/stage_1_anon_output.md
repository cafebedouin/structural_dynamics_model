# STAGE 1: FORMALIZATION OUTPUT

## Variable Mapping (Header Only — Stripped Post-Anonymization)

```
C₁ ← Formal_Responsibility
C₂ ← Outsider_Legitimacy_Test
C₃ ← Subversive_Solidarity
C₄ ← Fugitive_Status (deferred)
C₅ ← Institutional_Confinement (deferred)
C₆ ← Professional_Identity_Code (deferred)
C₇ ← Pre-judged_Morality (deferred)
```

---

## 1. CONSTRAINT FORMALIZATIONS

```xml
<constraint id="C1" name="Structural_Authority_Assignment">
  <properties>
    <epsilon>0.20</epsilon>
    <suppression>0.10</suppression>
    <coordination>true</coordination>
    <asymmetry>true</asymmetry>
  </properties>

  <agent ref="X1">
    <index>
      <power>powerful</power>          <!-- π = 0.6 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>local</scope>             <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.20 × 0.6 × 0.8 = 0.096</chi>
    <type>Rope</type>
    <threshold_check>χ ≤ 0.35 (rope_chi_ceiling): PASS. χ > 0 → ε ≤ 0.45 check: 0.20 ≤ 0.45 PASS.</threshold_check>
    <rationale>Low power-scaled extraction; coordination function intact; identity-lock on exit does not elevate χ since power position dampens extraction sufficiently.</rationale>
  </agent>

  <agent ref="X2">
    <index>
      <power>powerful</power>
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>local</scope>
    </index>
    <chi>ε × π × σ = 0.20 × 0.6 × 0.8 = 0.096</chi>
    <type>Rope</type>
    <threshold_check>χ ≤ 0.35: PASS. ε ≤ 0.45: PASS.</threshold_check>
    <rationale>Structurally identical position to X1 — both occupy the same formal-authority slot in parallel, non-overlapping instantiations.</rationale>
  </agent>

  <indexical_variance>
    C1: X1 χ=0.096 → Rope, X2 χ=0.096 → Rope
    No divergence. Both agents occupy formally symmetric positions within the authority structure; this constraint does not discriminate by individual index in its base form.
  </indexical_variance>

  <boltzmann_test>
    FAIL as candidate Mountain (not claimed). Classification is stable across the two tested agents at identical index tuples, but this reflects index identity, not natural-law independence — burden is contingent on occupying "powerful" position. A "powerless" test index (hypothetical low-rank agent) would show elevated χ, since π(powerless)=1.5 vs π(powerful)=0.6, a 2.5× multiplier. Fails factorizability requirement. Constructed, not natural. Consistent with Rope classification (coordination mechanism, not terrain).
  </boltzmann_test>
</constraint>
```

```xml
<constraint id="C2" name="Legitimacy_Deficit_Under_External_Scrutiny">
  <properties>
    <epsilon>0.40</epsilon>
    <suppression>0.30</suppression>
    <coordination>true</coordination>
    <asymmetry>true</asymmetry>
  </properties>

  <agent ref="X1">
    <index>
      <power>moderate</power>          <!-- π = 1.0 -->
      <time>biographical</time>
      <exit>trapped</exit>
      <scope>local</scope>             <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.40 × 1.0 × 0.8 = 0.32</chi>
    <type>Rope</type>
    <threshold_check>χ ≤ 0.35: PASS (0.32 ≤ 0.35). ε ≤ 0.45: PASS (0.40 ≤ 0.45).</threshold_check>
    <rationale>Power position degrades from powerful (C1) to moderate under this constraint — external scrutiny strips the deflection advantage. χ approaches but does not cross the Rope ceiling; classification remains coordination-type but is now proximate to the Tangled Rope boundary (0.35 strict floor), signaling structural fragility.</rationale>
  </agent>

  <agent ref="X3">
    <index>
      <power>organized</power>         <!-- π = 0.4 -->
      <time>biographical</time>
      <exit>constrained</exit>
      <scope>local</scope>             <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.40 × 0.4 × 0.8 = 0.128</chi>
    <type>Rope</type>
    <threshold_check>χ ≤ 0.35: PASS. ε ≤ 0.45: PASS.</threshold_check>
    <rationale>Collective burden-sharing among the organized agent group substantially dampens extraction. Coordination value dominates.</rationale>
  </agent>

  <indexical_variance>
    C2: X1 χ=0.32 → Rope, X3 χ=0.128 → Rope
    Both classify as Rope but at markedly different distances from the boundary. X1 sits at 91% of the χ ceiling (0.32/0.35); X3 sits at 37%. Single trigger event capable of pushing X1 across threshold into Tangled Rope territory without materially affecting X3's classification — this differential fragility is the load-bearing structural fact carried forward into C3.
  </indexical_variance>

  <boltzmann_test>
    FAIL as candidate Mountain (not claimed). Type varies with π across tested agents (0.32 vs 0.128 despite identical ε, Supp, σ) — non-factorizable dependency on power position confirms constructed status.
  </boltzmann_test>
</constraint>
```

```xml
<constraint id="C3" name="Concealed_Reciprocal_Obligation">
  <properties>
    <epsilon>0.75</epsilon>
    <suppression>0.90</suppression>
    <coordination>true</coordination>
    <asymmetry>true</asymmetry>
  </properties>

  <agent ref="X1">
    <index>
      <power>powerless</power>         <!-- π = 1.5 -->
      <time>immediate</time>
      <exit>trapped</exit>
      <scope>local</scope>             <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.75 × 1.5 × 0.8 = 0.90</chi>
    <type>Tangled Rope</type>
    <threshold_check>
      χ band (0.35, 0.90]: PASS (0.90 is the exact ceiling — included, per strict-floor/inclusive-ceiling partition).
      ε ≥ 0.30: PASS (0.75). Supp ≥ 0.40: PASS (0.90).
      Coord=true, Asym=true: PASS. RequiresActiveEnforcement: PASS (Supp=0.90 indicates active enforcement demand).
      ¬NaturalLawWithoutBeneficiary: PASS (beneficiary exists — see X4).
    </threshold_check>
    <rationale>Power position has collapsed from moderate (C2) to powerless under the weight of concealment obligation. Extraction is now maximally amplified by π=1.5. Genuine coordination value persists (the concealment produces a real alliance function) but is now inseparably fused with extraction cost borne by this agent. Canonical Tangled Rope — cannot be resolved into pure Rope or pure Snare without losing descriptive accuracy.</rationale>
  </agent>

  <agent ref="X4">
    <index>
      <power>institutional</power>     <!-- π = −0.2 -->
      <time>immediate</time>
      <exit>constrained</exit>
      <scope>local</scope>             <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.75 × (−0.2) × 0.8 = −0.12</chi>
    <type>Rope</type>
    <threshold_check>χ ≤ 0.35: PASS (−0.12 ≤ 0.35). χ ≤ 0 → ε ceiling bypassed per dual-threshold rule.</threshold_check>
    <rationale>Negative χ indicates net beneficiary status — this agent extracts FROM the system X1 is bound to. The identical base constraint (ε=0.75) that produces Tangled Rope for X1 produces Rope for X4 purely as a function of institutional power position. This is the textbook index-sensitive divergence the Snare/Tangled Rope framework is built to detect.</rationale>
  </agent>

  <agent ref="X3">
    <index>
      <power>moderate</power>          <!-- π = 1.0 -->
      <time>immediate</time>
      <exit>constrained</exit>
      <scope>local</scope>             <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.75 × 1.0 × 0.8 = 0.60</chi>
    <type>Tangled Rope</type>
    <threshold_check>χ band (0.35, 0.90]: PASS (0.60). ε ≥ 0.30: PASS. Supp ≥ 0.40: PASS. Coord=true, Asym=true: PASS.</threshold_check>
    <rationale>Mid-band Tangled Rope. Coordination and extraction both substantial and roughly co-present; collective position provides partial dampening relative to X1 but insufficient to escape the hybrid band.</rationale>
  </agent>

  <indexical_variance>
    C3: X1 χ=0.90 → Tangled Rope, X4 χ=−0.12 → Rope, X3 χ=0.60 → Tangled Rope
    Maximal divergence observed in the network. Identical constraint (ε=0.75, Supp=0.90) resolves to three distinct effective experiences purely as a function of π: powerless (1.5×) vs institutional (−0.2×) vs moderate (1.0×) sign-and-magnitude inversion. This is the network's central indexical-variance finding — the same coordination-obligation structure is simultaneously a trap and a benefit, factually, for different agents.
  </indexical_variance>

  <boltzmann_test>
    FAIL as candidate Mountain (not claimed — hypothesis was Tangled Rope). Confirmed constructed: type changes with π (Tangled Rope → Rope) and sign of χ inverts entirely between X1 and X4. No factorizable natural-law reading survives this variance.
  </boltzmann_test>
</constraint>
```

---

## 2. TRANSFORMATION RULES

```xml
<transformation_rule id="TR1">
  <trigger>
    <condition>disclose_concealed_obligation_to_enforcement_authority</condition>
    <target>C3</target>
    <agent>X1</agent>
  </trigger>
  <index_change>
    <from>P = powerless (π=1.5), E = trapped</from>
    <to>P = moderate (π=1.0), E = constrained</to>
  </index_change>
  <chi_recalculation>
    <before>χ = 0.75 × 1.5 × 0.8 = 0.90 → Tangled Rope</before>
    <after>χ = 0.75 × 1.0 × 0.8 = 0.60 → Tangled Rope</after>
  </chi_recalculation>
  <type_change>Tangled Rope → Tangled Rope (no type transition; magnitude reduction only)</type_change>
  <preconditions>Requires C2 legitimacy deficit to have resolved in agent's favor first (institutional standing must be restored before disclosure reduces rather than eliminates power deficit).</preconditions>
  <blocked_by>C1 (identity_locked exit persists regardless of disclosure — structural authority position cannot fully absorb the shift without exit-option change, which C1 does not permit).</blocked_by>
</transformation_rule>

<transformation_rule id="TR2">
  <trigger>
    <condition>exit_constraint_via_removal_of_concealed_party</condition>
    <target>C3</target>
    <agent>X1</agent>
  </trigger>
  <index_change>
    <from>P = powerless (π=1.5), T = immediate, E = trapped</from>
    <to>P = moderate (π=1.0), T = biographical, E = constrained</to>
  </index_change>
  <chi_recalculation>
    <before>χ = 0.75 × 1.5 × 0.8 = 0.90 → Tangled Rope</before>
    <after>χ = 0.75 × 1.0 × 0.8 = 0.60 → Tangled Rope (constraint persists at reduced magnitude; removal of counterparty does not zero the base ε since institutional exposure remains until time horizon extends)</after>
  </chi_recalculation>
  <type_change>Tangled Rope → Tangled Rope, with trajectory toward Rope as T→biographical fully realizes (χ continues declining toward 0.35 boundary as immediate pressure dissipates)</type_change>
  <preconditions>Physical/narrative removal of X4 from the shared local scope. Requires C5 (Institutional_Confinement, deferred) to permit exit at all.</preconditions>
  <blocked_by>C5 if confinement scope prevents any exit route (offstage function specifies universal amplification of secrecy difficulty — if C5 resolves to Mountain, TR2 has no valid trigger path).</blocked_by>
</transformation_rule>

<transformation_rule id="TR3">
  <trigger>
    <condition>organize_collective_denial_of_knowledge</condition>
    <target>C3</target>
    <agent>X3</agent>
  </trigger>
  <index_change>
    <from>P = moderate (π=1.0), E = constrained</from>
    <to>P = organized (π=0.4), E = constrained</to>
  </index_change>
  <chi_recalculation>
    <before>χ = 0.75 × 1.0 × 0.8 = 0.60 → Tangled Rope</before>
    <after>χ = 0.75 × 0.4 × 0.8 = 0.24 → Rope</after>
  </chi_recalculation>
  <type_change>Tangled Rope → Rope</type_change>
  <preconditions>Requires coordination among X3 sub-members sufficient to shift power classification from moderate to organized — a collective-action threshold, not automatic.</preconditions>
  <blocked_by>Omega (crew_awareness): the analysis cannot resolve whether X3's actual epistemic state supports organized denial or whether X1's projection of this capacity is itself unverifiable. TR3 is therefore conditionally specified — its preconditions may not be satisfiable given the unresolved omega.</blocked_by>
</transformation_rule>

<transformation_rule id="TR4">
  <trigger>
    <condition>external_verification_pressure_increases</condition>
    <target>C2</target>
    <agent>X1</agent>
  </trigger>
  <index_change>
    <from>P = moderate (π=1.0), Scope = local (σ=0.8)</from>
    <to>P = moderate (π=1.0), Scope = regional (σ=0.9)</to>
  </index_change>
  <chi_recalculation>
    <before>χ = 0.40 × 1.0 × 0.8 = 0.32 → Rope</before>
    <after>χ = 0.40 × 1.0 × 0.9 = 0.36 → Tangled Rope</after>
  </chi_recalculation>
  <type_change>Rope → Tangled Rope (T1 classical transition: Rope degradation via extraction accumulation, here driven by scope expansion raising verification difficulty)</type_change>
  <preconditions>C2's scope must expand beyond local — corresponds to widening of the external-scrutiny audience.</preconditions>
  <blocked_by>None identified; this transition is the one the constraint network is structurally primed for, given C2's proximity to the 0.35 boundary noted in its indexical_variance.</blocked_by>
</transformation_rule>
```

---

## 3. ERROR MANIFESTATIONS

```xml
<error id="E1">
  <type>Type III — Snare-as-Rope (Missing Extraction)</type>
  <agent>X1</agent>
  <constraint>C3</constraint>
  <actual_type>Tangled Rope (χ=0.90, at ceiling — functionally adjacent to Snare territory)</actual_type>
  <perceived_type>Rope (pure coordination/alliance value, extraction component unacknowledged)</perceived_type>
  <observable>Agent continues to escalate personal risk exposure under C3 without initiating any transformation rule (TR1/TR2) despite χ sitting at the Tangled Rope ceiling — behavior consistent with normalizing the extraction as a cost of coordination rather than as a distinct extractive component requiring separate mitigation.</observable>
  <correction_trigger>Explicit computation of χ at the powerless index (as performed in this formalization) — if agent's internal model produced χ=0.90 rather than treating the relationship as costless alliance, Type III would resolve.</correction_trigger>
</error>

<error id="E2">
  <type>Type V.a — Tangled Rope Mishandling (Tangled-as-Rope, ignoring extraction component)</type>
  <agent>X3</agent>
  <constraint>C3</constraint>
  <actual_type>Tangled Rope (χ=0.60)</actual_type>
  <perceived_type>Rope (coordination-only reading of collective loyalty function)</perceived_type>
  <observable>No collective bargaining or explicit acknowledgment-of-cost behavior observable despite χ=0.60 sitting well within the hybrid band (0.35, 0.90] rather than the Rope range — group-level classification does not register the suppression cost (Supp=0.90) it is jointly bearing.</observable>
  <correction_trigger>Suppression variable (Supp=0.90) becoming visible to the collective agent as a cost attributable to C3 specifically, rather than diffused across background institutional pressure (C5).</correction_trigger>
</error>

<error id="E3">
  <type>Type I — False Mountain</type>
  <agent>X1</agent>
  <constraint>C1</constraint>
  <actual_type>Rope (χ=0.096, changeable — classification confirmed non-Mountain by Boltzmann failure)</actual_type>
  <perceived_type>Mountain (structural authority treated as unchangeable terrain rather than constructed, exitable position)</perceived_type>
  <observable>No attempt to invoke identity_locked → constrained exit-option transition despite C1's low ε (0.20) and low Supp (0.10) indicating minimal enforcement resistance to such a transition.</observable>
  <correction_trigger>Recognition that exit=identity_locked is a cognitive/identity fusion, not a physical/structural barrier — distinguishing it from exit=trapped would reveal the constraint's actual changeability within biographical time horizon.</correction_trigger>
</error>

<error id="E4">
  <type>Type III — Snare-as-Rope (Missing Extraction), inverse-agent case</type>
  <agent>X4</agent>
  <constraint>C3</constraint>
  <actual_type>Rope (χ=−0.12, net beneficiary)</actual_type>
  <perceived_type>Not applicable as misclassification by X4 — flagged here as the structural condition that PRODUCES E1/E2: X4's net-beneficiary position is itself the unacknowledged extractive source that X1 and X3 absorb.</perceived_type>
  <observable>Absence of any reciprocal cost-bearing action by X4 commensurate with the coordination benefit received — testable via comparison of Supp burden distribution (0.90 total) against χ-derived share (X4 bears negative share, i.e., net gain).</observable>
  <correction_trigger>Reclassification of C3 by an analytical-index observer (π=1.15) would surface this asymmetry explicitly, per the degeneracy-breaking function of the analytical power modifier.</correction_trigger>
</error>
```

---

## 4. INSTITUTIONAL RATIONALITY MODEL

**Selected: Bounded Institutional Rationality (BIR)**

**Justification:**
- C1 and C2 involve human institutional actors (formal authority structures, peer-legitimacy assessment) operating under uncertainty about X1's true capacities and X3's true knowledge state (per unresolved omega `crew_awareness`) — not algorithmic or natural-law determinism.
- Principal-agent dynamics are explicit in C2 (X1 as principal-under-scrutiny, X3 as agent-collective whose trust cannot be commanded, only earned) — a hallmark BIR signature.
- C3's indexical variance (X1 Tangled Rope, X4 Rope, X3 Tangled Rope) is only coherent under satisficing behavior: X4's institutional position permits risk-averse minimal engagement while X1 and X3 satisfice under acute uncertainty about detection probability, rather than optimizing against a fully known utility function.
- PIR would require X1 to behave as a utility-maximizing enforcer of C1's formal logic without bounded deviation — but the entire dramatic mechanism of C3 depends on exactly such a bounded, satisficing deviation occurring. PIR is structurally incompatible with the selected attractor (see §5).

---

## 5. TERMINAL ATTRACTOR SELECTION

**Selected: Seeded Possibility**

**Justification:**

Per the Attractor Compatibility Matrix (§VII), a constraint profile with **Tangled Ropes dominant** under **BIR** maps to **Negotiated Equilibrium** as primary compatible attractor. However, the break_contract data specifies:

```
prior_status: DEAD
original_break: Institutional loyalty overrides personal-code loyalty
target_prior: Internal psychological victory > external ambiguous consequences
```

This break structure — surface-level constraint resolution remaining externally unresolved/ambiguous (C2's legitimacy deficit is never fully discharged; C3's concealment cost to X1 is never fully transformed away, per TR1/TR2's partial-reduction-only recalculations) while an internal/underground transformation (X1's relation to C1's formal authority) completes — is the signature of **Seeded Possibility**, not clean Negotiated Equilibrium.

**Compatibility check:**
- Piton present (deferred C6, hypothesized Rope, and C7 hypothesized Piton) → Piton-adjacent structural presence is compatible with Seeded Possibility per matrix row "Piton present: Either/Either → Seeded Possibility."
- Tangled Rope dominance (C3, the highest-centrality constraint) under BIR permits Negotiated Equilibrium OR, given the break-contract's insistence on unresolved external ambiguity, Seeded Possibility as the tragedy-surface variant.
- The `<missing_floor>` invariant (formal judgment system lacks competence to assess acts of extreme contextual duress) guarantees the external layer cannot resolve cleanly — ruling out pure Negotiated Equilibrium, which requires bargaining to a stable settled state. What remains stable is only the internal/relational transformation (the "untranslatable real" — unspoken recognition preceding formal code), which persists underground beneath an externally unresolved or tragic surface.

**Terminal attractor: Seeded Possibility** — confirmed compatible with BIR, confirmed compatible with Tangled-Rope-dominant profile with Piton-adjacent deferred constraints, confirmed required by break-contract's ambiguous-external / resolved-internal structure.

---

## 6. VALIDATION CHECKLIST

```
☑ All Stage 0 constraints formalized with ε, Supp, Coord, Asym (C1, C2, C3)
☑ All χ calculations shown with π and σ values explicit
☑ All characters use variable names (X1, X2, X3, X4) — no source identifiers in body
☑ Variable mapping table present in header (for traceability before anonymization)
☑ No source occupation, setting, or domain vocabulary anywhere in output
☑ No <experience>, <dialogue_markers>, or <narrative> fields present
☑ Transformation rules use abstract trigger descriptions (disclose_concealed_obligation_to_enforcement_authority, exit_constraint_via_removal_of_concealed_party, organize_collective_denial_of_knowledge, external_verification_pressure_increases)
☑ Error observables are testable structural conditions, not source-specific actions
☑ Institutional rationality model specified (BIR) with justification
☑ Terminal attractor selected (Seeded Possibility), justified, and compatibility-matrix-checked
☑ Indexical variance explicitly preserved across agents (C1: none; C2: magnitude divergence; C3: sign-and-type inversion)
☑ Boltzmann test run for each constraint — none claim Mountain status; all confirmed FAIL/constructed as expected given hypotheses (Rope/Tangled Rope, not Mountain)
☑ No ambiguity in specifications
☑ Output is pure symbolic network — source work not identifiable from body text
```

**Unresolved omega carried forward:** `crew_awareness` (X3's actual epistemic state vs. X1's projection) remains unresolved and is flagged as a blocking condition on TR3. This omega is preserved rather than arbitrarily resolved, per Stage 1 scope (formalization does not adjudicate underdetermined narrative facts).

<!-- ANONYMIZATION: 4 character names replaced with structural labels: Agent_A, Agent_B, Agent_C, Agent_D -->
