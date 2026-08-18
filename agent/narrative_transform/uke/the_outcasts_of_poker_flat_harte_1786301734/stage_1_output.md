# STAGE 1: FORMALIZATION OUTPUT

## Variable Mapping (header only — stripped at anonymization)

```
X₁ ← John Oakhurst
X₂ ← The Duchess
X₃ ← Poker Flat Committee (institutional group G₁)
X₄ ← Mother Shipton
X₅ ← Uncle Billy
G₁ ← Poker Flat Committee
G₂ ← All characters in camp (universal set {X₁, X₂, X₄, X₅, ...})

C₁ ← MoralPurge
C₂ ← EnvironmentalTrap
C₃ ← ResourceScarcity
C₄ ← SocialCaste (deferred)
C₅ ← GamblersCode (deferred)
C₆ ← SharedFate (deferred)
```

---

## 1. CONSTRAINT FORMALIZATIONS

```xml
<constraint id="C1">
  <properties>
    <epsilon>1.0</epsilon>
    <suppression>0.9</suppression>
    <coordination>true</coordination>
    <asymmetry>true</asymmetry>
  </properties>

  <agent ref="X1">
    <index>
      <power>powerless</power>       <!-- π = 1.5 -->
      <time>immediate</time>
      <exit>trapped</exit>
      <scope>local</scope>            <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 1.0 × 1.5 × 0.8 = 1.20</chi>
    <type>Snare</type>
    <threshold_check>χ ≥ 0.66 PASS; ε ≥ 0.46 PASS; Supp ≥ 0.60 PASS → Snare confirmed</threshold_check>
    <rationale>High base extraction amplified by powerless position; immutable to this index, changeable to institutional index — satisfies SnareImmutability.</rationale>
  </agent>

  <agent ref="X2">
    <index>
      <power>powerless</power>
      <time>immediate</time>
      <exit>trapped</exit>
      <scope>local</scope>
    </index>
    <chi>ε × π × σ = 1.0 × 1.5 × 0.8 = 1.20</chi>
    <type>Snare</type>
    <threshold_check>χ ≥ 0.66 PASS; ε ≥ 0.46 PASS; Supp ≥ 0.60 PASS → Snare confirmed</threshold_check>
    <rationale>Identical structural position to X1 — no differentiation by prior classification, only by present position within the expulsion mechanism.</rationale>
  </agent>

  <agent ref="X3">
    <index>
      <power>institutional</power>    <!-- π = -0.2 -->
      <time>immediate</time>
      <exit>arbitrage</exit>
      <scope>local</scope>             <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 1.0 × (-0.2) × 0.8 = -0.16</chi>
    <type>Rope</type>
    <threshold_check>χ ≤ 0.35 PASS (χ ≤ 0 → ε ceiling bypassed) → Rope confirmed</threshold_check>
    <rationale>Net beneficiary position. Coordination function (social cleansing) executed at negative extraction cost to enforcing agent.</rationale>
  </agent>

  <indexical_variance>
    C1: X1 χ=1.20 → Snare, X2 χ=1.20 → Snare, X3 χ=-0.16 → Rope
    Divergence driven entirely by π: powerless (π=1.5) amplifies ε=1.0 into extraction ceiling breach; institutional (π=-0.2) inverts sign, converting same base constraint into low-cost coordination tool. This is the paradigm case of the "most index-sensitive type" — Snare/Rope split on power position alone, scope and epsilon held constant.
  </indexical_variance>

  <boltzmann_test>FAIL — Type(C1) varies with P (Snare at powerless, Rope at institutional) holding S constant. Non-factorizable Power dependence confirms C1 is constructed, not natural. Disqualified from Mountain status regardless of ε/Supp values (moot here since ε=1.0 already exceeds Mountain ceiling of 0.25).</boltzmann_test>
</constraint>
```

```xml
<constraint id="C2">
  <properties>
    <epsilon>0.1</epsilon>
    <suppression>0.0</suppression>
    <coordination>false</coordination>
    <asymmetry>false</asymmetry>
  </properties>

  <agent ref="G2">
    <index>
      <power>powerless</power>        <!-- π = 1.5 -->
      <time>immediate</time>
      <exit>trapped</exit>
      <scope>universal</scope>         <!-- σ = 1.0 -->
    </index>
    <chi>ε × π × σ = 0.1 × 1.5 × 1.0 = 0.15</chi>
    <type>Mountain</type>
    <threshold_check>ε ≤ 0.25 PASS; Supp ≤ 0.05 PASS; NaturalEmergence PASS; Immutable(T=immediate, E=trapped) PASS → Mountain confirmed</threshold_check>
    <rationale>Minimal extraction, zero suppression, no enforcement apparatus. Constraint arises from physical conditions external to any agent's design.</rationale>
  </agent>

  <indexical_variance>
    None. Recompute at institutional index for verification: π(institutional)=-0.2, χ = 0.1 × (-0.2) × 1.0 = -0.02 → still within Rope/Mountain non-extraction range, and critically, no agent occupies an institutional position relative to this constraint (no one enforces or benefits from weather/terrain). Absence of any beneficiary index is itself diagnostic.
  </indexical_variance>

  <boltzmann_test>
    PASS. Testing across available indices:
    - P=powerless, S=universal: χ=0.15 → Mountain
    - P=moderate (π=1.0), S=universal: χ = 0.1 × 1.0 × 1.0 = 0.10 → still ε≤0.25, Supp≤0.05 → Mountain
    - P=institutional (π=-0.2), S=universal: χ=-0.02 → Mountain (no sign-based type flip since ε already below floor for Snare/Rope distinction to matter)
    Type invariant across all tested P at fixed S. No agent achieves extraction-beneficiary status. Classification does not factor through Power in a differentiating way → candidate Mountain confirmed.
  </boltzmann_test>
</constraint>
```

```xml
<constraint id="C3">
  <properties>
    <epsilon>0.4</epsilon>
    <suppression>0.5</suppression>
    <coordination>true</coordination>
    <asymmetry>true</asymmetry>
  </properties>

  <agent ref="X4">
    <index>
      <power>powerless</power>        <!-- π = 1.5 -->
      <time>immediate</time>
      <exit>trapped</exit>
      <scope>local</scope>             <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.4 × 1.5 × 0.8 = 0.48</chi>
    <type>Tangled Rope</type>
    <threshold_check>χ ∈ (0.35, 0.90] PASS; ε ≥ 0.30 PASS; Supp ≥ 0.40 PASS; Coord=true, Asym=true PASS; RequiresActiveEnforcement PASS → Tangled Rope confirmed</threshold_check>
    <rationale>Moderate-high extraction under powerless amplification, but genuine coordination value (rationing function) is co-present with cost asymmetry — irreducible hybrid, not disguised Snare.</rationale>
  </agent>

  <agent ref="X1">
    <index>
      <power>moderate</power>         <!-- π = 1.0 -->
      <time>immediate</time>
      <exit>trapped</exit>
      <scope>local</scope>             <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.4 × 1.0 × 0.8 = 0.32</chi>
    <type>Rope</type>
    <threshold_check>χ ≤ 0.35 PASS; ε ≤ 0.45 PASS; Changeable PASS → Rope confirmed</threshold_check>
    <rationale>Managerial position over the rationing mechanism reduces power-scaled extraction below Tangled Rope floor (0.35) into genuine coordination range.</rationale>
  </agent>

  <agent ref="X5">
    <index>
      <power>institutional</power>    <!-- π = -0.2 -->
      <time>immediate</time>
      <exit>arbitrage</exit>
      <scope>local</scope>             <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.4 × (-0.2) × 0.8 = -0.06</chi>
    <type>Rope</type>
    <threshold_check>χ ≤ 0 → ε ceiling bypassed; χ ≤ 0.35 PASS → Rope confirmed</threshold_check>
    <rationale>Exploitation of replenishment mechanism (theft of exit resource) converts institutional index into net beneficiary — temporary exit from constraint via arbitrage position.</rationale>
  </agent>

  <indexical_variance>
    C3: X4 χ=0.48 → Tangled Rope, X1 χ=0.32 → Rope, X5 χ=-0.06 → Rope
    Three-way split demonstrates full range of the formula: powerless index crosses Tangled Rope floor (0.35) via π=1.5 amplification; moderate index sits just under the same floor via π=1.0; institutional index inverts sign entirely via π=-0.2, exiting the constraint's extraction logic altogether despite identical ε and σ across all three.
  </indexical_variance>

  <boltzmann_test>FAIL — Type(C3) varies with P non-factorizably (Tangled Rope → Rope → Rope across powerless → moderate → institutional, with a hard classification-boundary crossing between X4 and X1). Constructed constraint, disqualified from Mountain candidacy independent of ε/Supp (moot: ε=0.4 exceeds Mountain ceiling regardless).</boltzmann_test>
</constraint>
```

---

## 2. TRANSFORMATION RULES

```xml
<transformation_rule id="TR1">
  <trigger>
    <condition>exit_constraint via resource_appropriation</condition>
    <target>C3</target>
    <agent>X5</agent>
  </trigger>
  <index_change>
    <from>P = moderate/powerless (π=1.0–1.5), E = trapped</from>
    <to>P = institutional (π=-0.2), E = arbitrage</to>
  </index_change>
  <chi_recalculation>
    <before>χ = 0.4 × 1.5 × 0.8 = 0.48 → Tangled Rope</before>
    <after>χ = 0.4 × (-0.2) × 0.8 = -0.06 → Rope</after>
  </chi_recalculation>
  <type_change>Tangled Rope → Rope</type_change>
  <preconditions>Access to shared resource pool; absence of monitoring; presence of replenishment-critical asset (exit vector) susceptible to unilateral removal</preconditions>
  <blocked_by>Collective monitoring protocol (not established in this network); C1's suppression regime does not extend surveillance to C3's resource layer</blocked_by>
</transformation_rule>

<transformation_rule id="TR2">
  <trigger>
    <condition>organize_collective_rationing</condition>
    <target>C3</target>
    <agent>X1</agent>
  </trigger>
  <index_change>
    <from>P = powerless (π=1.5), E = trapped</from>
    <to>P = moderate (π=1.0), E = trapped</to>
  </index_change>
  <chi_recalculation>
    <before>χ = 0.4 × 1.5 × 0.8 = 0.48 → Tangled Rope</before>
    <after>χ = 0.4 × 1.0 × 0.8 = 0.32 → Rope</after>
  </chi_recalculation>
  <type_change>Tangled Rope → Rope</type_change>
  <preconditions>Group compliance with allocation decisions; absence of competing extraction (violated by TR1 if concurrent)</preconditions>
  <blocked_by>TR1 — resource appropriation reduces total pool available for management, potentially reverting X1's position back toward Tangled Rope or Snare as pool shrinks (ε effectively rises for remaining trapped agents)</blocked_by>
</transformation_rule>

<transformation_rule id="TR3">
  <trigger>
    <condition>terminate_local_exposure via self-removal</condition>
    <target>C2</target>
    <agent>X1</agent>
  </trigger>
  <index_change>
    <from>E = trapped, T = immediate</from>
    <to>E = trapped, T = immediate (no change — Mountain confirmed immutable)</to>
  </index_change>
  <chi_recalculation>
    <before>χ = 0.1 × 1.5 × 1.0 = 0.15 → Mountain</before>
    <after>χ = 0.1 × 1.5 × 1.0 = 0.15 → Mountain (unchanged; per T7, Mountains do not transition)</after>
  </chi_recalculation>
  <type_change>Mountain → Mountain (null transition, T7 confirmed)</type_change>
  <preconditions>None available — this rule documents attempted transformation and its structural failure</preconditions>
  <blocked_by>Mountain immutability by definition; no index achievable within story's T/E range converts natural-emergence environmental constraint into constructed type</blocked_by>
</transformation_rule>

<transformation_rule id="TR4">
  <trigger>
    <condition>reclassify_target via institutional_reversal</condition>
    <target>C1</target>
    <agent>X1</agent>
  </trigger>
  <index_change>
    <from>P = powerless (π=1.5), E = trapped</from>
    <to>P = institutional (π=-0.2), E = arbitrage [hypothetical — not achieved in constraint network]</to>
  </index_change>
  <chi_recalculation>
    <before>χ = 1.0 × 1.5 × 0.8 = 1.20 → Snare</before>
    <after>χ = 1.0 × (-0.2) × 0.8 = -0.16 → Rope [counterfactual]</after>
  </chi_recalculation>
  <type_change>Snare → Rope [BLOCKED — hypothetical only]</type_change>
  <preconditions>Would require access to enforcement apparatus (G1's decision-making structure)</preconditions>
  <blocked_by>C1's suppression value (0.9) — enforcement mechanism structurally excludes X1 from institutional access; no transformation rule in the actual constraint network achieves this index change for X1</blocked_by>
</transformation_rule>
```

---

## 3. ERROR MANIFESTATIONS

```xml
<error id="E1">
  <type>Type III — Snare-as-Rope (Missing Extraction)</type>
  <agent>X3</agent>
  <constraint>C1</constraint>
  <actual_type>Rope (at X3's own institutional index, χ=-0.16)</actual_type>
  <perceived_type>[N/A — X3's classification is internally correct at its own index]</perceived_type>
  <observable>G1 does not recompute χ at powerless index before executing C1; no evidence of checking χ(X1)=1.20 or χ(X2)=1.20 prior to enforcement</observable>
  <correction_trigger>Institutional agent computes χ at trapped/powerless index and finds χ ≥ 0.66 → recognizes Snare exists regardless of own low-cost position (per §V Type III test: "Check χ at powerless index... if χ(powerless) > 0.70 → Snare exists regardless of χ(powerful)")</correction_trigger>
</error>

<error id="E2">
  <type>Type I — False Mountain</type>
  <agent>X1</agent>
  <constraint>C1</constraint>
  <actual_type>Snare (χ=1.20, changeable at institutional index per SnareImmutability)</actual_type>
  <perceived_type>Mountain (treated as unchangeable natural consequence of prior classification rather than constructed institutional action)</perceived_type>
  <observable>Does not attempt collective organization or appeal despite structural exit existing at a different index (G1's own arbitrage position demonstrates the constraint is not universal/natural)</observable>
  <correction_trigger>Demonstration that classification varies by Power × Scope (Boltzmann FAIL at C1) — if X1's index detects this variance, Snare-status confirmed changeable-in-principle, motivating a different action path than passive acceptance</correction_trigger>
</error>

<error id="E3">
  <type>Type V.a — Tangled-as-Rope (ignoring extraction component)</type>
  <agent>X4</agent>
  <constraint>C3</constraint>
  <actual_type>Tangled Rope (χ=0.48)</actual_type>
  <perceived_type>Rope (if agent normalizes rationing purely as coordination without registering personal extraction cost)</perceived_type>
  <observable>Compliance with allocation decisions without protest or resistance markers, despite χ exceeding Rope ceiling (0.35) at this agent's own index</observable>
  <correction_trigger>Recomputation showing χ(X4)=0.48 > 0.35 confirms extraction component is present and non-trivial — triggers reclassification from Rope to Tangled Rope</correction_trigger>
</error>

<error id="E4">
  <type>Type IV — Rope-as-Snare (Missing Coordination)</type>
  <agent>X1</agent>
  <constraint>C3</constraint>
  <actual_type>Rope (χ=0.32, at X1's managerial index)</actual_type>
  <perceived_type>[Potential] Snare — if X1 interprets managing scarce resources under group pressure as pure extraction rather than genuine coordination function</perceived_type>
  <observable>Test: does removing X1's rationing role eliminate a coordination function? If yes (allocation becomes disordered/purely competitive absent management), Rope-classification is correct and Snare-perception is Type IV error</observable>
  <correction_trigger>Verify coordination function persists independent of X1's presence — if allocation degrades to pure extraction upon removal, confirms genuine Rope status, not disguised Snare</correction_trigger>
</error>
```

---

## 4. INSTITUTIONAL RATIONALITY MODEL

```
Selected: PIR (Perfect Institutional Rationality) for C1/G1
          BIR (Bounded Institutional Rationality) for C3/X1, X4, X5

Justification:

C1 (MoralPurge): G1 operates under PIR. The committee's enforcement is
Coord=true, Asym=true, Supp=0.9 — no negotiation channel exists (no
Pareto-improving alternative offered to X1/X2). χ(X3)=-0.16 achieved via
unilateral position, not bargained settlement. The suppression ceiling
(0.9) and absence of any negotiated-equilibrium markers in the constraint
graph confirm PIR governs this constraint's institutional layer.

C3 (ResourceScarcity): Governance is BIR. Multiple agents (X1, X4, X5)
face uncertainty (immediate horizon, no perfect information about
resource duration). X5's appropriation (TR1) is a satisficing/risk-averse
move under principal-agent asymmetry (X5 has no formal authority but
exploits informal access) rather than utility-maximizing within an
implacable system. X1's management role (TR2) is a bounded, adaptive
response to uncertain scarcity — not global optimization.

C2 (EnvironmentalTrap): Rationality model is moot — Mountain-type
constraints are outside institutional agency entirely; no agent
optimizes against natural law, they merely endure it.
```

---

## 5. TERMINAL ATTRACTOR SELECTION

```
Selected: ☑ Deterministic Tragedy

Justification:

Constraint profile = Mountain (C2) + Snare (C1, at powerless index) +
Tangled Rope (C3, degrading toward Snare per T2 as resource pool shrinks
under TR1 pressure).

Per Attractor Compatibility Matrix:
- "Mountain + Snares" → PIR → Deterministic Tragedy (exact match for C1+C2 combination)
- "Tangled Ropes dominant" + PIR → Tragedy (crushed by hybrid) — applies
  to C3's trajectory once TR1 fires, since institutional exit reduces the
  pool for trapped agents, pushing residual χ upward (T2: Tangled Rope →
  Snare, "coordination loss")

Compatibility check: C1 governed by PIR (see §4) — Mountain+Snare/PIR
cell is explicitly Deterministic Tragedy with no alternative listed.
C3's BIR governance at the individual-agent level does not override the
terminal trajectory, since C3 is downstream of C1 in the generation
sequence (C1 → C2 → C3) and inherits the tragic ceiling C1 imposes: no
transformation rule in the network (TR1–TR4) achieves an exit from C1
for X1 or X2. TR4 is explicitly marked BLOCKED.

The unresolved omega (O1 — whether X1's final action constitutes
submission-to-code or calculated sacrifice) does not change attractor
classification: both readings are compatible with Deterministic Tragedy,
differing only in whether the terminal state carries residual signal
value (consistent with Seeded Possibility elements coexisting at the
margin, per the "Piton present → Seeded Possibility" row — though no
Piton is certified in this network, so this remains a secondary texture
rather than a competing attractor).
```

---

## 6. VALIDATION CHECKLIST

```
☑ All constraints (C1, C2, C3) formalized with ε, Supp, Coord, Asym
☑ All χ calculations shown with π and σ values explicit
☑ All characters use variable names (X1–X5, G1–G2) — no source identifiers in body
☑ Variable mapping table present in header only
☑ No source occupation, setting, or domain vocabulary in body
☑ No <experience>, <dialogue_markers>, or <narrative> fields present
☑ Transformation rules (TR1–TR4) use abstract trigger descriptions with mechanical χ recalculation
☑ Error observables (E1–E4) are testable structural conditions
☑ Institutional rationality model specified per-constraint (PIR for C1, BIR for C3) with justification
☑ Terminal attractor selected (Deterministic Tragedy), justified via Compatibility Matrix
☑ Indexical variance explicitly preserved across agents for C1 and C3
☑ Boltzmann test run for all three constraints — C2 PASS (Mountain certified), C1 and C3 FAIL (constructed, confirmed non-Mountain)
☑ No ambiguity in specifications
```