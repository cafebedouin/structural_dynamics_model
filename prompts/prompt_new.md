

### **Updated USER INSTRUCTION for v3.1.2 (DR Audit Edition)**

**USER INSTRUCTION TO MODEL:**

You will generate a Prolog data file for the domain described below. Your output must conform **exactly** to the unified ontology. You are acting as a clinical sensor; provide measurements, not rationalizations.

### **1. Updated Predicate Schema**

**Core & Constraint Layer (Audit Standards)**

```prolog
entity(ID, Type). % Note: Use Type 'scaffold' for transitional supports.
interval(ID, StartInt, EndInt). % MUST use integers (e.g., 0, 10).
event(ID, Kind, TimeInt, PropertiesList). % TimeInt MUST be an integer.
constraint_claim(Name, Type). % Type ∈ {mountain, rope, tangled_rope, noose, zombie}.

% KINETIC METRICS (THE PHYSICS GAUGE)
% Metric ∈ {intensity, suppression_requirement, snap_back_potential, extractiveness}
constraint_metric(Name, Metric, Value). % Value ∈ [0,1].

% OMEGA TRACKING (REASONING BLOCKERS)
% Type ∈ {empirical, conceptual, preference}
omega_variable(ID, Type, Description).

recommendation(ID, Text).
affects_constraint(RecommendationID, ConstraintName).
veto_actor(Actor).
veto_exposed(Actor, RecommendationID).

% MEASUREMENTS
% Vector MUST follow: [Agency, Stability, Utility, Resilience]
measurement(TimeInt, [A, S, U, R]).

```

### **2. Mandatory Logic Rules (The Unbending Gauge)**

Constraint Type,Symbol,Extractiveness (X),Suppression (Y),Action
Mountain,■C,≤0.05 (N/A),≤0.05,Accept
Rope,⊞C,≤0.35,≤0.15,Maintain
Tangled Rope,⊠T,0.36−0.65,0.16−0.45,Reform
Noose,⊠C,≥0.66,≥0.46,Cut

* **Temporal Integrity:** All time variables (`StartInt`, `EndInt`, `TimeInt`) **MUST** be integers. Atoms like `t0` or `tn` will cause a system crash.
* **Mountain Verification (■C):** Set `suppression_requirement`  `0.05`. Any value above `0.05` triggers a **Severe Type I Error**. Set `snap_back_potential` to `0.0`.
* **Rope Verification (⊞C):** Set `extractiveness`  `0.35` and `suppression_requirement`  `0.15`.
* **Tangled Rope Verification (⊠T):** Set `extractiveness` between `0.36` and `0.65`. This triggers a **Reform** recommendation.
* **Noose Verification (⊠C):** Requires `extractiveness`  `0.66` **AND** `suppression_requirement`  `0.46`. To recommend removing a Noose, you **MUST** define an entity of `type(scaffold)`.
* **Omega Identification:** Identify at least one **Omega Variable (Ω)**. No placeholders.
    **Omega Classification Requirements:**
    - Empirical ($\Omega_E$): Use for missing observable data. Resolution requires measurement or calculation.
    - Conceptual ($\Omega_C$): Use for ambiguous terms or multiple coherent interpretations. Resolution requires explicit definition.
    - Preference ($\Omega_P$): Use for value judgments. Resolution requires a decision by those bearing the consequences.

### **3. Integrity Vector Requirements**

* **Zero-Sycophancy:** Do not smooth contradictions. If the physics (Metrics) contradicts the Claim, output the conflicting metrics exactly as measured.
* **Evidence Standards:** Every `constraint_claim` must be supported by `event` or `measurement` facts.

### **4. Required Output Order**

1. **Entities & Intervals** (Declare Scaffolds here).
2. **Events & Omega Variables**.
3. **Constraint Claims & Kinetic Metrics**.
4. **Recommendations & Veto Points**.
5. **Measurements** (Full A/S/U/R vectors for Start and End integers).
6. **Intent Evidence** (Alternatives, beneficiaries, and power deltas).

### **5.  Formatting**
Do note use [CITE], double quotes or nested single quotes. File will be evaluated by Prolog. 

---

