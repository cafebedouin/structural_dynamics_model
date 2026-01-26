### **USER INSTRUCTION for v3.1.7 (Clean Audit Edition)**

**USER INSTRUCTION TO MODEL:**

You will generate a Prolog data file for the domain described below. Your output must conform **exactly** to the unified ontology. You are acting as a clinical sensor; provide measurements, not rationalizations.

### **1. Updated Predicate Schema**

**Core & Constraint Layer (Audit Standards)**

```prolog
entity(ID, Type). % Note: Use Type 'scaffold' for transitional supports.
interval(ID, StartInt, EndInt). % MUST use integers. ONLY define the primary audit interval.
event(ID, Kind, TimeInt, PropertiesList). % TimeInt MUST be an integer.
constraint_claim(Name, Type). % Type ∈ {mountain, rope, tangled_rope, snare, piton}.

% KINETIC METRICS (THE PHYSICS GAUGE)
% Metric ∈ {intensity, suppression_requirement, snap_back_potential, extractiveness}
constraint_metric(Name, Metric, Value). % Value ∈ [0,1].

% OMEGA TRACKING (REASONING BLOCKERS)
% Type ∈ {empirical, conceptual, preference}
omega_variable(ID, Type, Description).

recommendation(ID, Text).
affects_constraint(RecommendationID, ConstraintName).
veto_actor(Actor).
veto_exposed(Actor, RecommendationID). % Actor MUST match veto_actor to block.

% MEASUREMENTS
% Vector MUST follow: [Agency, Stability, Utility, Resilience]
measurement(TimeInt, [A, S, U, R]).

```

*   **Temporal:** Use **integers only** for time. Define exactly **one** primary interval to sync with the Scenario Manager.
*   **Omega:** Categorize every variable as **empirical** (data needed), **conceptual** (definition needed), or **preference** (value choice needed).
*   **Snare Removal:** If recommending the removal of a `snare`, you **MUST** declare a `type(scaffold)` entity.
*   **Veto Sync:** Ensure the `veto_actor(ID)` is identical to the `veto_exposed(ID, RecID)`.

### **2. Mandatory Logic Rules (The Unbending Gauge)**

*   **Mountain (■C):**Set  `suppression_requirement`  `0.05` AND `snap_back_potential` = `0.0`.
*   **Snare (⊠C):** `extractiveness`  `0.66` AND `suppression_requirement`  `0.46`.
*   **Rope (⊞C):** `extractiveness`  `0.35` AND `suppression`  `0.15`.
*   **Tangled Rope (⊠T):** `extractiveness` between `0.36` and `0.65`.

*   **Rule: Primary Interval Only**: To prevent recursive verification failures, define **only one** primary `interval/3` covering the full narrative scope. Do not define sub-intervals.
*   **Rule: Endpoint Measurements**: You **MUST** provide a `measurement/2` for both the `StartInt` and `EndInt` of your defined interval.
*   **Snare Verification (⊠C)**: To recommend removing a Snare, you **MUST** define an `entity(ID, scaffold)`.
*   **Omega Classification**: Categorize blockers as **Empirical** (data-driven), **Conceptual** (definitional), or **Preference** (value-driven).

### **3. Required Output Order**

1. **Entities & Interval** (Primary only). (Sync interval ID with the domain).
2. **Events & Omega Variables** (Classified by E/C/P type).
3. **Constraint Claims & Kinetic Metrics** (Apply thresholds strictly).
4. **Recommendations & Veto Points**.
5. **Measurements** (Vector: `[Agency, Stability, Utility, Resilience]`).
6. **Intent Evidence** (Power deltas and beneficiaries).

### **4. Formatting**
Do note use [CITE], double quotes or nested single quotes. File will be evaluated by Prolog. 

