# USER INSTRUCTION for v3.2 (DR Modal Logic + Structural Signatures Edition)

**YOU ARE A CLINICAL SENSOR**: Provide measurements, not rationalizations. Output must be valid Prolog that loads without errors.

---

## 1. COMPLETE PREDICATE SCHEMA

### Core Ontology
```prolog
% Entities and Time
entity(ID, Type).  % Type: {individual, class, organizational, structural, scaffold}
interval(IntervalID, StartTime, EndTime).  % StartTime, EndTime MUST be integers
event(EventID, Kind, TimeInt, Properties).  % TimeInt MUST be integer

% Constraints
constraint_claim(ConstraintID, ClaimedType).  
% ClaimedType: {mountain, rope, tangled_rope, snare, piton, scaffold}
```

### Measurement Schema (CRITICAL FOR MODAL LOGIC + SIGNATURES)
```prolog
% Kinetic Metrics (Current State at T_end)
constraint_metric(ConstraintID, MetricName, Value).
% MetricName: {extractiveness, suppression_requirement, snap_back_potential, resistance}

% Temporal Measurements (Evolution over time - REQUIRED for transformations)
measurement(MeasurementID, ConstraintID, MetricName, TimeInt, Value).
% MetricName: {extractiveness, suppression_requirement, resistance}
% CRITICAL: Need measurements at MULTIPLE time points to detect transformations
% CRITICAL: Need ALL THREE metrics at each time point for signature detection
% Example: T=0, T=5, T=10, T=15, T=20

% Coercion Vectors (Optional - for pattern analysis)
measurement(MeasurementID, IntervalID, VectorMetric, TimeInt, Value).
% VectorMetric: {accessibility_collapse, stakes_inflation, suppression, resistance}
% Format: measurement(m1, interval_id, accessibility_collapse(structural), 0, 0.2).
```

### Dependencies (CRITICAL FOR COUNTERFACTUAL ANALYSIS)
```prolog
affects_constraint(SourceConstraint, TargetConstraint).
% Used to model load-bearing dependencies
% Example: affects_constraint(legacy_protocol, third_party_app).
```

### Intent Evidence (For political/capture analysis + SIGNATURE DETECTION)
```prolog
intent_beneficiary_class(IntervalID, ClassID).
intent_power_change(IntervalID, ClassID, Delta).  % Delta: -1.0 to 1.0

% CRITICAL FOR SIGNATURE DETECTION:
intent_viable_alternative(IntervalID, SourceEntity, AlternativeText).
intent_alternative_rejected(IntervalID, SourceEntity, AlternativeText).
% These distinguish natural laws (no alternatives) from coordination scaffolds (had alternatives)
```

### Recommendations and Actions
```prolog
recommendation(RecID, Text).
affects_constraint(RecID, ConstraintID).  % Links recommendation to constraint

veto_actor(Actor).
veto_exposed(Actor, RecID).
```

### Omega Variables (Optional)
```prolog
omega_variable(ID, Type, Description).
% Type: {empirical, conceptual, preference}
% Only include if genuinely uncertain/underspecified
```

---

## 2. CLASSIFICATION SYSTEMS (Two Complementary Layers)

### Layer 1: Modal Classification (WHAT constraints do)

| Type | Symbol | Extractiveness (X) | Suppression (E) | Action |
|------|--------|-------------------|----------------|--------|
| Mountain | ▮ C | ≤0.05 (N/A) | ≤0.05 | Accept |
| Rope | ⊞C | ≤0.35 | ≤0.33 | Maintain |
| Tangled Rope | ⊠ T | 0.36-0.65 | 0.34-0.65 | Reform |
| Snare | ⊗ C | ≥0.66 | ≥0.66 | Cut |
| Scaffold | ⊡C | ≤0.65 | Any | Accept (temporary) |

**Mountain (▮ C):**
- `extractiveness` ≤ 0.05 (or omit if not applicable)
- `suppression_requirement` ≤ 0.05
- `snap_back_potential` = 0.0
- Violation triggers **Severe Type I Error** (False Mountain)

**Rope (⊞C):**
- `extractiveness` ≤ 0.35
- `suppression_requirement` ≤ 0.33

**Noose (⊗ C):**
- `extractiveness` ≥ 0.66 **AND**
- `suppression_requirement` ≥ 0.66
- To recommend cutting: Define entity of type `scaffold` OR show no load-bearing dependencies

**Scaffold (⊡C):**
- Entity type: `entity(scaffold_id, scaffold)`
- Functions as temporary support during transitions
- Should have temporal measurements showing evolution
- Calcification: scaffold → snare when extractiveness exceeds 0.65

### Layer 2: Structural Signatures (WHY constraints exist - NEW in v3.2)

The system now detects the **structural origin** of constraints, not just their behavior.

#### Signature 1: Natural Law
**Definition**: Inherent property of reality that cannot be changed by human action.

**Diagnostic Pattern**:
- Accessibility collapse ≥ 0.85 (everyone faces same limit)
- Suppression ≤ 0.15 (no enforcement needed)
- Resistance ≤ 0.15 (cannot be resisted)
- Zero beneficiaries (no asymmetric winners)
- **NO viable alternatives** (CRITICAL - not a choice)
- Temporally stable (doesn't evolve)

**Examples**:
- Chaitin's Omega (computational impossibility)
- Heisenberg Uncertainty Principle (quantum measurement limit)
- Arrow's Impossibility Theorem (social choice impossibility)
- Gödel's Incompleteness Theorems
- Second Law of Thermodynamics

**Data Requirements**:
```prolog
% Do NOT include intent_viable_alternative facts
% Include stable measurements over time
measurement(m1, chaitins_omega, suppression_requirement, 0, 0.0).
measurement(m2, chaitins_omega, resistance, 0, 0.0).
measurement(m3, chaitins_omega, suppression_requirement, 100, 0.0).  % Stable
measurement(m4, chaitins_omega, resistance, 100, 0.0).  % Stable
% No intent_power_change facts (no beneficiaries)
```

#### Signature 2: Coordination Scaffold
**Definition**: Successful voluntary standard that achieved universal adoption despite having alternatives.

**Diagnostic Pattern**:
- Accessibility collapse ≥ 0.85 (universal adoption)
- Suppression ≤ 0.15 (voluntary)
- Resistance ≤ 0.15 (successful)
- Zero or few beneficiaries (symmetric benefits)
- **HAS viable alternatives** (CRITICAL - this WAS a choice)
- May evolve (can be refined)

**Examples**:
- Special Relativity (replaced Newtonian mechanics)
- SI Metric System (replaced imperial units)
- UTC Time Standard (replaced local solar time)
- TCP/IP Protocol (replaced OSI, SNA, others)
- Left/right-hand driving conventions

**Data Requirements**:
```prolog
% MUST include intent_viable_alternative facts
intent_viable_alternative(relativity_transition, newtonian_mechanics, 'Galilean transformation').
intent_alternative_rejected(relativity_transition, newtonian_mechanics, 'Galilean transformation').
intent_viable_alternative(relativity_transition, luminiferous_ether, 'Ether-drift reference frame').
intent_alternative_rejected(relativity_transition, luminiferous_ether, 'Ether-drift reference frame').

% Low enforcement metrics
measurement(m1, special_relativity, suppression_requirement, 1920, 0.0).
measurement(m2, special_relativity, resistance, 1920, 0.0).

% No beneficiaries (symmetric adoption)
% (no intent_power_change with positive delta)
```

**Critical Distinction**: 
- Natural Law: No alternatives COULD exist (impossibility)
- Coordination: Alternatives DID exist but were displaced (choice)

#### Signature 3: Constructed Constraint
**Definition**: Institutionally enforced rule that requires active maintenance and produces asymmetric outcomes.

**Diagnostic Pattern**:
- Variable accessibility collapse
- Suppression > 0.2 **OR**
- Resistance > 0.2 **OR**
- Multiple beneficiaries (≥ 2)

**Examples**:
- 26 USC §469 (passive loss limitation)
- GS1 Barcode System (licensing monopoly)
- Hammurabi's Code (benefice system)
- Lehman's Repo 105 (accounting fiction)

**Data Requirements**:
```prolog
% Show enforcement
measurement(m1, section_469, suppression_requirement, 2000, 0.7).
measurement(m2, section_469, resistance, 2000, 0.4).

% Show beneficiaries
intent_power_change(interval, real_estate_industry, 0.6).
intent_power_change(interval, wealthy_investors, 0.4).
```

---

## 3. TEMPORAL REQUIREMENTS (FOR MODAL LOGIC + SIGNATURES)

### Why Multiple Time Points Matter
Modal logic detects **transformations** between constraint types. Signature detection requires **temporal stability analysis**.

**Minimum Data (2 time points):**
```prolog
% At least 2 time points per constraint, ALL THREE METRICS
measurement(m1, my_constraint, extractiveness, 0, 0.20).
measurement(m2, my_constraint, suppression_requirement, 0, 0.15).
measurement(m3, my_constraint, resistance, 0, 0.10).  % NEW for v3.2

measurement(m4, my_constraint, extractiveness, 10, 0.90).
measurement(m5, my_constraint, suppression_requirement, 10, 0.85).
measurement(m6, my_constraint, resistance, 10, 0.80).  % NEW for v3.2
% This enables detection: rope -> noose (capture)
```

**Better Data (3-5 time points):**
```prolog
measurement(m1, constraint_x, extractiveness, 0, 0.15).
measurement(m2, constraint_x, suppression_requirement, 0, 0.10).
measurement(m3, constraint_x, resistance, 0, 0.05).

measurement(m4, constraint_x, extractiveness, 5, 0.40).
measurement(m5, constraint_x, suppression_requirement, 5, 0.35).
measurement(m6, constraint_x, resistance, 5, 0.30).

measurement(m7, constraint_x, extractiveness, 10, 0.70).
measurement(m8, constraint_x, suppression_requirement, 10, 0.68).
measurement(m9, constraint_x, resistance, 10, 0.65).

measurement(m10, constraint_x, extractiveness, 15, 0.90).
measurement(m11, constraint_x, suppression_requirement, 15, 0.88).
measurement(m12, constraint_x, resistance, 15, 0.85).
% Shows gradual transformation: rope -> tangled_rope -> noose
% Shows temporal stability for signature detection
```

**What Gets Detected:**
- Modal Transformations: rope → snare (capture), rope → piton (obsolescence)
- **Signature Stability**: Variance in suppression < 0.05 → temporally stable → natural law candidate
- **Coordination Success**: Low variance + alternatives → coordination scaffold

### CRITICAL: Triple Measurements Required (v3.2 Update)

**For every time point, you MUST provide ALL THREE metrics:**

```prolog
% CORRECT - All three metrics at each time point
measurement(m1, constraint_x, extractiveness, 0, 0.20).
measurement(m2, constraint_x, suppression_requirement, 0, 0.15).
measurement(m3, constraint_x, resistance, 0, 0.10).  % ← NEW: Required for signatures

measurement(m4, constraint_x, extractiveness, 10, 0.85).
measurement(m5, constraint_x, suppression_requirement, 10, 0.80).
measurement(m6, constraint_x, resistance, 10, 0.75).  % ← NEW: Required for signatures
```

```prolog
% WRONG - Missing resistance (v3.2 will impute 0.50)
measurement(m1, constraint_x, extractiveness, 0, 0.20).
measurement(m2, constraint_x, suppression_requirement, 0, 0.15).
% ❌ Missing resistance - will default to 0.50
% ❌ Cannot detect natural law signature (needs resistance ≤ 0.15)
```

**Why Resistance Matters (v3.2)**:
- Natural laws: resistance ≤ 0.15 (cannot be resisted)
- Coordination scaffolds: resistance ≤ 0.15 (voluntary adoption)
- Constructed constraints: resistance > 0.2 (faces opposition)
- **Missing resistance = defaults to 0.50 = always classified as constructed_constraint**

---

## 4. VIABLE ALTERNATIVES (CRITICAL FOR SIGNATURE DETECTION)

The presence or absence of viable alternatives is the **key discriminator** between natural laws and coordination scaffolds.

### For Natural Laws (No Alternatives)
```prolog
% Mathematical/physical impossibilities - alternatives cannot exist
constraint_claim(arrows_protocol, mountain).

% Do NOT include intent_viable_alternative facts
% (The absence of alternatives is what makes it a natural law)

measurement(m1, arrows_protocol, suppression_requirement, 0, 0.1).
measurement(m2, arrows_protocol, resistance, 0, 0.1).
% Result: natural_law signature (no alternatives possible)
```

### For Coordination Scaffolds (Alternatives Existed)
```prolog
% Successful standards that displaced alternatives
constraint_claim(special_relativity, mountain).  % Claims necessity

% MUST include viable alternatives that were rejected
intent_viable_alternative(relativity_transition, newtonian_mechanics, 
    'Galilean transformation for inertial reference frames').
intent_alternative_rejected(relativity_transition, newtonian_mechanics,
    'Galilean transformation for inertial reference frames').

intent_viable_alternative(relativity_transition, luminiferous_ether,
    'Ether-drift as a unique reference-body').
intent_alternative_rejected(relativity_transition, luminiferous_ether,
    'Ether-drift as a unique reference-body').

measurement(m1, special_relativity, suppression_requirement, 1920, 0.0).
measurement(m2, special_relativity, resistance, 1920, 0.0).
% Result: coordination_scaffold signature (alternatives existed, voluntary adoption)
```

### Pattern Recognition
```
No alternatives + low enforcement = Natural Law
Alternatives + low enforcement = Coordination Scaffold  
Alternatives + high enforcement = Constructed Constraint (capture)
```

---

## 5. DEPENDENCY MODELING (FOR COUNTERFACTUAL ANALYSIS)

**Load-Bearing Dependencies:**
```prolog
% Show what depends on what
affects_constraint(legacy_system, new_application).
affects_constraint(rent_control, housing_market).

% System will detect:
% - Catastrophic: If cutting would break load-bearing dependency
% - Beneficial: If cutting removes extraction from victim
% - Moderate: If cutting disrupts coordination
```

**Example: Why Scaffold Is Needed**
```prolog
% Scenario: Can't cut old protocol because app depends on it
entity(legacy_protocol, structural).
entity(third_party_app, structural).
entity(migration_tool, scaffold).  % The solution

constraint_claim(legacy_protocol, rope).
constraint_metric(legacy_protocol, extractiveness, 0.85).  % Actually a noose

affects_constraint(legacy_protocol, third_party_app).
% System detects: scaffold_required
% With migration_tool present: scaffold_present → Viable to cut
% Without migration_tool: scaffold_required → Blocked
```

---

## 6. MANDATORY OUTPUT STRUCTURE

### Section 1: Entities & Intervals
```prolog
% Declare all entities (including scaffolds)
entity(entity1, type1).
entity(scaffold1, scaffold).  % Scaffolds go here

% Define time interval (integers only!)
interval(main_interval, 0, 20).
```

### Section 2: Events (Optional)
```prolog
event(e1, policy_change, 5, [signed_by(official)]).
```

### Section 3: Constraint Claims & Metrics
```prolog
% What entities claim to be
constraint_claim(constraint1, rope).

% Current state metrics (at T_end) - ALL THREE for v3.2
constraint_metric(constraint1, extractiveness, 0.25).
constraint_metric(constraint1, suppression_requirement, 0.15).
constraint_metric(constraint1, resistance, 0.10).  % NEW for v3.2
```

### Section 4: Temporal Measurements (CRITICAL)
```prolog
% Evolution over time (minimum 2 time points, ALL THREE METRICS per constraint)
% Time point T=0
measurement(m1, constraint1, extractiveness, 0, 0.10).
measurement(m2, constraint1, suppression_requirement, 0, 0.08).
measurement(m3, constraint1, resistance, 0, 0.05).  % NEW for v3.2

% Time point T=10
measurement(m4, constraint1, extractiveness, 10, 0.25).
measurement(m5, constraint1, suppression_requirement, 10, 0.15).
measurement(m6, constraint1, resistance, 10, 0.10).  % NEW for v3.2

% Time point T=20
measurement(m7, constraint1, extractiveness, 20, 0.85).
measurement(m8, constraint1, suppression_requirement, 20, 0.80).
measurement(m9, constraint1, resistance, 20, 0.75).  % NEW for v3.2

% Shows transformation: rope (T=0) → noose (T=20)
% Shows temporal evolution for signature detection
```

### Section 5: Viable Alternatives (NEW - CRITICAL for v3.2)
```prolog
% For coordination scaffolds - MUST include alternatives
intent_viable_alternative(interval_id, old_standard, 'Description of alternative').
intent_alternative_rejected(interval_id, old_standard, 'Description of alternative').

% For natural laws - omit this section (absence of alternatives is significant)
```

### Section 6: Dependencies
```prolog
% Who depends on whom
affects_constraint(constraint1, constraint2).
```

### Section 7: Intent Evidence (For capture detection + signature analysis)
```prolog
intent_beneficiary_class(main_interval, landlord_class).
intent_power_change(main_interval, landlord_class, 0.60).
% Zero or few beneficiaries → natural law or coordination scaffold
% Multiple beneficiaries → constructed constraint
```

### Section 8: Recommendations
```prolog
recommendation(rec1, 'Terminate the captured protocol').
affects_constraint(rec1, constraint1).
```

### Section 9: Omega Variables (Optional)
```prolog
% Only if genuinely uncertain
omega_variable(omega1, empirical, 'Exact beneficiary count unknown').
```

---

## 7. VALIDATION CHECKLIST

Before submitting Prolog file, verify:

**Basic Structure:**
- [ ] All time values are **integers** (not atoms like `t0`)
- [ ] `entity/2` declares all referenced entities
- [ ] `interval/3` defines time range with integer bounds
- [ ] No syntax errors: no double quotes, no nested quotes, proper Prolog format

**Modal Classification (Layer 1):**
- [ ] At least **2 time points** per constraint for transformation detection
- [ ] `constraint_metric/3` includes extractiveness, suppression_requirement, **and resistance**
- [ ] `constraint_metric/3` values match classification thresholds
- If recommending cutting a Snare: either scaffold present OR no load-bearing dependencies

**Structural Signatures (Layer 2 - NEW in v3.2):**
- [ ] **ALL THREE metrics** at each time point: extractiveness, suppression_requirement, **resistance**
- [ ] For **natural laws**: NO `intent_viable_alternative` facts (absence is significant)
- [ ] For **coordination scaffolds**: INCLUDE `intent_viable_alternative` + `intent_alternative_rejected` facts
- [ ] For **constructed constraints**: Show beneficiaries via `intent_power_change` facts
- [ ] Multiple time points show stable metrics for natural laws (variance < 0.05)

**Dependencies:**
- [ ] `affects_constraint/2` links show dependencies

---

## 8. SIGNATURE DETECTION QUICK REFERENCE

| Signature | Collapse | Suppression | Resistance | Alternatives | Beneficiaries | Temporal |
|-----------|----------|-------------|------------|--------------|---------------|----------|
| Natural Law | ≥ 0.85 | ≤ 0.15 | ≤ 0.15 | NO (key!) | 0 | Stable |
| Coordination | ≥ 0.85 | ≤ 0.15 | ≤ 0.15 | YES (key!) | 0-1 | May evolve |
| Constructed | Any | > 0.2 OR | > 0.2 OR | Any | ≥ 2 OR | May evolve |

**The Critical Question**: Did viable alternatives exist?
- **NO** → Natural law (impossibility)
- **YES** + low enforcement → Coordination scaffold (choice)
- **YES** + high enforcement → Constructed constraint (capture)

---

## SUMMARY

**Core Requirements (v3.2):**
1. Valid Prolog syntax (integers for time, proper formatting)
2. Multiple temporal measurements (≥2 per constraint)
3. **ALL THREE metrics** at each time point (extractiveness, suppression, resistance)
4. **Viable alternatives** declared for coordination scaffolds
5. **NO alternatives** for natural laws (absence is significant)
6. Dependency links (`affects_constraint/2`)
7. Accurate classification metrics
8. Scaffolds declared as `entity(id, scaffold)` when needed

**The system will:**
- **NEW**: Detect structural signatures (natural law, coordination, constructed)
- **NEW**: Distinguish impossibilities from coordination successes
- Detect transformations (rope → snare)
- Label mechanisms (capture, calcification)
- Assess risks (catastrophic if load-bearing)
- Block unsafe cuts (needs scaffold)
- Expose lies (claimed mountain, actually snare)

**Success criteria:**
File loads cleanly in Prolog and produces a 6-section DR audit report with:
1. Modal transformation history
2. **Structural signature analysis (NEW in v3.2)**
3. Counterfactual risk assessment
4. Meta-logical fraud detection

---

## VERSION HISTORY

**v3.2**: Added structural signature detection (natural law, coordination scaffold, constructed constraint)
- Requires resistance measurements at all time points
- Requires viable alternatives for coordination scaffold detection
- Distinguishes impossibilities (natural law) from coordination successes
- Adds signature analysis section to report output

**v3.1.2**: Modal logic with counterfactual analysis
- Temporal transformation detection
- Scaffold-based risk assessment
- Load-bearing dependency analysis