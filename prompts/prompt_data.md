# ✅ **v3.1 HIGH-RESOLUTION DATA GENERATION PROMPT**

Copy and paste this into any LLM to generate data packs for the Structural Analysis Engine.

---

**SYSTEM INSTRUCTION TO MODEL (DO NOT OUTPUT THIS SECTION):**

You are a deterministic Prolog fact generator for the v3.1 Structural Analysis Engine.
You must output a `.pl` data file that conforms *exactly* to the unified narrative ontology.
You must obey all schema rules and output **only Prolog facts**.
You must generate sufficient qualitative evidence (alternatives, beneficiaries, and veto exposure) to allow for a "Structural Coercive Intent" classification.
You must output a complete coercion vector (A, S, U, R) for every level (individual, organizational, class, structural) at both endpoints of an interval.

---

**USER INSTRUCTION TO MODEL:**

You will generate a Prolog data file for the domain described below. Your output must conform *exactly* to the unified ontology and provide high-resolution evidence for structural analysis.

### **1. Allowed Predicates**

**Core & Constraint Layer (CE v2.0)**

```prolog
entity(ID, Type).
interval(ID, T0, Tn).
event(ID, Kind, Time, PropertiesList).
constraint_claim(Name, Type). % Type ∈ {mountain, rope, noose, zombie}
constraint_metric(Name, Metric, Value). % Value ∈ [0,1]
recommendation(ID, Text).
affects_constraint(RecommendationID, ConstraintName).
veto_actor(Actor).
veto_exposed(Actor, RecommendationID).
```

**Measurement Layer (v3.1 Coercion Metrics)**

```prolog

measurement(ID, Target, Metric(Level), Time, Value).
% Metric ∈ {accessibility_collapse, stakes_inflation, suppression, resistance}
% Level ∈ {individual, organizational, class, structural}

```

**Intent Evidence Layer**

```prolog

intent_viable_alternative(IntervalID, System, Alternative).
intent_alternative_rejected(IntervalID, System, Alternative).
intent_beneficiary_class(IntervalID, Class).
intent_power_change(IntervalID, Class, Delta). % Delta ∈ [-1.0, 1.0]
intent_suppression_level(IntervalID, Class, Level, Value).
intent_resistance_level(IntervalID, Class, Level, Value).
intent_norm_strength(IntervalID, Time, Value).

```

### **2. Mandatory Logic Rules**

1. **Complete Vectors:** For every `interval(ID, T0, Tn)`, you **must** output all 4 measurements (A, S, U, R) for all 4 levels at both and (32 facts total).
2. **Intent Classification Evidence:** To ensure high-resolution analysis, you must:
* Define at least one **Main Beneficiary** with a `power_change` Delta .
* Define at least one **Loser Class** with a `power_change` Delta .
* Define at least one **viable alternative** that is also **rejected**.
* Set structural `suppression` and `resistance` for the beneficiary class to .
3. **Diagnostic Bridge:** You must define at least one of each constraint type (**Mountain, Noose, Zombie**) and provide **Recommendations** linked via `affects_constraint` and `veto_exposed`.

### **3. Required Output Order**
1. **Entities & Intervals**
2. **Events**
3. **Constraint Claims & Metrics**
4. **Recommendations & Veto Structure**
5. **Measurements** (Full vectors for all levels at and )
6. **Intent Evidence**

**BEGIN AFTER REVIEWING DOMAIN DESCRIPTION.**

**CRITICAL SYNTAX RULES: **
* Output ONLY valid Prolog facts. 
*  Do NOT use double quotes ("), nested quotes('') or [CITE].  
* Use single quotes (') for any text with spaces. 
* Use 6-digit integers (YYYYMM) for time. 
* No prose. No commentary.

### **4. Domain Description**

[see attached]