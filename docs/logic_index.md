# Deferential Realism: Logic System Index

**Version 4.0**  
**Purpose:** Navigation guide for the formal logic of indexed constraints  
**Last Updated:** February 2026

---

## Document Structure

The Deferential Realism logic system is organized across four documents, each serving a distinct purpose:

### **logic.md** — The Realist Core (Stages 1-6)
**Who:** Theorists, new users, philosophical foundations  
**What:** Indexed constraint logic, six types, power scaling, indexical relativity  
**Why:** Establishes the conceptual framework and core classification system

**Sections:**
- I. Foundation: Why Indexed Constraint-Logic?
- II. Basic Syntax: Indexed Constraint Operators
- III. Temporal Logic & Lifecycle States
- IV. Inference Rules & Priority Ordering
- V. Error Taxonomy & Misclassification
- VI. Decision Logic & Action Routing
- VII. Containment Logic (Structural Paradoxes)
- VIII. Self-Application & Meta-Logic
- IX. Conclusion

**Key Innovation:** Truth is index-relative but index-objective. The same constraint can be Mountain from one position and Snare from another—both classifications are objectively true.

---

### **logic_extensions.md** — The Structural Physics (Stages 7-9)
**Who:** Advanced users, implementation teams, researchers  
**What:** Boltzmann compliance, purity propagation, network dynamics  
**Why:** Mathematical foundation for natural law detection and structural health measurement

**Sections:**
- §1. The Boltzmann Axiom: Independence as Natural Law Test
- §2. Structural Purity: Coordination Health Measurement
- §3. Network Dynamics: Contamination Propagation
- §4. Lifecycle Extensions: Drift Types 8-11
- §5. Action Layer Extensions: Purity & Network-Qualified Decisions
- §6. Integration Architecture: How Stages 7-9 Extend Core

**Key Innovation:** Natural laws must factorize across index dimensions. Purity measures structural integrity. Contamination spreads through networks.

---

### **logic_thresholds.md** — The Parameter Registry
**Who:** Implementers, calibration teams, validation engineers  
**What:** Canonical threshold values for all 50+ system parameters  
**Why:** Single source of truth for config.pl, prevents drift between spec and implementation

**Organized by:**
- Power Modifiers (π)
- Scope Modifiers (σ)
- Type Classification Gates (Mountain, Rope, Snare, Tangled Rope, Scaffold, Piton)
- Boltzmann Compliance Parameters
- Purity Scoring Weights
- Network Dynamics Constants
- Lifecycle Drift Thresholds

**Format:** Category | Parameter | Default | Stage | Logic/Significance

---

### **logic_index.md** — This Document
**Who:** Everyone (start here)  
**What:** Navigation guide, reading paths, document relationships  
**Why:** Prevents getting lost in the system

---

## Reading Paths by Stakeholder

### **Theorist / Philosopher**
You want conceptual clarity and formal rigor without implementation details.

**Read:**
1. logic.md §I-II (Foundation + Operators) — Core concepts
2. logic.md §VII (Containment Logic) — Structural paradoxes
3. logic.md §VIII (Meta-Logic) — Self-application
4. logic_extensions.md §1 (Boltzmann Axiom) — Natural law test
5. logic_extensions.md §2.1-2.2 (Purity concept) — Skip implementation

**Skip:**
- logic_thresholds.md (too granular)
- logic_extensions.md §5-6 (action layer details)

---

### **Implementer / Engineer**
You need precise thresholds, predicate signatures, architectural discipline.

**Read:**
1. logic_index.md (this) — Orientation
2. logic_thresholds.md (all of it) — Canonical parameters
3. logic.md §II (Operators) — Formal definitions
4. logic_extensions.md §6 (Integration Architecture) — How modules connect
5. logic_extensions.md §1-5 — Stages 7-9 formal specs

**Reference frequently:**
- logic_thresholds.md (during implementation)
- logic_extensions.md §6.4 (Module Dependency Graph)

---

### **Pipeline Consumer (narrative_transform, isomorphism_engine, etc.)**
You need to know what's available, what it means, how to call it.

**Read:**
1. logic.md §II.B (Core Modal Operators) — Six types explained
2. logic.md §II.D-E (Power & Scope Scaling) — χ calculation
3. logic_thresholds.md (Classification Gates section) — When each type fires
4. logic_extensions.md §4 (Lifecycle Extensions) — Drift types 8-11
5. logic_extensions.md §2.3 (Purity Score API) — If using purity

**Key Questions Answered:**
- What are the six constraint types? → logic.md §II.B
- How do I calculate χ? → logic.md §II.D-E + logic_thresholds.md
- What drift types exist? → logic.md §III + logic_extensions.md §4
- How do I check Boltzmann compliance? → logic_extensions.md §1.3
- What's the purity score? → logic_extensions.md §2.3

---

### **Calibration / Validation Team**
You're testing the system, running corpus analysis, tuning parameters.

**Read:**
1. logic_thresholds.md (all parameters) — Know what can be tuned
2. logic.md §II.D (Power Modifiers) — π calibration rationale
3. logic_extensions.md §1.5 (Boltzmann Calibration) — Coupling thresholds
4. logic_extensions.md §2.5 (Purity Calibration) — Weight sensitivity
5. logic_extensions.md §6.5 (Known Limitations) — Where system is fragile

**Critical Notes:**
- Power modifiers were calibrated on 691-constraint corpus
- Analytical modifier (π=1.15) breaks moderate-analytical degeneracy
- Boltzmann thresholds are provisional, need non-WEIRD validation
- Purity weights (30/25/25/20) need sensitivity analysis

---

## Quick Reference: Where to Find X

| **If you need...** | **Go to...** |
|-------------------|-------------|
| Conceptual overview of the system | logic.md §I |
| Formal definition of Mountain/Rope/Snare/etc. | logic.md §II.B |
| Power modifier values (π) | logic_thresholds.md + logic.md §II.D |
| Scope modifier values (σ) | logic_thresholds.md + logic.md §II.E |
| How χ is calculated | logic.md §II.D (formula) |
| Classification threshold values | logic_thresholds.md (Classification Gates) |
| What drift types exist | logic.md §III + logic_extensions.md §4 |
| How to detect False Mountains | logic.md §V.1 |
| Boltzmann compliance test | logic_extensions.md §1.3 |
| Purity score calculation | logic_extensions.md §2.3 |
| Network contamination rules | logic_extensions.md §3.2 |
| Action routing logic | logic.md §VI + logic_extensions.md §5 |
| Prolog predicate signatures | logic_extensions.md §6.4 |
| Module dependency graph | logic_extensions.md §6.4 |
| Implementation architecture notes | logic_extensions.md §6 |
| Known system limitations | logic_extensions.md §6.5 |
| Canonical examples (carbon credits, etc.) | logic.md §VIII.4 |
| Error taxonomy | logic.md §V |
| Containment logic (unsolvable paradoxes) | logic.md §VII |

---

## Document Relationships

```
┌─────────────────────────────────────────────────────────────┐
│                     logic_index.md                          │
│                    (You are here)                           │
│                 Navigation & Entry Point                     │
└─────────────────────┬───────────────────────────────────────┘
                      │
        ┌─────────────┼─────────────┬─────────────────────┐
        │             │             │                     │
        ▼             ▼             ▼                     ▼
┌──────────────┐ ┌─────────────┐ ┌──────────────────┐ ┌────────────┐
│  logic.md    │ │logic_thresh │ │logic_extensions  │ │  config.pl │
│              │ │olds.md      │ │.md               │ │            │
│ Stages 1-6   │ │             │ │                  │ │ (impl)     │
│ Realist Core │ │ Parameter   │ │ Stages 7-9       │ │            │
│              │ │ Registry    │ │ Structural       │ │ Thresholds │
│ Conceptual   │ │             │ │ Physics          │ │ Source     │
│ Foundation   │ │ Single      │ │                  │ │            │
│              │ │ Source of   │ │ Boltzmann        │ │            │
│              │ │ Truth for   │ │ Purity           │ │            │
│              │ │ Thresholds  │ │ Network          │ │            │
└──────┬───────┘ └──────┬──────┘ └────────┬─────────┘ └─────┬──────┘
       │                │                 │                  │
       │                │                 │                  │
       │     References │                 │ Implements       │
       └────────────────┼─────────────────┴──────────────────┘
                        │
                        ▼
              ┌──────────────────────┐
              │  Prolog modules:     │
              │  - drl_core.pl       │
              │  - structural_       │
              │    signatures.pl     │
              │  - drl_lifecycle.pl  │
              │  - drl_modal_logic   │
              │    .pl               │
              └──────────────────────┘
```

**Flow:**
1. **Spec** (logic.md + logic_extensions.md) defines the formal system
2. **Registry** (logic_thresholds.md) catalogs canonical parameters
3. **Implementation** (Prolog) follows spec, uses registry values
4. **Validation** (corpus testing) refines registry, reports back to spec

**Critical Principle:** Changes flow **spec → registry → implementation**, never backward.

---

## Version History & Stage Evolution

**v1.0** (Original): 6 types, indexical relativity, power scaling  
**v2.0**: Lifecycle states, temporal operators, containment logic  
**v3.0**: Structural signatures (NL/CS/CC), two-regime architecture  
**v3.3**: Refined thresholds from 467-constraint corpus  
**v4.0** (Current): Boltzmann compliance (Stage 7), Purity propagation (Stage 8), Network dynamics (Stage 9), 691-constraint corpus

**Stages:**
- **1-6**: Core indexed constraint logic (logic.md)
- **7**: Boltzmann compliance engine (logic_extensions.md §1)
- **8**: Purity propagation & network contamination (logic_extensions.md §2-3)
- **9**: Network drift dynamics (logic_extensions.md §3.4)

---

## Key Concepts Across Documents

### **Indexical Relativity** (logic.md)
The same constraint can have different types from different structural positions. Carbon credits are simultaneously Rope (institutional), Tangled Rope (moderate), and Snare (powerless) from different power positions. All three classifications are objectively true.

### **Power-Scope Scaling** (logic.md + logic_thresholds.md)
Effective extractiveness χ = ε × π(P) × σ(S). Power position amplifies or dampens extraction. Scope increases verification difficulty.

### **Boltzmann Axiom** (logic_extensions.md)
Natural laws must factorize across index dimensions. If classification couples independent variables (Power, Scope), the constraint is constructed, not natural. This is the mathematical test for "physics-washing."

### **Structural Purity** (logic_extensions.md)
A continuous measure [0,1] of constraint health: factorization + scope invariance + coupling cleanliness + excess extraction. Purity < 0.3 = degraded; ≥ 0.9 = pristine.

### **Network Contamination** (logic_extensions.md)
Low-purity constraints contaminate neighbors. Contamination flows downward only (Snare → Rope, not vice versa). Attenuation factor 0.5, per-edge cap 0.3.

### **Drift Events** (logic.md + logic_extensions.md)
11 types of constraint degradation or evolution:
- Types 1-7: Classical (metric substitution, extraction accumulation, etc.)
- Types 8-11: Network-aware (coupling drift, purity drift, network drift)

---

## Architectural Principles

### **Single Source of Truth**
- **Thresholds**: logic_thresholds.md → config.pl
- **Classification**: `classify_from_metrics/6` in drl_core.pl
- **Signatures**: structural_signatures.pl
- **Lifecycle**: drl_lifecycle.pl

### **Two-Regime Classification**
1. **Metrics-first** (drl_core.pl): ε, χ, Supp → type via thresholds
2. **Signature-override** (structural_signatures.pl): NL, FNL, CI_Rope, FCR can override

### **Shadow Mode Discipline**
Stages 7-9 run alongside core logic without modifying classify_from_metrics/6. Ensures stability while enabling extensions.

### **Priority Ordering**
Mountain > Snare > Scaffold > Rope > Tangled Rope > Piton > unknown

---

## Critical Reminders

**For Theorists:**
- Indexical relativity ≠ pure relativism. Truth is position-dependent but objective within position.
- Containment logic (§VII) handles irreducible structural paradoxes—some tensions can't be resolved.

**For Implementers:**
- Always use classify_from_metrics/6 from config.pl thresholds—don't hardcode.
- Boltzmann compliance test requires ≥3 indexed classifications (epistemic access check).
- Network propagation is one-hop only (prevents infinite recursion).

**For Pipeline Consumers:**
- Power scaling is mandatory—raw ε alone is insufficient.
- Purity score can be -1.0 (sentinel for insufficient data).
- Network drift velocity requires temporal measurement data.

**For Calibration Teams:**
- Power modifiers are corpus-calibrated, not universal—may need recalibration for non-Western contexts.
- Boltzmann thresholds are provisional—sensitivity analysis needed.
- Purity weights (30/25/25/20) derived from theoretical considerations, not empirical fitting.

---

## Where to Get Help

**Conceptual confusion?** → Start with logic.md §I  
**Implementation question?** → Check logic_extensions.md §6  
**Threshold lookup?** → logic_thresholds.md  
**Can't find something?** → Use the Quick Reference table above  
**System behaving unexpectedly?** → logic_extensions.md §6.5 (Known Limitations)

---

**"Formal systems should track real structure, acknowledge power differentials, and guide action from where you actually stand. Start with the index that maps your reality."**
