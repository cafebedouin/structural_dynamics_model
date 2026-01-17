# Individual Belief Battery - Prolog Implementation

## Overview

This is a Prolog implementation of the **Individual Practical Battery** from Deferential Realism (DR). It systematically audits your provisional beliefs by classifying them as:

- **Mountains** (unchangeable constraints - physics, logic, death)
- **Ropes** (strategic choices - how to live within constraints)
- **Nooses** (extraction mechanisms claimed as Mountains)

The system detects when institutions present constructed constraints (Nooses) as natural law (Mountains) - this is called a **Type I Error** or **False Mountain**.

## Files

1. **individual_belief_battery_v2.pl** - Core belief mappings (40 questions)
2. **belief_audit_v2.pl** - Audit interface and reporting
3. **belief_constraint_indexing.pl** - Context handling (WHO/WHEN/WHERE/POWER)

## Installation

```prolog
% Load the modules
?- [individual_belief_battery_v2].
?- [belief_audit_v2].
```

## Usage Examples

### Quick Audit

```prolog
% Check a single belief
?- audit(eternal_soul).

=== QUICK AUDIT: eternal_soul ===
Classification: UNDEFINED (rejected)
Institutional Claim: mountain
⚠️  CONFLICT DETECTED
```

### Detailed Audit

```prolog
% Get full analysis with explanation
?- audit_detailed(mortality_acceptance).

╔══════════════════════════════════════════════════════════════╗
║ DETAILED BELIEF AUDIT: mortality_acceptance                  ║
╚══════════════════════════════════════════════════════════════╝

[YOUR CONTEXT]
  context(agent_power(individual_moderate), time_horizon(biographical), 
          exit_options(mobile), spatial_scope(national))

[CLASSIFICATION]
  Your Belief: mountain

[RATIONALE]
  Forced by T5 (death is final). Not pragmatic choice - this is 
  high-stakes power domain. Afterlife beliefs enable Nooses 
  (die for the cause, sacrifice now for later). Must accept 
  Mountain: death is final. Urgency follows necessarily.

[CASCADE ANALYSIS]
  If you accept this belief:
    mortality accepted → urgency in living → finite time matters
  If you reject this belief:
    afterlife belief → can demand your death → subjugation enabled
```

### Complete Battery Audit

```prolog
% Audit all 40 beliefs
?- audit_all.

╔══════════════════════════════════════════════════════════════╗
║        COMPLETE INDIVIDUAL BELIEF BATTERY AUDIT              ║
╚══════════════════════════════════════════════════════════════╝

[YOUR CONTEXT]
  Power Level:  individual_moderate
  Time Horizon: biographical
  Exit Options: mobile
  Spatial Scope: national

[TIER 0: UNIVERSAL MOUNTAINS]
Things true regardless of who/when/where:

  ■ logical_necessity
  ■ mathematical_truth
  ■ physical_laws
  ■ consciousness_substrate
  ■ death_finality

[TIER 1: INDIVIDUAL PRACTICE]
Pragmatic choices (35 beliefs):

  AGENCY & CHANGE:
    ⊞ meaningful_agency
    ⊞ personal_change
    ⊞ identity_stability
  
  [... continues through all 13 sections ...]

[EXTRACTION POINT DETECTION]
Nooses commonly claimed as Mountains:

Found 8 institutional Nooses:

  ⚠️  eternal_soul
      Claimed: Mountain (unchangeable)
      Actually: Noose (extraction mechanism)
```

### Find Extraction Points

```prolog
% Detect Type I Errors (False Mountains)
?- find_nooses.

╔══════════════════════════════════════════════════════════════╗
║           EXTRACTION POINT DETECTION                        ║
╚══════════════════════════════════════════════════════════════╝

Searching for Type I Errors (False Mountains)...

Found 8 extraction mechanisms:

─────────────────────────────────────────────────────────────
⚠️  eternal_soul

  INSTITUTIONAL CLAIM: mountain
  YOUR CLASSIFICATION: UNDEFINED (rejected as Mountain)

  WHY THIS MATTERS:
  If consciousness requires substrate (T4), then death (substrate 
  destruction) ends consciousness. Critical implication: enables 
  urgency, blocks afterlife-based Nooses.

─────────────────────────────────────────────────────────────
⚠️  cosmic_purpose

  INSTITUTIONAL CLAIM: mountain
  YOUR CLASSIFICATION: rope

  WHY THIS MATTERS:
  No evidence of cosmic purpose. Meaning is constructed through 
  projects/relationships/values, not discovered. Cosmic meaning 
  claims are often Nooses (see: Gita eternal soul enabling 
  subjugation). Constructed meaning preserves autonomy.

[... continues for all 8 Nooses ...]
```

### Cascade Analysis

```prolog
% See what follows from a belief
?- show_cascades(meaningful_agency).

=== CASCADE ANALYSIS: meaningful_agency ===

[IF YOU ACCEPT THIS BELIEF]
If agency → planning makes sense → effort is meaningful → responsibility exists

[IF YOU REJECT THIS BELIEF]
If no agency → learned helplessness → no point planning → motivation collapse
```

### Summary Statistics

```prolog
?- belief_summary.

=== BELIEF BATTERY SUMMARY ===

Mountains (Universal): 5
Ropes (Strategic choices): 33
Varies (Context-dependent): 2

Type I Errors detected: 8

Total beliefs mapped: 40
```

### Compare to Institutional Claims

```prolog
?- compare_to_institution(work_as_dignity).

=== INSTITUTIONAL COMPARISON: work_as_dignity ===

[YOUR POSITION]
  Classification: varies
  Reasoning: Work not inherently valuable. Some work is Rope 
  (chosen, meaningful, well-compensated). Some work is Noose 
  (extractive, necessary only for survival). "Work as dignity" 
  claim is Noose preventing questioning of exploitation.

[INSTITUTIONAL POSITION]
  Claimed as: mountain

[ANALYSIS]
  ⚠️  CONFLICT: Type I Error detected
  This Noose is being presented as Mountain
  Effect: Enables extraction/control
  Strategy: Reject institutional framing
```

## The 40 Questions

### Tier 0: Theory (5 Mountains)
1. T1: Logical necessity
2. T2: Mathematical truth
3. T3: Physical laws
4. T4: Consciousness substrate-dependence
5. T5: Death finality

### Tier 1: Practice (35 Questions)

**Agency & Change (3)**
- P1: Meaningful agency
- P2: Personal change
- P3: Identity stability

**Meaning & Purpose (2)**
- P4: Life meaning
- P5: Suffering meaning

**Relationships (2)**
- P6: Relationship meaning
- P7: Family obligations

**Work & Value (2)**
- P8: Work value
- P9: Wealth/status pursuit

**Epistemic Norms (2)**
- P10: Belief norms (truth vs utility)
- P11: Moral intuitions

**Power & Systems (2)**
- P12: Power structures natural
- P13: Participation in Nooses

**Death & Risk (3)**
- P14: Mortality acceptance
- P15: Risk-taking
- P16: Future planning

**Trust & Cooperation (3)**
- P17: Trust default
- P18: Human nature
- P19: Defector punishment

**Identity & Expression (3)**
- P20: Social conformity
- P21: Gender/sexual identity
- P22: Cultural/religious identity

**Consumption & Resources (3)**
- P23: Consumption level
- P24: Experiences vs possessions
- P25: Giving obligation

**Knowledge & Learning (3)**
- P26: Specialization
- P27: Formal education
- P28: Curiosity vs practicality

**Political Participation (3)**
- P29: Electoral participation
- P30: Direct action
- P31: Local vs global

**Existential Stance (4)**
- P32: Optimism/pessimism
- P33: Cosmic meaning
- P34: Having children
- P35: Time allocation

## Key Insights from the Battery

### What's Actually Mountain (Unchangeable)?
- Logic, math, physics (always true)
- Consciousness requires substrate (provisional)
- Death is final (follows from substrate)
- **That's it - ~5 actual Mountains**

### What's Rope (Strategic Choice)?
- **Everything else (~35 questions)**
- Most "deep metaphysical truths" are pragmatic frame choices

### What's Noose (When Claimed as Mountain)?
Common extraction mechanisms:
- Eternal soul (enables subjugation)
- Cosmic purpose (enables control)
- Natural hierarchy (justifies oppression)
- Work as dignity (prevents questioning exploitation)
- Family sacred (enables abuse)
- National duty (enables war)
- Gender roles natural (enables discrimination)
- Property rights natural (enables concentration)

## Philosophical Summary

**You're a pragmatist with materialist foundations:**
- Accept physical reality as constraint
- Construct meaning within constraints
- Treat social structures as tools
- Resist extraction mechanisms
- Preserve autonomy above all

The Deferential Realism framework itself is a **Scaffold** (temporary tool) for navigating this.

## Integration with Full DR System

This battery can be integrated with the full Deferential Realism Prolog system to:
- Apply personal context to domain analysis
- Compare individual vs institutional perspectives
- Detect extraction in real-world cases
- Make decisions based on provisional beliefs

## Next Steps

1. Customize `my_context/1` to match your actual power/time/options/scope
2. Run `audit_all` to see complete belief map
3. Run `find_nooses` to identify extraction points
4. Use `audit_detailed/1` on beliefs you're uncertain about
5. Apply to real decisions using full DR system

## License

Part of the Deferential Realism project.
Public domain (CC0-1.0).
