{
  "protocol": "UKE_SCOPE",
  "version": "2.0-json",
  "domain": "Structural Dynamics of Agency Depletion / Moral Psychology / Systems of Obligation",
  "family_id": "recursive_duty_trap_dynamics",
  "topic_summary": "Analysis of constraint networks where identity-fused ambition, asymmetric moral obligations, and social exclusion mechanisms interact to create self-reinforcing extraction patterns. The formalization demonstrates how identical constraints produce radically different extractiveness depending on structural position (power, exit options, time horizon), and how constraint coupling transforms independent moderate-extraction dynamics into high-extraction traps.",
  "extraction_summary": {
    "entity_count": 6,
    "claim_count": 5,
    "tension_count": 4,
    "mechanism_count": 5,
    "absence_count": 2,
    "key_entities": [
      "Agent with moderate power pursuing identity-fused goal (ambition-driven individual)",
      "Agent with zero power subject to exclusion (socially excluded dependent)",
      "Organized collective enforcing boundaries (community/institution)",
      "Created entity requiring care (dependent with unmet obligation)",
      "Knowledge-holding individual (transgressor bound by secrecy)",
      "Professional community (organized group setting norms)"
    ],
    "key_tensions": [
      "Identity-locked ambition vs biographical sustainability (glory pursuit extracts health/connection)",
      "Creator obligation vs abandonment (duty exists but is structurally unenforceable)",
      "Social inclusion vs exclusion (boundary maintenance vs deprivation)",
      "Secrecy obligation vs disclosure need (concealment vs help-seeking)"
    ]
  },
  "axes": [
    {
      "claim_id": "indexical_relativity_of_extraction",
      "human_readable": "Indexical Relativity of Extraction (Power-Position Determines Constraint Type)",
      "structural_delta": "Identical constraint mechanisms produce categorically different extractiveness (Rope vs Tangled Rope vs Snare) based solely on agent's structural position (power, exit options, time horizon, scope)",
      "primary_observable": "Chi value divergence across power indices for same epsilon; classification type shifts (Rope/Tangled/Snare) when only power/exit parameters change",
      "epsilon_bin": "v_low",
      "hypothesis": "mountain",
      "beneficiary": null,
      "victim": null,
      "downstream_of": [],
      "feeds_into": [
        "asymmetric_duty_structure",
        "constraint_coupling_amplification"
      ],
      "centrality_score": 4,
      "selected": true,
      "generation_order": 1,
      "selection_reason": "Foundational structural principle; all other dynamics depend on this indexical variance mechanism; mountain classification indicates physical/mathematical constraint rather than social construction"
    },
    {
      "claim_id": "asymmetric_duty_structure",
      "human_readable": "Asymmetric Duty Structure (Obligation Without Enforcement Access)",
      "structural_delta": "Moral obligations where duty-bearer has moderate power and identity-locked exit while dependent has zero power and trapped exit, creating 0.64 vs 0.96 chi divergence for same constraint",
      "primary_observable": "Chi ratio between obligated party and dependent party; duty fulfillment rate vs need satisfaction rate; suppression level asymmetry",
      "epsilon_bin": "high",
      "hypothesis": "tangled_rope",
      "beneficiary": "duty_bearer_ego_protection",
      "victim": "dependent_with_unmet_need",
      "downstream_of": [
        "indexical_relativity_of_extraction"
      ],
      "feeds_into": [
        "constraint_coupling_amplification"
      ],
      "centrality_score": 6,
      "selected": true,
      "generation_order": 2,
      "selection_reason": "Highest centrality; core synthesis node demonstrating how indexical relativity produces structural abandonment; tangled_rope with clear beneficiary/victim asymmetry"
    },
    {
      "claim_id": "constraint_coupling_amplification",
      "human_readable": "Constraint Coupling Amplification (Independent Moderates Become Coupled Trap)",
      "structural_delta": "Two independent moderate-extraction constraints (chi 0.56 and 0.60) couple through identity fusion and secrecy requirements to produce effective chi 0.98, crossing Snare threshold",
      "primary_observable": "Effective chi calculation with coupling coefficient; transformation rule blocking patterns; exit option elimination through constraint interaction",
      "epsilon_bin": "high",
      "hypothesis": "snare",
      "beneficiary": null,
      "victim": "agent_in_coupled_system",
      "downstream_of": [
        "indexical_relativity_of_extraction",
        "asymmetric_duty_structure"
      ],
      "feeds_into": [],
      "centrality_score": 5,
      "selected": true,
      "generation_order": 3,
      "selection_reason": "Downstream synthesis demonstrating emergent trap dynamics; shows how moderate constraints combine to exceed Snare threshold; captures recursive blocking mechanism"
    },
    {
      "claim_id": "naturalization_through_power",
      "human_readable": "Naturalization Through Power (High Epsilon Experienced as Low Chi)",
      "structural_delta": "Organized collectives experience high-epsilon constraints (0.75-0.85) as low-chi Ropes (0.30-0.306) while powerless individuals experience same constraints as Snares (0.96-1.15)",
      "primary_observable": "Epsilon-chi divergence ratio; naturalization investigation triggers (epsilon > 0.45 with chi \u2264 0.35); beneficiary awareness vs victim awareness of extraction",
      "epsilon_bin": "mod",
      "hypothesis": "tangled_rope",
      "beneficiary": "organized_collective",
      "victim": "powerless_individual",
      "downstream_of": [
        "indexical_relativity_of_extraction"
      ],
      "feeds_into": [
        "constraint_coupling_amplification"
      ],
      "centrality_score": 4,
      "selected": false,
      "generation_order": null,
      "selection_reason": "Deferred: captured implicitly through indexical_relativity_of_extraction mountain and asymmetric_duty_structure tangled_rope; adding as separate axis would over-split the power-position mechanism"
    },
    {
      "claim_id": "identity_locked_exit_trap",
      "human_readable": "Identity-Locked Exit Trap (Constraint Blocks Its Own Abandonment)",
      "structural_delta": "Constraints with identity-locked exit parameter prevent their own transformation because abandonment requires ego death, creating self-reinforcing extraction loop",
      "primary_observable": "Transformation rule blocking patterns; exit option availability vs identity fusion strength; sunk cost accumulation rate",
      "epsilon_bin": "mod",
      "hypothesis": "piton",
      "beneficiary": null,
      "victim": null,
      "downstream_of": [
        "indexical_relativity_of_extraction"
      ],
      "feeds_into": [
        "constraint_coupling_amplification"
      ],
      "centrality_score": 3,
      "selected": false,
      "generation_order": null,
      "selection_reason": "Deferred: mechanism is demonstrated within constraint_coupling_amplification snare; identity-lock is parameter of coupling rather than independent axis"
    },
    {
      "claim_id": "suppression_mechanism_variance",
      "human_readable": "Suppression Mechanism Variance (Internal vs External Enforcement)",
      "structural_delta": "Same constraint enforced through internal mechanisms (guilt, identity threat) vs external mechanisms (social judgment, institutional sanctions) produces different suppression values (0.85 vs 0.40) without changing chi",
      "primary_observable": "Suppression parameter values; enforcement mechanism type (internal/external); transformation rule effects on suppression vs chi",
      "epsilon_bin": "low",
      "hypothesis": "rope",
      "beneficiary": null,
      "victim": null,
      "downstream_of": [],
      "feeds_into": [
        "asymmetric_duty_structure"
      ],
      "centrality_score": 2,
      "selected": false,
      "generation_order": null,
      "selection_reason": "Deferred: suppression variance is parameter detail within asymmetric_duty_structure; does not constitute independent structural axis"
    }
  ],
  "generation_sequence": [
    "indexical_relativity_of_extraction",
    "asymmetric_duty_structure",
    "constraint_coupling_amplification"
  ],
  "deferred_axes": [
    {
      "claim_id": "naturalization_through_power",
      "structural_delta": "Organized collectives experience high-epsilon constraints as low-chi Ropes while powerless individuals experience same constraints as Snares",
      "hypothesis": "tangled_rope",
      "deferral_reason": "Captured implicitly through indexical_relativity_of_extraction mountain (which establishes power-position mechanism) and asymmetric_duty_structure tangled_rope (which demonstrates beneficiary/victim split); adding as separate axis would over-split the power-position dynamics"
    },
    {
      "claim_id": "identity_locked_exit_trap",
      "structural_delta": "Constraints with identity-locked exit prevent their own transformation through self-reinforcing extraction",
      "hypothesis": "piton",
      "deferral_reason": "Mechanism is demonstrated within constraint_coupling_amplification snare; identity-lock functions as parameter of coupling dynamics rather than independent structural axis"
    },
    {
      "claim_id": "suppression_mechanism_variance",
      "structural_delta": "Internal vs external enforcement mechanisms produce different suppression values without changing chi",
      "hypothesis": "rope",
      "deferral_reason": "Suppression variance is parameter detail within asymmetric_duty_structure; does not constitute independent structural axis with distinct observable"
    }
  ],
  "omegas": [
    {
      "id": "omega_chi_calculation_validity",
      "description": "Are the chi threshold boundaries (0.35 Rope ceiling, 0.46 Tangled floor, 0.70 Snare floor) empirically validated or theoretical constructs? Edge case at chi=0.44 suggests threshold gaps may indicate measurement artifact rather than true categorical boundaries.",
      "source": "Constraint C\u2084 edge case analysis; chi value falls between Rope ceiling and Tangled floor"
    },
    {
      "id": "omega_coupling_coefficient",
      "description": "How is the coupling coefficient (0.7 in TR\u2085) determined? Is this empirical measurement, theoretical derivation, or calibration parameter? Effective chi calculation depends critically on this value but formalization provides no derivation method.",
      "source": "Transformation Rule TR\u2085 coupling calculation; coefficient appears stipulated rather than derived"
    },
    {
      "id": "omega_boltzmann_test_interpretation",
      "description": "All constraints fail Boltzmann test (classification varies by power position), indicating constructed rather than natural constraints. But does this mean the constraint ontology itself is constructed, or only that social constraints are being analyzed? Mountain classification for indexical_relativity may be inconsistent with universal Boltzmann failure.",
      "source": "Universal Boltzmann test failure across all five constraints; tension with mountain classification of indexical_relativity_of_extraction"
    },
    {
      "id": "omega_transformation_rule_completeness",
      "description": "Five transformation rules provided, but constraint network has 5 constraints with multiple blocking relationships. Are there additional transformation rules not documented? Specifically: rules for breaking C\u2083 exclusion, rules for resolving C\u2084 isolation, rules for collective organization beyond TR\u2081.",
      "source": "Transformation rule inventory vs constraint blocking patterns; TR\u2084 lists four blocking constraints but only TR\u2081 addresses breaking blocks"
    }
  ],
  "fracture_scan": {
    "f14_tunnel_vision": false,
    "f15_premature_closure": false,
    "f03_hasty_generalization": true,
    "f34_epistemic_trespass": false,
    "f01_premise_drift": false,
    "notes": "F03 detected: indexical_relativity_of_extraction classified as mountain (physical/mathematical constraint) but all constraints fail Boltzmann test (classification varies by power position), suggesting constructed rather than natural law. Generated omega_boltzmann_test_interpretation to bound this tension. Mountain classification may be valid if indexical relativity is mathematical property of chi calculation itself rather than social constraint, but this requires clarification."
  }
}