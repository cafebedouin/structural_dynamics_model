{
  "protocol": "UKE_SCOPE",
  "version": "2.0-json",
  "domain": "Social Systems / Institutional Dynamics / Power Asymmetry",
  "family_id": "reputation_duty_agency_extraction",
  "topic_summary": "Analysis of constraint dynamics in systems where reputation mechanisms, filial obligation structures, and professional codes interact to produce asymmetric extraction patterns. The formalization examines how identical institutional rules generate different constraint experiences based on power position, exit options, and time horizons, with particular focus on how coordination mechanisms can function simultaneously as oppression for trapped actors and navigation tools for mobile actors.",
  "extraction_summary": {
    "entity_count": 4,
    "claim_count": 6,
    "tension_count": 3,
    "mechanism_count": 4,
    "absence_count": 2,
    "key_entities": [
      "Powerless actor (trapped in reputation system, biographical time horizon, local scope)",
      "Moderate-power actors (mobile within system, can deflect costs, local scope)",
      "Professional code (self-imposed discipline structure)",
      "Reputation coordination mechanism (distributed social enforcement system)"
    ],
    "key_tensions": [
      "Same institutional rule experienced as coordination vs extraction based on power position",
      "Professional code as existential identity vs instrumental technique",
      "Filial duty as reciprocal structure vs asymmetric burden"
    ]
  },
  "axes": [
    {
      "claim_id": "reputation_as_distributed_enforcement",
      "human_readable": "Reputation as Distributed Enforcement Mechanism",
      "structural_delta": "Social reputation systems that coordinate resource allocation through distributed enforcement, where the same mechanism provides useful information for mobile actors while functioning as inescapable trap for powerless actors",
      "primary_observable": "Differential constraint experience (\u03c7) across power positions: powerless actors experience high extraction (\u03c7 > 0.66, Snare classification) while moderate-power actors experience moderate extraction (0.46 < \u03c7 < 0.66, Tangled Rope classification) from identical institutional rules",
      "epsilon_bin": "high",
      "hypothesis": "tangled_rope",
      "beneficiary": "mobile_actors_with_exit_options",
      "victim": "trapped_actors_without_negotiation_leverage",
      "downstream_of": [],
      "feeds_into": [
        "duty_contamination_by_extraction",
        "collective_action_threshold"
      ],
      "centrality_score": 5,
      "selected": true,
      "generation_order": 1,
      "selection_reason": "Highest centrality; core mechanism generating asymmetric extraction; feeds into both downstream synthesis nodes"
    },
    {
      "claim_id": "duty_contamination_by_extraction",
      "human_readable": "Duty Contamination by Adjacent Extraction Systems",
      "structural_delta": "How high-purity reciprocal obligation structures (family duty, professional codes) experience effective purity degradation when contaminated by adjacent extractive mechanisms, transforming coordination into extraction without changing the duty structure itself",
      "primary_observable": "Network contamination coefficient: intrinsic purity of duty structure remains stable while effective purity drops due to coupling with degraded adjacent constraint; measurable through workaround behaviors (lying, concealment) and justification drift (economic calculation replacing reciprocity logic)",
      "epsilon_bin": "mod",
      "hypothesis": "tangled_rope",
      "beneficiary": "enforcers_of_contaminating_system",
      "victim": "actors_subject_to_both_constraints",
      "downstream_of": [
        "reputation_as_distributed_enforcement"
      ],
      "feeds_into": [],
      "centrality_score": 4,
      "selected": true,
      "generation_order": 3,
      "selection_reason": "Downstream synthesis node; captures network effects and purity degradation dynamics; highest structural distinctiveness from axis 1 (different observable: contamination vs direct extraction)"
    },
    {
      "claim_id": "collective_action_threshold",
      "human_readable": "Collective Action Threshold Under Distributed Enforcement",
      "structural_delta": "The minimum coordination required for trapped actors to transform distributed enforcement mechanisms from Snare (inescapable) to Rope (navigable), where collective organization changes power position (\u03c0) and enables exit options previously unavailable to isolated individuals",
      "primary_observable": "Index transformation mechanics: powerless (\u03c0=1.5, E=trapped) \u2192 organized (\u03c0=0.4, E=constrained) produces \u03c7 recalculation that crosses Snare/Tangled boundary (0.66 threshold); measurable through collective size, communication density, and institutional suppression resistance",
      "epsilon_bin": "mod",
      "hypothesis": "scaffold",
      "beneficiary": null,
      "victim": null,
      "downstream_of": [
        "reputation_as_distributed_enforcement"
      ],
      "feeds_into": [],
      "centrality_score": 3,
      "selected": true,
      "generation_order": 2,
      "selection_reason": "Transformation pathway for axis 1; scaffold hypothesis provides intervention logic; different beneficiary structure (null vs specific) and different observable (threshold dynamics vs contamination) from other selected axes"
    },
    {
      "claim_id": "professional_code_purity_drift",
      "human_readable": "Professional Code Purity Drift Across Generations",
      "structural_delta": "How self-imposed professional standards experience purity degradation when coordination function is mediated by technology, transforming existential discipline into instrumental technique without changing surface compliance",
      "primary_observable": "Purity coefficient over generational time: coordination value drops (technology substitutes for skill), theater increases (ritual without understanding), measurable through crisis response when technology fails",
      "epsilon_bin": "low",
      "hypothesis": "piton",
      "beneficiary": null,
      "victim": null,
      "downstream_of": [],
      "feeds_into": [
        "duty_contamination_by_extraction"
      ],
      "centrality_score": 2,
      "selected": false,
      "generation_order": null,
      "selection_reason": "Deferred: lower centrality than selected axes; purity drift captured indirectly through contamination dynamics in axis 2; piton hypothesis less structurally central than tangled_rope and scaffold"
    },
    {
      "claim_id": "false_mountain_naturalization",
      "human_readable": "False Mountain Naturalization (Type I Error Pattern)",
      "structural_delta": "How constructed constraints with distributed enforcement appear as natural laws to trapped actors, generating Type I errors (treating changeable systems as unchangeable) that waste agency and perpetuate unnecessary suffering",
      "primary_observable": "Error manifestation patterns: ontological framing ('I am X') vs social framing ('They call me X'); absence of resistance attempts; attribution to personal failing vs system design; measurable through Boltzmann test failures (unequal binding, scale-dependence, enforcement requirements)",
      "epsilon_bin": "mod",
      "hypothesis": "snare",
      "beneficiary": "system_maintainers",
      "victim": "trapped_actors_misclassifying_constraint",
      "downstream_of": [
        "reputation_as_distributed_enforcement"
      ],
      "feeds_into": [],
      "centrality_score": 2,
      "selected": false,
      "generation_order": null,
      "selection_reason": "Deferred: error pattern rather than primary constraint dynamic; captured as consequence of axis 1 (reputation mechanism generates naturalization); lower centrality than collective_action_threshold"
    },
    {
      "claim_id": "index_dependent_classification",
      "human_readable": "Index-Dependent Constraint Classification",
      "structural_delta": "How identical institutional rules receive different constraint type classifications (Mountain, Rope, Tangled Rope, Snare) based on observer's index position (power, time horizon, exit options, scope), where both classifications are objectively correct from their respective indices",
      "primary_observable": "Indexical variance (\u0394\u03c7) between actors subject to same rule: power differential (\u03c0 ratio), exit option difference (trapped vs mobile), time horizon difference (biographical vs analytical); measurable through \u03c7 recalculation across indices",
      "epsilon_bin": "v_low",
      "hypothesis": "mountain",
      "beneficiary": null,
      "victim": null,
      "downstream_of": [],
      "feeds_into": [
        "reputation_as_distributed_enforcement",
        "duty_contamination_by_extraction",
        "collective_action_threshold"
      ],
      "centrality_score": 4,
      "selected": false,
      "generation_order": null,
      "selection_reason": "Deferred: meta-level classification principle rather than substantive constraint; feeds into all axes but is methodological foundation rather than generation target; mountain hypothesis makes it structurally simpler than selected tangled_rope/scaffold axes"
    }
  ],
  "generation_sequence": [
    "reputation_as_distributed_enforcement",
    "collective_action_threshold",
    "duty_contamination_by_extraction"
  ],
  "deferred_axes": [
    {
      "claim_id": "professional_code_purity_drift",
      "structural_delta": "Self-imposed professional standards experiencing purity degradation through technology mediation",
      "hypothesis": "piton",
      "deferral_reason": "Lower centrality (2 vs 3-5 for selected axes); purity drift dynamics captured indirectly through contamination analysis in axis 2; piton hypothesis less structurally central than tangled_rope and scaffold"
    },
    {
      "claim_id": "false_mountain_naturalization",
      "structural_delta": "Constructed constraints appearing as natural laws to trapped actors",
      "hypothesis": "snare",
      "deferral_reason": "Error pattern rather than primary constraint dynamic; naturalization is consequence of reputation mechanism (axis 1) rather than independent structural delta; lower centrality than collective_action_threshold"
    },
    {
      "claim_id": "index_dependent_classification",
      "structural_delta": "Identical rules receiving different constraint classifications based on observer index",
      "hypothesis": "mountain",
      "deferral_reason": "Meta-level methodological principle rather than substantive constraint for generation; feeds into all axes as foundation but is not itself a constraint story target; mountain hypothesis makes it structurally simpler to analyze than selected downstream synthesis nodes"
    }
  ],
  "omegas": [
    {
      "id": "omega_collective_size_threshold",
      "description": "What is the minimum collective size required for transformation rule TR1 (collective organization) to succeed? The formalization specifies 'collective_size \u2265 threshold' but does not quantify the threshold. This depends on enforcement density, communication costs, and institutional suppression capacity\u2014all context-dependent variables.",
      "source": "Transformation Rule TR1 preconditions; Dark Matter Probe 2 (Absence Inventory)"
    },
    {
      "id": "omega_contamination_reversibility",
      "description": "Can duty structures recover intrinsic purity after contamination, or is effective purity degradation permanent? The formalization shows contamination mechanics but does not specify decontamination pathways. If contamination is irreversible, axis 2 may be Snare rather than Tangled Rope.",
      "source": "Network Contamination Arc (Stage 4); F03 (Hasty Generalization) check on Tangled Rope classification"
    },
    {
      "id": "omega_bir_boundary_conditions",
      "description": "Under what conditions does Bounded Institutional Rationality (BIR) break down into Perfect Institutional Rationality (PIR)? The formalization selects BIR but does not specify the boundary. If information asymmetry drops or enforcement becomes algorithmic, reputation mechanism may shift from Tangled Rope to Snare.",
      "source": "Institutional Rationality Model selection (Section IV); attractor selection sensitivity"
    },
    {
      "id": "omega_seeded_possibility_activation",
      "description": "What triggers underground transformation to surface? The formalization selects Seeded Possibility attractor but does not specify activation conditions. Manolin's loyalty 'seeds future change' but the mechanism for t=5 \u2192 t=\u221e transition is unspecified.",
      "source": "Terminal Attractor Selection (Section V); transformation timeline ambiguity"
    }
  ],
  "fracture_scan": {
    "f14_tunnel_vision": false,
    "f15_premature_closure": false,
    "f03_hasty_generalization": true,
    "f34_epistemic_trespass": false,
    "f01_premise_drift": false,
    "notes": "F03 detected: axis 2 (duty_contamination_by_extraction) classified as Tangled Rope (\u03b5=mod, 0.31-0.55 range) but contamination reversibility is unspecified. If contamination is permanent, effective purity cannot be restored and classification should be Snare (\u03b5=high). Generated omega_contamination_reversibility to bound this uncertainty. Additionally, index_dependent_classification deferred as Mountain but its invariance across political systems is assumed rather than demonstrated\u2014generated omega_bir_boundary_conditions to capture this."
  }
}