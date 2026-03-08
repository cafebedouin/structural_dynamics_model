{
  "protocol": "UKE_SCOPE",
  "version": "2.0-json",
  "domain": "Organizational Dynamics / Labor Relations / Institutional Power",
  "family_id": "collective_action_under_architectural_constraint",
  "topic_summary": "Analysis of a labor collective action (the Compact) formed by women workers in a spatially segregated industrial facility, examining how architectural constraint enables both oppression and organized resistance, and how institutional accommodation preserves underlying power asymmetries while appearing to resolve conflict.",
  "extraction_summary": {
    "entity_count": 6,
    "claim_count": 5,
    "tension_count": 3,
    "mechanism_count": 4,
    "absence_count": 2,
    "key_entities": [
      "The Compact (collective withholding action by 47 women workers)",
      "Consortium (institutional authority controlling station operations and policy)",
      "Extractor population (workers in high-gravity drum sections, surface-exposed)",
      "Coordinator population (management in low-gravity hub, minimal exposure)",
      "The Separation (architectural division enforcing spatial and operational segregation)",
      "Approval pathway system (bureaucratic mechanism replacing absolute prohibition)"
    ],
    "key_tensions": [
      "Collective leverage vs individual cost (Compact members sustaining organized action despite personal suffering)",
      "Architectural constraint as oppression vs foundation (same spatial division that traps also enables coordination)",
      "Policy modification as victory vs accommodation (equal access to denial-prone system)"
    ]
  },
  "axes": [
    {
      "claim_id": "architectural_constraint_as_dual_substrate",
      "human_readable": "Architectural Constraint as Dual Substrate (Oppression Infrastructure Becomes Coordination Foundation)",
      "structural_delta": "Physical/spatial constraint that simultaneously enforces hierarchy and enables collective organization by concentrating affected population",
      "primary_observable": "Spatial segregation metrics; coordination capacity before/after architectural division; collective action success rate in segregated vs integrated populations",
      "epsilon_bin": "v_low",
      "hypothesis": "mountain",
      "beneficiary": null,
      "victim": null,
      "downstream_of": [],
      "feeds_into": [
        "collective_action_as_leverage_conversion",
        "bureaucratic_accommodation_as_extraction_persistence"
      ],
      "centrality_score": 3,
      "selected": true,
      "generation_order": 1,
      "selection_reason": "Foundational mountain providing physical substrate for all downstream dynamics; captures irreducible architectural physics"
    },
    {
      "claim_id": "collective_action_as_leverage_conversion",
      "human_readable": "Collective Action as Leverage Conversion (Coordinated Withholding Transforms Powerlessness into Negotiating Position)",
      "structural_delta": "Transformation of individual powerlessness into collective bargaining capacity through sustained coordinated action",
      "primary_observable": "Cohesion maintenance rate over time; institutional response to disruption; policy modification as function of collective action duration",
      "epsilon_bin": "mod",
      "hypothesis": "rope",
      "beneficiary": null,
      "victim": null,
      "downstream_of": [
        "architectural_constraint_as_dual_substrate"
      ],
      "feeds_into": [
        "bureaucratic_accommodation_as_extraction_persistence"
      ],
      "centrality_score": 4,
      "selected": true,
      "generation_order": 2,
      "selection_reason": "Core coordination mechanism; structurally distinct from architectural substrate; feeds synthesis node"
    },
    {
      "claim_id": "bureaucratic_accommodation_as_extraction_persistence",
      "human_readable": "Bureaucratic Accommodation as Extraction Persistence (Institutional Modification Preserves Asymmetry Through Process)",
      "structural_delta": "Policy modification that appears to resolve conflict while maintaining structural inequality through procedural barriers and differential exposure costs",
      "primary_observable": "Approval rate stratification by worker category; application cost burden distribution; denial justification patterns; architectural access unchanged",
      "epsilon_bin": "high",
      "hypothesis": "tangled_rope",
      "beneficiary": "institutional_authority",
      "victim": "surface_exposed_workers",
      "downstream_of": [
        "architectural_constraint_as_dual_substrate",
        "collective_action_as_leverage_conversion"
      ],
      "feeds_into": [],
      "centrality_score": 7,
      "selected": true,
      "generation_order": 3,
      "selection_reason": "Highest centrality; downstream synthesis capturing institutional response dynamics; tangled_rope revealing extraction mechanism"
    },
    {
      "claim_id": "biological_drive_as_weaponizable_constraint",
      "human_readable": "Biological Drive as Weaponizable Constraint (Reproductive Impulse Leveraged for Collective Bargaining)",
      "structural_delta": "Biological imperative transformed into strategic withholding mechanism through collective coordination",
      "primary_observable": "Pulse frequency/intensity; withholding compliance rate; psychological cost metrics; defection triggers",
      "epsilon_bin": "low",
      "hypothesis": "rope",
      "beneficiary": null,
      "victim": null,
      "downstream_of": [],
      "feeds_into": [
        "collective_action_as_leverage_conversion"
      ],
      "centrality_score": 2,
      "selected": false,
      "generation_order": null,
      "selection_reason": "Deferred: captured implicitly through collective_action_as_leverage_conversion; biological substrate less structurally central than architectural"
    },
    {
      "claim_id": "exhaustion_as_collective_action_limit",
      "human_readable": "Exhaustion as Collective Action Limit (Sustained Coordination Cost Erodes Leverage Over Time)",
      "structural_delta": "Temporal degradation of collective cohesion through accumulated individual cost of sustained action",
      "primary_observable": "Vote margin trends; enforcement difficulty over time; defection risk assessment; psychological support utilization",
      "epsilon_bin": "mod",
      "hypothesis": "piton",
      "beneficiary": null,
      "victim": null,
      "downstream_of": [
        "collective_action_as_leverage_conversion"
      ],
      "feeds_into": [
        "bureaucratic_accommodation_as_extraction_persistence"
      ],
      "centrality_score": 3,
      "selected": false,
      "generation_order": null,
      "selection_reason": "Deferred: temporal constraint on rope dynamics; important but structurally downstream of core coordination mechanism"
    },
    {
      "claim_id": "observer_position_determines_system_legibility",
      "human_readable": "Observer Position Determines System Legibility (Architectural Location Shapes Perception of Constraint)",
      "structural_delta": "Physical position within segregated architecture determines which aspects of power structure are visible vs invisible",
      "primary_observable": "Perception divergence by spatial location; policy assessment variance by observer position; architectural boundary crossing frequency",
      "epsilon_bin": "low",
      "hypothesis": "scaffold",
      "beneficiary": null,
      "victim": null,
      "downstream_of": [
        "architectural_constraint_as_dual_substrate"
      ],
      "feeds_into": [],
      "centrality_score": 1,
      "selected": false,
      "generation_order": null,
      "selection_reason": "Deferred: epistemological scaffold; important for narrative perspective but lower structural centrality than core dynamics"
    }
  ],
  "generation_sequence": [
    "architectural_constraint_as_dual_substrate",
    "collective_action_as_leverage_conversion",
    "bureaucratic_accommodation_as_extraction_persistence"
  ],
  "deferred_axes": [
    {
      "claim_id": "biological_drive_as_weaponizable_constraint",
      "structural_delta": "Biological imperative transformed into strategic withholding mechanism",
      "hypothesis": "rope",
      "deferral_reason": "Captured implicitly through collective_action_as_leverage_conversion; biological substrate less structurally central than architectural foundation"
    },
    {
      "claim_id": "exhaustion_as_collective_action_limit",
      "structural_delta": "Temporal degradation of collective cohesion through accumulated cost",
      "hypothesis": "piton",
      "deferral_reason": "Temporal constraint on rope dynamics; structurally downstream of core coordination mechanism; important but secondary to primary topology"
    },
    {
      "claim_id": "observer_position_determines_system_legibility",
      "structural_delta": "Physical position within architecture determines constraint visibility",
      "hypothesis": "scaffold",
      "deferral_reason": "Epistemological scaffold with lower graph centrality; narrative perspective device rather than core structural dynamic"
    }
  ],
  "omegas": [
    {
      "id": "omega_accommodation_stability",
      "description": "Does bureaucratic accommodation stabilize or destabilize the system long-term? Equal access to denial-prone process may generate new organizing pressure or may fragment collective capacity through individualized application burden.",
      "source": "Axis 3 downstream uncertainty; narrative suggests both outcomes possible"
    },
    {
      "id": "omega_architectural_modification",
      "description": "Can the Separation itself be targeted for collective action, or is spatial constraint too fundamental to institutional operation? Narrative shows architecture as both oppression and coordination substrate\u2014unclear if modification is strategically viable.",
      "source": "Axis 1 mountain classification; Dark Matter Probe 2 (architectural constraint as unexamined background)"
    },
    {
      "id": "omega_extraction_visibility_threshold",
      "description": "At what point does making extraction mechanisms visible generate sufficient pressure for structural change vs merely documenting inequality? Narrative shows documentation without elimination\u2014threshold for transformation unclear.",
      "source": "Axis 3 tangled_rope dynamics; F03 concern about extraction persistence despite visibility"
    },
    {
      "id": "omega_collective_memory_persistence",
      "description": "Does successful collective action create durable organizing capacity or does dissolution fragment knowledge? Narrative suggests 'they know they can' but unclear if this knowledge survives individual contract completion and population turnover.",
      "source": "Deferred axis exhaustion_as_collective_action_limit; temporal degradation concern"
    }
  ],
  "fracture_scan": {
    "f14_tunnel_vision": false,
    "f15_premature_closure": false,
    "f03_hasty_generalization": true,
    "f34_epistemic_trespass": false,
    "f01_premise_drift": false,
    "notes": "F03 detected: architectural_constraint_as_dual_substrate classified as mountain (irreducible physical constraint) but narrative suggests spatial segregation is institutional design choice, potentially modifiable through policy. Generated omega_architectural_modification to bound this uncertainty. Classification retained as mountain because within narrative's constraint topology, architecture functions as unchangeable substrate\u2014but this may be narrative framing rather than structural truth."
  }
}