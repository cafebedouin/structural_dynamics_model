{
  "protocol": "UKE_SCOPE",
  "version": "2.0-json",
  "domain": "Organizational Systems / Labor Economics / Institutional Extraction",
  "family_id": "bureaucratic_extraction_topology",
  "topic_summary": "Analysis of a narrative depicting systematic extraction within a copying bureau, where quota escalation, debt accumulation, and structural barriers to coordination create a self-reinforcing trap that breaks workers while framing failure as individual inadequacy rather than systemic design.",
  "extraction_summary": {
    "entity_count": 6,
    "claim_count": 8,
    "tension_count": 4,
    "mechanism_count": 5,
    "absence_count": 3,
    "key_entities": [
      "Wei (clerk experiencing hand failure and quota shortfall)",
      "Feng (mentor clerk with failed hands after 40 years)",
      "Chen (supervisor who enforces then questions the system)",
      "The Board (retired governors setting policy)",
      "The Long Hall (physical structure housing 300 clerks)",
      "The Archive (institutional entity preserving empire's memory)"
    ],
    "key_tensions": [
      "Individual merit narrative vs structural extraction reality",
      "Supervisor enforcement role vs recognition of systemic harm",
      "Coordination necessity vs coordination prevention by design",
      "Document preservation mission vs cost-cutting degradation"
    ],
    "key_mechanisms": [
      "Quota escalation (increases faster than capacity adaptation)",
      "Debt trap (loans at interest to cover shortfalls, compounding faster than repayment)",
      "Spatial atomization (300 desks prevent coordination; post-work scatter prevents organizing)",
      "Visibility punishment (wall examination creates peer enforcement of standards)",
      "Replacement logic (dismissal and immediate replacement prevents collective leverage)"
    ],
    "key_absences": [
      "No collective bargaining structure or worker representation",
      "No ergonomic accommodation or hand therapy despite predictable injury",
      "No long-term employment security despite decades of service"
    ]
  },
  "axes": [
    {
      "claim_id": "quota_ratchet_asymmetry",
      "human_readable": "Quota Ratchet Asymmetry (Unidirectional Performance Extraction)",
      "structural_delta": "Performance targets increase automatically in response to capacity improvements but never decrease in response to capacity decline, creating asymmetric extraction that accelerates failure",
      "primary_observable": "Ratio of quota increase frequency to quota decrease frequency; correlation between efficiency improvements (e.g., better brushes) and quota adjustments vs correlation between capacity decline (hand failure rates) and quota adjustments",
      "epsilon_bin": "mod",
      "hypothesis": "tangled_rope",
      "beneficiary": "institutional_budget_managers",
      "victim": "front_line_workers",
      "downstream_of": [],
      "feeds_into": [
        "debt_trap_compounding",
        "coordination_barrier_topology"
      ],
      "centrality_score": 5,
      "selected": true,
      "generation_order": 1,
      "selection_reason": "Upstream mechanism driving both debt accumulation and coordination necessity; highest structural distinctiveness as pure extraction dynamic"
    },
    {
      "claim_id": "debt_trap_compounding",
      "human_readable": "Debt Trap Compounding (Survival Borrowing as Permanent Subordination)",
      "structural_delta": "Loans offered to cover performance shortfalls compound faster than wages can repay them, converting temporary assistance into permanent extraction and binding workers to the institution through negative equity",
      "primary_observable": "Interest rate on institutional loans vs wage growth rate; time-to-default distribution for borrowers; proportion of wages consumed by debt service over time",
      "epsilon_bin": "high",
      "hypothesis": "snare",
      "beneficiary": "loan_administrators",
      "victim": "below_quota_workers",
      "downstream_of": [
        "quota_ratchet_asymmetry"
      ],
      "feeds_into": [
        "coordination_barrier_topology"
      ],
      "centrality_score": 4,
      "selected": true,
      "generation_order": 2,
      "selection_reason": "Downstream synthesis of quota pressure into financial subordination; distinct observable (debt service ratio) and victim class (borrowers specifically)"
    },
    {
      "claim_id": "coordination_barrier_topology",
      "human_readable": "Coordination Barrier Topology (Spatial and Temporal Atomization Preventing Collective Action)",
      "structural_delta": "Physical workspace design (open surveillance, no private spaces) combined with temporal dispersion (immediate post-work scatter, no shared non-work time) structurally prevents the trust-building and planning required for collective action, despite shared awareness of exploitation",
      "primary_observable": "Ratio of surveillance-to-privacy in workspace; post-work geographic dispersion rate; time required for organizing vs time available for organizing; detection speed of coordination attempts vs coordination completion speed",
      "epsilon_bin": "v_low",
      "hypothesis": "mountain",
      "beneficiary": null,
      "victim": null,
      "downstream_of": [
        "quota_ratchet_asymmetry",
        "debt_trap_compounding"
      ],
      "feeds_into": [],
      "centrality_score": 6,
      "selected": true,
      "generation_order": 3,
      "selection_reason": "Highest centrality as downstream synthesis node; captures why extraction persists despite universal awareness; mountain classification reflects physical/architectural constraint rather than policy choice"
    },
    {
      "claim_id": "merit_narrative_inversion",
      "human_readable": "Merit Narrative Inversion (Structural Failure Reframed as Individual Inadequacy)",
      "structural_delta": "System-generated failure (quota increases exceeding biological capacity) is systematically reinterpreted as individual moral failure (lack of dedication), inverting causality and preventing recognition of structural extraction",
      "primary_observable": "Frequency of 'dedication' framing in dismissal justifications; correlation between biological capacity variance (hand strength) and success vs correlation between effort and success; supervisor belief in merit narrative vs objective performance determinants",
      "epsilon_bin": "high",
      "hypothesis": "tangled_rope",
      "beneficiary": "system_administrators",
      "victim": "failing_workers",
      "downstream_of": [
        "quota_ratchet_asymmetry"
      ],
      "feeds_into": [
        "coordination_barrier_topology"
      ],
      "centrality_score": 3,
      "selected": false,
      "generation_order": null,
      "selection_reason": "Deferred: ideological mechanism downstream of quota_ratchet_asymmetry; captured implicitly in generation of upstream axes; lower structural distinctiveness than coordination_barrier_topology"
    },
    {
      "claim_id": "replacement_velocity_leverage",
      "human_readable": "Replacement Velocity as Leverage Nullification (Instant Substitutability Preventing Strike Threat)",
      "structural_delta": "Maintained surplus labor pool plus minimal training requirements enable immediate replacement of dismissed workers, eliminating the collective leverage that would otherwise accrue from coordination",
      "primary_observable": "Time-to-replacement after dismissal; training duration for new clerks; size of waiting list for positions; strike threat credibility (measured by institutional response to coordination attempts)",
      "epsilon_bin": "low",
      "hypothesis": "rope",
      "beneficiary": null,
      "victim": null,
      "downstream_of": [],
      "feeds_into": [
        "coordination_barrier_topology"
      ],
      "centrality_score": 2,
      "selected": false,
      "generation_order": null,
      "selection_reason": "Deferred: upstream to coordination_barrier but lower centrality; labor market condition rather than organizational design; captured as context in coordination_barrier generation"
    },
    {
      "claim_id": "mission_cost_contradiction",
      "human_readable": "Mission-Cost Contradiction (Preservation Goal Undermined by Budget Extraction)",
      "structural_delta": "Institutional mission (permanent document preservation) is structurally contradicted by cost-minimization (degrading paper, overwork-induced errors), revealing extraction priority over stated purpose",
      "primary_observable": "Document degradation rate; paper quality vs budget allocation; error rate correlation with quota pressure; mission statement vs resource allocation pattern",
      "epsilon_bin": "mod",
      "hypothesis": "tangled_rope",
      "beneficiary": "budget_administrators",
      "victim": "future_document_users",
      "downstream_of": [],
      "feeds_into": [],
      "centrality_score": 1,
      "selected": false,
      "generation_order": null,
      "selection_reason": "Deferred: parallel extraction axis but lower centrality; affects document quality rather than worker extraction directly; omega material for institutional purpose analysis"
    }
  ],
  "generation_sequence": [
    "quota_ratchet_asymmetry",
    "debt_trap_compounding",
    "coordination_barrier_topology"
  ],
  "deferred_axes": [
    {
      "claim_id": "merit_narrative_inversion",
      "structural_delta": "Structural failure reframed as individual inadequacy through dedication discourse",
      "hypothesis": "tangled_rope",
      "deferral_reason": "Ideological mechanism downstream of quota_ratchet_asymmetry; captured implicitly in upstream generation; lower structural distinctiveness than selected axes"
    },
    {
      "claim_id": "replacement_velocity_leverage",
      "structural_delta": "Instant worker substitutability eliminates collective leverage from coordination",
      "hypothesis": "rope",
      "deferral_reason": "Labor market condition rather than organizational design; upstream to coordination_barrier but lower centrality; context rather than primary constraint"
    },
    {
      "claim_id": "mission_cost_contradiction",
      "structural_delta": "Preservation mission undermined by cost-minimization in paper quality and worker capacity",
      "hypothesis": "tangled_rope",
      "deferral_reason": "Parallel extraction axis affecting document quality rather than worker extraction directly; lower graph centrality; omega material for institutional purpose analysis"
    }
  ],
  "omegas": [
    {
      "id": "omega_biological_variance_as_selection",
      "description": "Is hand strength variance a Mountain (biological constraint) or does the system's quota design convert natural variance into a Snare (those with weaker hands are structurally selected for failure)? The narrative suggests the latter but classification depends on whether alternative quota structures could accommodate variance.",
      "source": "Dark Matter Probe 2 (Absence Inventory): no ergonomic accommodation despite predictable injury pattern"
    },
    {
      "id": "omega_supervisor_complicity_gradient",
      "description": "Chen's arc suggests supervisor enforcement is not uniform extraction but contains a gradient from true belief to coerced compliance. Is supervisor role a Rope (coordination problem) or Tangled_Rope (some supervisors benefit from enforcement)? Beneficiary analysis unclear.",
      "source": "Dark Matter Probe 3 (Beneficiary Scan): Chen's late awakening suggests belief rather than cynical extraction, but other supervisors may differ"
    },
    {
      "id": "omega_coordination_barrier_classification",
      "description": "Classified coordination_barrier_topology as Mountain (physical/architectural constraint) but could be Piton (policy choice to design workspace for surveillance). Depends on whether alternative spatial arrangements were feasible within institutional constraints.",
      "source": "F03 (Hasty Generalization): Mountain classification assumes spatial design is fixed rather than chosen"
    },
    {
      "id": "omega_narrative_vs_system",
      "description": "Analysis extracts constraint topology from a fictional narrative. Real-world bureaucratic extraction may have different observables, different beneficiary structures, different coordination barriers. Epsilon values are narrative-relative, not empirically grounded.",
      "source": "F34 (Epistemic Trespass): generalizing from literary representation to organizational systems without domain expertise in labor economics or institutional analysis"
    }
  ],
  "fracture_scan": {
    "f14_tunnel_vision": false,
    "f15_premature_closure": false,
    "f03_hasty_generalization": true,
    "f34_epistemic_trespass": true,
    "f01_premise_drift": false,
    "notes": "F03 detected: coordination_barrier_topology classified as Mountain but spatial design may be policy choice (Piton) rather than physical constraint. Generated omega_coordination_barrier_classification. F34 detected: extracting organizational dynamics from fictional narrative without domain expertise in labor economics; epsilon values are narrative-relative. Generated omega_narrative_vs_system. Both fractures bounded but not resolved."
  }
}