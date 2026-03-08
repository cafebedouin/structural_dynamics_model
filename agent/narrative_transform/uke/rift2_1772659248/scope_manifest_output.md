{
  "protocol": "UKE_SCOPE",
  "version": "2.0-json",
  "domain": "Epistemology / Organizational Psychology / Systems Theory",
  "family_id": "epistemic_closure_under_institutional_pressure",
  "topic_summary": "Analysis of a technical functionary (Ventkeeper) encountering persistent anomalous sensor readings that resist categorization within his institutional framework. The narrative tracks his progressive retreat from investigation into baseline recalibration\u2014resolving the anomaly by redefining normalcy rather than explaining the deviation.",
  "extraction_summary": {
    "entity_count": 5,
    "claim_count": 4,
    "tension_count": 3,
    "mechanism_count": 3,
    "absence_count": 2,
    "key_entities": [
      "Ventkeeper (technical functionary; sole authority over contamination monitoring)",
      "Contamination sensor array (measurement infrastructure; 22 sensors, Vent 14 anomalous)",
      "Institutional framework (implicit authority defining categories, protocols, baselines)",
      "Cores (labor force; bodies with lattice intrusion; source of contamination readings)",
      "Scuttlers (organic life; aggregating at Vent 12; outside Ventkeeper's categorical responsibility)"
    ],
    "key_tensions": [
      "Measurement accuracy vs categorical closure (readings are correct but don't fit framework)",
      "Investigation vs resolution (explaining anomaly vs eliminating it from view)",
      "Observed reality vs institutional legibility (scuttler behavior, oscillating readings vs 'not my category')"
    ]
  },
  "axes": [
    {
      "claim_id": "epistemic_authority_erosion_through_unresolvable_anomaly",
      "human_readable": "Epistemic Authority Erosion Through Unresolvable Anomaly",
      "structural_delta": "Authority derived from explanatory capacity degrades when phenomena resist available categories, forcing choice between admitting framework inadequacy or redefining normalcy",
      "primary_observable": "Number of modeling attempts before baseline recalibration; gap between anomaly detection and closure method; presence/absence of external consultation",
      "epsilon_bin": "mod",
      "hypothesis": "tangled_rope",
      "beneficiary": "institutional_framework",
      "victim": "epistemic_integrity",
      "downstream_of": [
        "measurement_fidelity_as_authority_substrate"
      ],
      "feeds_into": [],
      "centrality_score": 5,
      "selected": true,
      "generation_order": 2,
      "selection_reason": "Highest centrality; downstream synthesis node capturing the core dynamic of authority collapse under categorical pressure"
    },
    {
      "claim_id": "measurement_fidelity_as_authority_substrate",
      "human_readable": "Measurement Fidelity as Authority Substrate",
      "structural_delta": "Institutional authority grounded in measurement accuracy rather than explanatory power; readings are treated as ground truth independent of interpretive framework",
      "primary_observable": "Frequency of sensor verification vs model revision; trust placed in instrument output vs theoretical explanation; response to instrument-theory conflict",
      "epsilon_bin": "v_low",
      "hypothesis": "mountain",
      "beneficiary": null,
      "victim": null,
      "downstream_of": [],
      "feeds_into": [
        "epistemic_authority_erosion_through_unresolvable_anomaly",
        "categorical_boundary_as_cognitive_load_limiter"
      ],
      "centrality_score": 4,
      "selected": true,
      "generation_order": 1,
      "selection_reason": "Upstream mountain providing physical constraint base; measurement infrastructure as non-negotiable substrate"
    },
    {
      "claim_id": "categorical_boundary_as_cognitive_load_limiter",
      "human_readable": "Categorical Boundary as Cognitive Load Limiter",
      "structural_delta": "Institutional role boundaries function as cognitive load management by defining what phenomena require explanation vs what can be ignored as 'not my category'",
      "primary_observable": "Frequency of 'not my category' responses to observed anomalies; correlation between role boundary and investigation scope; presence of cross-category consultation mechanisms",
      "epsilon_bin": "low",
      "hypothesis": "rope",
      "beneficiary": null,
      "victim": null,
      "downstream_of": [
        "measurement_fidelity_as_authority_substrate"
      ],
      "feeds_into": [
        "epistemic_authority_erosion_through_unresolvable_anomaly"
      ],
      "centrality_score": 3,
      "selected": true,
      "generation_order": 3,
      "selection_reason": "Different observable from axis 1; captures the scuttler subplot as structural parallel to main dynamic"
    },
    {
      "claim_id": "baseline_drift_as_normalization_mechanism",
      "human_readable": "Baseline Drift as Normalization Mechanism",
      "structural_delta": "Recalibrating measurement baselines to incorporate anomalous data transforms deviance into normalcy without requiring explanation",
      "primary_observable": "Frequency of baseline recalibration; correlation between recalibration and unresolved anomalies; change in variance tolerance post-recalibration",
      "epsilon_bin": "high",
      "hypothesis": "snare",
      "beneficiary": "institutional_continuity",
      "victim": "signal_detection_capacity",
      "downstream_of": [
        "epistemic_authority_erosion_through_unresolvable_anomaly"
      ],
      "feeds_into": [],
      "centrality_score": 2,
      "selected": false,
      "generation_order": null,
      "selection_reason": "Deferred: captured as the resolution mechanism within epistemic_authority_erosion; would duplicate rather than add structural dimension"
    },
    {
      "claim_id": "labor_body_as_contamination_source",
      "human_readable": "Labor Body as Contamination Source",
      "structural_delta": "Worker bodies treated as mineral contamination sources rather than persons; bioelectric activity measured as atmospheric composition variable",
      "primary_observable": "Sensor targeting of worker bodies; classification of biological processes as contamination; absence of worker agency in contamination models",
      "epsilon_bin": "v_low",
      "hypothesis": "piton",
      "beneficiary": null,
      "victim": null,
      "downstream_of": [],
      "feeds_into": [
        "measurement_fidelity_as_authority_substrate"
      ],
      "centrality_score": 1,
      "selected": false,
      "generation_order": null,
      "selection_reason": "Deferred: worldbuilding detail rather than independent constraint axis; feeds into measurement substrate but lacks independent extractiveness"
    }
  ],
  "generation_sequence": [
    "measurement_fidelity_as_authority_substrate",
    "epistemic_authority_erosion_through_unresolvable_anomaly",
    "categorical_boundary_as_cognitive_load_limiter"
  ],
  "deferred_axes": [
    {
      "claim_id": "baseline_drift_as_normalization_mechanism",
      "structural_delta": "Recalibrating baselines to incorporate anomalies transforms deviance into normalcy without explanation",
      "hypothesis": "snare",
      "deferral_reason": "Mechanism is the resolution of epistemic_authority_erosion rather than independent axis; would duplicate core dynamic"
    },
    {
      "claim_id": "labor_body_as_contamination_source",
      "structural_delta": "Worker bodies measured as contamination sources; biological processes as atmospheric variables",
      "hypothesis": "piton",
      "deferral_reason": "Worldbuilding substrate; feeds into measurement_fidelity but lacks independent constraint yield"
    }
  ],
  "omegas": [
    {
      "id": "omega_scuttler_signal",
      "description": "Are the scuttlers' aggregation and the Vent 14 oscillation causally linked? Narrative presents them as parallel but does not establish connection. If linked, categorical_boundary becomes a Snare preventing signal detection across institutional silos.",
      "source": "Dark Matter Probe 2 (Absence Inventory) + narrative structure"
    },
    {
      "id": "omega_enforcement_circuit",
      "description": "What does the enforcement circuit measure that the Ventkeeper does not? Their instruments 'measure different things' but the enforcement function is never specified. Potential hidden constraint axis.",
      "source": "Dark Matter Probe 2 (Absence Inventory)"
    },
    {
      "id": "omega_disposal_event_causality",
      "description": "Did the disposal event actually cause the oscillation, or is the Ventkeeper's model a rationalization? Narrative ambiguity leaves causal chain unresolved. If rationalization, epistemic_authority_erosion is a Snare of self-deception rather than institutional pressure.",
      "source": "F03 (Hasty Generalization) concern + narrative ambiguity"
    }
  ],
  "fracture_scan": {
    "f14_tunnel_vision": false,
    "f15_premature_closure": false,
    "f03_hasty_generalization": false,
    "f34_epistemic_trespass": false,
    "f01_premise_drift": false,
    "notes": "No fractures detected. Three axes selected capture upstream substrate (measurement infrastructure), downstream synthesis (authority erosion), and parallel structural dynamic (categorical boundaries). Scuttler subplot integrated via categorical_boundary axis rather than treated as separate dimension."
  }
}