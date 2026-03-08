{
  "protocol": "UKE_SCOPE",
  "version": "2.0-json",
  "domain": "Institutional Violence / Medical Authority / Labor Extraction",
  "family_id": "constraint_reversal_dynamics",
  "topic_summary": "Analysis of a narrative depicting silk workers with occupational respiratory disease subjected to fumigation protocol designed to exclude them from evacuation. The protocol's expected lethal outcome inverts: workers experience complete physiological recovery. The narrative explores institutional violence, medical categorization as control mechanism, and the emergence of outcomes outside systemic prediction.",
  "extraction_summary": {
    "entity_count": 6,
    "claim_count": 4,
    "tension_count": 3,
    "mechanism_count": 3,
    "absence_count": 2,
    "key_entities": [
      "Ah-Yong and fei-gong workers (categorized as 'spent labor' with damaged lungs)",
      "Dr. Morrison (documenting physician, objector to protocol)",
      "Dr. Harding (consular medical officer, protocol designer)",
      "Patterson (workshop manager, contract enforcer)",
      "Fumigation protocol (institutional mechanism with exception clause)",
      "Workshop system (labor extraction apparatus producing both silk and damaged bodies)"
    ],
    "key_tensions": [
      "Medical categorization as death sentence vs workers' survival (protocol expects death, workers recover)",
      "Institutional prediction vs physiological outcome (system assumes bodies are measurable/containable, recovery occurs outside measurement)",
      "Extraction continuity vs constraint rupture (workshop resumes labor extraction, but something unmeasurable has changed)"
    ]
  },
  "axes": [
    {
      "claim_id": "categorical_violence_as_structural_exclusion",
      "human_readable": "Categorical Violence as Structural Exclusion",
      "structural_delta": "Administrative categories that render subjects excludable from protection mechanisms",
      "primary_observable": "Presence of exception clauses in protocols; differential treatment based on medical/administrative status classification",
      "epsilon_bin": "low",
      "hypothesis": "rope",
      "beneficiary": null,
      "victim": null,
      "downstream_of": [],
      "feeds_into": [
        "protocol_inversion_as_constraint_escape"
      ],
      "centrality_score": 2,
      "selected": true,
      "generation_order": 1,
      "selection_reason": "Upstream coordination mechanism; establishes the structural basis for exclusion that enables downstream inversion"
    },
    {
      "claim_id": "extraction_residue_as_bodily_inscription",
      "human_readable": "Extraction Residue as Bodily Inscription",
      "structural_delta": "Productive systems that generate physiological damage as co-product of economic output",
      "primary_observable": "Measurable degradation of worker bodies correlated with production duration; medical documentation of occupational disease progression",
      "epsilon_bin": "v_low",
      "hypothesis": "mountain",
      "beneficiary": null,
      "victim": null,
      "downstream_of": [],
      "feeds_into": [
        "protocol_inversion_as_constraint_escape"
      ],
      "centrality_score": 2,
      "selected": true,
      "generation_order": 2,
      "selection_reason": "Physical substrate; mountain providing the material constraint base that protocol acts upon"
    },
    {
      "claim_id": "protocol_inversion_as_constraint_escape",
      "human_readable": "Protocol Inversion as Constraint Escape",
      "structural_delta": "Institutional mechanisms designed to eliminate subjects instead producing outcomes outside systemic prediction or measurement",
      "primary_observable": "Gap between protocol's expected outcome and actual physiological result; institutional inability to explain or categorize the outcome",
      "epsilon_bin": "high",
      "hypothesis": "tangled_rope",
      "beneficiary": "workers_post_recovery",
      "victim": "institutional_predictive_authority",
      "downstream_of": [
        "categorical_violence_as_structural_exclusion",
        "extraction_residue_as_bodily_inscription"
      ],
      "feeds_into": [],
      "centrality_score": 6,
      "selected": true,
      "generation_order": 3,
      "selection_reason": "Highest centrality; downstream synthesis node capturing the core structural paradox where maximum constraint produces escape"
    },
    {
      "claim_id": "documentation_as_epistemic_capture",
      "human_readable": "Documentation as Epistemic Capture",
      "structural_delta": "Medical/administrative records that constitute subjects as knowable while rendering certain transformations illegible",
      "primary_observable": "Presence of worker files, medical journals, protocols; gap between documented measurements and observed outcomes",
      "epsilon_bin": "mod",
      "hypothesis": "snare",
      "beneficiary": "institutional_authority",
      "victim": "subjects_of_documentation",
      "downstream_of": [
        "categorical_violence_as_structural_exclusion"
      ],
      "feeds_into": [
        "protocol_inversion_as_constraint_escape"
      ],
      "centrality_score": 3,
      "selected": false,
      "generation_order": null,
      "selection_reason": "Deferred: captured structurally via categorical_violence_as_structural_exclusion (documentation enables categorization) and protocol_inversion (documentation's failure to explain recovery)"
    },
    {
      "claim_id": "contract_temporality_as_binding_horizon",
      "human_readable": "Contract Temporality as Binding Horizon",
      "structural_delta": "Temporal structures that extend extraction beyond immediate transaction through binding future labor",
      "primary_observable": "Contract duration remaining; worker's inability to exit despite physiological transformation",
      "epsilon_bin": "low",
      "hypothesis": "rope",
      "beneficiary": null,
      "victim": null,
      "downstream_of": [],
      "feeds_into": [
        "protocol_inversion_as_constraint_escape"
      ],
      "centrality_score": 1,
      "selected": false,
      "generation_order": null,
      "selection_reason": "Deferred: lower centrality; temporal binding is present but structurally secondary to categorical exclusion mechanism"
    }
  ],
  "generation_sequence": [
    "categorical_violence_as_structural_exclusion",
    "extraction_residue_as_bodily_inscription",
    "protocol_inversion_as_constraint_escape"
  ],
  "deferred_axes": [
    {
      "claim_id": "documentation_as_epistemic_capture",
      "structural_delta": "Medical/administrative records constituting subjects as knowable while rendering transformations illegible",
      "hypothesis": "snare",
      "deferral_reason": "Captured indirectly via categorical_violence (documentation enables categorization) and protocol_inversion (documentation's explanatory failure)"
    },
    {
      "claim_id": "contract_temporality_as_binding_horizon",
      "structural_delta": "Temporal structures extending extraction through binding future labor",
      "hypothesis": "rope",
      "deferral_reason": "Lower centrality; temporal binding present but structurally secondary to categorical exclusion mechanism"
    }
  ],
  "omegas": [
    {
      "id": "omega_recovery_mechanism",
      "description": "What physiological process enabled complete respiratory recovery during fumigation? Narrative presents this as unmeasurable/unexplainable within institutional framework. Is this a Mountain (actual biological process) or Scaffold (narrative device)?",
      "source": "Dark Matter Probe 2 (Absence Inventory) + F34 check"
    },
    {
      "id": "omega_post_recovery_extraction",
      "description": "Does workers' return to full labor status after recovery constitute resumed extraction or transformed relationship? Narrative suggests 'something has changed' but leaves structural implications unspecified.",
      "source": "Dark Matter Probe 3 (Beneficiary Scan)"
    },
    {
      "id": "omega_institutional_response",
      "description": "How do institutions respond when subjects survive mechanisms designed to eliminate them? Narrative shows Patterson resuming extraction, Morrison documenting inadequacy of documentation, but systemic adaptation remains unspecified.",
      "source": "Deferred axis documentation_as_epistemic_capture"
    }
  ],
  "fracture_scan": {
    "f14_tunnel_vision": false,
    "f15_premature_closure": false,
    "f03_hasty_generalization": false,
    "f34_epistemic_trespass": true,
    "f01_premise_drift": false,
    "notes": "F34 detected: The narrative's central event (complete physiological recovery during fumigation) is presented as outside medical explanation. Model lacks domain expertise to classify this as Mountain (actual biological mechanism) vs Scaffold (narrative device). Generated omega_recovery_mechanism to bound this uncertainty. Protocol_inversion axis treats the gap between institutional prediction and outcome as the structural phenomenon, regardless of recovery mechanism's ontological status."
  }
}