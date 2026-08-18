{
  "protocol": "UKE_SCOPE",
  "version": "2.0-json",
  "domain": "Social Epistemology / Domestic Power / Diagnostic Systems",
  "family_id": "damp_marsh_wasting_narrative",
  "topic_summary": "A narrative in which a young wife dies of a slow poisoning misdiagnosed as environmental illness ('the damp'), because every observer \u2014 folk healer, trained physician, community elders, the victim herself \u2014 possesses a diagnostic framework that structurally excludes the husband as a category of causation, and because the victim's own internalized responsibility-attribution forecloses the one report that could have redirected suspicion.",
  "extraction_summary": {
    "entity_count": 5,
    "claim_count": 4,
    "tension_count": 3,
    "mechanism_count": 3,
    "absence_count": 2,
    "key_entities": [
      "Marfa Osipovna (victim; attributes own decline to inherited bodily weakness)",
      "Foma Silovich (husband; provisions household, controls food administration, socially legible as exemplary caretaker)",
      "Praskovya Terentievna (znakharka; possesses pattern-recognition across cases but no categorical slot for domestic poisoning)",
      "Kirill Denisovich Struve (district doctor; rigorous within his taxonomy, taxonomy has no category for husband-as-vector)",
      "Prov Andreyevich (elder; reads husband's continued care of a non-productive wife as evidence of virtue, precluding suspicion)"
    ],
    "key_tensions": [
      "Victim's self-diagnostic vocabulary (inherited thinness, moral fault) vs. actual external cause (structural asymmetry: she cannot name what would indict the house that also, truthfully, shelters and feeds her)",
      "Healer's felt pattern-recognition ('the cold small motion under her ribs') vs. absence of any categorical slot to formalize the pattern into an accusation",
      "Community's virtue-accounting of the husband (patient, dutiful, doing more than his contract required) vs. the material fact that he is the one administering food and water throughout"
    ],
    "key_mechanisms": [
      "Threshold-keeping norm: wife is structurally forbidden from reporting illness/distress outward without indicting the house itself, which she cannot do because she experiences the house as good",
      "Diagnostic taxonomy exclusion: every trained observer (folk and formal) has categories for environmental/constitutional causes but no category admits 'caregiver as vector,' so confirming evidence is filed as coincidence rather than escalated",
      "Virtue-signaling substitution: husband's visible performance of care (broth before the doctor's visit, formal grief at the deathbed) is read by the community as proof against suspicion rather than as neutral or aggravating evidence"
    ],
    "absences": [
      "No social or epistemic mechanism exists anywhere in the narrative for cross-referencing similar cases across households to detect a pattern (the servant-girl case, structurally identical, is filed and forgotten)",
      "No account of motive is ever supplied or required \u2014 the narrative structurally withholds why Foma Silovich might be poisoning his wife, making the axis about the detection failure, not the perpetrator's psychology"
    ]
  },
  "axes": [
    {
      "claim_id": "victim_self_attribution_foreclosure",
      "human_readable": "Internalized Responsibility as Reporting Foreclosure",
      "structural_delta": "The victim's inherited vocabulary for interpreting her own suffering (constitutional weakness, thin blood, moral fault) is causally prior to and independent of the external mechanism harming her; this vocabulary forecloses the one act \u2014 outward report naming the house as the site of wrongness \u2014 that could redirect scrutiny, because any report would have to indict a house she experiences, truthfully by every available measure, as good.",
      "primary_observable": "Presence/absence of an outward report naming the household as causally implicated, cross-referenced against the victim's private vocabulary of self-blame in interior monologue",
      "epsilon_bin": "mod",
      "hypothesis": "tangled_rope",
      "beneficiary": "foma_silovich",
      "victim": "marfa_osipovna",
      "downstream_of": [],
      "feeds_into": [
        "diagnostic_taxonomy_blind_spot"
      ],
      "centrality_score": 5,
      "selected": true,
      "generation_order": 1,
      "selection_reason": "Highest independent \u03b5; distinct observable (interior attribution vocabulary) from the diagnostic-system axis; upstream condition that makes the taxonomy gap lethal rather than merely incomplete"
    },
    {
      "claim_id": "diagnostic_taxonomy_blind_spot",
      "human_readable": "Categorical Exclusion of the Caregiver as Causal Vector",
      "structural_delta": "Every diagnostic framework present in the narrative \u2014 folk empirical pattern-memory, formal clinical taxonomy, communal moral accounting \u2014 is rigorously applied within its own terms and simultaneously incapable of representing 'the person administering care and sustenance is the source of harm' as an admissible category, so confirming evidence (repeated grey nail-beds across unrelated households, a case that will not plateau) is filed as coincidence rather than escalated into suspicion.",
      "primary_observable": "Presence of a categorical slot in each observer's diagnostic vocabulary for caregiver-as-vector; count of instances where pattern-matching evidence is generated but not escalated due to absence of that slot",
      "epsilon_bin": "high",
      "hypothesis": "snare",
      "beneficiary": "foma_silovich",
      "victim": "marfa_osipovna",
      "downstream_of": [
        "victim_self_attribution_foreclosure"
      ],
      "feeds_into": [
        "virtue_performance_as_exculpation"
      ],
      "centrality_score": 6,
      "selected": true,
      "generation_order": 2,
      "selection_reason": "Central synthesis node; distinct observable (external professional taxonomies) from victim's interior vocabulary; explains why competent, good-faith actors systematically fail"
    },
    {
      "claim_id": "virtue_performance_as_exculpation",
      "human_readable": "Visible Caretaking Performance as Inverse Evidence",
      "structural_delta": "The husband's legible, socially-verified performance of dutiful care (broth before the doctor's visit, formal grief at the deathbed, choosing not to exercise his contractual right to return a non-productive wife) is read by the community not as neutral or as aggravating circumstantial proximity to the victim's decline, but as affirmative evidence against suspicion \u2014 the more visible the caretaking, the less visible the causal opportunity it in fact represents.",
      "primary_observable": "Directionality of inference: instances where evidence of proximity/control (feeding, administering broth, steadying the cup) is read by community observers as exculpatory rather than as opportunity",
      "epsilon_bin": "mod",
      "hypothesis": "tangled_rope",
      "beneficiary": "foma_silovich",
      "victim": "marfa_osipovna",
      "downstream_of": [
        "diagnostic_taxonomy_blind_spot"
      ],
      "feeds_into": [],
      "centrality_score": 4,
      "selected": true,
      "generation_order": 3,
      "selection_reason": "Distinct observable (community inference direction) from taxonomy gap itself; closes the loop showing how the same act (control over food/water) simultaneously enables harm and manufactures alibi"
    },
    {
      "claim_id": "threshold_keeping_norm",
      "human_readable": "Wifely Threshold-Guarding as Isolation Mechanism",
      "structural_delta": "A structural norm requiring the wife to actively repel outside inquiry (turning away old Xenia, not writing to her mother) functions to seal the household against exactly the kind of cross-household comparison that could surface a pattern",
      "primary_observable": "Frequency and success rate of outside-inquiry deflection events initiated by the victim herself",
      "epsilon_bin": "mod",
      "hypothesis": "rope",
      "beneficiary": "foma_silovich",
      "victim": "marfa_osipovna",
      "downstream_of": [],
      "feeds_into": [
        "victim_self_attribution_foreclosure"
      ],
      "centrality_score": 3,
      "selected": false,
      "generation_order": null,
      "selection_reason": "Deferred: largely subsumed within victim_self_attribution_foreclosure; the deflection behavior is a symptom of the same internalized attribution rather than a structurally independent mechanism"
    },
    {
      "claim_id": "cross_case_pattern_non_aggregation",
      "human_readable": "Absence of Inter-Household Case Aggregation",
      "structural_delta": "No institution or practice exists to aggregate structurally similar cases (Marfa Osipovna, the servant-girl three steadings over) across households, so a healer's felt pattern-recognition never crosses the threshold into formal suspicion",
      "primary_observable": "Existence/non-existence of a record-keeping or comparison mechanism spanning multiple households under one healer's practice",
      "epsilon_bin": "high",
      "hypothesis": "mountain",
      "beneficiary": null,
      "victim": null,
      "downstream_of": [],
      "feeds_into": [
        "diagnostic_taxonomy_blind_spot"
      ],
      "centrality_score": 2,
      "selected": false,
      "generation_order": null,
      "selection_reason": "Deferred: largely a restatement of diagnostic_taxonomy_blind_spot at the infrastructural rather than categorical level; retained as omega given budget ceiling of 3"
    }
  ],
  "generation_sequence": [
    "victim_self_attribution_foreclosure",
    "diagnostic_taxonomy_blind_spot",
    "virtue_performance_as_exculpation"
  ],
  "deferred_axes": [
    {
      "claim_id": "threshold_keeping_norm",
      "structural_delta": "Wife's active repulsion of outside inquiry seals the household against comparison",
      "hypothesis": "rope",
      "deferral_reason": "Subsumed as symptom of victim_self_attribution_foreclosure rather than independent mechanism"
    },
    {
      "claim_id": "cross_case_pattern_non_aggregation",
      "structural_delta": "No institution aggregates structurally similar cases across households",
      "hypothesis": "mountain",
      "deferral_reason": "Restates diagnostic_taxonomy_blind_spot at infrastructural level; caller budget ceiling of 3 applied"
    }
  ],
  "omegas": [
    {
      "id": "omega_motive_withheld",
      "description": "The narrative never establishes why Foma Silovich harms his wife (inheritance, remarriage, control, or something else) \u2014 the three selected axes describe the detection failure, not the perpetrator's psychology. A fourth axis on perpetrator motive structure was considered and rejected as insufficiently supported by the text.",
      "source": "Extraction absence inventory (\u00a71.1)"
    },
    {
      "id": "omega_servant_girl_survival",
      "description": "The servant-girl three steadings over, exhibiting the identical symptom pattern, survives. Whether this indicates a different (non-poisoning) cause for her case, a difference in dose, or a difference in the perpetrator's intent, is never resolved and would materially affect whether diagnostic_taxonomy_blind_spot is best modeled as systemic or as this-household-specific.",
      "source": "Dark Matter Probe 2 (Absence Inventory)"
    },
    {
      "id": "omega_mountain_or_piton_taxonomy",
      "description": "diagnostic_taxonomy_blind_spot is classified as snare (actively harming victim while serving husband's interest), but an alternate reading treats the taxonomic gap as a piton \u2014 a historically contingent limitation of pre-forensic medicine that would not exist in a system with toxicology. This reclassification would change the \u03b5 and the moral weight of the axis substantially.",
      "source": "F03 (Hasty Generalization) self-scan"
    }
  ],
  "fracture_scan": {
    "f14_tunnel_vision": false,
    "f15_premature_closure": false,
    "f03_hasty_generalization": true,
    "f34_epistemic_trespass": false,
    "f01_premise_drift": false,
    "notes": "F03 detected: diagnostic_taxonomy_blind_spot classified as snare (intentional-structure serving a beneficiary) but the narrative is agnostic about whether the taxonomy gap is exploited deliberately by Foma Silovich or merely exists as historical circumstance he passively benefits from. Recorded as omega_mountain_or_piton_taxonomy to bound this. No tunnel vision: axes draw from epistemology/diagnostics, domestic power structure, and social inference norms \u2014 three distinct lenses, not one repeated lens."
  }
}