{
  "protocol": "UKE_SCOPE",
  "version": "2.0-json",
  "domain": "Narrative Constraint Analysis / Organizational Legitimacy Under Scarcity",
  "family_id": "true_weight_tally_room",
  "topic_summary": "First-person narrative of a ship's tally-keeper who measures water rations with perfect mechanical accuracy while a parallel, unaudited system of resource extraction and arbitrary culling ('the sack') operates alongside her measurements without reference to them. The story tracks her recognition that precise measurement and legitimate authority are structurally decoupled, and her small, unmeasured act of resistance (over-filling a condemned man's water skin) within a system where accuracy has no power to protect anyone.",
  "extraction_summary": {
    "entity_count": 6,
    "claim_count": 5,
    "tension_count": 3,
    "mechanism_count": 3,
    "absence_count": 3,
    "key_entities": [
      "Narrator (quartermaster's mate, tally-keeper, promoted into proximity to the scale)",
      "Voss (reads the true-weight scale; also names who goes 'to the sack')",
      "Osan Duvray (steady-handed deckhand, culled despite soundness)",
      "Farro (general-ration crew member, tracks manifest discrepancy privately, performs small unaudited generosities)",
      "Thess (holds unaudited keys to the spare reserve cask; discrepancy between pencil figure and actual level)",
      "The true-weight scale (mechanically perfect instrument with zero decisional authority)"
    ],
    "key_tensions": [
      "Measurement accuracy vs. decisional authority \u2014 the scale never lies but never decides anything",
      "Visible ration law (the notch, the manifest) vs. invisible unaudited reserve (Thess's keys, the low cask level)",
      "Soundness/competence (Duvray's steady hands) vs. arbitrary selection for culling \u2014 no causal link between merit and survival"
    ],
    "key_mechanisms": [
      "Daily true-weight ceremony: public, ritualized, perfectly accurate, and entirely without power to alter outcomes",
      "The 'sack' selection: performed with the same flat affect as the water-weighing, no scale invoked, no criteria stated",
      "Unaudited discretionary reserve controlled by Thess, sitting below its own recorded figure, never drawn in daylight"
    ],
    "key_absences": [
      "No explanation is ever given for why Duvray specifically is culled \u2014 the text withholds and confirms the withholding is structural, not incidental",
      "No character above Farro's tier ever explains the missing water in the spare reserve",
      "No consequence or acknowledgment follows the narrator's small act of over-filling the skin"
    ]
  },
  "axes": [
    {
      "claim_id": "measurement_authority_decoupling",
      "human_readable": "Measurement Without Authority (Accuracy as Powerless Ritual)",
      "structural_delta": "A perfectly accurate, publicly ritualized measurement instrument produces true numbers that have zero causal influence on who is protected or destroyed; legitimacy is performed through the ritual of measurement while actual decisions occur in a separate, scale-less space",
      "primary_observable": "Frequency and outcome-correlation of true-weight readings vs. frequency and outcome-correlation of culling decisions \u2014 the two show zero correlation despite occurring in the same ceremonial register",
      "epsilon_bin": "mod",
      "hypothesis": "tangled_rope",
      "beneficiary": "authority_that_controls_selection_criteria",
      "victim": "those_selected_without_recourse_to_measurable_merit",
      "downstream_of": [],
      "feeds_into": [
        "arbitrary_selection_under_competence_signaling"
      ],
      "centrality_score": 5,
      "selected": true,
      "generation_order": 1,
      "selection_reason": "Highest centrality; the load-bearing dynamic of the entire narrative \u2014 every other axis is a variant expression of this decoupling"
    },
    {
      "claim_id": "arbitrary_selection_under_competence_signaling",
      "human_readable": "Culling Independent of Demonstrated Merit",
      "structural_delta": "A selection mechanism (who is culled/sacrificed) that produces outcomes uncorrelated with the visible competence signals (steady hands, punctuality, soundness) the narrative repeatedly establishes as present in the selected party \u2014 merit is depicted in detail specifically to demonstrate its irrelevance to survival",
      "primary_observable": "Presence/absence of stated criteria at the moment of selection; correlation between prior-established competence indicators and selection outcome",
      "epsilon_bin": "high",
      "hypothesis": "snare",
      "beneficiary": "captain_and_voss_as_unaccountable_selectors",
      "victim": "duvray_and_unnamed_predecessor_quartermasters_mate",
      "downstream_of": [
        "measurement_authority_decoupling"
      ],
      "feeds_into": [],
      "centrality_score": 3,
      "selected": true,
      "generation_order": 2,
      "selection_reason": "Distinct observable (selection criteria absence) from axis 1's observable (measurement-outcome correlation); necessary downstream instantiation of the decoupling"
    },
    {
      "claim_id": "unaudited_reserve_asymmetry",
      "human_readable": "Discretionary Reserve Invisible to the Rationed",
      "structural_delta": "A resource buffer exists, is tracked on paper, and is drawn down in practice by an unaudited keyholder, while the general population operates under a visible, notch-measured ration with no access to or knowledge of the buffer's actual state \u2014 the scarcity experienced by the rationed is not the true scarcity of the ship",
      "primary_observable": "Gap between the pencil-recorded reserve figure and the actual cask level; presence/absence of daylight draws against the reserve; key-ring audit status",
      "epsilon_bin": "high",
      "hypothesis": "snare",
      "beneficiary": "thess_and_officers_with_reserve_access",
      "victim": "general_ration_crew_farro_and_peers",
      "downstream_of": [],
      "feeds_into": [],
      "centrality_score": 1,
      "selected": true,
      "generation_order": 3,
      "selection_reason": "Independent observable (reserve-cask discrepancy) and independent beneficiary/victim structure from both selection axes; the third distinct extractive geometry in the text \u2014 resource asymmetry rather than judgment asymmetry"
    },
    {
      "claim_id": "small_unmeasured_generosity_as_resistance",
      "human_readable": "Unaudited Excess as the Only Available Agency",
      "structural_delta": "Within a fully surveilled ration system, the sole remaining act of agency available to a subordinate is a quantity too small to register on any measuring instrument \u2014 resistance is only possible below the threshold of detection",
      "primary_observable": "Size of the deviation (a finger's width of water, an unlogged fish twist) relative to the smallest unit the tally system can register",
      "epsilon_bin": "low",
      "hypothesis": "rope",
      "beneficiary": "duvray_recipient_of_marginal_excess",
      "victim": null,
      "downstream_of": [
        "measurement_authority_decoupling"
      ],
      "feeds_into": [],
      "centrality_score": 1,
      "selected": false,
      "generation_order": null,
      "selection_reason": "Deferred: emotionally central to the narrator's arc but structurally a low-epsilon corollary of axis 1 rather than an independent extractive dynamic; ceiling of 3 reached by higher-centrality, more independent candidates"
    },
    {
      "claim_id": "narrative_proximity_as_false_epistemic_gain",
      "human_readable": "Closeness to the Instrument Mistaken for Closeness to the Decision",
      "structural_delta": "Moving physically nearer to the site of measurement produces the illusion of greater understanding or influence over outcomes, when in fact proximity only increases the resolution at which one observes one's own powerlessness",
      "primary_observable": "Narrator's stated expectation ('I thought closeness would explain something') vs. stated outcome ('there is nothing here to explain')",
      "epsilon_bin": "v_low",
      "hypothesis": "piton",
      "beneficiary": null,
      "victim": null,
      "downstream_of": [
        "measurement_authority_decoupling"
      ],
      "feeds_into": [],
      "centrality_score": 1,
      "selected": false,
      "generation_order": null,
      "selection_reason": "Deferred: a first-person epistemic framing device rather than a distinct structural axis; largely restates axis 1 from the narrator's subjective vantage"
    }
  ],
  "generation_sequence": [
    "measurement_authority_decoupling",
    "arbitrary_selection_under_competence_signaling",
    "unaudited_reserve_asymmetry"
  ],
  "deferred_axes": [
    {
      "claim_id": "small_unmeasured_generosity_as_resistance",
      "structural_delta": "Resistance only possible below the threshold of measurement detection",
      "hypothesis": "rope",
      "deferral_reason": "Low independent centrality; functions as corollary/resolution of measurement_authority_decoupling rather than a separate extractive geometry"
    },
    {
      "claim_id": "narrative_proximity_as_false_epistemic_gain",
      "structural_delta": "Physical/positional proximity to an instrument of measurement mistaken for proximity to decisional power",
      "hypothesis": "piton",
      "deferral_reason": "Restates axis 1 through subjective narrator lens; not independently observable apart from the decoupling it dramatizes"
    }
  ],
  "omegas": [
    {
      "id": "omega_criteria_absence",
      "description": "The text never discloses what determines who is sent 'to the sack' \u2014 is there a hidden criterion (cost, discipline, expendability) or is selection genuinely random? The narrative withholds this deliberately; a generated story could either preserve the withholding or resolve it, and the two choices produce very different epsilon values for arbitrary_selection_under_competence_signaling.",
      "source": "Dark Matter Probe 2 (Absence Inventory)"
    },
    {
      "id": "omega_thess_complicity_scope",
      "description": "Is Thess an independent beneficiary of the reserve discrepancy, or merely an unaudited node within a larger officer-class extraction the text never surfaces (the captain, the House)? The unaudited_reserve_asymmetry axis may be a visible fragment of a larger snare.",
      "source": "Dark Matter Probe 3 (Beneficiary Scan)"
    },
    {
      "id": "omega_scale_as_mountain_or_scaffold",
      "description": "Is the true-weight scale's perfect accuracy a Mountain (physical/mechanical fact independent of the ship's social order) or a Scaffold (a legitimating ritual the House maintains precisely because its powerlessness makes it safe to keep accurate)? The narrative supports both readings without resolving which.",
      "source": "F03 (Hasty Generalization) self-scan"
    }
  ],
  "fracture_scan": {
    "f14_tunnel_vision": false,
    "f15_premature_closure": false,
    "f03_hasty_generalization": true,
    "f34_epistemic_trespass": false,
    "f01_premise_drift": false,
    "notes": "F03 flagged on measurement_authority_decoupling: the narrative strongly implies the scale's accuracy is deliberately preserved because it is powerless (a Scaffold maintained by the House to launder legitimacy), but classifying it as tangled_rope assumes the entanglement is structural rather than designed. Recorded as omega_scale_as_mountain_or_scaffold pending generation-stage resolution. No tunnel vision: axes span epistemic (measurement), procedural (selection), and material (reserve) lenses rather than a single reading. No premise drift: extraction stays tightly bound to textual anchors (the notch, the pencil figure, the key ring, Duvray's hands)."
  }
}