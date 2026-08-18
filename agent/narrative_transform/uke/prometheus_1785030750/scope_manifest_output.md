{
  "protocol": "UKE_SCOPE",
  "version": "2.0-json",
  "domain": "Political Economy of Resource Allocation / Institutional Authority Theory",
  "family_id": "measured_authority_narrative",
  "topic_summary": "A first-person account of a water-rights Warden whose gate is shut by a central authority (the Ordinate) after he acts on unsanctioned knowledge his daughter's innate 'reading' ability surfaced, revealing that his fifteen years of being 'heard' in council never constituted real power, and that only collective procedural refusal \u2014 not appeal, not knowledge, not standing \u2014 could compel the authority to act.",
  "extraction_summary": {
    "entity_count": 6,
    "claim_count": 4,
    "tension_count": 3,
    "mechanism_count": 3,
    "absence_count": 2,
    "key_entities": [
      "Sethrei (narrator, Warden of Torn Hollow, downstream/lowest terrace)",
      "Ila (daughter, a 'reader' whose innate sensing ability detects unlisted water sources)",
      "Ren Osar-Ked (Warden-Speaker of the Kesh-Ordinate, central authority, unchanging affect)",
      "The Ordinate / the Throat (central adjudicating body that controls the sluice mechanism and recitation)",
      "Marn Kohl (terrace-worker without gate-standing, structurally powerless observer)",
      "The other Sluice-Wardens (peer authorities who collectively withhold recitation)"
    ],
    "key_tensions": [
      "Being rhetorically anticipated/heard by authority vs. actually holding leverage over authority (the council-standing illusion)",
      "The reading-discipline's demand for self-distrust (wait for confirmation from above) vs. the survival-need to act on directly-known, unconfirmed knowledge",
      "The measure as genuine mutual-protection mechanism (mountain) vs. the measure as a tool whose enforcement is asymmetric and punitive when defied (snare)"
    ]
  },
  "axes": [
    {
      "claim_id": "voice_without_leverage",
      "human_readable": "Rhetorical Accommodation as Simulated Power (Being Heard vs. Being Able to Compel)",
      "structural_delta": "An actor granted fifteen years of rhetorical concession and anticipation by an authority mistakes procedural courtesy for actual power; the authority's willingness to yield small, cosmetic concessions coexists with, and masks, its unilateral capacity to revoke the actor's entire operating standing without appeal, in the same even register used for any other administrative act.",
      "primary_observable": "Ratio of concessions granted in deliberation vs. unilateral enforcement actions taken outside deliberation; whether the authority's affect/register changes between the two",
      "epsilon_bin": "mod",
      "hypothesis": "tangled_rope",
      "beneficiary": "central_authority",
      "victim": "local_authority_holder",
      "downstream_of": [],
      "feeds_into": [
        "collective_refusal_as_sole_leverage"
      ],
      "centrality_score": 5,
      "selected": true,
      "generation_order": 1,
      "selection_reason": "Highest structural centrality; names the core disillusionment the entire narrative turns on, distinct observable (concession-rate vs enforcement-rate) from the other candidates"
    },
    {
      "claim_id": "disciplined_self_distrust_as_inherited_suppression",
      "human_readable": "Trained Self-Distrust of Direct Knowledge (Suppression Passed as Virtue Across Generations)",
      "structural_delta": "A perceptual or intuitive capacity is deliberately trained, generation over generation, to defer to an external confirming authority before it may be acted upon or even spoken; the suppression is transmitted as discipline and integrity rather than as constraint, until a crisis makes the withheld knowledge materially fatal to withhold.",
      "primary_observable": "Presence/absence of a codified deferral rule taught to the capacity-holder ('wait for confirmation'); measurable lag between private sensing and permitted disclosure; whether the lag persists after the capacity-holder's private knowledge proves correct",
      "epsilon_bin": "low",
      "hypothesis": "rope",
      "beneficiary": "central_authority",
      "victim": "capacity_holder_and_dependents",
      "downstream_of": [],
      "feeds_into": [
        "collective_refusal_as_sole_leverage"
      ],
      "centrality_score": 3,
      "selected": true,
      "generation_order": 2,
      "selection_reason": "Distinct observable (training/deferral mechanism transmitted across generations) and distinct victim structure (the reader herself, not the Warden) from axis 1; upstream feed into the collective-action resolution"
    },
    {
      "claim_id": "collective_refusal_as_sole_leverage",
      "human_readable": "Procedural Refusal as the Only Compelling Force Against Unilateral Authority",
      "structural_delta": "An authority structure that can unilaterally seize or suspend any single actor's standing cannot be moved by appeal, evidence, or grievance from that actor alone; it can only be compelled when a procedural requirement built into its own legitimating ritual is collectively withheld by peers who individually hold no more formal power than the aggrieved actor.",
      "primary_observable": "Whether the authority's core legitimating ritual (the recitation) has a structural veto point requiring multiple independent actors' participation; count of peer-actors required to withhold before the authority alters its position",
      "epsilon_bin": "mod",
      "hypothesis": "tangled_rope",
      "beneficiary": "peer_collective_when_unified",
      "victim": "isolated_individual_petitioner",
      "downstream_of": [
        "voice_without_leverage",
        "disciplined_self_distrust_as_inherited_suppression"
      ],
      "feeds_into": [],
      "centrality_score": 6,
      "selected": true,
      "generation_order": 3,
      "selection_reason": "Highest centrality as synthesis node with two inbound edges; distinct observable (procedural veto point requiring plurality) from both upstream axes; resolves the narrative's actual turning mechanism"
    },
    {
      "claim_id": "punitive_symmetry_masked_as_maintenance",
      "human_readable": "Enforcement Framed as Neutral Maintenance Regardless of Target",
      "structural_delta": "An authority's disciplinary action against a defector is executed with the identical affect, procedure, and self-justification it would use against anyone, which the authority experiences as proof of impartiality but which the target experiences as proof that personal standing, history, and relationship never mattered to the mechanism at all.",
      "primary_observable": "Whether the enforcing agent's demeanor/procedure varies by target identity or history; target's own testimony about whether personal standing affected the outcome",
      "epsilon_bin": "low",
      "hypothesis": "rope",
      "beneficiary": null,
      "victim": "individual_defector",
      "downstream_of": [
        "voice_without_leverage"
      ],
      "feeds_into": [],
      "centrality_score": 1,
      "selected": false,
      "generation_order": null,
      "selection_reason": "Deferred: largely restates the affective texture of voice_without_leverage from the authority's interior rather than adding an independent observable; overlap Type A risk with axis 1"
    },
    {
      "claim_id": "retroactive_absorption_of_dissent",
      "human_readable": "Concession Absorbed Into Unchanged Legitimating Language",
      "structural_delta": "After a defector wins a substantive concession through collective pressure, the authority folds the concession into its record using the same unchanged ceremonial language it always used, such that the victory becomes indistinguishable, in the historical record, from ordinary administration \u2014 erasing the fact that anything had to be fought for.",
      "primary_observable": "Comparison of pre- and post-concession ceremonial language; whether the record marks the concession as exceptional or silently absorbs it as though always-present",
      "epsilon_bin": "low",
      "hypothesis": "piton",
      "beneficiary": "central_authority",
      "victim": "collective_memory_of_the_defiant_act",
      "downstream_of": [
        "collective_refusal_as_sole_leverage"
      ],
      "feeds_into": [],
      "centrality_score": 1,
      "selected": false,
      "generation_order": null,
      "selection_reason": "Deferred: genuinely distinct observable (record-language before/after) but low centrality; strong candidate for omega rather than full axis given narrative's own explicit foregrounding of this exact unease in closing section"
    },
    {
      "claim_id": "ambivalent_dependence_on_the_constraining_mechanism",
      "human_readable": "Simultaneous Resentment of and Reliance on the Same Constraint",
      "structural_delta": "An actor harmed by a rule during scarcity discovers, on reflection, that the identical rule protected them during a different crisis (abundance/flood), producing genuine ambivalence rather than simple opposition toward the constraining mechanism itself.",
      "primary_observable": "Actor's own testimony contrasting harm-instance and protection-instance attributable to the same fixed rule",
      "epsilon_bin": "v_low",
      "hypothesis": "mountain",
      "beneficiary": null,
      "victim": null,
      "downstream_of": [],
      "feeds_into": [],
      "centrality_score": 0,
      "selected": false,
      "generation_order": null,
      "selection_reason": "Deferred: zero centrality, no edges to selected axes; a mountain-classification the narrator himself explicitly validates rather than a contested structural claim; better suited as omega"
    }
  ],
  "generation_sequence": [
    "voice_without_leverage",
    "disciplined_self_distrust_as_inherited_suppression",
    "collective_refusal_as_sole_leverage"
  ],
  "deferred_axes": [
    {
      "claim_id": "punitive_symmetry_masked_as_maintenance",
      "structural_delta": "Enforcement executed identically regardless of target, experienced by authority as impartiality and by target as erasure of relationship",
      "hypothesis": "rope",
      "deferral_reason": "Overlaps substantially with voice_without_leverage's affective register; insufficiently distinct observable"
    },
    {
      "claim_id": "retroactive_absorption_of_dissent",
      "structural_delta": "Won concessions folded into unchanged ceremonial language, erasing the historical fact of struggle",
      "hypothesis": "piton",
      "deferral_reason": "Distinct but low centrality; narrative already surfaces this explicitly as the narrator's own closing unease, better carried as omega"
    },
    {
      "claim_id": "ambivalent_dependence_on_the_constraining_mechanism",
      "structural_delta": "The same fixed rule that harmed the actor in scarcity protected the actor in a different crisis, producing genuine ambivalence",
      "hypothesis": "mountain",
      "deferral_reason": "Zero centrality, no graph edges; narrator's own testimony already resolves this as a validated mountain rather than a contested axis"
    }
  ],
  "omegas": [
    {
      "id": "omega_retroactive_erasure",
      "description": "The narrative explicitly flags that the concession, once won, was folded into unchanged recitation language ('as it was measured, so it remains') such that the historical record shows no trace of the struggle. Whether this retroactive absorption is itself a distinct extractive mechanism (piton) or simply the natural settling of any institutional record is left unresolved by the text.",
      "source": "Deferred axis retroactive_absorption_of_dissent"
    },
    {
      "id": "omega_readers_interiority_withheld",
      "description": "The narrative is explicit that Ila's internal experience of her ability, her ambivalence about being watched, and what she 'feels in the ground' are never accessed directly \u2014 only inferred by the narrator from the outside. A generation targeting the reader's suppression axis should flag that the source material itself withholds the primary subject's interiority.",
      "source": "Dark Matter Probe 2 (Absence Inventory)"
    },
    {
      "id": "omega_authority_good_faith",
      "description": "Ren Osar-Ked's even affect is read by the narrator as evidence of pure procedural indifference ('maintenance, not cruelty'), but the text never confirms this from Ren's own interiority \u2014 it remains the narrator's interpretation. Whether the Ordinate's neutrality is genuine or a performance is not settled by the source.",
      "source": "Dark Matter Probe 3 (Beneficiary Scan)"
    }
  ],
  "fracture_scan": {
    "f14_tunnel_vision": false,
    "f15_premature_closure": false,
    "f03_hasty_generalization": false,
    "f34_epistemic_trespass": false,
    "f01_premise_drift": false,
    "notes": "No fractures detected requiring orchestrator pause. Candidate axes were drawn from at least three distinct lenses (institutional/procedural authority theory, generational-transmission/suppression dynamics, collective-action theory) plus a beneficiary scan (retroactive absorption) and an absence probe (reader's withheld interiority), avoiding tunnel vision from a single framing. The three selected axes pass pairwise independence: voice_without_leverage observes concession-vs-enforcement asymmetry from the Warden's position; disciplined_self_distrust observes a cross-generational training mechanism acting on the daughter; collective_refusal_as_sole_leverage observes a structural veto-point in the authority's own legitimating procedure. Each has a distinct primary_observable and distinct victim/beneficiary structure."
  }
}