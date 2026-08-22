% ============================================================================
% CONSTRAINT STORY: divine_legitimacy_substrate__folk_syncretistic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_legitimacy_substrate__folk_syncretistic_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: divine_legitimacy_substrate__folk_syncretistic_reading
 *   human_readable: Household/Village Ritual Practice as Divine Legitimacy Substrate (Folk Syncretistic Reading)
 *   domain: religious/political/economic
 *
 * SUMMARY:
 *   This constraint models one reading of a contested kernel about where
 *   legitimate divine authority is located in ancient Egyptian society. This
 *   reading holds that divine legitimacy in practice runs through household
 *   and village ritual — pragmatic, syncretistic, results-oriented, and
 *   largely indifferent to whether a deity belongs to the official Amun-Ra
 *   cosmology or to Atenist exclusivity. Under this reading, both the Amun
 *   priesthood and the pharaonic court (whether promoting Amun orthodoxy or
 *   Atenist monotheism) are distant elites whose doctrinal claims barely
 *   penetrate the level at which most people actually manage their
 *   relationship to the divine. The beneficiary structure is genuinely
 *   diffuse: no institution captures rents from this substrate the way a
 *   temple captures tithes or a state captures tribute. This is precisely why
 *   the sibling readings (amun_polytheistic_reading,
 *   atenist_monotheistic_reading) are separate constraint stories rather than
 *   alternate observables of this one — each reading names a different
 *   arrangement of authority, a different beneficiary structure, and a
 *   different epsilon, and averaging across them would misrepresent all
 *   three.
 *
 * KEY AGENTS:
 *   - household_heads: local ritual agenda-setters, moderate power, constrained exit
 *   - village_ritual_specialists: diffuse beneficiaries, moderate power, generational time horizon
 *   - local_shrine_keepers: powerless beneficiaries with modest, community-contingent livelihoods
 *   - rural_farming_households: powerless, pay in offerings/labor, receive coping framework
 *   - amun_priesthood: excluded institutional actor, cannot penetrate or correct folk practice
 *   - pharaonic_court: excluded institutional actor, royal religious policy unenforceable at village scale
 *   - modern_historians: analytical observers reconstructing the practice from material remains
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_legitimacy_substrate__folk_syncretistic_reading, 0.18).
domain_priors:suppression_score(divine_legitimacy_substrate__folk_syncretistic_reading, 0.22).
domain_priors:theater_ratio(divine_legitimacy_substrate__folk_syncretistic_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_legitimacy_substrate__folk_syncretistic_reading, rope).
narrative_ontology:human_readable(divine_legitimacy_substrate__folk_syncretistic_reading, "Household/Village Ritual Practice as Divine Legitimacy Substrate (Folk Syncretistic Reading)").
narrative_ontology:topic_domain(divine_legitimacy_substrate__folk_syncretistic_reading, "religious/political/economic").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_legitimacy_substrate__folk_syncretistic_reading, '525c6ccb-502f-42d1-99d3-13a2a4e05257').
narrative_ontology:cs_kernel_codification('525c6ccb-502f-42d1-99d3-13a2a4e05257', implicit).
narrative_ontology:cs_authority_grounding('525c6ccb-502f-42d1-99d3-13a2a4e05257', practice).
narrative_ontology:cs_interpretation_layer_present('525c6ccb-502f-42d1-99d3-13a2a4e05257').
narrative_ontology:cs_reading_relation('525c6ccb-502f-42d1-99d3-13a2a4e05257', divine_legitimacy_substrate__amun_polytheistic_reading, coexists_with).
narrative_ontology:cs_reading_relation('525c6ccb-502f-42d1-99d3-13a2a4e05257', divine_legitimacy_substrate__atenist_monotheistic_reading, coexists_with).
narrative_ontology:cs_axiom('525c6ccb-502f-42d1-99d3-13a2a4e05257', foundational, efficacy_grounds_divine_legitimacy).
narrative_ontology:cs_axiom_status(efficacy_grounds_divine_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('525c6ccb-502f-42d1-99d3-13a2a4e05257', efficacy_grounds_divine_legitimacy, instrumental).
narrative_ontology:cs_axiom('525c6ccb-502f-42d1-99d3-13a2a4e05257', foundational, ritual_authority_is_locally_distributed).
narrative_ontology:cs_axiom_status(ritual_authority_is_locally_distributed, holdable).
narrative_ontology:cs_axiom_grounding('525c6ccb-502f-42d1-99d3-13a2a4e05257', ritual_authority_is_locally_distributed, conventional).
narrative_ontology:cs_reference_frame('525c6ccb-502f-42d1-99d3-13a2a4e05257', pre_dynastic_household_cult_continuum).
narrative_ontology:cs_drift_state('525c6ccb-502f-42d1-99d3-13a2a4e05257', amarna_period_state_religious_upheaval, gap(stable, minor, false)).
narrative_ontology:cs_created_at('525c6ccb-502f-42d1-99d3-13a2a4e05257', '').
narrative_ontology:cs_kernel_id(divine_legitimacy_substrate__folk_syncretistic_reading, divine_legitimacy_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__folk_syncretistic_reading, household_heads).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__folk_syncretistic_reading, village_ritual_specialists).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__folk_syncretistic_reading, local_shrine_keepers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__folk_syncretistic_reading, rural_farming_households).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__folk_syncretistic_reading, rural_farming_households).
narrative_ontology:constraint_vindicates(divine_legitimacy_substrate__folk_syncretistic_reading, pragmatic_efficacy_as_divine_warrant).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain household shrines, choose which deities to petition for which needs (fertility, harvest, healing, protection from specific dangers), and decide how offerings are allocated. They set the practical terms of divine engagement for their families without reference to temple doctrine, adjusting the pantheon they invoke based on what has worked before.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, household_heads, agenda_setter,
    moderate, biographical, constrained, local).

% Local wise-women, lay priests, and healers who mediate rituals at village shrines and crossroads, incorporating whichever deities or spirits are locally efficacious. They gain modest standing and material support (food, favors, small payment) from continuing to serve a community that judges them by results, not by doctrinal correctness.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, village_ritual_specialists, beneficiary,
    moderate, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(divine_legitimacy_substrate__folk_syncretistic_reading, village_ritual_specialists, agenda_setter).

% Tend small wayside and household shrines to protective or fertility deities that never appear in state temple liturgy. They receive small offerings and community regard in exchange for keeping the shrine active; their livelihood depends on the practice continuing to feel useful to villagers, not on any central authority recognizing it.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, local_shrine_keepers, beneficiary,
    powerless, biographical, constrained, local).

% Ordinary families who petition whichever deity seems to answer their immediate needs — a birth goddess for childbirth, a household protector against illness, a harvest spirit at planting. They pay in offerings, labor, and time, and receive psychological reassurance, social cohesion, and a sense of managed uncertainty in an unpredictable agrarian life. Exit from the practice would mean facing crisis and misfortune without any explanatory or coping framework.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, rural_farming_households, beneficiary,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(divine_legitimacy_substrate__folk_syncretistic_reading, rural_farming_households, payer).

% The Amun temple hierarchy has no operational role in household or village ritual practice and no mechanism to correct or absorb it into official cosmology. From their vantage the folk practice is either beneath notice or a diffuse rival substrate that dilutes the doctrinal claim that legitimate divine access runs through Amun-Ra's temple network. They are not consulted by villagers and cannot easily suppress or systematize what is too dispersed to target.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, amun_priesthood, excluded,
    institutional, generational, analytical, national).

% The pharaoh's court, whether promoting Amun orthodoxy or Atenist exclusivity, has no practical channel into household ritual. Royal religious policy is read by villagers, if at all, as one more distant claim among many, adopted or ignored depending on local utility rather than obeyed as revelation. The court cannot verify or enforce compliance at this scale.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, pharaonic_court, excluded,
    institutional, generational, analytical, national).

% Reconstruct folk religious practice from archaeological remains (household shrines, amulets, votive objects) that survive independently of, and often outnumber, official temple records. They note the practice's persistence across dynastic religious upheavals as evidence that its legitimacy substrate was never dependent on priestly or pharaonic sanction.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, modern_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides households and villages with a flexible, low-overhead system for managing uncertainty (illness, childbirth, harvest failure, misfortune) by matching problems to whichever locally known deity or spirit has a reputation for addressing that specific concern, without requiring doctrinal consistency or centralized adjudication.
% TRANSFER_FUNCTION: Moves small offerings, labor, and social regard from households to local ritual specialists and shrine keepers, and moves psychological reassurance and a shared explanatory framework from the practice back to the household. No significant wealth or authority flows upward to temple or crown institutions through this channel.
% ABSENT_VOICES: The Amun priesthood and the pharaonic court would object that legitimate divine access should run through their sanctioned channels, but they are structurally absent from village-level practice — too diffuse to monitor, too local to be worth contesting, and in any case unable to verify compliance at household scale.
% DISAPPEARANCE_RATIONALE: If folk syncretistic practice vanished, rural households would lose their primary framework for managing everyday misfortune and would either fall back on whatever official cult was locally accessible (a poor substitute given distance and cost) or develop a new substrate; village ritual specialists and shrine keepers would lose their modest livelihoods and social standing entirely.
% FOUNDING_PROBLEM: Ordinary households facing illness, infertility, crop failure, and unpredictable danger needed accessible, low-cost ritual recourse that did not depend on travel to distant state temples or acceptance of a single official cosmology.
% FOUNDING_PROBLEM_CORROBORATION: Archaeological evidence of household shrines and votive practice persisting continuously across religious-political upheavals (including the Amarna period) is documented by modern historians and archaeologists outside the practice itself, who are not beneficiaries of the folk substrate and have no stake in its continuation.
narrative_ontology:disappearance_verdict(divine_legitimacy_substrate__folk_syncretistic_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_legitimacy_substrate__folk_syncretistic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_legitimacy_substrate__folk_syncretistic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(divine_legitimacy_substrate__folk_syncretistic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_legitimacy_substrate__folk_syncretistic_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_legitimacy_substrate__folk_syncretistic_reading_tests).
:- end_tests(divine_legitimacy_substrate__folk_syncretistic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored low (0.18) because no concentrated actor captures rents from this substrate — offerings and labor flow to local specialists and shrine keepers at a scale that functions more as reciprocal community exchange than extraction. Suppression is low-moderate (0.22) reflecting the practical, non-coercive nature of participation (a household can quietly favor one deity over another without sanction) tempered by real social pressure to conform to village norms. Theater ratio is modest and rises slightly over the interval (0.22 to 0.28) as certain shrine practices ossify into customary performance even where their original crisis-response function persists alongside habit. Accessibility collapse is low (0.25) — alternative coping frameworks (state cult access, medical/magical specialists, other village traditions) remain genuinely available, distinguishing this from a totalizing constraint. Resistance is moderate (0.35), reflecting occasional friction between folk practice and state religious campaigns (Atenist suppression attempts, orthodox Amun reassertion) that villagers largely absorbed or ignored rather than actively fought.
 *
 * DIRECTIONALITY LOGIC:
 *   Household heads and ritual specialists sit near the beneficiary end: they set the terms of engagement and derive social/material benefit from continuing the practice. Rural farming households are near-symmetric — they pay in offerings and labor but receive real coping value, and their exit options are constrained mainly by the absence of a better alternative rather than by coercion. No stakeholder is authored as a clean victim: this reading's structural claim is precisely that the beneficiary/victim structure is unclear and diffuse, which is why victims[] is empty and the claimed type sits at rope rather than tangled_rope or snare.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (accessible ritual recourse for ordinary misfortune) remains live and is corroborated by archaeological continuity across the Amarna disruption — the practice did not depend on, and was not eliminated by, either priestly or pharaonic religious policy. This blocks a mandatrophy misreading: an observer expecting all Egyptian religious authority to route through temple or crown might mistake this substrate's persistence as failed compliance with official doctrine, when in fact it never depended on that doctrine's success or failure to begin with.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    diffuse_authority_vs_invisible_capture,
    'Is the beneficiary structure of folk ritual practice genuinely diffuse (no institution captures rents), or does it conceal a form of capture too dispersed for the historical record to register — e.g., systematic extraction by village ritual specialists that simply lacks the documentary trail temple economies leave?',
    'Comparative anthropological data from analogous living folk-ritual economies, where offering flows and specialist compensation can be directly observed, could establish whether dispersed ritual economies typically develop concentrated beneficiaries over time or remain genuinely diffuse.',
    'If capture is present but undocumented, this reading would need to move toward tangled_rope with village ritual specialists as a concentrated beneficiary class; if genuinely diffuse, the rope classification and unclear beneficiary structure stand.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diffuse_authority_vs_invisible_capture, empirical, 'Whether apparent diffuseness of folk ritual benefit is real or an artifact of the archaeological record''s blindness to informal extraction.').

omega_variable(
    sibling_reading_foreclosure_boundary,
    'Does this reading''s claim that pharaoh and priesthood are both ''distant elites'' irrelevant to lived religious practice logically foreclose the amun_polytheistic_reading''s claim that legitimate divine access runs through priestly interpretation, or can both be true simultaneously at different social strata?',
    'Examine whether elite and folk practice operated as non-overlapping magisteria (different social strata, no competition for the same legitimacy claim) versus genuinely competing accounts of the same legitimacy question.',
    'If non-overlapping, coexists_with is the correct relation (as authored); if genuinely competing for the same legitimacy claim within a single framework, a forecloses relation would be more accurate and would require re-examining the amun_polytheistic_reading''s exclusivity claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_boundary, conceptual, 'Whether folk and priestly legitimacy claims coexist across social strata or genuinely compete for the same authority claim.').

omega_variable(
    atenist_pressure_penetration,
    'Did Atenist monotheistic policy under Akhenaten actually reach and suppress village-level folk practice, or is the folk substrate''s apparent imperviousness itself evidence that state religious campaigns never had village-level enforcement capacity?',
    'Archaeological survey of household shrine continuity specifically during the Amarna period (rather than before/after) would show whether folk practice paused, went underground, or continued openly.',
    'Continued open practice during Amarna would strongly support this reading''s claim of structural insulation from top-down revision; evidence of suppression or concealment would suggest the folk substrate was more exposed to state power than this reading assumes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(atenist_pressure_penetration, empirical, 'Whether Atenist state policy actually penetrated village ritual practice during its enforcement period.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_legitimacy_substrate__folk_syncretistic_reading, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t0, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(divi_tr_t50, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 50, 0.24).
narrative_ontology:measurement(divi_tr_t100, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 100, 0.25).
narrative_ontology:measurement(divi_tr_t150, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 150, 0.26).
narrative_ontology:measurement(divi_tr_t200, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 200, 0.27).
narrative_ontology:measurement(divi_tr_t250, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 250, 0.28).
narrative_ontology:measurement(divi_tr_t300, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 300, 0.28).

% Extraction over time
narrative_ontology:measurement(divi_be_t0, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(divi_be_t50, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 50, 0.16).
narrative_ontology:measurement(divi_be_t100, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 100, 0.17).
narrative_ontology:measurement(divi_be_t150, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 150, 0.18).
narrative_ontology:measurement(divi_be_t200, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 200, 0.18).
narrative_ontology:measurement(divi_be_t250, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 250, 0.18).
narrative_ontology:measurement(divi_be_t300, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 300, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(divine_legitimacy_substrate__folk_syncretistic_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_legitimacy_substrate__folk_syncretistic_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(divine_legitimacy_substrate__folk_syncretistic_reading, 0.1).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__folk_syncretistic_reading, amun_polytheistic_reading).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__folk_syncretistic_reading, atenist_monotheistic_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the divine_legitimacy_substrate kernel. amun_polytheistic_reading locates legitimacy in priestly interpretation of a multi-deity cosmology anchored on Amun-Ra; atenist_monotheistic_reading locates it exclusively in pharaonic revelation of Aten; this story (folk_syncretistic_reading) locates it in diffuse household/village practice indifferent to both. Each carries its own epsilon and beneficiary structure: this reading's epsilon is low (0.18, diffuse near-symmetric exchange) versus the priestly reading's likely higher, concentrated extraction and the Atenist reading's likely high, contested, enforcement-dependent extraction. The readings are linked, not merged, per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
