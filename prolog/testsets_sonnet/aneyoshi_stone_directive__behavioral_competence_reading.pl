% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_directive__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_stone_directive__behavioral_competence_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: aneyoshi_stone_directive__behavioral_competence_reading
 *   human_readable: Aneyoshi Tsunami Stone as Live Behavioral Land-Use Boundary
 *   domain: disaster_anthropology/institutional_memory/land_use_governance
 *
 * SUMMARY:
 *   In 1933, survivors of the Showa Sanriku tsunami in the hamlet of
 *   Aneyoshi, Iwate Prefecture, erected a stone inscribed with a warning: do
 *   not build below this point, no matter how many generations pass. For 78
 *   years, across a generational gap with no living witnesses to the original
 *   disaster by the time of the next major event, households in Aneyoshi
 *   built and rebuilt only above the marked line. When the 2011 Tohoku
 *   tsunami struck, the wave reportedly stopped just short of the stone; the
 *   handful of houses above it were undamaged. This reading treats the
 *   directive as a genuine, behaviorally competent land-use constraint — a
 *   low-cost, non-coercive, non-institutional transmission of an empirically
 *   grounded hazard boundary that was actually followed, not merely
 *   commemorated.
 *
 * KEY AGENTS:
 *   - aneyoshi_households_above_the_marker: primary beneficiary (powerless/trapped) — followed the directive at zero enforcement cost and received the entire survival benefit
 *   - aneyoshi_village_elders: agenda_setter with no institutional apparatus (moderate/constrained) — transmitted the warning generation to generation via retelling
 *   - prefectural_planning_authorities: institutional observer (institutional/analytical) — never codified the boundary into formal law; the constraint operated entirely outside administrative planning
 *   - post_1933_migrants_and_descendants: bore the ordinary opportunity cost of building on less desirable slope land (powerless/constrained) — retroactively vindicated in 2011
 *   - disaster_anthropology_researchers: analytical observer (analytical/global) — documents the case as evidence for durable oral risk transmission
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_directive__behavioral_competence_reading, 0.04).
domain_priors:suppression_score(aneyoshi_stone_directive__behavioral_competence_reading, 0.06).
domain_priors:theater_ratio(aneyoshi_stone_directive__behavioral_competence_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, extractiveness, 0.04).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 0.06).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_directive__behavioral_competence_reading, mountain).
narrative_ontology:human_readable(aneyoshi_stone_directive__behavioral_competence_reading, "Aneyoshi Tsunami Stone as Live Behavioral Land-Use Boundary").
narrative_ontology:topic_domain(aneyoshi_stone_directive__behavioral_competence_reading, "disaster_anthropology/institutional_memory/land_use_governance").

domain_priors:emerges_naturally(aneyoshi_stone_directive__behavioral_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_directive__behavioral_competence_reading, 'a92cd241-7e12-455b-974e-55439477c4a7').
narrative_ontology:cs_kernel_codification('a92cd241-7e12-455b-974e-55439477c4a7', implicit).
narrative_ontology:cs_authority_grounding('a92cd241-7e12-455b-974e-55439477c4a7', practice).
narrative_ontology:cs_interpretation_layer_present('a92cd241-7e12-455b-974e-55439477c4a7').
narrative_ontology:cs_reading_relation('a92cd241-7e12-455b-974e-55439477c4a7', aneyoshi_stone_directive__commemorative_husk_reading, coexists_with).
narrative_ontology:cs_axiom('a92cd241-7e12-455b-974e-55439477c4a7', foundational, oral_transmission_preserves_behavioral_force_across_generational_gap).
narrative_ontology:cs_axiom_status(oral_transmission_preserves_behavioral_force_across_generational_gap, holdable).
narrative_ontology:cs_axiom_grounding('a92cd241-7e12-455b-974e-55439477c4a7', oral_transmission_preserves_behavioral_force_across_generational_gap, empirically_contingent).
narrative_ontology:cs_axiom('a92cd241-7e12-455b-974e-55439477c4a7', secondary, inundation_boundary_is_stable_physical_fact_not_social_construction).
narrative_ontology:cs_axiom_status(inundation_boundary_is_stable_physical_fact_not_social_construction, holdable).
narrative_ontology:cs_axiom_grounding('a92cd241-7e12-455b-974e-55439477c4a7', inundation_boundary_is_stable_physical_fact_not_social_construction, empirically_contingent).
narrative_ontology:cs_reference_frame('a92cd241-7e12-455b-974e-55439477c4a7', post_1933_survivor_transmitted_boundary).
narrative_ontology:cs_drift_state('a92cd241-7e12-455b-974e-55439477c4a7', pre_2011_contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('a92cd241-7e12-455b-974e-55439477c4a7', '').
narrative_ontology:cs_kernel_id(aneyoshi_stone_directive__behavioral_competence_reading, aneyoshi_stone_directive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_stone_directive__behavioral_competence_reading, aneyoshi_households_above_the_marker).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_directive__behavioral_competence_reading, post_1933_migrants_and_descendants).
narrative_ontology:constraint_victim(aneyoshi_stone_directive__behavioral_competence_reading, post_1933_migrants_and_descendants).
narrative_ontology:constraint_vindicates(aneyoshi_stone_directive__behavioral_competence_reading, tsunami_inundation_line_is_geographically_stable).
narrative_ontology:constraint_vindicates(aneyoshi_stone_directive__behavioral_competence_reading, oral_transmission_of_hazard_boundary_survives_multigenerational_gap).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Households built and maintained above the stone's inscribed line, following the directive without any external enforcement. When the 2011 tsunami reached almost exactly the marker, these households were physically untouched; the directive cost them nothing to follow and provided the entire benefit when the wave came. No one collects rent from their compliance.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__behavioral_competence_reading, aneyoshi_households_above_the_marker, beneficiary,
    powerless, generational, trapped, local).

% Successive generations of village elders retold the stone's warning and the 1896 and 1933 tsunami histories at community gatherings, without any institutional mandate, budget, or enforcement apparatus. They administer nothing beyond memory transmission; there is no office, fee, or licensing structure behind their role.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__behavioral_competence_reading, aneyoshi_village_elders, agenda_setter,
    moderate, generational, constrained, local).

% Postwar and modern land-use planners in Iwate Prefecture never formally codified the stone's line into zoning law; the boundary persisted purely through village oral tradition and household siting decisions, parallel to and unconnected with formal administrative planning.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__behavioral_competence_reading, prefectural_planning_authorities, observer,
    institutional, biographical, analytical, regional).

% Households that settled or resettled in the decades between 1933 and 2011 bore the ordinary cost of building above the marker line — less flat land, longer commutes to the harbor, higher construction cost on slope — for a hazard event that, from any single lifetime's vantage, might never recur. When it did recur in 2011, this cost was retroactively vindicated.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__behavioral_competence_reading, post_1933_migrants_and_descendants, payer,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(aneyoshi_stone_directive__behavioral_competence_reading, post_1933_migrants_and_descendants, beneficiary).

% Post-2011 researchers documented that Aneyoshi was among the few Sanriku coast villages where the 2011 tsunami stopped almost exactly at a pre-existing generational warning marker, and treat the case as evidence for durable oral risk transmission across long inter-catastrophe intervals.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__behavioral_competence_reading, disaster_anthropology_researchers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Encodes an empirically validated tsunami inundation boundary observed across the 1896 and 1933 events into a durable, low-cost, non-institutional siting rule: build below this line and lose your house and possibly your life when the wave returns.
% TRANSFER_FUNCTION: Transfers no resources between parties. The only thing moved is information — the inundation-line observation — from the generation that measured it to generations that had not personally witnessed a tsunami, at essentially zero transmission cost (oral retelling, a carved stone).
% ABSENT_VOICES: No party is excluded from this reading's operation: the constraint has no gatekeeper, no toll, and no licensing function that a locked-out party could contest. The nearest analog to an excluded voice is anyone who might argue the geological boundary itself is contestable, but no such party appears in the historical or documentary record.
% DISAPPEARANCE_RATIONALE: If the directive's behavioral force had vanished before 2011 — i.e., if households above the marker had drifted downslope onto the floodplain, as happened in many neighboring settlements — the 2011 tsunami would have struck occupied structures instead of stopping at an empty boundary. The world quite literally did not rearrange around this constraint in 2011 specifically because the constraint held; its removal is not hypothetical, it is the counterfactual the disaster demonstrates.
% FOUNDING_PROBLEM: The 1896 Meiji Sanriku tsunami and the 1933 Showa Sanriku tsunami killed most of the village's population; survivors needed a transmissible, low-literacy-dependent, generation-spanning way to mark the water's reach so that rebuilding did not repeat the same siting error.
% FOUNDING_PROBLEM_CORROBORATION: Independent post-2011 field surveys by disaster-anthropology and seismology researchers (outside the village and with no stake in the directive's continued authority) documented that the 2011 tsunami inundation line coincided closely with the stone's inscribed boundary and that Aneyoshi's above-line structures were undamaged while comparable unmarked settlements were destroyed — corroboration from parties with no benefit riding on the directive's validity.
narrative_ontology:disappearance_verdict(aneyoshi_stone_directive__behavioral_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_stone_directive__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_directive__behavioral_competence_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(aneyoshi_stone_directive__behavioral_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_stone_directive__behavioral_competence_reading, 0.04, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_stone_directive__behavioral_competence_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, ExtMetricName, E),
    domain_priors:suppression_score(aneyoshi_stone_directive__behavioral_competence_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(aneyoshi_stone_directive__behavioral_competence_reading),
    narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(aneyoshi_stone_directive__behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored near zero (0.02-0.04) across the full interval because no party collects rent, fee, or advantage from another's compliance — the directive costs compliant households only the ordinary opportunity cost of building on less convenient land, and that cost was borne by the same people who received the benefit. Suppression is low (0.06) because there is no coercive enforcement apparatus; the constraint held through voluntary generational transmission, not compulsion. Accessibility collapse is authored high (0.88) reflecting how completely the marked boundary closed off 'build below the line' as a live option for anyone who took the retelling seriously — once you know the water reached here twice, building below it stops looking like a real alternative. Resistance is near-zero (0.05): there is no recorded case of a household resisting or contesting the boundary; the closest friction is ordinary land-use inconvenience, not opposition to the directive itself.
 *
 * DIRECTIONALITY LOGIC:
 *   The households above the marker are declared beneficiaries because they are the direct recipients of the coordination function (survival) with no offsetting extraction — this is a case where beneficiary status does NOT imply extraction from anyone else, which is exactly the pattern the false-summit-mountain signature exists to test. No victim group is declared under this reading because no party pays a cost that flows to another party's benefit; the migrants who bore ordinary siting inconvenience are the same population that benefited, so this is intrapersonal opportunity cost, not extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status is authored 'live' rather than 'dead' specifically because this reading holds that the hazard the directive addresses (periodic Sanriku tsunami inundation to a fixed geographic line) recurred and was met by the directive still operating — this is the opposite of mandatrophy. A mandatrophic reading of a stone marker would be one where the underlying hazard had genuinely receded (say, coastal geology had changed to make the line irrelevant) while the marker persisted as ritual. This reading's entire structural claim is that no such drift occurred: the founding problem and the operative solution remained aligned across 78 years, which is why the constraint computes as mountain rather than piton or scaffold. The commemorative_husk_reading is the piton-flavored sibling that would apply if the transmission had actually decayed and the 2011 alignment were coincidental.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coincidence_vs_transmission,
    'Did the 2011 inundation line coincide with the stone''s marker because the directive was actually behaviorally followed across 78 years, or because slope, land value, and unrelated settlement economics independently kept households above that elevation, with the stone''s warning playing no causal role?',
    'Oral history interviews with pre-2011 Aneyoshi residents about whether the stone''s warning was actively cited in household siting decisions versus regarded as inert memorial; comparison with other Sanriku villages that had similar markers but were destroyed in 2011 to isolate whether the marker''s presence correlates with survival independent of terrain.',
    'If transmission was genuinely causal, this reading (behavioral_competence) is the structurally accurate one and the sibling commemorative_husk_reading is the false reading for this case. If terrain/economics fully explain the siting independent of the stone, the sibling reading is closer to correct and this reading overstates the directive''s live behavioral force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coincidence_vs_transmission, empirical, 'Whether the marker caused compliant siting or merely coincided with it.').

omega_variable(
    beneficiary_without_extraction_naturalness,
    'Is this constraint a genuine natural-law-adjacent mountain (an accurately transmitted physical hazard boundary) despite declaring a beneficiary group, or does the beneficiary declaration indicate a constructed social constraint masquerading as natural fact?',
    'Check whether the tsunami inundation boundary itself is a stable physical-geographic fact (bathymetry, harbor shape, historical wave run-up records) independent of any human institution — if the line is geologically/hydrodynamically determined rather than socially negotiated, the beneficiary declaration reflects differential exposure to a natural hazard rather than differential access to an extracted resource.',
    'If the inundation line is a stable physical fact, FSM evaluation should find no concentrated extractive beneficiary and the mountain classification should survive; if the boundary is better understood as a socially renegotiated or drifting convention, the constraint may be closer to a constructed scaffold than a mountain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_without_extraction_naturalness, conceptual, 'Whether declaring a beneficiary on a mountain-claimed constraint here reflects natural hazard exposure rather than constructed extraction.').

omega_variable(
    generational_transmission_fidelity,
    'How much did the specific content of the directive (build above THIS line) degrade or drift across the two to three generations between 1933 and 2011, and would a fourth or fifth generation have retained the same fidelity absent the 2011 reinforcement event?',
    'Comparative study of other Japanese tsunami-stone villages with markers older than Aneyoshi''s, tracking documented compliance decay curves against elapsed generations since the last confirming disaster.',
    'If fidelity was already visibly decaying by 2011 (some households drifting downslope, only some households still citing the stone), the behavioral_competence_reading would need revision toward a mixed or declining-fidelity account rather than clean mountain persistence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(generational_transmission_fidelity, empirical, 'Whether transmission fidelity was stable or already eroding by the time of the 2011 test.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_directive__behavioral_competence_reading, 1933, 2011).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t1933, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 1933, 0.05).
narrative_ontology:measurement_basis(aney_tr_t1933, observed).
narrative_ontology:measurement(aney_tr_t1946, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 1946, 0.05).
narrative_ontology:measurement_basis(aney_tr_t1946, observed).
narrative_ontology:measurement(aney_tr_t1960, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 1960, 0.06).
narrative_ontology:measurement_basis(aney_tr_t1960, observed).
narrative_ontology:measurement(aney_tr_t1975, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 1975, 0.07).
narrative_ontology:measurement_basis(aney_tr_t1975, observed).
narrative_ontology:measurement(aney_tr_t1990, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 1990, 0.07).
narrative_ontology:measurement_basis(aney_tr_t1990, observed).
narrative_ontology:measurement(aney_tr_t2000, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 2000, 0.08).
narrative_ontology:measurement_basis(aney_tr_t2000, observed).
narrative_ontology:measurement(aney_tr_t2011, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 2011, 0.08).
narrative_ontology:measurement_basis(aney_tr_t2011, observed).

% Extraction over time
narrative_ontology:measurement(aney_be_t1933, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 1933, 0.02).
narrative_ontology:measurement_basis(aney_be_t1933, observed).
narrative_ontology:measurement(aney_be_t1946, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 1946, 0.02).
narrative_ontology:measurement_basis(aney_be_t1946, observed).
narrative_ontology:measurement(aney_be_t1960, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 1960, 0.03).
narrative_ontology:measurement_basis(aney_be_t1960, observed).
narrative_ontology:measurement(aney_be_t1975, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 1975, 0.03).
narrative_ontology:measurement_basis(aney_be_t1975, observed).
narrative_ontology:measurement(aney_be_t1990, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 1990, 0.04).
narrative_ontology:measurement_basis(aney_be_t1990, observed).
narrative_ontology:measurement(aney_be_t2000, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 2000, 0.04).
narrative_ontology:measurement_basis(aney_be_t2000, observed).
narrative_ontology:measurement(aney_be_t2011, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 2011, 0.04).
narrative_ontology:measurement_basis(aney_be_t2011, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(aneyoshi_stone_directive__behavioral_competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_directive__behavioral_competence_reading, information_standard).
narrative_ontology:boltzmann_floor_override(aneyoshi_stone_directive__behavioral_competence_reading, 0.02).
narrative_ontology:affects_constraint(aneyoshi_stone_directive__behavioral_competence_reading, aneyoshi_stone_directive__commemorative_husk_reading).

% DUAL FORMULATION NOTE:
% This story (behavioral_competence_reading) and aneyoshi_stone_directive__commemorative_husk_reading decompose the single natural-language label 'the Aneyoshi tsunami stone directive' into two structurally distinct constraints per the ε-invariance principle. This reading claims the directive retained live behavioral force across 78 years (mountain, negligible extraction, no coercive enforcement, beneficiary present but non-extractive). The sibling claims the directive's behavioral force atrophied into memorial theater during the inter-catastrophe period, with any 2011 alignment attributable to independent factors — a piton-flavored reading with a materially different theater_ratio and a different account of what, if anything, the stone actually did. The two stories share the same physical artifact and interval but diverge sharply on whether ε reflects genuine ongoing coordination or decayed performance; they are linked here rather than merged because their metric profiles are incompatible within a single constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
