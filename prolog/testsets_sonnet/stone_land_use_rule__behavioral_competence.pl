% ============================================================================
% CONSTRAINT STORY: stone_land_use_rule__behavioral_competence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_stone_land_use_rule__behavioral_competence, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: stone_land_use_rule__behavioral_competence
 *   human_readable: Tsunami Stone Land-Use Prohibition (Live Behavioral Reading)
 *   domain: disaster_anthropology/institutional_memory/land_use_governance
 *
 * SUMMARY:
 *   This story instantiates the BEHAVIORAL COMPETENCE reading of the
 *   stone_land_use_rule kernel: the inscribed tsunami-warning stones are read
 *   as a live, binding land-use prohibition, sustained not by any statute or
 *   enforcement agency but by continuous daily spatial practice — households
 *   climbing the hill, elders retelling the warning, newcomers being told
 *   where not to build. Across the 78-year interval since the marking event
 *   this reading assumes, compliance holds, the flatland below the line
 *   remains structurally undeveloped, and the accepted economic cost (steeper
 *   commute, less convenient land) is the visible signature of a genuinely
 *   functioning coordination mechanism, not decorative memory. This is
 *   deliberately NOT the same constraint as the sibling commemorative_husk
 *   reading, in which the identical physical stone has decayed into symbolic
 *   gesture with no behavioral force and materially different epsilon.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(stone_land_use_rule__behavioral_competence, 0.12).
domain_priors:suppression_score(stone_land_use_rule__behavioral_competence, 0.28).
domain_priors:theater_ratio(stone_land_use_rule__behavioral_competence, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, extractiveness, 0.12).
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(stone_land_use_rule__behavioral_competence, rope).
narrative_ontology:human_readable(stone_land_use_rule__behavioral_competence, "Tsunami Stone Land-Use Prohibition (Live Behavioral Reading)").
narrative_ontology:topic_domain(stone_land_use_rule__behavioral_competence, "disaster_anthropology/institutional_memory/land_use_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(stone_land_use_rule__behavioral_competence, '54786c83-8cf0-44a0-a27c-08d6970f4e23').
narrative_ontology:cs_kernel_codification('54786c83-8cf0-44a0-a27c-08d6970f4e23', fixed_text).
narrative_ontology:cs_authority_grounding('54786c83-8cf0-44a0-a27c-08d6970f4e23', practice).
narrative_ontology:cs_interpretation_layer_present('54786c83-8cf0-44a0-a27c-08d6970f4e23').
narrative_ontology:cs_reading_relation('54786c83-8cf0-44a0-a27c-08d6970f4e23', stone_land_use_rule__commemorative_husk, coexists_with).
narrative_ontology:cs_axiom('54786c83-8cf0-44a0-a27c-08d6970f4e23', foundational, inscribed_line_remains_operatively_binding).
narrative_ontology:cs_axiom_status(inscribed_line_remains_operatively_binding, holdable).
narrative_ontology:cs_axiom_grounding('54786c83-8cf0-44a0-a27c-08d6970f4e23', inscribed_line_remains_operatively_binding, empirically_contingent).
narrative_ontology:cs_axiom('54786c83-8cf0-44a0-a27c-08d6970f4e23', secondary, daily_spatial_practice_constitutes_live_enforcement).
narrative_ontology:cs_axiom_status(daily_spatial_practice_constitutes_live_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('54786c83-8cf0-44a0-a27c-08d6970f4e23', daily_spatial_practice_constitutes_live_enforcement, empirically_contingent).
narrative_ontology:cs_reference_frame('54786c83-8cf0-44a0-a27c-08d6970f4e23', post_disaster_marking_event).
narrative_ontology:cs_drift_state('54786c83-8cf0-44a0-a27c-08d6970f4e23', contemporary_78_year_mark, gap(stable, minor, true)).
narrative_ontology:cs_created_at('54786c83-8cf0-44a0-a27c-08d6970f4e23', '').
narrative_ontology:cs_kernel_id(stone_land_use_rule__behavioral_competence, stone_land_use_rule).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(stone_land_use_rule__behavioral_competence, coastal_hillside_residents).
narrative_ontology:constraint_beneficiary(stone_land_use_rule__behavioral_competence, future_generations_in_tsunami_path).
narrative_ontology:constraint_victim(stone_land_use_rule__behavioral_competence, lowland_commuters_and_daily_climbers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Households that settled and continue to build above the stone's inscribed line, accepting the daily cost of a steeper commute and less convenient access to the harbor and flatland services in exchange for standing outside the tsunami inundation zone. They pass the stone routinely and orient new construction relative to its line.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, coastal_hillside_residents, beneficiary,
    moderate, generational, constrained, local).

% Villagers whose work, schooling, or family ties pull them toward the harbor flatland every day; they bear the accepted cost of the hill climb in physical effort, transit time, and forgone flat, cheap land, precisely because the stone's line is treated as live rather than historical. Their compliance is what keeps the flatland empty of permanent housing.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, lowland_commuters_and_daily_climbers, payer,
    moderate, biographical, constrained, local).

% Those who retell the stone's warning at community gatherings, school visits, and to newcomers, keeping the inscription's instruction ('do not build below this point') operative as a live rule rather than a historical curiosity. They administer no formal permitting system but they are the mechanism by which the rule is transmitted and re-asserted each generation.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, village_elders_and_oral_transmitters, agenda_setter,
    moderate, generational, constrained, local).

% Not yet born or not yet party to the decision, they inherit whatever settlement pattern current compliance produces. If the line holds, they inherit housing outside the inundation zone; if it erodes, they inherit exposure without having had any voice in the drift.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, future_generations_in_tsunami_path, beneficiary,
    powerless, civilizational, trapped, local).

% External and internal actors who see cheap, flat, harbor-adjacent land sitting underused and would prefer to build hotels, shops, or housing there. They are not party to the oral-transmission network and have no formal channel to contest the stone's authority; their interest is structurally absent from the compliance-reinforcing gatherings.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, developers_and_land_speculators, excluded,
    organized, biographical, mobile, regional).

% Occasionally references the stones when drafting or defending post-disaster zoning ordinances, converting the informal, practice-based rule into formal code where it aligns with the stone's line. Watches compliance drift as a signal for whether formal codification is needed.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, municipal_planning_office, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(stone_land_use_rule__behavioral_competence, municipal_planning_office, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(stone_land_use_rule__behavioral_competence, diffuse).
narrative_ontology:fixing_cost_class(stone_land_use_rule__behavioral_competence, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves an intergenerational transmission problem: how to preserve a hard-won, empirically validated safety threshold (the historical tsunami inundation line) across a timespan longer than living memory, without relying on any single institution surviving intact.
% TRANSFER_FUNCTION: Moves land-use opportunity away from flat, economically convenient lowland toward steeper, less convenient hillside; moves daily physical and time cost onto those whose lives pull them toward the harbor; moves the benefit of reduced inundation exposure onto everyone who builds above the line, including people not yet born.
% ABSENT_VOICES: Developers and land speculators who would profit from building on the vacant flatland have no seat in the oral-transmission network that sustains the rule; their absence is structural, not incidental — the rule's persistence depends partly on their exclusion from the local meaning-making apparatus that keeps the stone live.
% DISAPPEARANCE_RATIONALE: If the stone's behavioral authority vanished overnight — if the daily practice of treating its line as binding simply stopped being transmitted — the economic pressure to build on the flat, harbor-adjacent land would very likely reassert itself within a generation, as it has in many nearby settlements whose markers decayed to commemorative status. Compliance is doing real, load-bearing work; it is not free-floating custom.
% FOUNDING_PROBLEM: Repeated historical tsunamis destroyed lowland settlements; survivors erected inscribed stones marking the maximum observed inundation line and instructing descendants never to build below it, because written records and formal government zoning could not be trusted to survive the same disasters that would recur only once every few generations.
% FOUNDING_PROBLEM_CORROBORATION: Independent disaster-anthropology fieldwork and post-2011 tsunami surveys (conducted by researchers with no stake in any individual village's compliance) documented that villages where the stones remained behaviorally operative suffered dramatically lower structural loss in the lowland zone than neighboring villages where equivalent markers had become purely commemorative — corroboration external to the benefiting residents themselves.
narrative_ontology:disappearance_verdict(stone_land_use_rule__behavioral_competence, world_rearranges).
narrative_ontology:founding_problem_status(stone_land_use_rule__behavioral_competence, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(stone_land_use_rule__behavioral_competence, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(stone_land_use_rule__behavioral_competence, 'none', 1).
narrative_ontology:epsilon_provenance(stone_land_use_rule__behavioral_competence, 0.12, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(stone_land_use_rule__behavioral_competence_tests).
:- end_tests(stone_land_use_rule__behavioral_competence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.12 at interval end) because the rule's operation approximates genuine coordination: it solves a real, validated safety problem and the costs borne by lowland commuters are the ordinary price of any land-use zoning that steers development away from a hazard zone, not rent extracted by an administering party. Theater ratio is authored very low (0.08 at end) because the mechanism is functionally live — people are still climbing the hill and still building above the line — not performing compliance while building below it anyway. Suppression is authored moderate (0.28), reflecting real but soft social pressure (a newcomer who builds below the line faces disapproval, not legal penalty) rather than coercive enforcement. Accessibility collapse is high (0.72) because, once internalized, the line genuinely forecloses lowland building as a live option in most residents' practical reasoning.
 *
 * DIRECTIONALITY LOGIC:
 *   Hillside residents and future generations are the structural beneficiaries — the rule subsidizes their long-run safety at the cost of daily inconvenience they have accepted. Lowland commuters and daily climbers bear the transfer most directly: the hill climb is a real, recurring cost imposed by compliance, though they are simultaneously beneficiaries of the same protection, which is why exit is authored as constrained rather than trapped. Developers and speculators are excluded rather than coordinated: their absence from the oral-transmission network is structural, and they have no channel to contest a rule that costs them a specific, valuable parcel of flat land.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status is authored as 'live' — tsunamis of the historically recorded magnitude remain a real recurring risk to this coastline, corroborated by disaster-anthropology fieldwork external to the village's own testimony. Because status=live pairs with disappearance_verdict=world_rearranges, this reading shows no mismatch signature: the arrangement is not a zombie mandate persisting past its function. This is precisely the reading that must be kept structurally distinct from the sibling commemorative_husk reading, where an identical physical marker would show status=dead paired with world_rearranges or contested — the capture/zombie flag that the mismatch-only consumer is built to catch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compliance_verification_across_stones,
    'For any given stone, is the behavioral_competence reading or the commemorative_husk reading the empirically correct account of its current operative status?',
    'Site survey correlating structure density and building permits below the inscribed line against post-disaster damage assessments and interviews with residents about whether the stone''s line is treated as binding in practice.',
    'If a given village''s stone shows dense modern construction below the line, that stone instantiates the commemorative_husk reading, not this one, regardless of how the marker is discussed in tourism or commemorative contexts; classification must follow the site''s actual compliance data, not the marker''s continued physical presence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_verification_across_stones, empirical, 'Whether a specific stone''s kernel reading is behavioral_competence or commemorative_husk is a site-by-site empirical question, not a property of the kernel itself.').

omega_variable(
    drift_risk_within_this_reading,
    'Even within villages where the behavioral_competence reading currently holds, is compliance trending toward erosion as living memory of the original disaster recedes further from the 78-year mark?',
    'Longitudinal tracking of construction permits and lot occupancy below the inscribed line over the next generation, cross-referenced with whether oral transmission events (elder talks, school visits) continue at historical frequency.',
    'A detected downward trend in transmission frequency would predict eventual transition from this reading to the commemorative_husk reading at that site — the omega marks that the two readings are not just alternative interpretations but plausible successive phases at the same physical location.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(drift_risk_within_this_reading, empirical, 'Whether the currently-live reading is stable or is itself drifting toward the sibling reading over time.').

omega_variable(
    exclusion_of_developers_naturalness,
    'Is the exclusion of developers and speculators from the oral-transmission network a benign structural feature of informal community governance, or does it constitute a suppressed constituency whose absence artificially understates contestation?',
    'Examine whether developers have ever attempted and failed to build below the line, versus never having attempted due to anticipated social cost — the former indicates active suppression, the latter indicates genuine absence of demand.',
    'If developers have been actively rebuffed (permit denials, social ostracism of violators), the low authored suppression score understates the true suppressive force maintaining the rule and this reading edges toward tangled_rope; if no such attempts exist, the rope reading with low suppression stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exclusion_of_developers_naturalness, conceptual, 'Whether developer exclusion reflects genuine absence of contestation or an unmeasured suppression mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(stone_land_use_rule__behavioral_competence, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ston_tr_t0, stone_land_use_rule__behavioral_competence, theater_ratio, 0, 0.02).
narrative_ontology:measurement(ston_tr_t13, stone_land_use_rule__behavioral_competence, theater_ratio, 13, 0.03).
narrative_ontology:measurement(ston_tr_t26, stone_land_use_rule__behavioral_competence, theater_ratio, 26, 0.03).
narrative_ontology:measurement(ston_tr_t39, stone_land_use_rule__behavioral_competence, theater_ratio, 39, 0.04).
narrative_ontology:measurement(ston_tr_t52, stone_land_use_rule__behavioral_competence, theater_ratio, 52, 0.05).
narrative_ontology:measurement(ston_tr_t65, stone_land_use_rule__behavioral_competence, theater_ratio, 65, 0.06).
narrative_ontology:measurement(ston_tr_t78, stone_land_use_rule__behavioral_competence, theater_ratio, 78, 0.08).

% Extraction over time
narrative_ontology:measurement(ston_be_t0, stone_land_use_rule__behavioral_competence, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(ston_be_t13, stone_land_use_rule__behavioral_competence, base_extractiveness, 13, 0.06).
narrative_ontology:measurement(ston_be_t26, stone_land_use_rule__behavioral_competence, base_extractiveness, 26, 0.08).
narrative_ontology:measurement(ston_be_t39, stone_land_use_rule__behavioral_competence, base_extractiveness, 39, 0.09).
narrative_ontology:measurement(ston_be_t52, stone_land_use_rule__behavioral_competence, base_extractiveness, 52, 0.1).
narrative_ontology:measurement(ston_be_t65, stone_land_use_rule__behavioral_competence, base_extractiveness, 65, 0.11).
narrative_ontology:measurement(ston_be_t78, stone_land_use_rule__behavioral_competence, base_extractiveness, 78, 0.12).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(stone_land_use_rule__behavioral_competence, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(stone_land_use_rule__behavioral_competence, identity_coordination).
narrative_ontology:boltzmann_floor_override(stone_land_use_rule__behavioral_competence, 0.1).
narrative_ontology:affects_constraint(stone_land_use_rule__behavioral_competence, stone_land_use_rule__commemorative_husk).

% DUAL FORMULATION NOTE:
% This story and stone_land_use_rule__commemorative_husk are sibling readings of the same kernel (stone_land_use_rule): the physical marker and its inscribed line are held constant, but this reading asserts the line is currently behaviorally binding (low epsilon, genuine coordination, real accepted cost) while the sibling asserts the line has decayed to symbolic status (behavioral force absent, whatever cost structure existed no longer explains settlement patterns). They are not the same constraint measured differently; they are different empirical claims about compliance state, hence different epsilon values and different classifications are expected and correct.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
