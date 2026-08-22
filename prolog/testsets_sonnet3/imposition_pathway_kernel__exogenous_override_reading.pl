% ============================================================================
% CONSTRAINT STORY: imposition_pathway_kernel__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_pathway_kernel__exogenous_override_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: imposition_pathway_kernel__exogenous_override_reading
 *   human_readable: Meiji State-Decreed Calendar and Dress Reform (Exogenous Override Reading)
 *   domain: historical_sociology/state_formation
 *
 * SUMMARY:
 *   This story instantiates the exogenous_override_reading of the
 *   imposition_pathway_kernel: the claim that state capacity can displace an
 *   entrenched commitment structure (here, the lunisolar calendar and
 *   traditional court dress) directly by decree and enforcement, without any
 *   preceding fringe-adoption or gradual-climb phase. The Meiji government's
 *   1872-73 reforms are read here as a case where compliance was manufactured
 *   top-down through administrative registration, school curricula, and court
 *   protocol -- not as a compressed or accelerated version of organic
 *   adoption. The extraction is real (rural households and provincial trades
 *   absorb disruption costs with no transition pathway, traditionalist
 *   officials lose status basis overnight) but so is the coordination
 *   function (national administrative legibility, treaty compatibility).
 *   Suppression peaks sharply at the decree point (1873) and decays over
 *   subsequent decades as enforcement infrastructure matures and formal
 *   compliance becomes habitual -- but never falls to the low baseline a
 *   genuinely emergent norm would show, evidencing continued informal
 *   dual-practice beneath the formal layer.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_pathway_kernel__exogenous_override_reading, 0.55).
domain_priors:suppression_score(imposition_pathway_kernel__exogenous_override_reading, 0.72).
domain_priors:theater_ratio(imposition_pathway_kernel__exogenous_override_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_pathway_kernel__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(imposition_pathway_kernel__exogenous_override_reading, "Meiji State-Decreed Calendar and Dress Reform (Exogenous Override Reading)").
narrative_ontology:topic_domain(imposition_pathway_kernel__exogenous_override_reading, "historical_sociology/state_formation").

domain_priors:requires_active_enforcement(imposition_pathway_kernel__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_pathway_kernel__exogenous_override_reading, 'e5e2c34a-3f76-478a-a678-358a9545cbd9').
narrative_ontology:cs_kernel_codification('e5e2c34a-3f76-478a-a678-358a9545cbd9', formalized).
narrative_ontology:cs_authority_grounding('e5e2c34a-3f76-478a-a678-358a9545cbd9', extraction).
narrative_ontology:cs_interpretation_layer_present('e5e2c34a-3f76-478a-a678-358a9545cbd9').
narrative_ontology:cs_reading_relation('e5e2c34a-3f76-478a-a678-358a9545cbd9', imposition_pathway_kernel__endogenous_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('e5e2c34a-3f76-478a-a678-358a9545cbd9', imposition_pathway_kernel__hybrid_cascade_reading, influences).
narrative_ontology:cs_axiom('e5e2c34a-3f76-478a-a678-358a9545cbd9', foundational, state_capacity_sufficient_for_unmediated_displacement).
narrative_ontology:cs_axiom_status(state_capacity_sufficient_for_unmediated_displacement, holdable).
narrative_ontology:cs_axiom_grounding('e5e2c34a-3f76-478a-a678-358a9545cbd9', state_capacity_sufficient_for_unmediated_displacement, empirically_contingent).
narrative_ontology:cs_axiom('e5e2c34a-3f76-478a-a678-358a9545cbd9', secondary, compliance_absent_fringe_is_coercion_not_emergence).
narrative_ontology:cs_axiom_status(compliance_absent_fringe_is_coercion_not_emergence, holdable).
narrative_ontology:cs_axiom_grounding('e5e2c34a-3f76-478a-a678-358a9545cbd9', compliance_absent_fringe_is_coercion_not_emergence, empirically_contingent).
narrative_ontology:cs_reference_frame('e5e2c34a-3f76-478a-a678-358a9545cbd9', tokugawa_lunisolar_ritual_order).
narrative_ontology:cs_drift_state('e5e2c34a-3f76-478a-a678-358a9545cbd9', meiji_decree_enforcement_period, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('e5e2c34a-3f76-478a-a678-358a9545cbd9', '').
narrative_ontology:cs_kernel_id(imposition_pathway_kernel__exogenous_override_reading, imposition_pathway_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__exogenous_override_reading, meiji_state_apparatus).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__exogenous_override_reading, westernizing_elite_bureaucrats).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__exogenous_override_reading, foreign_treaty_powers).
narrative_ontology:constraint_victim(imposition_pathway_kernel__exogenous_override_reading, rural_agricultural_households).
narrative_ontology:constraint_victim(imposition_pathway_kernel__exogenous_override_reading, traditionalist_court_officials).
narrative_ontology:constraint_victim(imposition_pathway_kernel__exogenous_override_reading, provincial_almanac_producers).
narrative_ontology:constraint_vindicates(imposition_pathway_kernel__exogenous_override_reading, state_capacity_sufficient_for_commitment_displacement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the 1872-73 decrees replacing the lunisolar calendar with the Gregorian calendar and mandating Western dress for court and military officials. Enforces compliance through administrative registration, official ceremony, and school curricula, with no preceding period of voluntary grassroots adoption documented before the decree. Collects legitimacy with foreign powers and administrative uniformity as the payoff.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, meiji_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Already oriented toward Western institutional models, this faction advocated the decree and benefits from being pre-adapted to the new commitment structure -- their existing cultural capital converts directly into administrative advantage once the state imposes the new calendar and dress code on everyone else.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, westernizing_elite_bureaucrats, beneficiary,
    organized, biographical, arbitrage, national).

% Western diplomatic and commercial actors benefit from Japan's calendar and dress conforming to their own conventions, easing treaty negotiation, trade scheduling, and diplomatic protocol. They exert no direct enforcement but their expectations are cited by the state as partial justification for the decree.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, foreign_treaty_powers, beneficiary,
    institutional, generational, analytical, global).

% Depend on the lunisolar calendar for planting, harvest timing, and festival scheduling embedded in generations of agricultural practice. The decree displaces this without offering an adoption pathway -- there is no fringe of rural households who had begun using the Gregorian calendar before the state mandated it. They absorb the disruption cost directly, forced to maintain informal dual-calendar reckoning for agricultural purposes while complying formally with the new official calendar.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, rural_agricultural_households, payer,
    powerless, biographical, trapped, local).

% Hold status and ritual authority tied to the old calendar's festival cycle and to traditional dress as a marker of court rank. The decree strips this basis of authority overnight; they can comply and lose distinctiveness, or resist and lose position, with no gradual accommodation period in which their prior practice was incrementally validated.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, traditionalist_court_officials, payer,
    moderate, biographical, constrained, national).

% Their entire trade -- producing and distributing lunisolar almanacs used for agricultural and ritual timing across provinces -- is rendered officially obsolete by decree with no transition market in which Gregorian-calendar products first competed alongside theirs. Many are put out of livelihood without a phase during which demand for the new commitment grew before enforcement arrived.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, provincial_almanac_producers, payer,
    powerless, biographical, trapped, regional).

% Study the Meiji reforms as a test case for whether commitment displacement always proceeds through fringe adoption and gradual climb, or whether sufficient state capacity can substitute enforcement for emergent adoption entirely. Their analysis is the site of the kernel contest this story instantiates.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, historical_sociologists_of_state_formation, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imposition_pathway_kernel__exogenous_override_reading, meiji_state_apparatus).
narrative_ontology:fixing_cost_class(imposition_pathway_kernel__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single official time-reckoning and dress standard across the state apparatus, courts, military, and eventually schools and commerce, enabling synchronized administration, treaty-compatible scheduling, and legible diplomatic presentation to foreign powers.
% TRANSFER_FUNCTION: Moves ritual authority, calendrical expertise rents, and status markers away from traditional court officials, agricultural timekeepers, and almanac producers toward the modernizing bureaucratic class and toward compatibility with foreign treaty partners; compliance costs are transferred onto rural households and provincial trades with no compensating transition support.
% ABSENT_VOICES: Rural agricultural households and provincial almanac producers were not consulted before the decree and had no organized channel to object; their informal continuation of lunisolar practice for agricultural purposes was never legally accommodated, only tolerated as private behavior beneath the official calendar.
% DISAPPEARANCE_RATIONALE: If the decree and its enforcement apparatus (administrative registration, school curricula, court protocol) were rescinded, official use of the Gregorian calendar and Western dress in Japanese state administration would not simply persist by inertia -- the underlying compliance was never organically adopted at scale outside the westernizing elite, so removing enforcement would likely see a reversion toward hybrid or traditional practice in provincial administration and rural life, evidencing that the arrangement's persistence depends on continued state enforcement rather than internalized commitment.
% FOUNDING_PROBLEM: The Meiji state needed rapid international legibility and internal administrative uniformity to renegotiate unequal treaties and project a 'civilized' state image to Western powers, plus a unified national time-and-status system to displace domain-level (han) variation inherited from the Tokugawa era.
% FOUNDING_PROBLEM_CORROBORATION: The state apparatus and its intellectual allies (compiled in Meiji government records and contemporary press) attest the founding problem was real and successfully solved. Independent historical-sociology analysis (outside both the historical beneficiary class and the current state) documents continued informal dual-calendar practice in rural Japan well into the twentieth century, suggesting the founding problem of full internal legibility was only partially and unevenly solved despite formal decree compliance.
narrative_ontology:disappearance_verdict(imposition_pathway_kernel__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_pathway_kernel__exogenous_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_pathway_kernel__exogenous_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(imposition_pathway_kernel__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_pathway_kernel__exogenous_override_reading, 0.55, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_pathway_kernel__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imposition_pathway_kernel__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imposition_pathway_kernel__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is highest immediately post-decree (0.62 at 1873) reflecting the shock of imposed cost with no adoption runway, then gradually declines as the westernizing elite's advantage normalizes and enforcement becomes routine rather than acute. Suppression is authored as the dominant metric (peaking 0.85 at decree point) because this reading's defining claim is that compliance was coerced, not chosen -- the suppression_requirement series is the direct empirical signature the exogenous_override_reading predicts and the endogenous_climb_reading would not. Theater ratio rises modestly post-decree (courtly ceremony, official calendar displays) but stays well below dominance, since the coordination function (administrative synchronization, treaty legibility) is genuinely functional, not merely performed.
 *
 * DIRECTIONALITY LOGIC:
 *   The state apparatus and the westernizing elite are structural beneficiaries: pre-adapted cultural capital converts directly into advantage once the decree levels the field by force. Foreign treaty powers benefit passively without direct enforcement action, hence lower directionality intensity despite institutional power. Rural households, provincial almanac producers, and traditionalist court officials are targets: no exit exists from the new official calendar (it governs administration and school life), even though informal, private continuation of the old calendar persists as a form of internal exile from full compliance -- captured here as 'trapped' rather than 'mobile' exit, since formal participation in state life requires nominal adherence regardless of private practice.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists conflating imposed compliance with organic obsolescence of the old calendar's coordination function -- the lunisolar system's agricultural utility did not vanish, it was administratively superseded. Classifying this as tangled_rope rather than pure snare preserves the genuine coordination gain (national legibility, treaty compatibility) that the endogenous_climb_reading and hybrid_cascade_reading would attribute to slower organic processes; the exogenous_override_reading's distinct contribution is asserting this gain was achieved via a mechanism (unmediated state decree) that neither sibling reading's M-set cell can represent, because both require some fringe or cascade stage this case never exhibited.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    invisible_fringe_vs_genuine_absence,
    'Did a pre-decree fringe of Gregorian-calendar or Western-dress adopters exist among Japanese elites with foreign contact (diplomats, some merchants, students returned from abroad) that has simply gone undocumented in the historical record, or was there genuinely no meaningful fringe-adoption stage before the 1872-73 decrees?',
    'Systematic archival search of merchant diaries, mission records, and treaty-port correspondence for evidence of informal pre-decree calendar or dress switching among any population segment, compared against the null hypothesis of zero meaningful adoption.',
    'If a genuine fringe existed and merely escaped record, the endogenous_climb_reading''s ''compressed climb with invisible fringe'' account gains support and this override reading''s core premise weakens. If the absence is real, the override reading is corroborated and the M-set framework''s gap is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(invisible_fringe_vs_genuine_absence, empirical, 'Whether the apparent absence of pre-decree fringe adoption is a genuine absence or a documentation gap.').

omega_variable(
    coercion_vs_rapid_internalization,
    'Was post-decree compliance sustained primarily by ongoing coercive enforcement, or did it rapidly internalize into genuine preference within a generation, such that the ''override'' mechanism produced a durable commitment structurally indistinguishable from organic adoption after the transition period?',
    'Compare enforcement intensity trajectories against measures of voluntary compliance in domains where enforcement later weakened (e.g., rural informal calendar use post-1900) -- persistence without enforcement would indicate internalization; reversion would indicate continued coercion-dependence.',
    'If internalization occurred rapidly, the exogenous_override_reading''s distinctiveness from hybrid_cascade_reading narrows, since the outcome converges with cascade-completion dynamics. If dependence on enforcement persisted, the override reading''s claim of a mechanistically distinct M-set cell is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_vs_rapid_internalization, empirical, 'Whether post-decree compliance became self-sustaining independent of enforcement, bearing on whether override and cascade mechanisms are truly distinct.').

omega_variable(
    state_capacity_threshold_generalizability,
    'Is the Meiji case evidence of a general mechanism (state capacity above some threshold enables override-without-fringe for any commitment structure) or an idiosyncratic case tied to Japan''s specific crisis conditions (unequal treaty pressure, centralizing Restoration momentum) that may not generalize to other imposed-commitment cases?',
    'Comparative case analysis against other purported top-down commitment impositions (e.g., calendar reforms elsewhere, dress code mandates in other modernizing states) to test whether the override mechanism recurs independent of Japan-specific crisis conditions.',
    'If idiosyncratic, the override reading''s claim to require a distinct general M-set cell weakens to a special-case footnote; if generalizable, the case supports a structurally necessary addition to the framework as claimed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_capacity_threshold_generalizability, conceptual, 'Whether the override mechanism generalizes beyond the Meiji case or is a special-case artifact of unique historical conditions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_pathway_kernel__exogenous_override_reading, 1868, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t1868, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 1868, 0.1).
narrative_ontology:measurement(impo_tr_t1873, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 1873, 0.35).
narrative_ontology:measurement(impo_tr_t1878, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 1878, 0.32).
narrative_ontology:measurement(impo_tr_t1885, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 1885, 0.3).
narrative_ontology:measurement(impo_tr_t1892, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 1892, 0.28).
narrative_ontology:measurement(impo_tr_t1900, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 1900, 0.28).

% Extraction over time
narrative_ontology:measurement(impo_be_t1868, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 1868, 0.3).
narrative_ontology:measurement(impo_be_t1873, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 1873, 0.62).
narrative_ontology:measurement(impo_be_t1878, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 1878, 0.58).
narrative_ontology:measurement(impo_be_t1885, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 1885, 0.55).
narrative_ontology:measurement(impo_be_t1892, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 1892, 0.5).
narrative_ontology:measurement(impo_be_t1900, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 1900, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t1868, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 1868, 0.2).
narrative_ontology:measurement(impo_su_t1873, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 1873, 0.85).
narrative_ontology:measurement(impo_su_t1878, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 1878, 0.78).
narrative_ontology:measurement(impo_su_t1885, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 1885, 0.7).
narrative_ontology:measurement(impo_su_t1892, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 1892, 0.62).
narrative_ontology:measurement(impo_su_t1900, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 1900, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_pathway_kernel__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(imposition_pathway_kernel__exogenous_override_reading, imposition_pathway_kernel__endogenous_climb_reading).
narrative_ontology:affects_constraint(imposition_pathway_kernel__exogenous_override_reading, imposition_pathway_kernel__hybrid_cascade_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of imposition_pathway_kernel, each instantiating a structurally distinct claim about how commitment displacement occurs. endogenous_climb_reading holds all displacement traces to fringe adoption and gradual climb, treating this case as a compressed climb with an undocumented fringe stage. hybrid_cascade_reading holds that state imposition creates an artificial fringe (conscripts, bureaucrats) that then climbs organically -- override initiates, climb completes. This exogenous_override_reading holds that the initial displacement event required no fringe stage at all and was accomplished directly by decree and enforcement, making top-down imposition a mechanistically distinct M-set cell rather than a variant of climb dynamics. The three readings share the same historical episode (Meiji calendar/dress reform) but author different epsilon, different beneficiary/victim structure emphasis, and different classification because they disagree about the generative mechanism, not merely its evaluation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
