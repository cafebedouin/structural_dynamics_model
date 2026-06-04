% ============================================================================
% CONSTRAINT STORY: equality_code__horizontal_reach_question_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equality_code__horizontal_reach_question_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: equality_code__horizontal_reach_question_reading
 *   human_readable: Horizontal Reach of Equality Code: Private Gatekeepers and Civil Society Exclusion
 *   domain: constitutional_law/equality_doctrine
 *
 * SUMMARY:
 *   The horizontal reach question asks whether equality guarantees in
 *   constitutional codes extend to private actors — clubs, employers,
 *   platforms — or are limited to state action. This reading frames the
 *   constraint as a boundary question: how far equality doctrine reaches past
 *   the state is the live doctrinal frontier, not which test or standard
 *   applies once the boundary is crossed. The constraint exhibits mixed
 *   extractiveness because private gatekeeping creates genuine coordination
 *   problems (property rights, contractual freedom, associational choice must
 *   exist) while simultaneously suppressing remedies for excluded individuals
 *   who face no realistic private alternatives. The extractiveness value
 *   (0.52) reflects that private discrimination is partially answerable
 *   through the doctrine but not fully — many cases remain in the gap between
 *   state-action immunity and statutory coverage. Suppression (0.58) reflects
 *   that absent horizontal reach, claimants have no constitutional remedy for
 *   private exclusion; the suppression mechanism is doctrinal (the
 *   state-action requirement itself suppresses claims) rather than merely
 *   procedural. Theater ratio (0.48) indicates the doctrine is still
 *   substantially functional — courts are actively articulating and applying
 *   horizontal reach principles — though formalist state/private categories
 *   increasingly perform theater (the conceptual distinction persists while
 *   substantive reasoning tracks functional power asymmetries).
 *
 * KEY AGENTS:
 *   - Claimants against private exclusion (powerless/trapped): primary beneficiaries of horizontal reach expansion — the only remedy for exclusion by gatekeepers controlling essential access
 *   - Private gatekeepers — clubs, employers, platforms (powerful/mobile): primary targets of horizontal reach doctrine — bear extraction burden of justifying exclusions, losing autonomy to exclude arbitrarily
 *   - The state (institutional/arbitrage): benefits from doctrinal clarity; experiences the boundary question as coordination problem, not extraction
 *   - Civil rights coalitions (organized/constrained): benefit from expanded reach but bear litigation cost and doctrinal instability
 *   - Legislatures (organized/constrained): developing statutory alternative frameworks that bypass the constitutional question through targeted regulation
 *   - Formalist doctrine tradition (institutional/arbitrage): institutional carrier of the state/private binary; persists through inertia as functional pressure mounts
 *   - Analytical observer (analytical/analytical): risks naturalizing a contingent institutional boundary as an immutable feature of freedom
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equality_code__horizontal_reach_question_reading, 0.52).
domain_priors:suppression_score(equality_code__horizontal_reach_question_reading, 0.58).
domain_priors:theater_ratio(equality_code__horizontal_reach_question_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equality_code__horizontal_reach_question_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(equality_code__horizontal_reach_question_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(equality_code__horizontal_reach_question_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equality_code__horizontal_reach_question_reading, tangled_rope).
narrative_ontology:human_readable(equality_code__horizontal_reach_question_reading, "Horizontal Reach of Equality Code: Private Gatekeepers and Civil Society Exclusion").
narrative_ontology:topic_domain(equality_code__horizontal_reach_question_reading, "constitutional_law/equality_doctrine").

domain_priors:requires_active_enforcement(equality_code__horizontal_reach_question_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equality_code__horizontal_reach_question_reading, '300b83d3-8c7e-412a-a9fc-9e3b3d3e348f').
narrative_ontology:cs_kernel_codification('300b83d3-8c7e-412a-a9fc-9e3b3d3e348f', fixed_text).
narrative_ontology:cs_authority_grounding('300b83d3-8c7e-412a-a9fc-9e3b3d3e348f', lineage).
narrative_ontology:cs_interpretation_layer_present('300b83d3-8c7e-412a-a9fc-9e3b3d3e348f').
narrative_ontology:cs_reading_relation('300b83d3-8c7e-412a-a9fc-9e3b3d3e348f', equality_code__arbitrariness_doctrine_reading, influences).
narrative_ontology:cs_reading_relation('300b83d3-8c7e-412a-a9fc-9e3b3d3e348f', equality_code__classification_test_reading, influences).
narrative_ontology:cs_axiom('300b83d3-8c7e-412a-a9fc-9e3b3d3e348f', foundational, private_actors_subject_to_equality).
narrative_ontology:cs_axiom_status(private_actors_subject_to_equality, holdable).
narrative_ontology:cs_axiom_grounding('300b83d3-8c7e-412a-a9fc-9e3b3d3e348f', private_actors_subject_to_equality, deontological).
narrative_ontology:cs_axiom('300b83d3-8c7e-412a-a9fc-9e3b3d3e348f', foundational, functional_power_asymmetry_triggers_doctrine).
narrative_ontology:cs_axiom_status(functional_power_asymmetry_triggers_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('300b83d3-8c7e-412a-a9fc-9e3b3d3e348f', functional_power_asymmetry_triggers_doctrine, empirically_contingent).
narrative_ontology:cs_reference_frame('300b83d3-8c7e-412a-a9fc-9e3b3d3e348f', state_action_requirement).
narrative_ontology:cs_drift_state('300b83d3-8c7e-412a-a9fc-9e3b3d3e348f', contemporary_corporate_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('300b83d3-8c7e-412a-a9fc-9e3b3d3e348f', '').
narrative_ontology:cs_kernel_id(equality_code__horizontal_reach_question_reading, equality_code).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equality_code__horizontal_reach_question_reading, claimants_against_private_exclusion).
narrative_ontology:constraint_victim(equality_code__horizontal_reach_question_reading, private_autonomy_defenses).
narrative_ontology:constraint_victim(equality_code__horizontal_reach_question_reading, civil_society_gatekeeping).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED INDIVIDUAL FROM PRIVATE GATEKEEPER (SNARE) — An individual excluded by a club, employer, or platform has no exit option within civil society. The exclusion is absolute within their local sphere. If horizontal reach does not extend to private actors, the individual is fully trapped — the equality guarantee becomes inaccessible. Maximum experienced extraction: excluded from both state and private institutional access.
constraint_indexing:constraint_classification(equality_code__horizontal_reach_question_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PRIVATE GATEKEEPER (TANGLED ROPE) — The private actor benefits from autonomy to select membership/employment/access (coordination function: property rights, contractual freedom, associational choice). Simultaneously, if horizontal reach extends, they bear extraction burden: obligation to justify exclusions, exposure to discrimination claims, reduced ability to exclude arbitrarily. Mixed experience — genuine coordination benefit alongside asymmetric extraction of decision-making power.
constraint_indexing:constraint_classification(equality_code__horizontal_reach_question_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE STATE (ROPE) — The state benefits from clarity on the horizontal reach boundary: it clarifies what equality obligations apply to state actors vs private actors, reducing jurisdictional ambiguity. The state experiences the constraint as pure coordination — defining the scope of equality doctrine enables consistent rule-of-law administration. Net beneficiary through arbitrage: the state can exit the question only by deciding it (and its decision shapes all subsequent cases).
constraint_indexing:constraint_classification(equality_code__horizontal_reach_question_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CIVIL RIGHTS COALITION (TANGLED ROPE) — Organized claimants benefit from expanded horizontal reach (coordination: collective defense against discrimination). They also bear costs: litigation risk, strategic dependence on judicial interpretation, doctrinal instability. Constrained exit: organized groups can exit only by accepting narrower protections. Mixed experience: significant benefit (access to remedy) alongside significant cost (doctrinal uncertainty, enforcement gaps).
constraint_indexing:constraint_classification(equality_code__horizontal_reach_question_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGISLATIVE ANTI-DISCRIMINATION FRAMEWORK (SCAFFOLD) — Statutory frameworks (employment discrimination laws, public accommodations statutes, platform regulation) represent temporary judicial solutions. As legislatures enact narrower, more precisely targeted anti-discrimination regimes, the constitutional question's urgency decays. The scaffold has a sunset: doctrinal clarity becomes less critical once statutory alternatives mature. Low effective extraction because organized agents see an exit path (statutory codification).
constraint_indexing:constraint_classification(equality_code__horizontal_reach_question_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: FORMALIST DOCTRINE / STATE ACTION REQUIREMENT (PITON) — The formalist distinction between state and private action (the state-action doctrine) persists largely as a conceptual theater piece. Courts acknowledge horizontal pressure (corporate power, platform gatekeeping) and carve exceptions (company towns, platforms as quasi-public forums) while maintaining the formal binary. The ritual persists through institutional inertia: the state/private distinction remains the organizing taxonomy of constitutional doctrine despite systematic pressure to abandon or significantly revise it. Piton classification: theater ratio ≥ 0.70, core functional purpose (clarifying state obligation boundary) increasingly performed by statutory rather than constitutional interpretation.
constraint_indexing:constraint_classification(equality_code__horizontal_reach_question_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some boundary between state obligation and private autonomy is inevitable: individuals and voluntary associations must have some sphere of decision-making free from constitutional constraint, or freedom of association becomes meaningless. This perspective sees the horizontal reach question as touching an immutable limit — there must be some private domain. However, the structural data reveals this as a false summit: which domain is 'private' is entirely contingent on institutional choices (property law, corporate form, franchise agreements, platform terms of service), not on any intrinsic feature of autonomy.
constraint_indexing:constraint_classification(equality_code__horizontal_reach_question_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equality_code__horizontal_reach_question_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(equality_code__horizontal_reach_question_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(equality_code__horizontal_reach_question_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(equality_code__horizontal_reach_question_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(equality_code__horizontal_reach_question_reading, TR),
    TR >= 0.70.

:- end_tests(equality_code__horizontal_reach_question_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint exhibits genuine extraction because private gatekeepers experience the doctrine as an obligation to justify exclusions (burden of persuasion shifts; decision-making power becomes transparent to challenge). But the extraction is not severe (0.72+) because statutory alternatives are maturing and because most private gatekeeping remains outside the doctrine's reach (scope limitation). The measurement trajectory (0.38 → 0.52) reflects increasing corporate and platform power — as private actors control more essential access (employment, social participation, credit, housing information), the functional pressure to extend horizontal reach intensifies. Suppression (0.58): Moderate-high. The state-action requirement itself is the suppression mechanism: it forecloses claims against private actors entirely, regardless of power asymmetry. Suppression rises over time (0.42 → 0.58) because the gap between state and private power narrows — institutional change (corporate consolidation, platform dominance) makes the suppression more visible as private exclusion becomes more consequential. Theater ratio (0.48): Moderate. The doctrine remains substantially functional — courts articulate horizontal reach principles and apply them — but formalist categories increasingly perform conceptual theater (the state/private divide persists as the organizing taxonomy while courts systematically depart from its logic through functional analysis). Rising theater (0.35 → 0.48) indicates the formalist framework is degrading relative to the functional analysis courts actually conduct.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is between the trapped claimant (Snare: no remedy available without horizontal reach) and the beneficiary private gatekeeper (Tangled Rope: both coordination benefit and extraction burden). The claimant sees binary closure — either the doctrine applies or it doesn't, and if it doesn't, exclusion is absolute. The gatekeeper sees mixed constraint — they benefit from autonomy but also from the legitimacy that comes with being able to justify (rather than merely assert) exclusion decisions. The analytical observer risks seeing mountain (an inevitable private/public distinction) when the structural data reveals false summit: the boundary is entirely contingent on institutional choices about corporate form, platform status, property law, franchise regulation. The scaffold perspective (legislative alternative) sees the constraint as temporary — statutory regimes are building an exit path that makes the constitutional question less urgent. The piton perspective (formalist doctrine) sees degradation — the state/private distinction persists as organizing concept even as courts depart from its logic.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by the agent's power level, exit options, and beneficiary/victim status. Claimants are victims (no exit, trapped) with high d → high χ; private gatekeepers are beneficiaries (arbitrage exit, institutional power) with low d → negative χ; the state is a beneficiary (arbitrage through decision-making authority) with institutional power and low d; civil rights coalitions are organized victims with constrained exit (moderate d); legislatures are organized beneficiaries with constrained exit (moderate d, negative χ through alternative pathway). The analytical observer (analytical/analytical) derives d from the perspective's position as an observer with no stake in the extraction flow — canonical d for analytical power is 0.73, producing f(d) ≈ 1.15, indicating the analytical perspective experiences the constraint as a structural problem that requires examination from outside the immediate interests.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves mandatrophy by identifying the horizontal reach question as a REAL structural tension, not a mere classification problem. The arbitrariness doctrine reading (sibling) focuses on whether *any* classification is rational; the classification test reading (sibling) focuses on *how* to determine rationality. This reading (horizontal reach) is distinct: it asks *to whom* the equality requirement applies. The three readings have empirically different outcomes: a court adopting the arbitrariness reading might extend it to private action as an aspect of general anti-caprice; a court adopting the classification reading might refuse to extend it because private actors aren't engaged in classifying for state purposes; a court adopting the horizontal reach reading is directly litigating whether the boundary exists. The mandatrophy is resolved by recognizing that these are three separate doctrinal framings that produce empirically different case outcomes and would need to be linked as a constraint family with network edges showing how each reading influences the others.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    private_autonomy_threshold,
    'Where is the boundary between constitutionally protected private autonomy and private action that must comply with equality guarantees? Is the boundary jurisdictional (state vs non-state actors) or functional (power over essential access)?',
    'Doctrinal analysis of existing exceptions: company towns, state-chartered corporations, platforms meeting public forum criteria. Empirical assessment of functional exclusivity: does the private actor control access to essential goods/services/social participation?',
    'If jurisdictional boundary holds: horizontal reach is limited to state and state-like actors; most private exclusion remains outside equality doctrine (Snare/Mountain from claimant perspective). If functional boundary prevails: significant expansion of horizontal reach based on power asymmetry (Tangled Rope/Rope from claimant perspective).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(private_autonomy_threshold, conceptual, 'Whether the horizontal reach boundary is jurisdictional or functional').

omega_variable(
    counterexample_to_private_autonomy,
    'Does modern private power (corporate employment, platform access, credit determination) functionally resemble state power in its gatekeeping capacity? Can the distinction between ''choosing your associations'' (authentic autonomy) and ''controlling access to essential participation'' (functional sovereignty) be sustained?',
    'Historical comparison: labor relations in early vs late capitalism; social participation dependency on private platform access; credit access as determinant of economic participation. Functional analysis of exit costs: can excluded individuals realistically find alternative private actors, or is exclusion effectively absolute?',
    'If modern private power functionally resembles state power: horizontal reach doctrine expands substantially; private gatekeepers must justify exclusions (Tangled Rope from all perspectives). If authentic autonomy distinctions hold: horizontal reach remains narrow; private gatekeeping retains broad immunity (Snare from claimant perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterexample_to_private_autonomy, empirical, 'Whether modern private power resembles state gatekeeping capacity').

omega_variable(
    this_reading_vs_sibling_underspecification,
    'Is the horizontal reach question a distinct constraint from the arbitrariness doctrine and the classification test, or are the three readings describing the same doctrinal framework from different angles?',
    'Doctrinal mapping: identify which equality guarantees each reading invokes; trace how a court adopting one reading would handle cases the other reading addresses; check whether the three readings produce empirically different case outcomes.',
    'If distinct constraints (three separate ε values): each reading has its own Prolog module; network edges link them as a constraint family. If variants of one constraint: they should be represented as perspectives on a single story, not separate kernel readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(this_reading_vs_sibling_underspecification, conceptual, 'Whether the three readings are distinct constraints or variants of the same doctrine').

omega_variable(
    legislative_displacement_of_constitutional_question,
    'As statutory anti-discrimination frameworks expand (employment law, public accommodations, platform regulation), does the constitutional horizontal reach question become moot, or does it retain salvific force for cases statutory coverage misses?',
    'Longitudinal analysis of horizontal reach doctrine citation post-legislation: has statutory regime displaced constitutional claims? Doctrinal mapping: what cases remain where horizontal reach doctrine is the only available equality claim?',
    'If legislative displacement occurs: scaffold sunset materializes; the constraint degrades to piton (institutional theater without functional dispute resolution). If residual salvific force persists: the constraint remains tangled rope; statutory regime addresses most cases but constitutional doctrine retains edge cases and principles.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legislative_displacement_of_constitutional_question, empirical, 'Whether statutory anti-discrimination regimes displace the constitutional horizontal reach question').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equality_code__horizontal_reach_question_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t0, equality_code__horizontal_reach_question_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(equa_tr_t10, equality_code__horizontal_reach_question_reading, theater_ratio, 10, 0.42).
narrative_ontology:measurement(equa_tr_t20, equality_code__horizontal_reach_question_reading, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(equa_be_t0, equality_code__horizontal_reach_question_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(equa_be_t10, equality_code__horizontal_reach_question_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(equa_be_t20, equality_code__horizontal_reach_question_reading, base_extractiveness, 20, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t0, equality_code__horizontal_reach_question_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(equa_su_t10, equality_code__horizontal_reach_question_reading, suppression_requirement, 10, 0.51).
narrative_ontology:measurement(equa_su_t20, equality_code__horizontal_reach_question_reading, suppression_requirement, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equality_code__horizontal_reach_question_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equality_code__horizontal_reach_question_reading, equality_code__arbitrariness_doctrine_reading).
narrative_ontology:affects_constraint(equality_code__horizontal_reach_question_reading, equality_code__classification_test_reading).

% DUAL FORMULATION NOTE:
% The equality code kernel is instantiated in three separate constraint stories representing three live doctrinal readings. Each reading has its own constraint_id and ε value. The horizontal reach question reading (this file) focuses on scope (state vs private); it is upstream of classification test questions (how to apply the test once scope is fixed) and downstream of arbitrariness questions (what triggers the need for any test). All three readings are linked as a constraint family sharing the kernel_id 'equality_code'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equality_code__horizontal_reach_question_reading, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
