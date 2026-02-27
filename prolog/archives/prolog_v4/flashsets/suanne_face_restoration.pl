% ============================================================================
% CONSTRAINT STORY: suanne_face_restoration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-28
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_suanne_face_restoration, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: suanne_face_restoration
 *   human_readable: The Coup of Positive Politeness
 *   domain: social
 *
 * SUMMARY:
 *   During a 1988 basketball game, SuAnne Marie Big Crow, a Lakota player,
 *   faced a hostile, taunting crowd. This event highlights the complex
 *   interplay of prejudice, resilience, and the restoration of dignity. The
 *   'coup of positive politeness' refers to SuAnne's ability to turn the
 *   hostile environment into a platform for demonstrating strength and
 *   restoring 'face' to herself and her community, a face historically
 *   damaged by injustices. The Lakota community used SuAnne's performance to
 *   rewrite this social dynamic.
 *
 * KEY AGENTS:
 *   - SuAnne Marie Big Crow: Beneficiary and victim (moderate/constrained) - constrained by the environment but benefits from showing resilience.
 *   - Hostile Taunting Crowd: Victim (powerless/trapped) - trapped by its own actions.
 *   - Lakota Community: Beneficiary (institutional/arbitrage) - benefits from SuAnne's actions reinforcing cultural pride.
 *   - Opposing Team: Victim (powerful/mobile) - victims of negative publicity and benefit from demonstrating sportsmanship
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(suanne_face_restoration, 0.6).
domain_priors:suppression_score(suanne_face_restoration, 0.7).
domain_priors:theater_ratio(suanne_face_restoration, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(suanne_face_restoration, extractiveness, 0.6).
narrative_ontology:constraint_metric(suanne_face_restoration, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(suanne_face_restoration, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(suanne_face_restoration, tangled_rope).
narrative_ontology:human_readable(suanne_face_restoration, "The Coup of Positive Politeness").
narrative_ontology:topic_domain(suanne_face_restoration, "social").

domain_priors:requires_active_enforcement(suanne_face_restoration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(suanne_face_restoration, suanne_marie_big_crow).
narrative_ontology:constraint_beneficiary(suanne_face_restoration, lakota_community).
narrative_ontology:constraint_victim(suanne_face_restoration, hostile_taunting_crowd).
narrative_ontology:constraint_victim(suanne_face_restoration, opposing_team).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The Lakota community benefits from SuAnne's resilience and the restoration of positive self-representation against historical injustices. They can 'arbitrage' this event into cultural pride and reinforcement of community values.
constraint_indexing:constraint_classification(suanne_face_restoration, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% The crowd is trapped by its own actions and prejudices. They become the target of restored justice and positive politeness, unable to exit the situation without social consequences. The community has no ability to resist or exit the unfolding event.
constraint_indexing:constraint_classification(suanne_face_restoration, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% SuAnne is constrained by the circumstances of the game and the hostile environment, but she also benefits from the opportunity to display strength and resilience, thus restoring 'face' to herself and her community. She is both a victim and a beneficiary.
constraint_indexing:constraint_classification(suanne_face_restoration, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% The opposing team is in an awkward position. They are mobile but constrained by the social pressure of the situation. They are the victims of negative publicity, but also can benefit from demonstrating sportsmanship and respect.
constraint_indexing:constraint_classification(suanne_face_restoration, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% An analytical observer sees the event as a tangled rope, a complex interplay of power, prejudice, resilience, and face-saving. The historical context of Lakota-white relations, combined with the social dynamics of a high-stakes basketball game, create a complicated and revealing case study.
constraint_indexing:constraint_classification(suanne_face_restoration, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(suanne_face_restoration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(suanne_face_restoration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(suanne_face_restoration, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(suanne_face_restoration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(suanne_face_restoration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness: 0.6 - The hostile crowd extracts emotional and mental energy from SuAnne, while SuAnne extracts social capital from the situation by demonstrating grace under pressure. Suppression: 0.7 - The crowd's hostile behavior suppresses SuAnne's ability to freely express herself and play the game without facing prejudice. SuAnne's subsequent actions suppresses the crowd's ability to negatively influence the event.
 *
 * PERSPECTIVAL GAP:
 *   The Lakota community sees a 'rope' situation as the community is building stronger narratives, the hostile crowd is trapped in the 'snare' of their actions, SuAnne experiences a tangled rope, while the other team balances sportsmanship. The analytical observer can examine each of these types from a historical and cultural background.
 *
 * DIRECTIONALITY LOGIC:
 *   SuAnne and the Lakota community are beneficiaries because the event, despite its initial hostility, ultimately led to a restoration of dignity and cultural pride. The hostile crowd is a victim because their behavior backfired and led to negative consequences for them. The opposing team is victim because they have social pressure to manage.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    crowd_motivation,
    'What was the primary motivation behind the crowd''s hostile behavior?',
    'Analysis of historical context, interviews with attendees, and media coverage of the event.',
    'If motivated by racism, the constraint is more accurately classified as a snare for SuAnne. If motivated by general sports rivalry, the tangled rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crowd_motivation, empirical, 'Understanding the motivation behind the hostile crowd behavior.').

omega_variable(
    lasting_impact,
    'What was the lasting impact of the event on SuAnne and the Lakota community?',
    'Longitudinal study of SuAnne''s life and the Lakota community''s cultural pride and resilience.',
    'If positive, the rope classification for the community strengthens. If negative, the snare classification gains more weight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lasting_impact, empirical, 'Assessing the long-term effects of the event.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(suanne_face_restoration, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(suan_tr_t0, suanne_face_restoration, theater_ratio, 0, 0.2).
narrative_ontology:measurement(suan_tr_t5, suanne_face_restoration, theater_ratio, 5, 0.3).
narrative_ontology:measurement(suan_tr_t10, suanne_face_restoration, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(suan_be_t0, suanne_face_restoration, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(suan_be_t5, suanne_face_restoration, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(suan_be_t10, suanne_face_restoration, base_extractiveness, 10, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(suanne_face_restoration, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
