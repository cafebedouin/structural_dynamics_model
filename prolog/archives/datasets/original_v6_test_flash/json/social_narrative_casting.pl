% ============================================================================
% CONSTRAINT STORY: social_narrative_casting
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_social_narrative_casting, []).

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
 *   constraint_id: social_narrative_casting
 *   human_readable: Social Narrative Casting (Criticism-as-Projection)
 *   domain: social/psychological
 *
 * SUMMARY:
 *   This constraint models the act of criticism as an attempt by a critic
 *   (the 'Director') to 'hire' the subject into a specific role (villain,
 *   victim, obstacle) within the critic's internal narrative. The Director
 *   benefits by reinforcing their worldview while the Subject can experience
 *   extraction depending on their power and ability to escape the narrative.
 *
 * KEY AGENTS:
 *   - The Director: Primary beneficiary (powerful/mobile) - benefits from reinforcing their internal narrative.
 *   - The Subject: Primary victim (powerless/trapped) - experiences diminished agency and imposition of a negative role.
 *   - Analytical Observer: Analyzes the interaction. (analytical/analytical)
 *   - Institutional Observer: Therapist or social worker (institutional/arbitrage)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(social_narrative_casting, 0.55).
domain_priors:suppression_score(social_narrative_casting, 0.45).
domain_priors:theater_ratio(social_narrative_casting, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(social_narrative_casting, extractiveness, 0.55).
narrative_ontology:constraint_metric(social_narrative_casting, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(social_narrative_casting, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(social_narrative_casting, tangled_rope).
narrative_ontology:human_readable(social_narrative_casting, "Social Narrative Casting (Criticism-as-Projection)").
narrative_ontology:topic_domain(social_narrative_casting, "social/psychological").

domain_priors:requires_active_enforcement(social_narrative_casting).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(social_narrative_casting, the_director).
narrative_ontology:constraint_victim(social_narrative_casting, the_subject).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The subject, when unable to escape the narrative imposed upon them, experiences the criticism as a snare. Their agency is diminished, and their actions are reinterpreted to fit the director's narrative.
constraint_indexing:constraint_classification(social_narrative_casting, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% The subject may attempt to resist the role imposed on them, leading to a tangled rope dynamic. They are constrained by the director's influence, but also have some capacity to negotiate or redefine their role. Some subjects also derive benefit from playing the role cast for them.
constraint_indexing:constraint_classification(social_narrative_casting, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% The director benefits by reinforcing their internal narrative and exercising control over the relationship. The criticism, in their view, serves to maintain the stability of their own worldview.
constraint_indexing:constraint_classification(social_narrative_casting, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% From an analytical perspective, the social narrative casting is a tangled rope - the director benefits from narrative control, the subject is extracted from, and the dynamic is actively enforced. The degree of extraction and power dynamic shifts based on the power dynamic.
constraint_indexing:constraint_classification(social_narrative_casting, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% An institutional observer, such as a therapist or social worker, might see the dynamic as a rope if they believe it serves a therapeutic purpose or maintains social order, even if it involves some extraction.
constraint_indexing:constraint_classification(social_narrative_casting, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(social_narrative_casting_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(social_narrative_casting, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(social_narrative_casting, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(social_narrative_casting, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(social_narrative_casting_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness: Moderate (0.55). The Director extracts from the Subject by limiting their agency and imposing a specific role upon them. Suppression: Moderate (0.45). The Director suppresses the Subject's alternative narratives. Theater ratio: Low (0.30) because in many cases, there is genuine care and concern, but it devolves into narrative casting.
 *
 * PERSPECTIVAL GAP:
 *   The Director sees their actions as helpful or necessary, while the Subject experiences them as oppressive. The analytical observer recognizes the power dynamic and the extraction taking place. An institutional observer might see it as a necessary, if imperfect, means of maintaining social order.
 *
 * DIRECTIONALITY LOGIC:
 *   The Director benefits by reinforcing their internal narrative, leading to a low directionality value. The Subject experiences a loss of agency and imposition of a negative role, leading to a high directionality value. The Analytical Observer attempts to understand the dynamics and assess the overall impact. The institutional observer's directionality depends on their assessment of the overall social benefit.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    narrative_fixity,
    'How fixed is the director''s internal narrative?',
    'Psychological assessment of the director''s cognitive rigidity and resistance to alternative interpretations.',
    'High fixity implies greater resistance from the director, reinforcing the tangled rope dynamic. Low fixity allows for negotiation and shifting of roles.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(narrative_fixity, empirical, 'The degree to which the director''s internal narrative is resistant to change.').

omega_variable(
    subject_agency,
    'How capable is the subject of resisting the imposed narrative?',
    'Assessment of the subject''s self-esteem, assertiveness, and social support network.',
    'Low agency reinforces the snare dynamic. High agency leads to negotiation or escape from the narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subject_agency, empirical, 'The degree to which the subject can resist the imposed narrative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(social_narrative_casting, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soci_tr_t0, social_narrative_casting, theater_ratio, 0, 0.1).
narrative_ontology:measurement(soci_tr_t5, social_narrative_casting, theater_ratio, 5, 0.2).
narrative_ontology:measurement(soci_tr_t10, social_narrative_casting, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(soci_be_t0, social_narrative_casting, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(soci_be_t5, social_narrative_casting, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(soci_be_t10, social_narrative_casting, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(social_narrative_casting, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
