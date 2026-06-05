% ============================================================================
% CONSTRAINT STORY: credentialism_national_security
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_credentialism_national_security, []).

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
 *   constraint_id: credentialism_national_security
 *   human_readable: "The Harvard Rule" - Elite Credentialism in US National Security
 *   domain: political/social
 *
 * SUMMARY:
 *   An informal but highly powerful constraint in the US national security
 *   establishment that effectively requires candidates for top-tier positions
 *   (e.g., Secretary of Defense, National Security Advisor) to hold degrees
 *   from a small handful of elite universities. This "Harvard Rule"
 *   reinforces existing power structures and limits access to leadership
 *   positions for individuals from diverse backgrounds and institutions.
 *
 * KEY AGENTS:
 *   - Elite Universities: Primary beneficiaries (institutional/arbitrage) - benefit from prestige and influence.
 *   - Graduates of Elite Programs: Secondary beneficiaries (moderate/constrained) - benefit from preferential access but also constrained by expectations.
 *   - Non-Elite Candidates: Primary victims (powerless/trapped) - excluded from top positions due to lack of elite credentials.
 *   - Meritocratic Selection: Abstract victim (powerless/trapped) - the principle of selecting candidates based on merit is undermined by credentialism.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(credentialism_national_security, 0.6).
domain_priors:suppression_score(credentialism_national_security, 0.7).
domain_priors:theater_ratio(credentialism_national_security, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(credentialism_national_security, extractiveness, 0.6).
narrative_ontology:constraint_metric(credentialism_national_security, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(credentialism_national_security, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(credentialism_national_security, tangled_rope).
narrative_ontology:human_readable(credentialism_national_security, "\"The Harvard Rule\" - Elite Credentialism in US National Security").
narrative_ontology:topic_domain(credentialism_national_security, "political/social").

domain_priors:requires_active_enforcement(credentialism_national_security).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(credentialism_national_security, elite_universities).
narrative_ontology:constraint_beneficiary(credentialism_national_security, graduates_elite_programs).
narrative_ontology:constraint_victim(credentialism_national_security, non_elite_candidates).
narrative_ontology:constraint_victim(credentialism_national_security, meritocratic_selection).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Candidates from non-elite institutions find it nearly impossible to break into the top echelons of US national security. They are trapped by the credentialism and lack the necessary network connections. High extraction.
constraint_indexing:constraint_classification(credentialism_national_security, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% Graduates of elite programs benefit from the credentialing system but are also constrained by its expectations and the pressure to maintain the system. They have some mobility but are ultimately tied to the network.
constraint_indexing:constraint_classification(credentialism_national_security, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Elite universities benefit from the system by maintaining their prestige and influence. They can arbitrage their position to attract top students and faculty. Low extraction, high coordination.
constraint_indexing:constraint_classification(credentialism_national_security, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% An analytical observer sees the system as a tangled rope: a mix of coordination (signaling competence) and extraction (excluding non-elite candidates). The long-term consequences for national security are uncertain.
constraint_indexing:constraint_classification(credentialism_national_security, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(credentialism_national_security_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(credentialism_national_security, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(credentialism_national_security, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(credentialism_national_security, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(credentialism_national_security_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60): High, as the system extracts opportunity from non-elite candidates. Suppression (0.70): High, as it is difficult to overcome the credentialing barrier. Theater Ratio (0.30): Relatively low, as the credential often has a genuine signaling value (even if imperfect) for competence, especially to outside observers unfamiliar with specific institutions.
 *
 * PERSPECTIVAL GAP:
 *   Non-elite candidates experience the system as a snare, while elite universities see it as a rope that coordinates talent towards them. The graduates of elite programs have a mixed experience (tangled rope). An analytical observer recognizes the blend of coordination (signaling quality) and extraction (limiting diversity).
 *
 * DIRECTIONALITY LOGIC:
 *   Elite Universities: d=0.05 (beneficiary, arbitrage). Non-Elite Candidates: d=0.95 (victim, trapped). Graduates of Elite Programs: d=0.5 (both benefit and are constrained). Analytical Observer: d=0.72 (observes and assesses the system's effects, primarily observing extraction).
 *
 * MANDATROPHY ANALYSIS:
 *   This is a tangled rope, not a pure snare, because the credentials do provide some signal of competence and coordinate talent toward important positions. However, it is not a pure rope because it also excludes qualified candidates and reinforces existing power structures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    performance_vs_pedigree,
    'Does elite pedigree correlate with superior performance in national security roles?',
    'Comparative performance analysis of individuals with and without elite credentials in similar national security positions, controlling for experience and other factors.',
    'If pedigree strongly correlates with performance, the constraint is partially justified as a heuristic (moving toward rope). If not, the extraction effect is dominant and harmful (entrenching snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_vs_pedigree, empirical, 'The relationship between elite pedigree and actual performance.').

omega_variable(
    diversity_costs,
    'What are the opportunity costs associated with limiting the pool of talent to a small set of elite institutions?',
    'Analysis of the diversity of perspectives, experiences, and skill sets among national security professionals with and without elite credentials. Examination of how different perspectives affect policy outcomes.',
    'If diversity is critical for effective policy-making, the exclusion of non-elite candidates can lead to suboptimal decisions (increasing net extraction). If homogeneity is beneficial, the constraint is less harmful (decreasing net extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diversity_costs, conceptual, 'The costs of limiting diversity in national security leadership.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(credentialism_national_security, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cred_tr_t0, credentialism_national_security, theater_ratio, 0, 0.2).
narrative_ontology:measurement(cred_tr_t10, credentialism_national_security, theater_ratio, 10, 0.3).
narrative_ontology:measurement(cred_tr_t20, credentialism_national_security, theater_ratio, 20, 0.35).

% Extraction over time
narrative_ontology:measurement(cred_be_t0, credentialism_national_security, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(cred_be_t10, credentialism_national_security, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(cred_be_t20, credentialism_national_security, base_extractiveness, 20, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(credentialism_national_security, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
