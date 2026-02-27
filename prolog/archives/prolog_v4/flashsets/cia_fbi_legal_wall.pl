% ============================================================================
% CONSTRAINT STORY: cia_fbi_legal_wall
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cia_fbi_legal_wall, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: cia_fbi_legal_wall
 *   human_readable: The CIA/FBI Intelligence-Criminal "Wall" (pre-PATRIOT Act)
 *   domain: political/legal
 *
 * SUMMARY:
 *   The CIA/FBI "Wall" was a set of legal and procedural barriers erected to
 *   separate foreign intelligence from domestic criminal investigations,
 *   primarily to protect civil liberties. This separation aimed to prevent
 *   intelligence gathered for foreign purposes from being used in domestic
 *   criminal cases, thereby avoiding potential abuses of power. However, it
 *   also created a bottleneck in information sharing that critics argued
 *   hindered national security, especially in the context of
 *   counter-terrorism efforts prior to the passage of the PATRIOT Act.
 *
 * KEY AGENTS:
 *   - Civil Liberties: Primary beneficiary (institutional/arbitrage) - Protected by the separation of intelligence and law enforcement.
 *   - Domestic Criminal Investigations: Primary victim (powerless/trapped) - Hindered by the inability to access potentially relevant intelligence.
 *   - National Security: Secondary victim (powerless/trapped) - Compromised by the difficulty in connecting intelligence leads to prevent attacks.
 *   - FBI: Powerful, constrained - Limited by the Wall in conducting investigations, but also provided clarity.
 *   - CIA: Powerful, constrained - Operates autonomously but may benefit from inter-agency cooperation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cia_fbi_legal_wall, 0.55).
domain_priors:suppression_score(cia_fbi_legal_wall, 0.6).
domain_priors:theater_ratio(cia_fbi_legal_wall, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cia_fbi_legal_wall, extractiveness, 0.55).
narrative_ontology:constraint_metric(cia_fbi_legal_wall, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(cia_fbi_legal_wall, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cia_fbi_legal_wall, tangled_rope).
narrative_ontology:human_readable(cia_fbi_legal_wall, "The CIA/FBI Intelligence-Criminal \"Wall\" (pre-PATRIOT Act)").
narrative_ontology:topic_domain(cia_fbi_legal_wall, "political/legal").

domain_priors:requires_active_enforcement(cia_fbi_legal_wall).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cia_fbi_legal_wall, civil_liberties).
narrative_ontology:constraint_beneficiary(cia_fbi_legal_wall, cia_operational_autonomy).
narrative_ontology:constraint_victim(cia_fbi_legal_wall, domestic_criminal_investigations).
narrative_ontology:constraint_victim(cia_fbi_legal_wall, national_security).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: National Security (Snare) - The inability to connect intelligence and criminal investigations directly harmed national security. Trapped because unable to unilaterally dismantle the wall pre-PATRIOT Act.
constraint_indexing:constraint_classification(cia_fbi_legal_wall, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% Perspective 2: Civil Liberties Advocates (Rope) - The wall provided a coordination mechanism protecting citizens from intelligence overreach. Beneficiary with arbitrage exit via legal challenges.
constraint_indexing:constraint_classification(cia_fbi_legal_wall, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Perspective 3: FBI Agents (Tangled Rope) - Constrained by the wall, but also benefited from the clarity it provided regarding legal boundaries. Extraction exists, but there's also a coordination benefit in knowing the rules.
constraint_indexing:constraint_classification(cia_fbi_legal_wall, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective 4: CIA (Tangled Rope) - Benefitted in some ways from the separation of intelligence and criminal investigations. The agency was free to operate abroad with minimal oversight.
constraint_indexing:constraint_classification(cia_fbi_legal_wall, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective 5: Analytical Observer (Tangled Rope) - The wall represents a tangled effort to balance civil liberties and national security, creating both coordination and extraction for different parties.
constraint_indexing:constraint_classification(cia_fbi_legal_wall, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cia_fbi_legal_wall_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cia_fbi_legal_wall, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cia_fbi_legal_wall, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cia_fbi_legal_wall, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(cia_fbi_legal_wall_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.55) reflects the degree to which the separation of intelligence and criminal investigations hindered law enforcement's ability to pursue leads and prevent attacks. Suppression (0.60) accounts for the restrictions placed on information sharing. Theater ratio (0.40) indicates a moderate level of performative activity, where the wall was sometimes used to demonstrate compliance without necessarily achieving its intended purpose.
 *
 * PERSPECTIVAL GAP:
 *   National security agencies see the wall as an obstacle to effective intelligence gathering and response. Civil liberties advocates see it as a necessary protection against government overreach. FBI agents experience the constraint as limiting but also clarifying, providing guidelines for investigations. The analytical observer sees the wall as a tangled effort to balance competing interests, resulting in both benefits and costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (civil liberties, CIA operational autonomy) experience lower effective extraction, as the wall serves to protect their interests. Victims (domestic criminal investigations, national security) experience higher effective extraction, as their ability to function is hindered. The FBI and CIA have a more complex relationship to the wall, experiencing both benefits and constraints.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    effectiveness_vs_intrusion,
    'What is the optimal balance between intelligence effectiveness and intrusion on civil liberties?',
    'Analysis of intelligence failures before and after the PATRIOT Act, balanced against data on civil liberties violations.',
    'Determines whether the wall was a necessary safeguard or an impediment to national security.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effectiveness_vs_intrusion, preference, 'The fundamental tradeoff between intelligence effectiveness and privacy.').

omega_variable(
    scope_of_foreign_intelligence,
    'What constitutes legitimate ''foreign intelligence'' and how easily can it be used to circumvent domestic laws?',
    'Legal analysis of court cases and statutes related to foreign intelligence gathering.',
    'Defines the boundary between legitimate intelligence gathering and potential abuse.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scope_of_foreign_intelligence, conceptual, 'The conceptual boundary defining foreign intelligence activities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cia_fbi_legal_wall, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cia__tr_t0, cia_fbi_legal_wall, theater_ratio, 0, 0.3).
narrative_ontology:measurement(cia__tr_t5, cia_fbi_legal_wall, theater_ratio, 5, 0.35).
narrative_ontology:measurement(cia__tr_t10, cia_fbi_legal_wall, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(cia__be_t0, cia_fbi_legal_wall, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(cia__be_t5, cia_fbi_legal_wall, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(cia__be_t10, cia_fbi_legal_wall, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cia_fbi_legal_wall, enforcement_mechanism).
narrative_ontology:affects_constraint(cia_fbi_legal_wall, patriot_act_information_sharing).
narrative_ontology:affects_constraint(cia_fbi_legal_wall, fisa_court_oversight).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
