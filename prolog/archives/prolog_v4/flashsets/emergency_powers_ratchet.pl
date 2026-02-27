% ============================================================================
% CONSTRAINT STORY: emergency_powers_ratchet
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_emergency_powers_ratchet, []).

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
 *   constraint_id: emergency_powers_ratchet
 *   human_readable: The Permanent Crisis Scaffold
 *   domain: political/legal/social
 *
 * SUMMARY:
 *   This constraint represents the systemic tendency for temporary
 *   "emergency" legal powers, enacted during a crisis (war, pandemic,
 *   economic collapse), to persist and become normalized as permanent
 *   administrative fixtures. These powers often impinge on civil liberties,
 *   legislative oversight, and judicial review, concentrating authority in
 *   the executive branch and security agencies. The 'permanent crisis'
 *   dynamic creates a scaffold where emergency measures initially justified
 *   by exceptional circumstances become entrenched aspects of governance.
 *
 * KEY AGENTS:
 *   - Executive Branch: Primary beneficiary (institutional/arbitrage) – gains expanded powers and control during crises.
 *   - Security Agencies: Secondary beneficiary (organized/mobile) – benefit from increased resources and authority, constrained by oversight.
 *   - Civil Liberties: Primary victim (powerless/trapped) – eroded by emergency powers, difficult to restore.
 *   - Legislative Oversight: Secondary victim (moderate/constrained) – struggle to reclaim powers delegated during crises.
 *   - Judicial Review: Institutional actor (institutional/constrained) – deference during crises sets precedents.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) - identifies ratchet effect and its long-term risks.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(emergency_powers_ratchet, 0.55).
domain_priors:suppression_score(emergency_powers_ratchet, 0.65).
domain_priors:theater_ratio(emergency_powers_ratchet, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(emergency_powers_ratchet, extractiveness, 0.55).
narrative_ontology:constraint_metric(emergency_powers_ratchet, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(emergency_powers_ratchet, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(emergency_powers_ratchet, tangled_rope).
narrative_ontology:human_readable(emergency_powers_ratchet, "The Permanent Crisis Scaffold").
narrative_ontology:topic_domain(emergency_powers_ratchet, "political/legal/social").

domain_priors:requires_active_enforcement(emergency_powers_ratchet).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(emergency_powers_ratchet, executive_branch).
narrative_ontology:constraint_beneficiary(emergency_powers_ratchet, security_agencies).
narrative_ontology:constraint_victim(emergency_powers_ratchet, civil_liberties).
narrative_ontology:constraint_victim(emergency_powers_ratchet, legislative_oversight).
narrative_ontology:constraint_victim(emergency_powers_ratchet, judicial_review).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Civil liberties are the primary victim. Once eroded, they are difficult to restore, creating a ratchet effect where emergency powers become normalized. Individuals are trapped within the legal system with little recourse.
constraint_indexing:constraint_classification(emergency_powers_ratchet, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% Legislative bodies often find it difficult to claw back powers delegated to the executive branch during crises. They are constrained by public opinion, lack of expertise, and political polarization, but they retain some power to amend or repeal legislation.
constraint_indexing:constraint_classification(emergency_powers_ratchet, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The executive branch benefits from expanded powers, which provide greater flexibility and control. The constraint acts as a rope, facilitating quick action in future emergencies and consolidating administrative authority.
constraint_indexing:constraint_classification(emergency_powers_ratchet, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Judicial review, while intended as a check on executive power, often becomes a piton. Courts may defer to the executive branch during emergencies, setting precedents that are difficult to overturn later. The theater ratio is high as the appearance of oversight masks a weakened constraint.
constraint_indexing:constraint_classification(emergency_powers_ratchet, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% Security agencies benefit from increased resources and authority, but they are also constrained by legal limits and public scrutiny. They extract resources but also provide a coordination function (security). Their mobility refers to their ability to refocus efforts should one set of powers be curtailed.
constraint_indexing:constraint_classification(emergency_powers_ratchet, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% From a civilizational perspective, the analytical observer sees the ratchet effect as a tangled rope. Emergency powers provide a temporary solution to crises, but they also create a long-term risk of eroding democratic norms and concentrating power in the executive branch.
constraint_indexing:constraint_classification(emergency_powers_ratchet, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(emergency_powers_ratchet_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(emergency_powers_ratchet, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(emergency_powers_ratchet, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(emergency_powers_ratchet, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(emergency_powers_ratchet, TR),
    TR >= 0.70.

:- end_tests(emergency_powers_ratchet_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. Emergency powers extract resources from civil liberties, legislative oversight, and judicial review. The extraction is not total, as some checks and balances remain, but the concentration of power in the executive branch is significant. Suppression (0.65): Moderate-high. Emergency powers suppress alternatives to executive action, limiting public debate and judicial challenges. The suppression is higher than the extractiveness because emergency measures often curtail dissent and limit legal challenges. Theater ratio (0.40): Moderate. Some performative oversight mechanisms exist, such as legislative hearings and judicial review, but they often defer to the executive branch during emergencies. The theater is increasing over time as emergency powers become normalized.
 *
 * PERSPECTIVAL GAP:
 *   This constraint generates a perspectival gap due to the differing experiences of the key agents. The executive branch sees a rope, providing greater flexibility and control. Civil liberties see a snare, as their freedoms are eroded. Legislative oversight and judicial review see a tangled rope, struggling to maintain checks and balances. Security agencies see a tangled rope, balancing increased authority with oversight. The analytical observer sees the tangled rope as a long-term risk to democratic norms.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality (d) is derived from each agent's structural position. The executive branch, as the primary beneficiary, has a low d value, while civil liberties, as the primary victim, have a high d value. Legislative oversight and judicial review have moderate d values, reflecting their constrained ability to check executive power. Security agencies also have a moderate d value, balancing their beneficiary status with legal limits. The analytical observer's d value is based on the civilizational time horizon and analytical exit option.
 *
 * MANDATROPHY ANALYSIS:
 *   The 'permanent crisis scaffold' resolves the mandatrophy by recognizing that all six types are legitimate perspectives on the same phenomenon. The executive branch's 'rope' is their genuine experience of increased power and control. The citizen’s 'snare' represents the actual loss of liberty. The tangled ropes illustrate competing interests. The analytical observer recognizes the ratchet effect, which requires assessing a long term time scale rather than the immediate benefits seen in other perspectives. The system needs to account for what is perceived by whom, when, and how.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    crisis_definition,
    'What constitutes a legitimate ''crisis'' justifying extraordinary powers?',
    'Historical analysis of past crises and the use of emergency powers; establishment of clear legal criteria for declaring a crisis.',
    'A broad definition of ''crisis'' will lead to more frequent use of emergency powers and greater erosion of civil liberties. A narrow definition will limit the executive branch''s ability to respond to genuine emergencies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crisis_definition, conceptual, 'Definition of ''crisis'' justifying emergency powers.').

omega_variable(
    sunset_clause_effectiveness,
    'How effective are sunset clauses in limiting the duration of emergency powers?',
    'Empirical study of sunset clauses in different countries; analysis of factors that lead to their renewal or expiration.',
    'Effective sunset clauses will prevent emergency powers from becoming permanent. Ineffective sunset clauses will be easily circumvented or renewed, leading to the ratchet effect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_clause_effectiveness, empirical, 'Effectiveness of sunset clauses').

omega_variable(
    public_opinion_influence,
    'To what extent does public opinion influence the use and duration of emergency powers?',
    'Polling and survey data on public attitudes towards emergency powers; analysis of media coverage and political discourse.',
    'Strong public support for emergency powers will make it difficult to repeal them, even after the crisis has passed. Weak public support will create political pressure to limit or abolish them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_opinion_influence, empirical, 'Influence of public opinion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(emergency_powers_ratchet, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(emer_tr_t0, emergency_powers_ratchet, theater_ratio, 0, 0.25).
narrative_ontology:measurement(emer_tr_t10, emergency_powers_ratchet, theater_ratio, 10, 0.32).
narrative_ontology:measurement(emer_tr_t20, emergency_powers_ratchet, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(emer_be_t0, emergency_powers_ratchet, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(emer_be_t10, emergency_powers_ratchet, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(emer_be_t20, emergency_powers_ratchet, base_extractiveness, 20, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(emergency_powers_ratchet, enforcement_mechanism).
narrative_ontology:affects_constraint(emergency_powers_ratchet, surveillance_state).
narrative_ontology:affects_constraint(emergency_powers_ratchet, executive_overreach).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
