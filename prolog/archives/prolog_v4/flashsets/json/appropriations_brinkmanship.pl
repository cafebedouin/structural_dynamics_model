% ============================================================================
% CONSTRAINT STORY: appropriations_brinkmanship
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_appropriations_brinkmanship, []).

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
 *   constraint_id: appropriations_brinkmanship
 *   human_readable: Government Shutdown Threat via Appropriations Process
 *   domain: political
 *
 * SUMMARY:
 *   The government shutdown threat via the appropriations process is a
 *   recurring political tactic where the legislative deadline for funding the
 *   government is used as leverage to force policy concessions. This tactic
 *   leads to uncertainty and disruption for government employees and the
 *   public, while benefiting political parties and advocacy groups seeking to
 *   advance their agendas. The reliance on brinkmanship undermines the
 *   intended function of the congressional budget process, which is meant to
 *   ensure the orderly allocation of resources.
 *
 * KEY AGENTS:
 *   - Government Employees: Powerless/trapped - bear the brunt of shutdowns with potential job insecurity and loss of income.
 *   - Public Services: Moderate/constrained - face disruptions and uncertainty, affecting their ability to deliver essential services.
 *   - Political Parties: Institutional/arbitrage - use the shutdown threat as a political tool to achieve policy objectives.
 *   - Policy Advocacy Groups: Powerful/mobile - mobilize around the shutdown threat to push their agendas and influence policy outcomes.
 *   - Congressional Budget Process: Institutional/constrained - the formal process is often undermined by brinkmanship.
 *   - Analytical Observer: Analytical/analytical - analyzes the complex interplay of incentives, procedures, and consequences.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(appropriations_brinkmanship, 0.6).
domain_priors:suppression_score(appropriations_brinkmanship, 0.7).
domain_priors:theater_ratio(appropriations_brinkmanship, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(appropriations_brinkmanship, extractiveness, 0.6).
narrative_ontology:constraint_metric(appropriations_brinkmanship, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(appropriations_brinkmanship, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(appropriations_brinkmanship, tangled_rope).
narrative_ontology:human_readable(appropriations_brinkmanship, "Government Shutdown Threat via Appropriations Process").
narrative_ontology:topic_domain(appropriations_brinkmanship, "political").

domain_priors:requires_active_enforcement(appropriations_brinkmanship).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(appropriations_brinkmanship, political_parties).
narrative_ontology:constraint_beneficiary(appropriations_brinkmanship, policy_advocacy_groups).
narrative_ontology:constraint_victim(appropriations_brinkmanship, government_employees).
narrative_ontology:constraint_victim(appropriations_brinkmanship, public_services).
narrative_ontology:constraint_victim(appropriations_brinkmanship, overall_economy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Government employees face job insecurity and potential loss of income during shutdowns, with limited ability to influence the process.
constraint_indexing:constraint_classification(appropriations_brinkmanship, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% Essential public services are disrupted, facing constrained resources and the need to operate under uncertainty, while still benefiting from eventual funding resolution.
constraint_indexing:constraint_classification(appropriations_brinkmanship, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Political parties use the threat of government shutdown to achieve policy objectives, gaining political leverage and potentially influencing public opinion.
constraint_indexing:constraint_classification(appropriations_brinkmanship, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% Policy advocacy groups mobilize around the shutdown threat to push their agendas, gaining visibility and potentially influencing policy outcomes, but also bearing the costs of political instability.
constraint_indexing:constraint_classification(appropriations_brinkmanship, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% The formal congressional budget process, designed for orderly appropriations, often degrades into brinkmanship, with procedures followed theatrically but functional effectiveness diminished.
constraint_indexing:constraint_classification(appropriations_brinkmanship, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% Analytical observers recognize the complex interplay of political incentives, institutional procedures, and economic consequences that drive the brinkmanship over appropriations.
constraint_indexing:constraint_classification(appropriations_brinkmanship, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(appropriations_brinkmanship_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(appropriations_brinkmanship, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(appropriations_brinkmanship, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(appropriations_brinkmanship, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(appropriations_brinkmanship, TR),
    TR >= 0.70.

:- end_tests(appropriations_brinkmanship_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60): High. The political parties extract significant concessions and public attention from the brinkmanship. Suppression (0.70): High. The process limits alternatives for government employees and public services. Theater ratio (0.75): High. While there is some genuine negotiation and effort to pass appropriations bills, the process is often driven by performative politics, with much of the activity focused on public messaging and blame assignment rather than substantive compromise.
 *
 * PERSPECTIVAL GAP:
 *   Government employees and public services experience the appropriations brinkmanship as a snare, as they bear the direct costs of shutdowns with limited power to influence the process. Political parties and policy advocacy groups, on the other hand, view the process as a tool (rope or tangled rope) to advance their agendas, benefiting from the political leverage and public attention generated by the shutdown threat. The analytical observer sees the broader implications of this tactic, recognizing the negative impact on government functioning and public trust.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the power level and exit options of each agent. Government employees are powerless and trapped, experiencing maximal extraction. Political parties have institutional power and arbitrage opportunities, benefiting from the process. Public services are constrained but benefit from eventual funding, experiencing a mixed effect. The sigmoid function reflects these relationships, resulting in different chi values for each perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate resolution highlights that the appropriations process involves both coordination and extraction. While the process is intended to coordinate government funding, the use of brinkmanship introduces a significant element of extraction, as political parties use the shutdown threat to force policy concessions. Different perspectives reveal different aspects of this interplay, highlighting the complexity of the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    policy_priority_threshold,
    'At what point does the pursuit of specific policy priorities outweigh the negative consequences of government shutdowns?',
    'Analysis of political rhetoric, public opinion data, and historical policy outcomes to determine the relative importance of different policy issues.',
    'Higher threshold: Shutdowns are more frequent and prolonged. Lower threshold: Political parties are more willing to compromise, reducing shutdown risk.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_priority_threshold, preference, 'Determines the relative value of policy priorities vs. shutdown costs').

omega_variable(
    political_polarization_level,
    'How does the level of political polarization influence the likelihood and severity of government shutdowns?',
    'Correlation analysis of polarization metrics (e.g., ideological distance between parties) and shutdown frequency/duration.',
    'Higher polarization: Shutdowns are more frequent and severe. Lower polarization: Political parties are more willing to compromise, reducing shutdown risk.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(political_polarization_level, empirical, 'Assesses the influence of polarization on shutdown likelihood').

omega_variable(
    institutional_reform_feasibility,
    'What institutional reforms could reduce the incentive for brinkmanship in the appropriations process?',
    'Comparative analysis of different budget processes, expert opinions on potential reforms, and political feasibility assessments.',
    'Feasible reforms: Reduced shutdown risk and improved government functioning. Unfeasible reforms: Continued brinkmanship and political instability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_reform_feasibility, conceptual, 'Explores potential institutional reforms to reduce brinkmanship').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(appropriations_brinkmanship, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(appr_tr_t0, appropriations_brinkmanship, theater_ratio, 0, 0.6).
narrative_ontology:measurement(appr_tr_t5, appropriations_brinkmanship, theater_ratio, 5, 0.7).
narrative_ontology:measurement(appr_tr_t10, appropriations_brinkmanship, theater_ratio, 10, 0.75).

% Extraction over time
narrative_ontology:measurement(appr_be_t0, appropriations_brinkmanship, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(appr_be_t5, appropriations_brinkmanship, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(appr_be_t10, appropriations_brinkmanship, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(appropriations_brinkmanship, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
