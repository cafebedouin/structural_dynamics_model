% ============================================================================
% CONSTRAINT STORY: dutch_minority_govt_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dutch_minority_govt_2026, []).

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
 *   constraint_id: dutch_minority_govt_2026
 *   human_readable: Dutch Minority Government External Support Agreement (2026)
 *   domain: political
 *
 * SUMMARY:
 *   Following a fragmented 2025 election result in the Netherlands, the
 *   far-right PVV, despite being the largest party, cannot form a majority
 *   coalition. A minority government is formed, relying on an external
 *   support agreement with other parties to pass legislation. This
 *   arrangement creates a complex web of political incentives and
 *   compromises, affecting different actors in different ways. The base
 *   extractiveness reflects the fact that the government is extracting more
 *   control by relying on external support while also limiting the power of
 *   other parties and the general electorate.
 *
 * KEY AGENTS:
 *   - Governing Coalition: Primary beneficiary (institutional/arbitrage) - maintains power and implements core policy agenda.
 *   - External Support Parties: Secondary beneficiary (powerful/mobile) - gains influence and political concessions.
 *   - Opposition Parties: Primary victim (moderate/constrained) - limited influence and ability to shape policy.
 *   - General Electorate: Primary victim (powerless/trapped) - reduced influence on policy decisions and accountability.
 *   - Dutch Parliamentary System: Degraded Institution (analytical/generational) - System's function is degraded by external supports and constant negotiation
 *   - Analytical Observer: (analytical/civilizational) Sees both the coordination and extraction aspects of the government agreement.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dutch_minority_govt_2026, 0.55).
domain_priors:suppression_score(dutch_minority_govt_2026, 0.65).
domain_priors:theater_ratio(dutch_minority_govt_2026, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dutch_minority_govt_2026, extractiveness, 0.55).
narrative_ontology:constraint_metric(dutch_minority_govt_2026, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(dutch_minority_govt_2026, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dutch_minority_govt_2026, tangled_rope).
narrative_ontology:human_readable(dutch_minority_govt_2026, "Dutch Minority Government External Support Agreement (2026)").
narrative_ontology:topic_domain(dutch_minority_govt_2026, "political").

domain_priors:requires_active_enforcement(dutch_minority_govt_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dutch_minority_govt_2026, governing_coalition).
narrative_ontology:constraint_beneficiary(dutch_minority_govt_2026, external_support_parties).
narrative_ontology:constraint_victim(dutch_minority_govt_2026, opposition_parties).
narrative_ontology:constraint_victim(dutch_minority_govt_2026, general_electorate).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE GENERAL ELECTORATE (SNARE) - Feels trapped by the limited policy options and reduced accountability of a minority government reliant on external support. Loses direct influence on policy decisions due to opaque negotiations and compromises.
constraint_indexing:constraint_classification(dutch_minority_govt_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: OPPOSITION PARTIES (TANGLED ROPE) - Constrained by the agreement's pre-negotiated terms and limited ability to influence policy. However, they can gain political leverage by highlighting the government's instability and lack of a clear mandate. They benefit from the situation by potentially gaining support from disillusioned voters.
constraint_indexing:constraint_classification(dutch_minority_govt_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: GOVERNING COALITION (ROPE) - Benefits from maintaining power and implementing their core policy agenda, even if it requires compromises. They can leverage the external support agreement to achieve stability and avoid snap elections. They arbitrage the political system to maintain control.
constraint_indexing:constraint_classification(dutch_minority_govt_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: EXTERNAL SUPPORT PARTIES (TANGLED ROPE) - Benefits from influencing policy and gaining political concessions in exchange for their support. They have increased influence compared to being in opposition. However, they are constrained by the need to maintain their credibility and avoid being seen as unconditionally supporting the government. They have the power to withdraw support but risk political backlash.
constraint_indexing:constraint_classification(dutch_minority_govt_2026, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: DUTCH PARLIAMENTARY SYSTEM (PITON) - The system's intended function of stable governance is degraded due to the reliance on external support and constant negotiation. The agreement creates a performative aspect where the government must constantly demonstrate its stability, even if policy outcomes are suboptimal.
constraint_indexing:constraint_classification(dutch_minority_govt_2026, piton,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) - The agreement is a mixed bag. It provides short-term stability but undermines long-term democratic accountability and policy effectiveness. The agreement represents both a coordination mechanism (allowing the government to function) and an extraction mechanism (reducing the influence of opposition parties and the electorate).
constraint_indexing:constraint_classification(dutch_minority_govt_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dutch_minority_govt_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dutch_minority_govt_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dutch_minority_govt_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(dutch_minority_govt_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(dutch_minority_govt_2026, TR),
    TR >= 0.70.

:- end_tests(dutch_minority_govt_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is relatively high because the government must rely on external support to function which takes away from the electorate. The suppression is relatively high because the external support agreement limits accountability. The theater ratio is now above 0.7, reflecting the performative aspect where the government must constantly demonstrate its stability, even if policy outcomes are suboptimal.
 *
 * PERSPECTIVAL GAP:
 *   The general electorate feels trapped and without influence, seeing a snare. Opposition parties are constrained, seeing a tangled rope. The governing coalition benefits, seeing a rope. The external support parties see a tangled rope because they must balance power and influence. The system is also being degraded, resembling a piton. And the analytical observer sees it as a tangled rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is derived from the structural position of each agent within the political system. The governing coalition and external support parties are beneficiaries, while the opposition parties and the general electorate are victims. The government is relying on external support and therefore extracting more control while also limiting the power of other parties and the general electorate. They have power to pass legislation and make decisions, therefore benefiting. While other parties and the electorate have limited influence.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    policy_effectiveness_tradeoff,
    'To what extent does the need for compromise in a minority government with external support lead to sub-optimal policy outcomes?',
    'Comparative analysis of policy outcomes under different government formations in the Netherlands and other countries with similar parliamentary systems.',
    'If policy outcomes are significantly worse, the constraint leans towards snare. If outcomes are comparable, it''s more of a tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_effectiveness_tradeoff, empirical, 'The degree to which policy effectiveness is compromised.').

omega_variable(
    democratic_accountability_erosion,
    'Does the reliance on external support reduce democratic accountability and transparency in the policy-making process?',
    'Analysis of parliamentary debates, public consultations, and access to information requests regarding policy decisions made under the minority government.',
    'If accountability is significantly reduced, the constraint leans toward snare. If accountability remains relatively high, it''s more of a tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_accountability_erosion, empirical, 'The degree to which democratic accountability is undermined.').

omega_variable(
    political_instability_risk,
    'What is the likelihood of the external support agreement collapsing, leading to a government crisis and potential snap elections?',
    'Monitoring of political dynamics, public opinion polls, and the stability of the relationship between the governing coalition and the external support parties.',
    'If the risk of collapse is high, the constraint becomes more unstable and scaffold-like. If the agreement proves durable, it''s more of a tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_instability_risk, empirical, 'The risk of the agreement collapsing and causing political instability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dutch_minority_govt_2026, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dutc_tr_t0, dutch_minority_govt_2026, theater_ratio, 0, 0.6).
narrative_ontology:measurement(dutc_tr_t12, dutch_minority_govt_2026, theater_ratio, 12, 0.7).
narrative_ontology:measurement(dutc_tr_t24, dutch_minority_govt_2026, theater_ratio, 24, 0.75).

% Extraction over time
narrative_ontology:measurement(dutc_be_t0, dutch_minority_govt_2026, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(dutc_be_t12, dutch_minority_govt_2026, base_extractiveness, 12, 0.5).
narrative_ontology:measurement(dutc_be_t24, dutch_minority_govt_2026, base_extractiveness, 24, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dutch_minority_govt_2026, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
