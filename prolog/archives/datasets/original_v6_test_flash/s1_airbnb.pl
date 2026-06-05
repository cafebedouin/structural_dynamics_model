% ============================================================================
% CONSTRAINT STORY: s1_airbnb
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_s1_airbnb, []).

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
 *   constraint_id: s1_airbnb
 *   human_readable: Short-Term Rental (STR) Regulatory Framework
 *   domain: economic/political
 *
 * SUMMARY:
 *   A global patchwork of municipal laws, zoning restrictions, and tax
 *   requirements that govern home-sharing. These regulations represent a
 *   complex interplay of economic, political, and social factors, balancing
 *   the interests of homeowners, tourists, local residents, and the hotel
 *   industry. The framework seeks to manage the externalities associated with
 *   STRs, such as noise, traffic, and housing affordability, while also
 *   capturing economic benefits through tourism and tax revenue. The
 *   effectiveness of these regulations varies widely depending on local
 *   context, enforcement capacity, and stakeholder engagement.
 *
 * KEY AGENTS:
 *   - Short-Term Rental Hosts: Primary target (powerless/trapped) - bears the brunt of compliance costs and restrictions.
 *   - Municipal Governments: Primary beneficiary (institutional/constrained) - extracts revenue and manages externalities, but constrained by community concerns.
 *   - Hotel Industry: Secondary beneficiary (institutional/arbitrage) - benefits from reduced competition and increased demand for traditional lodging.
 *   - Tourists: Secondary actor (moderate/mobile) - face constrained choices and potentially higher prices due to regulations.
 *   - Local Residents: Stakeholder (moderate/constrained) - impacted by both the positive and negative externalities of STRs, and whose voice influences regulatory outcomes.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(s1_airbnb, 0.55).
domain_priors:suppression_score(s1_airbnb, 0.45).
domain_priors:theater_ratio(s1_airbnb, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(s1_airbnb, extractiveness, 0.55).
narrative_ontology:constraint_metric(s1_airbnb, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(s1_airbnb, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(s1_airbnb, tangled_rope).
narrative_ontology:human_readable(s1_airbnb, "Short-Term Rental (STR) Regulatory Framework").
narrative_ontology:topic_domain(s1_airbnb, "economic/political").

domain_priors:requires_active_enforcement(s1_airbnb).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(s1_airbnb, municipal_governments).
narrative_ontology:constraint_beneficiary(s1_airbnb, hotel_industry).
narrative_ontology:constraint_victim(s1_airbnb, short_term_rental_hosts).
narrative_ontology:constraint_victim(s1_airbnb, tourists).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The individual STR host faces a complex and evolving web of local regulations, often with limited capacity to exit or arbitrage the system. The regulatory burden extracts significant time and resources, with limited coordination benefits viewed from this perspective.
constraint_indexing:constraint_classification(s1_airbnb, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% Established hotel chains benefit from STR regulations that limit competition and increase demand for traditional lodging. They have the resources and influence to lobby for favorable regulations and arbitrage the system to their advantage. This is viewed as a coordination mechanism to protect market share.
constraint_indexing:constraint_classification(s1_airbnb, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% From a global, long-term perspective, the STR regulatory framework represents a tangled rope, balancing the needs of various stakeholders but also creating opportunities for extraction and regulatory capture. The balance shifts over time as new business models emerge and regulatory frameworks adapt.
constraint_indexing:constraint_classification(s1_airbnb, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% Tourists can be victims of STR regulations by facing fewer lodging options and higher prices. They are moderately affected, and while they may be mobile, their choices are constrained within a given region due to the regulations.
constraint_indexing:constraint_classification(s1_airbnb, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% Municipal governments extract revenue through taxes and fees on STRs. However, they are constrained by needing to balance revenue generation with addressing affordable housing concerns and managing community disruption.
constraint_indexing:constraint_classification(s1_airbnb, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(s1_airbnb_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(s1_airbnb, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(s1_airbnb, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(s1_airbnb, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(s1_airbnb_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. STR regulations extract value from hosts through taxes, fees, and compliance costs. They also limit the supply of STRs, potentially increasing prices for tourists. However, some of the regulations also provide benefits to the community, such as mitigating noise and traffic. Suppression (0.45): Moderate. STR regulations suppress the supply of STRs, limiting competition and potentially increasing prices for tourists. However, they do not completely eliminate STRs, and hosts still have some flexibility in how they operate. Theater Ratio (0.30): Low. While there may be some performative aspects to STR regulations, they are primarily focused on achieving real-world outcomes, such as protecting housing affordability and managing community disruption.
 *
 * PERSPECTIVAL GAP:
 *   The different perspectives on STR regulations reflect the diverse interests and experiences of the stakeholders involved. Hosts tend to view the regulations as burdensome and restrictive, while municipal governments see them as a way to manage externalities and generate revenue. The hotel industry sees the regulations as a way to protect their market share, while tourists may experience them as a constraint on their lodging options. An analytical observer recognizes the complex interplay of these factors and the need for a balanced approach.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value (d) is determined by the agent's structural position - their power level, exit options, and relationship to the extraction flow. The pipeline computes d from these context parameters and applies the sigmoid f(d) to produce experienced extractiveness chi. Beneficiaries with arbitrage options experience low or negative effective extraction; trapped agents with no exit bear maximum extraction; organized agents with exit paths experience moderate extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by recognizing that STR regulations serve multiple functions simultaneously. They extract value from hosts and tourists, but they also provide benefits to the community by mitigating externalities and generating revenue. They suppress the supply of STRs, but they also enable a more sustainable and equitable tourism ecosystem. The key challenge is to design regulations that strike the right balance between these competing goals.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    optimal_regulation_level,
    'What level of regulation strikes the right balance between protecting housing affordability, managing community disruption, and enabling economic activity?',
    'Comparative studies of different regulatory approaches, analysis of housing market data, community surveys, and economic impact assessments.',
    'Higher regulation could lead to fewer STRs and increased housing affordability but reduced tourism revenue. Lower regulation could boost tourism but exacerbate housing shortages and community disruption.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(optimal_regulation_level, empirical, 'Determining the optimal level of STR regulation').

omega_variable(
    enforcement_effectiveness,
    'How effectively are STR regulations enforced, and what are the costs and benefits of different enforcement strategies?',
    'Audits of STR listings, analysis of complaint data, assessments of enforcement personnel resources, and cost-benefit analyses of different enforcement technologies.',
    'Weak enforcement could lead to widespread non-compliance and undermine the intended benefits of the regulations. Overly aggressive enforcement could stifle innovation and harm legitimate STR hosts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_effectiveness, empirical, 'Assessing the effectiveness of STR regulation enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(s1_airbnb, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(s1_a_tr_t0, s1_airbnb, theater_ratio, 0, 0.1).
narrative_ontology:measurement(s1_a_tr_t5, s1_airbnb, theater_ratio, 5, 0.2).
narrative_ontology:measurement(s1_a_tr_t10, s1_airbnb, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(s1_a_be_t0, s1_airbnb, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(s1_a_be_t5, s1_airbnb, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(s1_a_be_t10, s1_airbnb, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(s1_airbnb, resource_allocation).
narrative_ontology:affects_constraint(s1_airbnb, housing_affordability_policies).
narrative_ontology:affects_constraint(s1_airbnb, tourism_development_strategies).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
