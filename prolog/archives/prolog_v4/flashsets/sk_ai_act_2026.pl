% ============================================================================
% CONSTRAINT STORY: sk_ai_act_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sk_ai_act_2026, []).

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
 *   constraint_id: sk_ai_act_2026
 *   human_readable: South Korea's Proposed AI Industry Promotion Act
 *   domain: technological/political
 *
 * SUMMARY:
 *   South Korea's proposed AI Industry Promotion Act aims to accelerate its
 *   national AI industry by creating a legal safe harbor for AI developers.
 *   This involves granting broad exemptions from existing privacy and
 *   copyright laws, allowing the collection and processing of personal data
 *   without explicit consent for research and development purposes. This act
 *   presents a complex interplay of benefits and costs, impacting various
 *   stakeholders differently.
 *
 * KEY AGENTS:
 *   - sk_citizens: Primary target (powerless/trapped) - Bear the cost of reduced privacy protections
 *   - sk_ai_developers: Primary beneficiary (institutional/arbitrage) - Benefit from relaxed regulations and increased data access
 *   - sk_government: Secondary actor (powerful/constrained) - Balances industrial growth with citizen rights and international norms
 *   - non_sk_ai_developers: Victims (moderate/constrained) - Face a competitive disadvantage
 *   - analytical_observer: Observer (analytical/analytical) - Sees the complex interplay of benefits and costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sk_ai_act_2026, 0.55).
domain_priors:suppression_score(sk_ai_act_2026, 0.45).
domain_priors:theater_ratio(sk_ai_act_2026, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sk_ai_act_2026, extractiveness, 0.55).
narrative_ontology:constraint_metric(sk_ai_act_2026, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(sk_ai_act_2026, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sk_ai_act_2026, tangled_rope).
narrative_ontology:human_readable(sk_ai_act_2026, "South Korea's Proposed AI Industry Promotion Act").
narrative_ontology:topic_domain(sk_ai_act_2026, "technological/political").

domain_priors:requires_active_enforcement(sk_ai_act_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sk_ai_act_2026, sk_ai_developers).
narrative_ontology:constraint_beneficiary(sk_ai_act_2026, sk_government).
narrative_ontology:constraint_victim(sk_ai_act_2026, sk_citizens).
narrative_ontology:constraint_victim(sk_ai_act_2026, non_sk_ai_developers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Citizens have limited recourse against data collection and usage.  They are effectively trapped within the system. No ability to exit or meaningfully control the use of their data, resulting in high extraction.
constraint_indexing:constraint_classification(sk_ai_act_2026, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% Benefits from reduced legal burdens and increased access to data. Can arbitrage regulatory exemptions to gain a competitive advantage. High coordination due to ease of AI development.
constraint_indexing:constraint_classification(sk_ai_act_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% The act serves as a form of national industrial policy, but it comes at the cost of citizen privacy and could stifle innovation by non-favored entities. Contains both coordination and extraction aspects.
constraint_indexing:constraint_classification(sk_ai_act_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% Benefits from an accelerated AI industry but is constrained by potential backlash from citizens and international norms. Has the power to shape the regulatory landscape but is not entirely free from constraints.
constraint_indexing:constraint_classification(sk_ai_act_2026, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Constrained by the competitive disadvantage resulting from the legal safe harbor afforded to SK developers. Limited ability to compete on an equal footing. Moderate power but trapped within global market dynamics.
constraint_indexing:constraint_classification(sk_ai_act_2026, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sk_ai_act_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sk_ai_act_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sk_ai_act_2026, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sk_ai_act_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sk_ai_act_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate to high. The act allows for the extraction of personal data without explicit consent, potentially leading to privacy violations and misuse of information. Suppression (0.45): Moderate. While the act aims to promote innovation, it may suppress competition from foreign companies and limit individual autonomy over personal data. Theater ratio (0.30): Low. The act's focus is primarily on practical implementation and industrial promotion, with limited emphasis on performative aspects.
 *
 * PERSPECTIVAL GAP:
 *   South Korean citizens experience the act as a snare, facing reduced privacy and limited recourse. South Korean AI developers benefit from the relaxed regulations, viewing it as a rope that facilitates innovation. The South Korean government sees a tangled rope, balancing the benefits of industrial growth with potential risks to citizen rights and international relations. Non-South Korean AI developers also see a snare, as they face a competitive disadvantage. The analytical observer recognizes the tangled rope, acknowledging both the coordination and extraction aspects of the act.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the agent's structural relationship to the constraint. South Korean AI developers benefit from relaxed regulations and increased data access (low d). South Korean citizens bear the cost of reduced privacy protections (high d). The government balances industrial growth with citizen rights (moderate d). Non-South Korean AI developers face a competitive disadvantage (high d). The analytical observer assesses the overall impact (moderate d).
 *
 * MANDATROPHY ANALYSIS:
 *   The act's classification as a tangled rope reflects the inherent tension between promoting AI innovation and protecting citizen rights. The act contains elements of both coordination (facilitating AI development) and extraction (reducing privacy protections). Resolving the mandatrophy requires careful consideration of the trade-offs between these competing objectives and finding a balance that maximizes benefits while minimizing harms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    data_anonymization_efficacy,
    'How effective are data anonymization techniques at preventing re-identification of individuals?',
    'Independent audits and penetration testing of anonymized datasets.',
    'If anonymization is weak, citizen risk increases, strengthening the snare classification. If robust, the citizen classification shifts towards constrained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(data_anonymization_efficacy, empirical, 'Efficacy of data anonymization in protecting privacy.').

omega_variable(
    scope_of_exemptions,
    'To what extent will the legal safe harbor shield AI developers from liability for misuse of personal data?',
    'Legal challenges and court rulings interpreting the scope of the act''s exemptions.',
    'Narrow interpretation limits the benefits to developers, shifting their perspective towards tangled rope. Broad interpretation increases extraction from citizens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_exemptions, conceptual, 'Scope of legal exemptions for AI developers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sk_ai_act_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sk_a_tr_t0, sk_ai_act_2026, theater_ratio, 0, 0.2).
narrative_ontology:measurement(sk_a_tr_t5, sk_ai_act_2026, theater_ratio, 5, 0.25).
narrative_ontology:measurement(sk_a_tr_t10, sk_ai_act_2026, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(sk_a_be_t0, sk_ai_act_2026, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(sk_a_be_t5, sk_ai_act_2026, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(sk_a_be_t10, sk_ai_act_2026, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sk_ai_act_2026, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
