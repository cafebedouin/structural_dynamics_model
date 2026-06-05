% ============================================================================
% CONSTRAINT STORY: brazil_mexico_financial_requirement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_brazil_mexico_financial_requirement, []).

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
 *   constraint_id: brazil_mexico_financial_requirement
 *   human_readable: Mexican Financial Proof Requirement for Brazilian Travelers
 *   domain: geopolitical
 *
 * SUMMARY:
 *   Mexico, under pressure from the United States to curb irregular
 *   migration, has imposed a rule requiring Brazilian travelers to prove they
 *   possess at least R$10,000 (~US$1,900) to cover their expenses. This
 *   constraint creates a complex geopolitical dynamic, extracting from
 *   Brazilian travelers and tourism while benefiting the Mexican and US
 *   governments.
 *
 * KEY AGENTS:
 *   - Brazilian Travelers: Primary victims (powerless/trapped) - Face barriers to travel.
 *   - Brazilian Tourism Sector: Secondary victims (moderate/constrained) - Suffers economic losses.
 *   - Mexican Government: Primary beneficiaries (institutional/arbitrage) - Gains political capital and potentially avoids repercussions from the US.
 *   - United States Government: Secondary beneficiaries (institutional/arbitrage) - Achieves border control objectives through external enforcement.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(brazil_mexico_financial_requirement, 0.6).
domain_priors:suppression_score(brazil_mexico_financial_requirement, 0.7).
domain_priors:theater_ratio(brazil_mexico_financial_requirement, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(brazil_mexico_financial_requirement, extractiveness, 0.6).
narrative_ontology:constraint_metric(brazil_mexico_financial_requirement, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(brazil_mexico_financial_requirement, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(brazil_mexico_financial_requirement, snare).
narrative_ontology:human_readable(brazil_mexico_financial_requirement, "Mexican Financial Proof Requirement for Brazilian Travelers").
narrative_ontology:topic_domain(brazil_mexico_financial_requirement, "geopolitical").

domain_priors:requires_active_enforcement(brazil_mexico_financial_requirement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(brazil_mexico_financial_requirement, mexican_government).
narrative_ontology:constraint_beneficiary(brazil_mexico_financial_requirement, united_states_government).
narrative_ontology:constraint_victim(brazil_mexico_financial_requirement, brazilian_travelers).
narrative_ontology:constraint_victim(brazil_mexico_financial_requirement, brazilian_tourism_sector).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Brazilian travelers, particularly those with limited financial resources, are trapped by this requirement. They may be unable to travel to Mexico, losing out on tourism, business, or personal opportunities.
constraint_indexing:constraint_classification(brazil_mexico_financial_requirement, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(regional))).

% The Brazilian tourism sector is constrained as potential travelers are deterred by the financial requirement, impacting revenue and growth. However, they can adapt by promoting alternative destinations.
constraint_indexing:constraint_classification(brazil_mexico_financial_requirement, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% The Mexican government benefits by complying with US pressure, potentially avoiding further economic or political repercussions and stemming migration. They experience this as a coordination mechanism.
constraint_indexing:constraint_classification(brazil_mexico_financial_requirement, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% The United States benefits by having Mexico enforce stricter border controls, reducing the flow of irregular migrants. They see this as a coordination mechanism for border security.
constraint_indexing:constraint_classification(brazil_mexico_financial_requirement, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% From an analytical perspective, this policy is a tangled rope. It serves as a tool for geopolitical maneuvering, extracting from some while providing perceived benefits to others, with long-term implications for international relations and human mobility.
constraint_indexing:constraint_classification(brazil_mexico_financial_requirement, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(brazil_mexico_financial_requirement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(brazil_mexico_financial_requirement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(brazil_mexico_financial_requirement, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(brazil_mexico_financial_requirement, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(brazil_mexico_financial_requirement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness: The financial requirement directly extracts from Brazilian travelers by creating a financial barrier to entry. Suppression: The policy suppresses travel opportunities and economic growth in the tourism sector. Theater Ratio: The relatively low theater ratio reflects that the policy's stated purpose (reducing irregular migration) is somewhat aligned with its actual effect, although its impact is disproportionately borne by legitimate travelers.
 *
 * PERSPECTIVAL GAP:
 *   Brazilian travelers experience the policy as a snare, limiting their mobility. The Mexican government views it as a rope, enabling them to meet US demands and manage migration flows. The analytical observer sees a tangled rope, where geopolitical maneuvering extracts from one group to benefit another.
 *
 * DIRECTIONALITY LOGIC:
 *   The Mexican and US governments benefit from the policy, seeing it as a tool to manage migration. Brazilian travelers and the tourism sector bear the costs of the policy through restricted travel and economic losses. The directionality is determined by the structural relationship to the constraint, with beneficiaries experiencing low d values and victims experiencing high d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The policy could be misinterpreted as a rope if focusing solely on the benefits to the US and Mexican governments. However, analyzing the policy through the lens of Brazilian travelers reveals its extractive nature, justifying its classification as a snare. The multi-perspective analysis clarifies the complex dynamics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    effectiveness_of_deterrence,
    'How effectively does the financial requirement deter irregular migration?',
    'Data analysis of migration patterns before and after the policy implementation, surveys of potential migrants, and comparison with other border control measures.',
    'If highly effective: justifies the policy from the perspective of the US and Mexico. If ineffective: raises questions about the policy''s true purpose and potential for unintended consequences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effectiveness_of_deterrence, empirical, 'Assesses the efficacy of the financial requirement in deterring irregular migration.').

omega_variable(
    economic_impact_on_brazil,
    'What is the overall economic impact on the Brazilian tourism sector?',
    'Analysis of tourism revenue, number of Brazilian tourists visiting Mexico, and feedback from tourism businesses and associations.',
    'If significant negative impact: calls for diplomatic intervention or alternative solutions. If minimal impact: reduces the urgency for addressing the policy''s consequences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_impact_on_brazil, empirical, 'Evaluates the economic consequences for the Brazilian tourism industry.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(brazil_mexico_financial_requirement, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(braz_tr_t0, brazil_mexico_financial_requirement, theater_ratio, 0, 0.2).
narrative_ontology:measurement(braz_tr_t6, brazil_mexico_financial_requirement, theater_ratio, 6, 0.3).
narrative_ontology:measurement(braz_tr_t12, brazil_mexico_financial_requirement, theater_ratio, 12, 0.35).

% Extraction over time
narrative_ontology:measurement(braz_be_t0, brazil_mexico_financial_requirement, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(braz_be_t6, brazil_mexico_financial_requirement, base_extractiveness, 6, 0.6).
narrative_ontology:measurement(braz_be_t12, brazil_mexico_financial_requirement, base_extractiveness, 12, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(brazil_mexico_financial_requirement, enforcement_mechanism).
narrative_ontology:affects_constraint(brazil_mexico_financial_requirement, us_mexico_migration_agreement).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
