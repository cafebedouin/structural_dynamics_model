% ============================================================================
% CONSTRAINT STORY: project_vault_extraction_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_project_vault_extraction_2026, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: project_vault_extraction_2026
 *   human_readable: Project Vault: Debt-Financed Strategic Extraction
 *   domain: economic/political
 *
 * SUMMARY:
 *   Project Vault is a $12 billion critical minerals reserve funded by a $10
 *   billion EXIM Bank loan with an explicit profit mandate. This creates a
 *   complex extraction dynamic between investors, the EXIM Bank, local mining
 *   communities, and downstream industries that rely on these minerals. The
 *   project seeks to address strategic mineral dependencies but does so via a
 *   debt-financed profit mandate, which introduces a significant extraction
 *   vector.
 *
 * KEY AGENTS:
 *   - Project Vault Investors: Primary beneficiary (institutional/arbitrage) - Profit from mineral extraction, arbitrage through loan agreements.
 *   - EXIM Bank: Beneficiary (institutional/arbitrage) - Receives loan repayments.
 *   - Local Mining Communities: Primary victim (powerless/trapped) - Suffer environmental degradation and displacement.
 *   - Downstream Industries: Secondary victim (moderate/constrained) - Constrained by the price and availability of minerals.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(project_vault_extraction_2026, 0.6).
domain_priors:suppression_score(project_vault_extraction_2026, 0.7).
domain_priors:theater_ratio(project_vault_extraction_2026, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(project_vault_extraction_2026, extractiveness, 0.6).
narrative_ontology:constraint_metric(project_vault_extraction_2026, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(project_vault_extraction_2026, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(project_vault_extraction_2026, tangled_rope).
narrative_ontology:human_readable(project_vault_extraction_2026, "Project Vault: Debt-Financed Strategic Extraction").
narrative_ontology:topic_domain(project_vault_extraction_2026, "economic/political").

domain_priors:requires_active_enforcement(project_vault_extraction_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(project_vault_extraction_2026, project_vault_investors).
narrative_ontology:constraint_beneficiary(project_vault_extraction_2026, exim_bank).
narrative_ontology:constraint_victim(project_vault_extraction_2026, local_mining_communities).
narrative_ontology:constraint_victim(project_vault_extraction_2026, downstream_industries).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Local communities are trapped by the project. Limited alternative employment, environmental degradation, and displacement create a snare.
constraint_indexing:constraint_classification(project_vault_extraction_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Downstream industries are constrained by the price of minerals. They benefit from the supply but are negatively affected by inflated prices. The project is extraction but also provides a level of reliable supply.
constraint_indexing:constraint_classification(project_vault_extraction_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% EXIM bank benefits from the repayments of the loan and is able to meet its obligations.
constraint_indexing:constraint_classification(project_vault_extraction_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Project Vault Investors see the project as a vehicle for securing a profit while securing access to strategic minerals. They are extracted from via the loan interest rates, but the profit motive supersedes any extraction.
constraint_indexing:constraint_classification(project_vault_extraction_2026, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% From a civilizational perspective, the project represents a tangled rope as the benefits and extraction are asymmetric, with a bias toward profit extraction from vulnerable communities. The strategic mineral reserves is beneficial on a global scale, but the risks of the program are not distributed evenly.
constraint_indexing:constraint_classification(project_vault_extraction_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(project_vault_extraction_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(project_vault_extraction_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(project_vault_extraction_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(project_vault_extraction_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(project_vault_extraction_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.6): High. Project Vault's structure creates a substantial extraction force. The loan creates pressure to maximize profits, which often comes at the expense of local communities. Suppression (0.7): High. The structure allows for considerable suppression as the project benefits from political support. Theater Ratio (0.3): Low. Theater isn't a driving force here. This is about mineral extraction.
 *
 * PERSPECTIVAL GAP:
 *   The perspective from local communities paints Project Vault as pure extraction, whereas the investors see a balanced extraction due to the profit motive. The analytical observer perspective agrees with this but still frames the project as a tangled rope as the benefits of the project are not shared equally.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (Investors, EXIM Bank) have arbitrage exit options; victims (local communities) have trapped exit options. The analytical observer takes an aggregate view that considers extraction from the local level and the implications for broader mineral security.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mineral_price_volatility,
    'To what extent will Project Vault''s extraction impact global mineral prices?',
    'Economic analysis of the supply/demand balance for the specific minerals being extracted.',
    'High volatility: Snare for downstream industries. Low volatility: closer to a rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mineral_price_volatility, empirical, 'The degree to which Project Vault impacts mineral price volatility.').

omega_variable(
    environmental_impact_mitigation,
    'How effective are the mitigation strategies at minimizing environmental damage to the local mining communities?',
    'Independent environmental audits and monitoring of key environmental indicators.',
    'Ineffective mitigation: Snare for local communities. Effective mitigation: closer to tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(environmental_impact_mitigation, empirical, 'Effectiveness of environmental mitigation strategies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(project_vault_extraction_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(proj_tr_t0, project_vault_extraction_2026, theater_ratio, 0, 0.2).
narrative_ontology:measurement(proj_tr_t5, project_vault_extraction_2026, theater_ratio, 5, 0.3).
narrative_ontology:measurement(proj_tr_t10, project_vault_extraction_2026, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(proj_be_t0, project_vault_extraction_2026, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(proj_be_t5, project_vault_extraction_2026, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(proj_be_t10, project_vault_extraction_2026, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
