% ============================================================================
% CONSTRAINT STORY: project_vault_2026
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-06
% ============================================================================

:- module(constraint_project_vault, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: project_vault_2026
 * human_readable: Project Vault Strategic Mineral Reserve
 * domain: economic/geopolitical
 * * SUMMARY:
 * A $12B strategic reserve (announced Feb 2, 2026) designed to shield U.S. 
 * manufacturers from China’s rare earth monopoly. Financed via a $10B EXIM 
 * loan with a "make a profit" mandate, it creates a captive market where 
 * protection and extraction are inseparable.
 * * KEY AGENTS:
 * - Manufacturers: Subject (Powerless) - Captive buyers with no exit options.
 * - EXIM Bank/Gov: Beneficiary (Institutional) - Profit-seeking auditor of security.
 * - Private Investors: Beneficiary (Institutional) - $2B capital de-risked by captive pricing.
 * * THEORETICAL MODEL:
 * The extraction $E$ is a function of the debt-servicing requirements $D$ 
 * relative to the marginal security utility $U$:
 * $$E_{vault} = \frac{D_{exim} + P_{profit}}{U_{security}}$$
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Base extraction is high (> 0.46) due to the explicit profit mandate 
% and the transfer of risk to manufacturers via captive pricing.
domain_priors:base_extractiveness(project_vault_2026, 0.55). 
domain_priors:suppression_score(project_vault_2026, 0.94).   % 94% Chinese magnet monopoly.
domain_priors:theater_ratio(project_vault_2026, 0.65).       % National security framing.

% Primary keys for classification engine
narrative_ontology:constraint_metric(project_vault_2026, extractiveness, 0.55).
narrative_ontology:constraint_metric(project_vault_2026, suppression_requirement, 0.94).
narrative_ontology:constraint_metric(project_vault_2026, theater_ratio, 0.65).

% Constraint classification claim
narrative_ontology:constraint_claim(project_vault_2026, tangled_rope).

% Scaffold/Tangled Rope Requirements
domain_priors:requires_active_enforcement(project_vault_2026). % Mandatory buy-in contracts.
narrative_ontology:has_sunset_clause(project_vault_2026).       % Bridge to 10-18 year capacity dev.
narrative_ontology:constraint_beneficiary(project_vault_2026, exim_bank).
narrative_ontology:constraint_victim(project_vault_2026, manufacturers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE MANUFACTURER (SNARE)
% Effective Extraction: 0.55 * 1.5 (powerless) * 1.2 (global) = 0.99.
% At χ ≈ 1.0, the manufacturer is in a state of total extraction/captivity.
constraint_indexing:constraint_classification(project_vault_2026, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(global))).

% PERSPECTIVE 2: THE GOVERNMENT (SCAFFOLD)
% Effective Extraction: 0.55 * -0.2 (institutional) * 1.2 = -0.132.
% Viewed as a net-gain coordination structure to build domestic independence.
constraint_indexing:constraint_classification(project_vault_2026, scaffold, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Identifies the "Tangled Rope" transition as extraction intensifies.
constraint_indexing:constraint_classification(project_vault_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(project_vault_2026_tests).

test(extraction_paradox) :-
    % Verify that the same policy yields a Snare and a Scaffold based on power.
    constraint_indexing:constraint_classification(project_vault_2026, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(project_vault_2026, scaffold, context(agent_power(institutional), _, _, _)).

test(scaffold_integrity) :-
    % Scaffold requires coordination (beneficiary) and a sunset.
    narrative_ontology:has_sunset_clause(project_vault_2026),
    narrative_ontology:constraint_beneficiary(project_vault_2026, _).

:- end_tests(project_vault_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * Project Vault represents the financialization of national security. By replacing 
 * traditional appropriations with an EXIM loan, the government creates a 
 * "Yield-bearing Scaffold." The 0.55 base extraction is the "insurance premium" 
 * for safety, but because manufacturers have zero exit options ($\sigma=1.2, 
 * \pi=1.5$), this coordination structure collapses into a Snare for the subject.
 *
 * MANDATROPHY RESOLUTION:
 * The "Mandatrophy" arises because the protection is real but the extraction 
 * is coerced. The classification system resolves this by mapping it as 
 * simultaneously a Scaffold (coordination) and a Snare (predation). 
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_vault_sufficiency,
    "Is $12B sufficient to prevent shutdowns during a 12-month cutoff?",
    "Stress-test modeling inventory vs. demand across a 24-month disruption.",
    "Sufficiency maintains the Scaffold; insufficiency reveals a fraudulent Piton.",
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(project_vault_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction Drift (Modeling intensification as profit mandates normalize)
narrative_ontology:measurement(pv_ex_t0, project_vault_2026, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(pv_ex_t5, project_vault_2026, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(pv_ex_t10, project_vault_2026, base_extractiveness, 10, 0.65).

% Theater Ratio (Modeling the shift from crisis response to institutionalization)
narrative_ontology:measurement(pv_tr_t0, project_vault_2026, theater_ratio, 0, 0.75).
narrative_ontology:measurement(pv_tr_t5, project_vault_2026, theater_ratio, 5, 0.65).
narrative_ontology:measurement(pv_tr_t10, project_vault_2026, theater_ratio, 10, 0.50).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
