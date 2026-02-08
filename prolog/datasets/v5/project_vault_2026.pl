% ============================================================================
% CONSTRAINT STORY: project_vault_2026
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-05
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
    narrative_ontology:constraint_metric/3,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: project_vault_2026
 * human_readable: Project Vault Critical Minerals Reserve
 * domain: economic/political
 * * SUMMARY:
 * A $12 billion strategic stockpile initiative (Project Vault) designed to 
 * shield U.S. manufacturers from China's dominance in rare earth elements. 
 * Funded by a $10bn Ex-Im Bank loan and private capital, it functions as 
 * a state-backed floor for supply chain resilience.
 * * KEY AGENTS:
 * - [US Manufacturers]: Subject (Powerless) - Vulnerable to supply chokeholds.
 * - [US Government]: Beneficiary (Institutional) - Gaining leverage and profit.
 * - [Global Market Analyst]: Auditor (Analytical) - Observing geopolitical drift.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(project_vault_2026, 0.58). % High extraction via $12bn capital lockup and loan interest.
domain_priors:suppression_score(project_vault_2026, 0.72).   % High suppression of market-based sourcing alternatives.
domain_priors:theater_ratio(project_vault_2026, 0.35).       % Functional coordination with moderate performative messaging.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(project_vault_2026, extractiveness, 0.58).
narrative_ontology:constraint_metric(project_vault_2026, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(project_vault_2026, theater_ratio, 0.35).

% Active enforcement required to manage the reserve and trade restrictions.
domain_priors:requires_active_enforcement(project_vault_2026).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% Manufacturers feel the "chokehold" of China and the predatory nature of 
% geopolitical supply shocks as an immutable snare.
constraint_indexing:constraint_classification(project_vault_2026, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Viewed as essential infrastructure to ensure businesses are "never harmed."
constraint_indexing:constraint_classification(project_vault_2026, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects hybrid extraction/coordination: genuine resilience (Rope) 
% entangled with state rent-seeking and debt-based leverage (Snare).
constraint_indexing:constraint_classification(project_vault_2026, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))) :-
    domain_priors:base_extractiveness(project_vault_2026, E), E >= 0.50,
    domain_priors:suppression_score(project_vault_2026, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(project_vault_tests).

test(perspectival_gap) :-
    % Verify gap between powerless subject and institutional beneficiary.
    constraint_indexing:constraint_classification(project_vault_2026, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(project_vault_2026, rope, context(agent_power(institutional), _, _, _)).

test(extraction_threshold) :-
    domain_priors:base_extractiveness(project_vault_2026, E),

    E >= 0.46. % Triggers high-extraction logic.

:- end_tests(project_vault_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.58) reflects the government's explicit intent to 
 * "make a profit" on the loans used to fund the reserve, effectively 
 * extracting surplus from the crisis resilience process.
 * * [RESOLVED MANDATROPHY]
 * The Tangled Rope classification prevents mislabeling this as pure extraction 
 * because the coordination function (shielding manufacturers from China) 
 * provides a tangible survival benefit to the U.S. industrial base.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_vault_efficacy,
    'Can a $12bn reserve successfully offset a 90% processing dominance?',
    'Empirical stress-test data on U.S. processing capacity mid-trade-showdown.',
    'If Vault succeeds, it remains a Rope; if it fails, it becomes a Piton.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(project_vault_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time (monitors performative vs functional drift)
narrative_ontology:measurement(pv_tr_t0, project_vault_2026, theater_ratio, 0, 0.15).
narrative_ontology:measurement(pv_tr_t5, project_vault_2026, theater_ratio, 5, 0.25).
narrative_ontology:measurement(pv_tr_t10, project_vault_2026, theater_ratio, 10, 0.35).

% Extraction over time (monitors intensification of capital lockup)
narrative_ontology:measurement(pv_ex_t0, project_vault_2026, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(pv_ex_t5, project_vault_2026, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(pv_ex_t10, project_vault_2026, base_extractiveness, 10, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
