% ============================================================================
% CONSTRAINT STORY: project_vault_extraction_2026
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_project_vault_extraction_2026, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: project_vault_extraction_2026
 * human_readable: Project Vault: Debt-Financed Strategic Extraction
 * domain: economic/political
 * * SUMMARY:
 * Project Vault is a $12 billion critical minerals reserve funded by a $10 billion
 * EXIM Bank loan with an explicit profit mandate. While it shields
 * manufacturers from China's 90% processing monopoly, the debt-based structure
 * creates a captive market where manufacturers must pre-commit to purchase at
 * government-set prices.
 * * KEY AGENTS:
 * - [U.S. Manufacturers]: Subject (Powerless) - Trapped in a "monopsony"
 * requirement with no alternatives for 10-18 years.
 * - [EXIM Bank / U.S. Govt]: Beneficiary (Institutional) - Capturing yield
 * from industrial vulnerability.
 * - [Geopolitical Auditor]: Auditor (Analytical) - Observing the "financialization
 * of national security".
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.78) due to the explicit "make a profit" mandate and
% captive pricing mechanism.
domain_priors:base_extractiveness(project_vault_extraction_2026, 0.78).

% High suppression (0.84); manufacturers have no alternative for rare
% earth magnets for 10-18 years.
domain_priors:suppression_score(project_vault_extraction_2026, 0.84).

% Moderate-High theater (0.65) predicted as institutional capture
% prioritizes yield over genuine supply safety.
domain_priors:theater_ratio(project_vault_extraction_2026, 0.65).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(project_vault_extraction_2026, extractiveness, 0.78).
narrative_ontology:constraint_metric(project_vault_extraction_2026, suppression_requirement, 0.84).
narrative_ontology:constraint_metric(project_vault_extraction_2026, theater_ratio, 0.65).

% Constraint self-claim: It claims to be a coordination mechanism for national security.
narrative_ontology:constraint_claim(project_vault_extraction_2026, tangled_rope).

% Binary flags
domain_priors:requires_active_enforcement(project_vault_extraction_2026). % Required for Tangled Rope

% Structural property derivation hooks:
% These are required for Tangled Rope classification.
narrative_ontology:constraint_beneficiary(project_vault_extraction_2026, exim_bank_us_govt).
narrative_ontology:constraint_victim(project_vault_extraction_2026, us_manufacturers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% Manufacturers view the reserve as a "trap with no exit," paying
% "permanently elevated prices" to avoid factory shutdowns.
constraint_indexing:constraint_classification(project_vault_extraction_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Government officials see "strategic infrastructure" that provides
% negotiating leverage and taxpayer returns.
constraint_indexing:constraint_classification(project_vault_extraction_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Coordination (security) and extraction (profit) are inseparable due to
% the debt-financed SPV structure.
constraint_indexing:constraint_classification(project_vault_extraction_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(project_vault_extraction_2026_tests).

test(perspectival_gap) :-
    % Verify the "coerced exchange" between subject (Snare) and beneficiary (Rope).
    constraint_indexing:constraint_classification(project_vault_extraction_2026, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(project_vault_extraction_2026, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(project_vault_extraction_2026, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_properties) :-
    % Ensure all three required properties for Tangled Rope are present.
    domain_priors:requires_active_enforcement(project_vault_extraction_2026),
    narrative_ontology:constraint_beneficiary(project_vault_extraction_2026, _),
    narrative_ontology:constraint_victim(project_vault_extraction_2026, _).

:- end_tests(project_vault_extraction_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.78) is anchored in the "make a profit" signal, which
 * implies surplus capture beyond mere cost-recovery for infrastructure.
 * The Perspectival Gap is extreme because while the institution captures
 * "upside" yield, the manufacturer bears the "operational risk" and
 * mandatory markups.
 *
 * * MANDATROPHY ANALYSIS:
 * The Tangled Rope classification resolves the mandatrophy by acknowledging that
 * the reserve's "protection" function is genuine and necessary (preventing
 * shutdowns), even while its debt-based financing mechanism is highly
 * extractive. This prevents the system from misclassifying it as a pure Snare,
 * which would ignore the coordination benefit, or a pure Rope, which would
 * ignore the asymmetric extraction. [RESOLVED MANDATROPHY]
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_vault_sufficiency,
    'Is the $12bn stockpile actually sufficient to prevent shutdowns during a 12-month cutoff?',
    'Stress-test modeling of inventory quantities vs. total manufacturing demand.',
    'If sufficient, it is a Tangled Rope; if insufficient, it is a Piton (pure theater).',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(project_vault_extraction_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Required for extraction (0.78) > 0.46.
% Interval: T=0 (2026 Launch) to T=10 (Institutional Capture Phase).

% Theater ratio: Increases as "normalization" and "institutional capture"
% favor yield over emergency response.
narrative_ontology:measurement(pv_tr_t0, project_vault_extraction_2026, theater_ratio, 0, 0.15).
narrative_ontology:measurement(pv_tr_t5, project_vault_extraction_2026, theater_ratio, 5, 0.45).
narrative_ontology:measurement(pv_tr_t10, project_vault_extraction_2026, theater_ratio, 10, 0.65).

% Extraction: Intensifies as EXIM Bank optimizes pricing to maximize returns
% on its largest-ever loan portfolio.
narrative_ontology:measurement(pv_ex_t0, project_vault_extraction_2026, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(pv_ex_t5, project_vault_extraction_2026, base_extractiveness, 5, 0.60).
narrative_ontology:measurement(pv_ex_t10, project_vault_extraction_2026, base_extractiveness, 10, 0.78).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The reserve is a strategic resource allocation mechanism.
narrative_ontology:coordination_type(project_vault_extraction_2026, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */