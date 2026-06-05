% ============================================================================
% CONSTRAINT STORY: epistemic_process_of_verification_u3_str_r1
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_epistemic_process_of_verification_u3_str_r1, []).

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
 *   constraint_id: epistemic_process_of_verification_u3_str_r1
 *   human_readable: Epistemic Process of Scientific Verification
 *   domain: scientific/epistemology
 *
 * SUMMARY:
 *   This constraint represents the institutionalized process of scientific
 *   verification, where novel claims must be independently replicated to be
 *   accepted. While this serves the vital function of coordinating the
 *   scientific community towards a reliable, shared body of knowledge, it
 *   also functions as a powerful gatekeeping mechanism. The costs of
 *   verification—in time, funding, and career risk—are disproportionately
 *   borne by junior researchers and those with heterodox ideas, while the
 *   benefits of stability and control accrue to established institutions,
 *   journals, and funding bodies.
 *
 * KEY AGENTS:
 *   - Junior Researchers / Heterodox Theorists: Primary targets (powerless/trapped) who bear the highest costs of the verification process.
 *   - Established Institutions / Funding Agencies: Primary beneficiaries (institutional/arbitrage) who control the process and benefit from the stability and quality control it provides.
 *   - Scientific Publishers: Secondary beneficiaries (institutional/arbitrage) who monetize the process through journal prestige.
 *   - Tenured Principal Investigators: Beneficiaries/enforcers (powerful/mobile) who have successfully navigated the system and now operate within it.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(epistemic_process_of_verification_u3_str_r1, 0.55).
domain_priors:suppression_score(epistemic_process_of_verification_u3_str_r1, 0.7).
domain_priors:theater_ratio(epistemic_process_of_verification_u3_str_r1, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(epistemic_process_of_verification_u3_str_r1, extractiveness, 0.55).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u3_str_r1, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u3_str_r1, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(epistemic_process_of_verification_u3_str_r1, tangled_rope).
narrative_ontology:human_readable(epistemic_process_of_verification_u3_str_r1, "Epistemic Process of Scientific Verification").
narrative_ontology:topic_domain(epistemic_process_of_verification_u3_str_r1, "scientific/epistemology").

domain_priors:requires_active_enforcement(epistemic_process_of_verification_u3_str_r1).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u3_str_r1, established_research_institutions).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u3_str_r1, scientific_publishers).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u3_str_r1, funding_agencies).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u3_str_r1, junior_researchers).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u3_str_r1, heterodox_theorists).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u3_str_r1, underfunded_labs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For a junior researcher whose career depends on validating a novel claim, the process is a high-risk gauntlet controlled by incumbents. The cost of failure is catastrophic, and the rules are enforced by those with competing interests, making it feel like a Snare.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u3_str_r1, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% From the perspective of a funding agency or premier journal, the process is a crucial Rope for coordinating the entire scientific enterprise, filtering noise, managing reputational risk, and ensuring the stability of the shared knowledge base. The extraction is viewed as a necessary cost for quality control.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u3_str_r1, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% The analytical view recognizes both the essential coordination function (Rope) and the asymmetric extraction (Snare). It is a Tangled Rope that simultaneously builds collective knowledge while imposing heavy costs on challengers and reinforcing existing power structures.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u3_str_r1, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% A tenured professor with an established lab has navigated the gauntlet and now benefits from the system's stability. While they still face its pressures, they have the resources and security to treat it as a coordination problem (Rope) for advancing their research program, rather than an existential threat.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u3_str_r1, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epistemic_process_of_verification_u3_str_r1_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(epistemic_process_of_verification_u3_str_r1, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(epistemic_process_of_verification_u3_str_r1, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(epistemic_process_of_verification_u3_str_r1, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(epistemic_process_of_verification_u3_str_r1_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (ε=0.55) reflects the significant, often uncompensated, labor, resources, and career risk required to satisfy the demands of replication, especially for paradigm-challenging work. The suppression score (0.70) is high because there are no viable alternatives within mainstream science; failure to engage with this process results in professional ostracization and loss of funding. The process requires active enforcement through peer review and funding panels.
 *
 * PERSPECTIVAL GAP:
 *   A significant gap exists between the junior researcher (who sees a Snare in the form of a career filter with high personal costs) and the institutional actor (who sees a Rope for maintaining epistemic quality across the entire field). The analytical classification of Tangled Rope is necessary to hold both truths: the process is both an essential coordination tool and a system of asymmetric extraction and control.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the flow of costs and benefits. Established institutions, publishers, and funding agencies are beneficiaries; they gain prestige, control, and a stable research environment. Junior researchers and proponents of non-mainstream ideas are victims; they bear the direct costs of replication and the risk of having their work suppressed or delayed by incumbent interests.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this process as a pure Rope would be a mandatrophy, ignoring the coercive and extractive elements that can stifle innovation and punish dissent. Conversely, classifying it as a pure Snare would ignore its undeniable and critical function in building reliable collective knowledge. The Tangled Rope classification correctly identifies the tension between its stated goal (coordination for truth) and its structural reality (asymmetric extraction and social control).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    verification_as_progress_vs_control,
    'Is the high cost and conservatism of the verification process a necessary feature to filter error, or a bug that primarily serves to suppress paradigm-shifting competition?',
    'Comparative analysis of career trajectories and funding outcomes for researchers with paradigm-challenging claims versus those with incremental claims, correlated with eventual validation or falsification.',
    'If primarily a filter for error, the constraint is closer to a Rope. If primarily a mechanism of social control by incumbents, it is closer to a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_as_progress_vs_control, empirical, 'Whether the verification process primarily filters for truth or suppresses dissent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(epistemic_process_of_verification_u3_str_r1, 1950, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epis_tr_t1950, epistemic_process_of_verification_u3_str_r1, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(epis_tr_t1985, epistemic_process_of_verification_u3_str_r1, theater_ratio, 1985, 0.2).
narrative_ontology:measurement(epis_tr_t2025, epistemic_process_of_verification_u3_str_r1, theater_ratio, 2025, 0.3).

% Extraction over time
narrative_ontology:measurement(epis_be_t1950, epistemic_process_of_verification_u3_str_r1, base_extractiveness, 1950, 0.2).
narrative_ontology:measurement(epis_be_t1985, epistemic_process_of_verification_u3_str_r1, base_extractiveness, 1985, 0.4).
narrative_ontology:measurement(epis_be_t2025, epistemic_process_of_verification_u3_str_r1, base_extractiveness, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(epistemic_process_of_verification_u3_str_r1, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
