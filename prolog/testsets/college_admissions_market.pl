% ============================================================================
% CONSTRAINT STORY: college_admissions_market
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-22
% ============================================================================

:- module(constraint_college_admissions_market, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: college_admissions_market
 * human_readable: The US Elite College Admissions Market
 * domain: social/economic
 * * SUMMARY:
 * The US elite college admissions system functions as a high-stakes, many-to-one matching market.
 * It coordinates students and universities but also imposes significant "signaling costs" (prep work,
 * application fees, emotional labor) on a vast pool of applicants, from which only a small
 * fraction are selected. This creates a structural tension between its coordination function
 * and its role as a hierarchical sorting mechanism.
 * * KEY AGENTS:
 * - The Applicant: Subject (Powerless), seeks a match at the highest possible prestige level.
 * - The Elite University: Beneficiary (Institutional), seeks to optimize its class profile and maintain prestige.
 * - The Market Analyst: Auditor (Analytical), observes the system's overall efficiency and fairness.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Base extractiveness is high (0.70). Elite colleges extract massive signaling
% costs from thousands of applicants to select only a few, benefiting from the
% large, competitive applicant pool to enhance their own prestige.
domain_priors:base_extractiveness(college_admissions_market, 0.70).

% Suppression is moderate (0.50). While alternative paths exist (less selective
% colleges, trade schools), the perceived social and economic cost of opting out
% of the elite track is very high for certain demographics.
domain_priors:suppression_score(college_admissions_market, 0.50).

% Theater ratio is low (0.15). The process is highly functional and consequential,
% not merely performative, though some aspects of applications are theatrical.
domain_priors:theater_ratio(college_admissions_market, 0.15).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(college_admissions_market, extractiveness, 0.70).
narrative_ontology:constraint_metric(college_admissions_market, suppression_requirement, 0.50).
narrative_ontology:constraint_metric(college_admissions_market, theater_ratio, 0.15).

% The system claims to be a fair coordination mechanism.
narrative_ontology:constraint_claim(college_admissions_market, tangled_rope).
narrative_ontology:human_readable(college_admissions_market, "The US Elite College Admissions Market").

% Binary flags
domain_priors:requires_active_enforcement(college_admissions_market). % Deadlines, standardized testing, etc.

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(college_admissions_market, elite_universities).
narrative_ontology:constraint_victim(college_admissions_market, unmatched_applicants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE APPLICANT (SNARE)
% For the student who invests years and significant resources only to be
% rejected, the system is a Snare. The sunk costs are immense, and the lack
% of alternatives at the same prestige level feels punitive and extractive.
constraint_indexing:constraint_classification(college_admissions_market, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE UNIVERSITY (ROPE)
% From the perspective of the admissions office, the system is a Rope. It is an
% essential coordination tool that allows them to efficiently sort a massive
% applicant pool and construct a class that meets institutional goals,
% thereby maintaining their elite status and financial health.
constraint_indexing:constraint_classification(college_admissions_market, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% An analyst sees both sides. The system performs a genuine coordination
% function (Rope) but does so via a mechanism that generates significant
% asymmetric extraction (Snare). It requires active enforcement to maintain.
% This hybrid nature is the definition of a Tangled Rope.
constraint_indexing:constraint_classification(college_admissions_market, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(college_admissions_market_tests).

test(perspectival_gap_applicant_vs_university) :-
    constraint_indexing:constraint_classification(college_admissions_market, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(college_admissions_market, rope,
        context(agent_power(institutional), _, _, _)),
    !.

test(analytical_observer_detects_tangled_rope) :-
    constraint_indexing:constraint_classification(college_admissions_market, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_requirements_met) :-
    narrative_ontology:constraint_beneficiary(college_admissions_market, _), % -> has_coordination_function
    narrative_ontology:constraint_victim(college_admissions_market, _),     % -> has_asymmetric_extraction
    domain_priors:requires_active_enforcement(college_admissions_market).

:- end_tests(college_admissions_market_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The scores reflect a system that is undeniably functional but also highly extractive.
 * Base extractiveness is 0.70 because the "cost of entry" (years of prep, fees,
 * emotional labor) is paid by all applicants, while the direct benefit (admission)
 * is granted to a very small minority. The beneficiaries (universities) gain prestige
 * and a highly curated student body from this large, competitive pool.
 *
 * PERSPECTIVAL GAP:
 * The gap is stark. For the applicant (powerless, trapped), the process is a high-cost,
 * low-probability gamble, making it a Snare. For the university (institutional, mobile),
 * it is an efficient sorting mechanism—a Rope—that is essential to its operational model.
 *
 * MANDATROPHY ANALYSIS:
 * The Tangled Rope classification is critical here. A simpler analysis might label the
 * system a pure Snare, ignoring its genuine and complex coordination function.
 * Conversely, accepting the institutional view of a pure Rope would ignore the immense
 * deadweight loss and extraction imposed on the majority of participants. Tangled Rope
 * correctly identifies and holds both truths: it is a system of coordination
 * *and* asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_college_admissions_market,
    'Is the prestige conferred by elite degrees a durable economic signal or a bubble susceptible to credential inflation and skill-based hiring?',
    'Longitudinal study tracking career earnings and mobility of elite vs. non-elite graduates in AI-disrupted industries over 15 years.',
    'If it is a bubble, the suppression score will drop as exit options become more viable, potentially loosening the Tangled Rope. If durable, the extraction may intensify.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(college_admissions_market, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% The admissions market has become more competitive and performative over time,
% increasing both the required signaling costs (extraction) and the theatrical
% elements of applications.
%
% Theater ratio over time (metric_substitution):
narrative_ontology:measurement(cam_tr_t0, college_admissions_market, theater_ratio, 0, 0.10).
narrative_ontology:measurement(cam_tr_t5, college_admissions_market, theater_ratio, 5, 0.12).
narrative_ontology:measurement(cam_tr_t10, college_admissions_market, theater_ratio, 10, 0.15).

% Extraction over time (extraction_accumulation):
narrative_ontology:measurement(cam_ex_t0, college_admissions_market, base_extractiveness, 0, 0.60).
narrative_ontology:measurement(cam_ex_t5, college_admissions_market, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(cam_ex_t10, college_admissions_market, base_extractiveness, 10, 0.70).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The constraint is a matching market, a form of resource allocation.
narrative_ontology:coordination_type(college_admissions_market, resource_allocation).

% The high cost of this market directly influences the student debt crisis.
narrative_ontology:affects_constraint(college_admissions_market, student_loan_burden).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */