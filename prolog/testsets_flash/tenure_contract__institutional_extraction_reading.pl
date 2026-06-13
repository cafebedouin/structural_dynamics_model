% ============================================================================
% CONSTRAINT STORY: tenure_contract__institutional_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tenure_contract__institutional_extraction_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: tenure_contract__institutional_extraction_reading
 *   human_readable: Tenure Contract (Institutional Extraction Reading)
 *   domain: higher_education_governance/labor_economics/institutional_theory
 *
 * SUMMARY:
 *   This constraint models the tenure contract as a mechanism for
 *   institutional extraction, where tenured faculty (early winners) secure
 *   permanent claims on university resources, leading to employment rigidity.
 *   This rigidity prevents efficient resource reallocation and shifts the
 *   burden of flexibility onto contingent faculty and students. This is one
 *   reading of the 'tenure_contract' kernel, focusing on its economic and
 *   labor market effects rather than its academic freedom or demographic
 *   reproduction aspects.
 *
 * KEY AGENTS:
 *   - tenured_faculty: Primary beneficiary (institutional/arbitrage) — permanent claim on resources
 *   - contingent_faculty: Primary victim (powerless/trapped) — bears employment precarity and resource scarcity
 *   - students: Secondary victim (moderate/constrained) — bears costs through tuition and reduced instructional quality
 *   - university_administrators: Secondary victim (institutional/constrained) — constrained in resource reallocation by tenure rigidity
 *   - academic_freedom_advocates: Excluded (organized/analytical) — their framing of tenure is sidelined by the extraction dynamics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tenure_contract__institutional_extraction_reading, 0.85).
domain_priors:suppression_score(tenure_contract__institutional_extraction_reading, 0.75).
domain_priors:theater_ratio(tenure_contract__institutional_extraction_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenure_contract__institutional_extraction_reading, snare).
narrative_ontology:human_readable(tenure_contract__institutional_extraction_reading, "Tenure Contract (Institutional Extraction Reading)").
narrative_ontology:topic_domain(tenure_contract__institutional_extraction_reading, "higher_education_governance/labor_economics/institutional_theory").

domain_priors:requires_active_enforcement(tenure_contract__institutional_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tenure_contract__institutional_extraction_reading, '5a1d3485-c39b-46ce-ae60-7740de4e8153').
narrative_ontology:cs_kernel_codification('5a1d3485-c39b-46ce-ae60-7740de4e8153', formalized).
narrative_ontology:cs_authority_grounding('5a1d3485-c39b-46ce-ae60-7740de4e8153', lineage).
narrative_ontology:cs_interpretation_layer_present('5a1d3485-c39b-46ce-ae60-7740de4e8153').
narrative_ontology:cs_reading_relation('5a1d3485-c39b-46ce-ae60-7740de4e8153', tenure_contract__academic_freedom_reading, coexists_with).
narrative_ontology:cs_reading_relation('5a1d3485-c39b-46ce-ae60-7740de4e8153', tenure_contract__demographic_reproduction_reading, coexists_with).
narrative_ontology:cs_axiom('5a1d3485-c39b-46ce-ae60-7740de4e8153', foundational, resource_allocation_must_be_flexible).
narrative_ontology:cs_axiom_status(resource_allocation_must_be_flexible, holdable).
narrative_ontology:cs_axiom_grounding('5a1d3485-c39b-46ce-ae60-7740de4e8153', resource_allocation_must_be_flexible, empirically_contingent).
narrative_ontology:cs_axiom('5a1d3485-c39b-46ce-ae60-7740de4e8153', foundational, permanent_claims_create_deadweight_loss).
narrative_ontology:cs_axiom_status(permanent_claims_create_deadweight_loss, holdable).
narrative_ontology:cs_axiom_grounding('5a1d3485-c39b-46ce-ae60-7740de4e8153', permanent_claims_create_deadweight_loss, empirically_contingent).
narrative_ontology:cs_reference_frame('5a1d3485-c39b-46ce-ae60-7740de4e8153', efficient_resource_allocation_framework).
narrative_ontology:cs_drift_state('5a1d3485-c39b-46ce-ae60-7740de4e8153', contemporary_neoliberal_university, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('5a1d3485-c39b-46ce-ae60-7740de4e8153', '').
narrative_ontology:cs_kernel_id(tenure_contract__institutional_extraction_reading, tenure_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenure_contract__institutional_extraction_reading, tenured_faculty).
narrative_ontology:constraint_victim(tenure_contract__institutional_extraction_reading, contingent_faculty).
narrative_ontology:constraint_victim(tenure_contract__institutional_extraction_reading, students).
narrative_ontology:constraint_victim(tenure_contract__institutional_extraction_reading, university_administrators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold permanent positions, often with higher salaries and fewer teaching obligations, securing a stable claim on university resources. They benefit from job security and academic freedom, which this reading argues is a cover for resource capture.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, tenured_faculty, beneficiary,
    institutional, generational, arbitrage, national).

% Comprise the majority of instructional staff, working on short-term contracts with low pay, no benefits, and no job security. They bear the costs of employment flexibility that tenured faculty avoid, often with limited career progression options within academia.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, contingent_faculty, payer,
    powerless, immediate, trapped, national).

% Pay high tuition fees, which this reading argues subsidize the rigid cost structure created by tenure. They may experience reduced instructional quality due to the reliance on precarious contingent faculty and limited investment in innovative teaching methods.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, students, payer,
    moderate, biographical, constrained, local).

% Are responsible for managing university budgets and strategic planning. They are constrained by the fixed costs of tenured faculty, limiting their ability to reallocate resources to new programs, research areas, or address changing student demands.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, university_administrators, payer,
    institutional, generational, constrained, national).

% Argue that tenure is essential for protecting intellectual inquiry and dissent. In this reading, their arguments are used to legitimize a system that has become primarily extractive, and their concerns about academic freedom are often sidelined by the economic realities of the system.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, academic_freedom_advocates, excluded,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tenure_contract__institutional_extraction_reading, tenured_faculty).
narrative_ontology:fixing_cost_class(tenure_contract__institutional_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable core faculty for universities, ensuring continuity in research and teaching, and theoretically protecting intellectual independence.
% TRANSFER_FUNCTION: Transfers a permanent claim on university resources (salary, benefits, reduced workload) from the general university budget (ultimately students and taxpayers) to tenured faculty, in exchange for a one-time grant of 'academic freedom'.
% ABSENT_VOICES: Prospective faculty and early-career researchers who are effectively locked out of stable academic careers, and taxpayers who subsidize the system without direct representation in its governance. They would advocate for a more flexible and equitable academic labor market.
% DISAPPEARANCE_RATIONALE: If tenure disappeared overnight, universities would immediately gain significant flexibility in resource allocation and hiring. The academic labor market would undergo a massive restructuring, likely leading to more short-term contracts but potentially also more merit-based hiring and resource shifts to high-demand areas. The power dynamics within universities would fundamentally change.
% FOUNDING_PROBLEM: To protect scholars from institutional or political interference, ensuring intellectual independence and the pursuit of knowledge without fear of reprisal, and to attract and retain top talent by offering long-term security.
% FOUNDING_PROBLEM_CORROBORATION: Academic freedom advocates and tenured faculty assert the problem is live, citing ongoing threats to intellectual inquiry. Contingent faculty, students, and some university administrators argue the problem is largely solved or that tenure no longer effectively addresses it, and that its current function is primarily to protect entrenched interests. Economic analyses from outside the benefiting parties corroborate the shift towards extraction and rigidity.
narrative_ontology:disappearance_verdict(tenure_contract__institutional_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(tenure_contract__institutional_extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tenure_contract__institutional_extraction_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(tenure_contract__institutional_extraction_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tenure_contract__institutional_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(tenure_contract__institutional_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(tenure_contract__institutional_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the permanent claim on resources by tenured faculty, which is decoupled from current productivity or market demand. Suppression (0.75) is high due to the limited entry points into tenure-track positions and the precariousness of contingent labor, which discourages dissent. Theater ratio (0.4) indicates that while some academic freedom functions are maintained, a significant portion of the system's activity is performative maintenance of the status quo. Accessibility collapse (0.6) is moderate, as alternative academic careers are limited, but non-academic options exist. Resistance (0.7) is substantial, primarily from contingent faculty and student groups advocating for reform.
 *
 * PERSPECTIVAL GAP:
 *   Tenured faculty experience this as a legitimate reward for past achievement and a guarantor of intellectual independence (a Rope or even Mountain from their seat). Contingent faculty and students experience it as a Snare, extracting resources and opportunity. University administrators, while benefiting from a stable core faculty, are also victims of the rigidity that prevents strategic resource allocation.
 *
 * DIRECTIONALITY LOGIC:
 *   Tenured faculty are clear beneficiaries (d=0.0-0.1) due to their permanent positions and resource claims. Contingent faculty are clear targets (d=0.9-1.0) due to precarity and exploitation. Students are targets (d=0.7-0.8) through tuition and reduced educational quality. University administrators are complex victims (d=0.6-0.7), as the system constrains their ability to manage resources effectively, despite their institutional power. The system itself, as an institutional structure, is the agenda-setter, perpetuating its own logic.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading classifies tenure as a Snare, indicating that its primary function has drifted from its stated purpose (academic freedom) to one of rent extraction. The persistence of the constraint is due to the concentrated benefits for tenured faculty and the diffuse costs borne by others, rather than a genuine coordination problem. The high extractiveness and suppression, coupled with the contested founding problem status, point to a mandatrophic state where the original mandate has atrophied, but the structure persists due to its extractive function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the tenure contract primarily a mechanism for institutional extraction, or for academic freedom, or for demographic reproduction?',
    'Empirical analysis of resource flows, employment rigidity, and demographic patterns over time, compared against stated academic freedom protections and diversity goals.',
    'If primarily extraction, the constraint is a Snare; if academic freedom, it''s a Rope; if demographic reproduction, it''s a Tangled Rope with specific victim groups.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Ambiguity in the primary function of the tenure contract.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of contingent faculty structural (lack of alternative employment, legal barriers) or internalized (professional identity, fear of reprisal)?',
    'Post-exit employment trajectory and survey data from former contingent faculty: if suppression persists after leaving academia, it suggests internalized components.',
    'If internalized, the effective suppression is higher than structural measures suggest, as contingent faculty carry the suppression with them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for contingent faculty.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenure_contract__institutional_extraction_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tenu_tr_t0, tenure_contract__institutional_extraction_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(tenu_tr_t10, tenure_contract__institutional_extraction_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(tenu_tr_t20, tenure_contract__institutional_extraction_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(tenu_tr_t30, tenure_contract__institutional_extraction_reading, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(tenu_be_t0, tenure_contract__institutional_extraction_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(tenu_be_t10, tenure_contract__institutional_extraction_reading, base_extractiveness, 10, 0.7).
narrative_ontology:measurement(tenu_be_t20, tenure_contract__institutional_extraction_reading, base_extractiveness, 20, 0.8).
narrative_ontology:measurement(tenu_be_t30, tenure_contract__institutional_extraction_reading, base_extractiveness, 30, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(tenu_su_t0, tenure_contract__institutional_extraction_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(tenu_su_t10, tenure_contract__institutional_extraction_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(tenu_su_t20, tenure_contract__institutional_extraction_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(tenu_su_t30, tenure_contract__institutional_extraction_reading, suppression_requirement, 30, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenure_contract__institutional_extraction_reading, resource_allocation).
narrative_ontology:affects_constraint(tenure_contract__institutional_extraction_reading, academic_labor_market_precarity).
narrative_ontology:affects_constraint(tenure_contract__institutional_extraction_reading, university_tuition_inflation).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'tenure_contract' kernel, focusing on its institutional extraction dynamics. It is linked to the 'academic_freedom_reading' and 'demographic_reproduction_reading' through the shared kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
