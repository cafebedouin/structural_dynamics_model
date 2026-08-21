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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: tenure_contract__institutional_extraction_reading
 *   human_readable: Tenure as Institutional Rent Extraction
 *   domain: higher_education/labor_economics/institutional_theory
 *
 * SUMMARY:
 *   This constraint story analyzes academic tenure through an 'institutional
 *   extraction' lens, viewing it as a mechanism for early winners (tenured
 *   faculty) to secure permanent rents, leading to employment rigidity and
 *   the loading of costs onto contingent labor and students. This is one
 *   reading of the 'tenure_contract' kernel, distinct from readings
 *   emphasizing academic freedom or demographic reproduction. The high
 *   extractiveness and suppression metrics reflect this reading's focus on
 *   the system's coercive and rent-seeking aspects.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tenure_contract__institutional_extraction_reading, 0.85).
domain_priors:suppression_score(tenure_contract__institutional_extraction_reading, 0.78).
domain_priors:theater_ratio(tenure_contract__institutional_extraction_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenure_contract__institutional_extraction_reading, snare).
narrative_ontology:human_readable(tenure_contract__institutional_extraction_reading, "Tenure as Institutional Rent Extraction").
narrative_ontology:topic_domain(tenure_contract__institutional_extraction_reading, "higher_education/labor_economics/institutional_theory").

domain_priors:requires_active_enforcement(tenure_contract__institutional_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tenure_contract__institutional_extraction_reading, 'a29c87fb-d577-4da2-b1a0-ef0b75e6e116').
narrative_ontology:cs_kernel_codification('a29c87fb-d577-4da2-b1a0-ef0b75e6e116', formalized).
narrative_ontology:cs_authority_grounding('a29c87fb-d577-4da2-b1a0-ef0b75e6e116', extraction).
narrative_ontology:cs_interpretation_layer_present('a29c87fb-d577-4da2-b1a0-ef0b75e6e116').
narrative_ontology:cs_reading_relation('a29c87fb-d577-4da2-b1a0-ef0b75e6e116', tenure_contract__academic_freedom_reading, coexists_with).
narrative_ontology:cs_reading_relation('a29c87fb-d577-4da2-b1a0-ef0b75e6e116', tenure_contract__demographic_reproduction_reading, coexists_with).
narrative_ontology:cs_axiom('a29c87fb-d577-4da2-b1a0-ef0b75e6e116', foundational, academic_labor_is_a_market_good).
narrative_ontology:cs_axiom_status(academic_labor_is_a_market_good, holdable).
narrative_ontology:cs_axiom_grounding('a29c87fb-d577-4da2-b1a0-ef0b75e6e116', academic_labor_is_a_market_good, empirically_contingent).
narrative_ontology:cs_axiom('a29c87fb-d577-4da2-b1a0-ef0b75e6e116', foundational, institutional_resources_are_finite_and_contestable).
narrative_ontology:cs_axiom_status(institutional_resources_are_finite_and_contestable, holdable).
narrative_ontology:cs_axiom_grounding('a29c87fb-d577-4da2-b1a0-ef0b75e6e116', institutional_resources_are_finite_and_contestable, empirically_contingent).
narrative_ontology:cs_reference_frame('a29c87fb-d577-4da2-b1a0-ef0b75e6e116', market_efficiency_and_resource_optimization).
narrative_ontology:cs_drift_state('a29c87fb-d577-4da2-b1a0-ef0b75e6e116', contemporary_higher_education_crisis, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('a29c87fb-d577-4da2-b1a0-ef0b75e6e116', '').
narrative_ontology:cs_kernel_id(tenure_contract__institutional_extraction_reading, tenure_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenure_contract__institutional_extraction_reading, tenured_faculty).
narrative_ontology:constraint_beneficiary(tenure_contract__institutional_extraction_reading, university_administration).
narrative_ontology:constraint_victim(tenure_contract__institutional_extraction_reading, contingent_faculty).
narrative_ontology:constraint_victim(tenure_contract__institutional_extraction_reading, students).
narrative_ontology:constraint_victim(tenure_contract__institutional_extraction_reading, taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold permanent, high-status positions with significant autonomy and job security, effectively a lifetime claim on institutional resources. They benefit from the system's rigidity and the transfer of costs to other groups.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, tenured_faculty, beneficiary,
    institutional, biographical, arbitrage, national).

% Bear the costs of employment rigidity, working precarious, low-paid positions with limited benefits and no job security. Their labor subsidizes the tenured system, and their career paths are often identity_locked to academia despite poor conditions.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, contingent_faculty, payer,
    powerless, immediate, constrained, local).

% Pay high tuition fees that partly fund the tenured system, often receiving less instructional investment due to resource allocation rigidity. Their options are limited by the need for credentials and the structure of higher education.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, students, payer,
    moderate, biographical, constrained, local).

% Manages the tenure system, balancing the demands of tenured faculty with financial pressures. Benefits from the prestige associated with tenured research, but also faces constraints from the system's inflexibility and public scrutiny over costs.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, university_administration, agenda_setter,
    institutional, generational, constrained, national).

% Subsidize public universities, indirectly funding the tenure system. They bear the costs of institutional rigidity and question the value proposition of higher education when it appears to prioritize faculty security over educational outcomes or affordability.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, taxpayers, payer,
    organized, generational, mobile, national).

% Highly trained individuals seeking academic positions who face an extremely constrained market due to the limited turnover of tenured positions. They are excluded from the primary benefits of the system and often forced into contingent roles or out of academia entirely.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, academic_job_market_entrants, excluded,
    powerless, biographical, identity_locked, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tenure_contract__institutional_extraction_reading, tenured_faculty).
narrative_ontology:fixing_cost_class(tenure_contract__institutional_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The tenure system coordinates academic labor by providing a stable, long-term employment framework for a subset of faculty, theoretically ensuring continuity of research and teaching.
% TRANSFER_FUNCTION: Transfers a permanent claim on institutional resources (salary, benefits, research support) from the university budget (ultimately students and taxpayers) to tenured faculty, while transferring employment precarity and lower wages to contingent faculty.
% ABSENT_VOICES: Prospective academics and the broader public (taxpayers) are largely absent from the direct governance of tenure, though their concerns are voiced through policy debates and media. They would advocate for greater accountability, flexibility, and affordability in higher education.
% DISAPPEARANCE_RATIONALE: If tenure vanished overnight, universities would face immediate pressure to restructure faculty employment, likely leading to a more flexible, market-driven labor model. Resource allocation would shift, potentially impacting research priorities and instructional quality, and the academic labor market would undergo a profound reorganization.
% FOUNDING_PROBLEM: To protect academic freedom and attract top talent by offering job security, shielding scholars from political interference, and fostering long-term research projects.
% FOUNDING_PROBLEM_CORROBORATION: Tenured faculty and some university administrators attest the problem is still live, citing ongoing threats to academic freedom and the need for stable research environments. Contingent faculty, students, and labor economists attest the founding problem is largely solved or has been superseded by institutional rent-seeking, with the system now primarily serving to protect existing incumbents; economic analysis and labor market data from outside the benefiting parties support the shifted-function reading.
narrative_ontology:disappearance_verdict(tenure_contract__institutional_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(tenure_contract__institutional_extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tenure_contract__institutional_extraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(tenure_contract__institutional_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tenure_contract__institutional_extraction_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.85) because tenured positions represent a significant, largely unchallengeable claim on institutional resources, disproportionate to current marginal productivity. Suppression (0.78) is high due to the structural barriers to entry for new faculty, the limited exit options for contingent faculty (identity_locked to academia), and the active enforcement of employment rigidity by university administrations. Theater ratio (0.45) is moderate, reflecting that while some academic freedom functions persist, a substantial portion of the system's maintenance is performative, defending existing privileges rather than core academic values. The increasing trend in extractiveness and suppression over the interval reflects the growing precarity of academic labor and rising tuition costs.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of tenured faculty, the system is a 'rope' or 'scaffold' for academic freedom and stable research. From the perspective of contingent faculty and students, it operates as a 'snare' of extraction and precarity. The engine's classification will highlight this divergence based on the structural data provided for each seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Tenured faculty are clear beneficiaries (d near 0.0) due to their permanent claims. Contingent faculty, students, and taxpayers are victims (d near 1.0) as they bear the costs of rigidity and extraction. University administration acts as an agenda-setter, benefiting from the prestige of a tenured faculty while managing the system's inherent contradictions. Academic job market entrants are excluded, trapped by the system's structure.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading suggests that the original mandate of tenure (protecting academic freedom) has atrophied, and the constraint now primarily serves an extractive function. The 'contested' status of the founding problem and the 'world_rearranges' disappearance verdict, combined with high extractiveness, indicate a potential mandatrophy where the system persists due to inertia and concentrated benefits, rather than its original purpose. The classification as a snare prevents mislabeling this as coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    academic_freedom_vs_extraction,
    'To what extent does tenure genuinely protect academic freedom, versus primarily enabling rent extraction?',
    'Comparative analysis of academic output and controversial research topics in tenured vs. non-tenured systems, alongside detailed financial audits of resource allocation and faculty compensation.',
    'If academic freedom benefits are negligible, the constraint is more purely extractive (snare). If they are substantial, the constraint might be a tangled_rope, balancing coordination with extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(academic_freedom_vs_extraction, empirical, 'Distinguishing the functional coordination from the extractive component.').

omega_variable(
    resource_reallocation_potential,
    'What is the true potential for resource reallocation and innovation if tenure rigidity were reduced, and what would be the costs?',
    'Economic modeling of hypothetical tenure reform scenarios, including impact on research funding, faculty hiring, and educational programs, with sensitivity analysis for various cost structures.',
    'If reallocation yields significant benefits without undue costs to academic quality, the current system''s rigidity is a higher-cost snare. If costs are prohibitive, the rigidity is a more unavoidable feature of academic production.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(resource_reallocation_potential, empirical, 'Assessing the economic efficiency of the tenure system.').

omega_variable(
    identity_lock_strength_contingent_faculty,
    'How strong is the identity-lock mechanism for contingent faculty, and what proportion of their suppression is internalized versus structural?',
    'Longitudinal studies tracking career paths and psychological well-being of contingent faculty who exit academia versus those who remain, combined with qualitative interviews on perceived barriers and self-concept.',
    'If identity-lock is strong and internalized suppression is high, the effective extraction from contingent faculty is amplified, making the constraint more snare-like. If suppression is primarily structural, policy interventions might be more effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_strength_contingent_faculty, empirical, 'Structural vs. internalized suppression mechanism for contingent faculty.').

omega_variable(
    kernel_framing_ambiguity,
    'Is the ''tenure_contract'' primarily a mechanism for academic freedom, demographic reproduction, or institutional extraction?',
    'Analysis of historical documents, legal precedents, and institutional practices, weighted by their impact on resource allocation and labor market outcomes, to determine which framing best explains the system''s persistence and effects.',
    'The classification of the constraint (and its siblings) depends on which framing is adopted. This reading (institutional_extraction_reading) yields a snare; the academic_freedom_reading would yield a rope/scaffold; the demographic_reproduction_reading would yield a tangled_rope/snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Ambiguity in the primary function and purpose of the tenure contract.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenure_contract__institutional_extraction_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tenu_tr_t0, tenure_contract__institutional_extraction_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(tenu_tr_t8, tenure_contract__institutional_extraction_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(tenu_tr_t16, tenure_contract__institutional_extraction_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement(tenu_tr_t24, tenure_contract__institutional_extraction_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement(tenu_tr_t32, tenure_contract__institutional_extraction_reading, theater_ratio, 32, 0.43).
narrative_ontology:measurement(tenu_tr_t40, tenure_contract__institutional_extraction_reading, theater_ratio, 40, 0.45).

% Extraction over time
narrative_ontology:measurement(tenu_be_t0, tenure_contract__institutional_extraction_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(tenu_be_t8, tenure_contract__institutional_extraction_reading, base_extractiveness, 8, 0.72).
narrative_ontology:measurement(tenu_be_t16, tenure_contract__institutional_extraction_reading, base_extractiveness, 16, 0.78).
narrative_ontology:measurement(tenu_be_t24, tenure_contract__institutional_extraction_reading, base_extractiveness, 24, 0.82).
narrative_ontology:measurement(tenu_be_t32, tenure_contract__institutional_extraction_reading, base_extractiveness, 32, 0.84).
narrative_ontology:measurement(tenu_be_t40, tenure_contract__institutional_extraction_reading, base_extractiveness, 40, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(tenu_su_t0, tenure_contract__institutional_extraction_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(tenu_su_t8, tenure_contract__institutional_extraction_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement(tenu_su_t16, tenure_contract__institutional_extraction_reading, suppression_requirement, 16, 0.69).
narrative_ontology:measurement(tenu_su_t24, tenure_contract__institutional_extraction_reading, suppression_requirement, 24, 0.74).
narrative_ontology:measurement(tenu_su_t32, tenure_contract__institutional_extraction_reading, suppression_requirement, 32, 0.77).
narrative_ontology:measurement(tenu_su_t40, tenure_contract__institutional_extraction_reading, suppression_requirement, 40, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenure_contract__institutional_extraction_reading, resource_allocation).
narrative_ontology:affects_constraint(tenure_contract__institutional_extraction_reading, academic_job_market_precarity).
narrative_ontology:affects_constraint(tenure_contract__institutional_extraction_reading, university_tuition_inflation).
narrative_ontology:affects_constraint(tenure_contract__institutional_extraction_reading, academic_research_funding_allocation).

% DUAL FORMULATION NOTE:
% This story is one reading of the 'tenure_contract' kernel, focusing on its function as institutional rent extraction. It is linked to other readings (academic_freedom_reading, demographic_reproduction_reading) which offer alternative interpretations of the same underlying commitment system.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
