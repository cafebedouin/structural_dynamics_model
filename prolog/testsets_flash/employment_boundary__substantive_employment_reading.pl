% ============================================================================
% CONSTRAINT STORY: employment_boundary__substantive_employment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_employment_boundary__substantive_employment_reading, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: employment_boundary__substantive_employment_reading
 *   human_readable: Substantive Employment Boundary for Platform Workers
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'substantive employment reading'
 *   of the 'employment_boundary' kernel. It posits that employment should be
 *   defined by economic dependence and algorithmic control, rather than
 *   formal contract, thereby classifying platform workers as employees. This
 *   reading challenges the prevailing 'formalist employment reading' which
 *   allows platform companies to avoid labor obligations. The metrics reflect
 *   the current extractive nature of platform work under the formalist
 *   reading, and the ongoing struggle to enforce a substantive definition.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(employment_boundary__substantive_employment_reading, 0.65).
domain_priors:suppression_score(employment_boundary__substantive_employment_reading, 0.7).
domain_priors:theater_ratio(employment_boundary__substantive_employment_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(employment_boundary__substantive_employment_reading, tangled_rope).
narrative_ontology:human_readable(employment_boundary__substantive_employment_reading, "Substantive Employment Boundary for Platform Workers").
narrative_ontology:topic_domain(employment_boundary__substantive_employment_reading, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(employment_boundary__substantive_employment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(employment_boundary__substantive_employment_reading, '1b9533eb-f400-44a3-aef6-c32f5d76f227').
narrative_ontology:cs_kernel_codification('1b9533eb-f400-44a3-aef6-c32f5d76f227', distributed).
narrative_ontology:cs_authority_grounding('1b9533eb-f400-44a3-aef6-c32f5d76f227', distributed).
narrative_ontology:cs_reading_relation('1b9533eb-f400-44a3-aef6-c32f5d76f227', employment_boundary__formalist_employment_reading, influences).
narrative_ontology:cs_reading_relation('1b9533eb-f400-44a3-aef6-c32f5d76f227', employment_boundary__hybrid_security_reading, coexists_with).
narrative_ontology:cs_axiom('1b9533eb-f400-44a3-aef6-c32f5d76f227', foundational, economic_dependence_defines_employment).
narrative_ontology:cs_axiom_status(economic_dependence_defines_employment, holdable).
narrative_ontology:cs_axiom_grounding('1b9533eb-f400-44a3-aef6-c32f5d76f227', economic_dependence_defines_employment, deontological).
narrative_ontology:cs_axiom('1b9533eb-f400-44a3-aef6-c32f5d76f227', secondary, algorithmic_control_is_supervision).
narrative_ontology:cs_axiom_status(algorithmic_control_is_supervision, holdable).
narrative_ontology:cs_axiom_grounding('1b9533eb-f400-44a3-aef6-c32f5d76f227', algorithmic_control_is_supervision, empirically_contingent).
narrative_ontology:cs_reference_frame('1b9533eb-f400-44a3-aef6-c32f5d76f227', substantive_labor_law_tradition).
narrative_ontology:cs_drift_state('1b9533eb-f400-44a3-aef6-c32f5d76f227', contemporary_platform_economy, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1b9533eb-f400-44a3-aef6-c32f5d76f227', '').
narrative_ontology:cs_kernel_id(employment_boundary__substantive_employment_reading, employment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(employment_boundary__substantive_employment_reading, platform_companies).
narrative_ontology:constraint_victim(employment_boundary__substantive_employment_reading, platform_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(employment_boundary__substantive_employment_reading, labor_unions_advocates).
narrative_ontology:constraint_beneficiary(employment_boundary__substantive_employment_reading, traditional_employers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Experience economic dependence and algorithmic control, leading to precarity, lack of benefits, and limited recourse. This reading reclassifies them as employees, making them beneficiaries of labor protections but targets of the current extractive system.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, platform_workers, payer,
    powerless, immediate, constrained, global).

% Benefit from classifying workers as independent contractors, avoiding social insurance contributions, minimum wage, and other labor protections. This reading would obligate them to provide these benefits, increasing their costs but formalizing their workforce.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, platform_companies, agenda_setter,
    institutional, generational, mobile, global).

% Advocate for this substantive definition of employment, seeking to extend labor protections to platform workers. They benefit from the reclassification as it expands their mandate and membership base.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, labor_unions_advocates, beneficiary,
    organized, generational, analytical, national).

% Are the institutional actors tasked with interpreting and enforcing employment law. This reading provides a framework for them to reclassify platform workers, potentially leading to new regulations and legal precedents.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, regulators_courts, agenda_setter,
    institutional, biographical, constrained, national).

% Face a competitive disadvantage from platform companies that avoid labor costs. This reading levels the playing field by imposing similar obligations on platform companies.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, traditional_employers, beneficiary,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate labor protections and social welfare provisions across all economically dependent workers, regardless of the formal contract used by employers, ensuring a baseline of security and rights.
% TRANSFER_FUNCTION: Transfers social insurance contributions, minimum wage obligations, and other employment benefits from platform companies to platform workers, and potentially transfers competitive advantage from platform companies to traditional employers.
% ABSENT_VOICES: Gig economy consumers, who might face increased costs or reduced service availability if platform companies pass on new labor costs, are often not directly represented in these debates.
% DISAPPEARANCE_RATIONALE: If this substantive definition of employment vanished, platform companies would continue to classify workers as independent contractors, perpetuating precarity. Labor advocates would lose a key legal and conceptual tool, and the debate over platform worker rights would revert to a more formalistic interpretation, significantly altering the landscape of labor law and worker protections.
% FOUNDING_PROBLEM: The rise of the gig economy created a class of workers who, despite economic dependence and control by platforms, were denied traditional employment rights and protections due to their classification as independent contractors.
% FOUNDING_PROBLEM_CORROBORATION: Labor economists, social policy researchers, and international labor organizations consistently document the precarity and lack of protections for platform workers, corroborating that the problem remains live and significant. Legal scholars also attest to the ongoing challenge of applying traditional labor law to new forms of work.
narrative_ontology:disappearance_verdict(employment_boundary__substantive_employment_reading, world_rearranges).
narrative_ontology:founding_problem_status(employment_boundary__substantive_employment_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(employment_boundary__substantive_employment_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(employment_boundary__substantive_employment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(employment_boundary__substantive_employment_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(employment_boundary__substantive_employment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(employment_boundary__substantive_employment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(employment_boundary__substantive_employment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is high because platform companies currently extract significant value by avoiding labor costs. Suppression (0.70) is also high, as platforms actively resist reclassification through legal challenges, lobbying, and algorithmic management that reinforces independent contractor status. The claimed type is 'tangled_rope' because while there's a genuine coordination function (connecting workers to tasks), it's coupled with asymmetric extraction from workers, requiring active enforcement to maintain the current status quo.
 *
 * PERSPECTIVAL GAP:
 *   Platform workers experience the current system as a snare, while platform companies frame it as a rope (efficient coordination). This reading, if adopted, would shift the burden of costs onto platforms, making them the payers and workers the beneficiaries, thus altering the perceived type from each seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform workers are victims (payers) of the current system, bearing the costs of precarity. Platform companies are beneficiaries (agenda-setters) of the current system, profiting from the lack of labor obligations. Labor unions and traditional employers are beneficiaries of this reading, as it aligns with their interests in worker protection and fair competition. Regulators and courts are agenda-setters, as they hold the power to implement and enforce this redefinition.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (to define employment) has not atrophied, but its interpretation has been contested. The 'substantive employment reading' seeks to resolve this by re-aligning the definition with the economic realities of platform work, preventing the mislabeling of what is effectively an employment relationship as independent contracting.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substantive_vs_formalist_definition,
    'Is employment fundamentally defined by the substance of the work relationship (dependence, control) or by its formal contractual terms?',
    'Legal precedent from supreme courts or legislative action explicitly codifying a substantive test for employment across all sectors.',
    'If resolved towards substance, this reading becomes the dominant legal framework, significantly altering labor markets. If resolved towards formalism, this reading remains a minority position, and platform workers continue to lack protections.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(substantive_vs_formalist_definition, conceptual, 'Ambiguity in the core definition of employment.').

omega_variable(
    economic_impact_of_reclassification,
    'What would be the net economic impact (on platform companies, workers, and consumers) of widespread reclassification of platform workers as employees?',
    'Empirical studies from jurisdictions that have implemented such reclassification, analyzing changes in employment rates, service costs, and platform profitability.',
    'If the economic impact is largely negative (e.g., job losses, service reduction), it could weaken the political will for this reading. If positive (e.g., improved worker welfare, sustainable business models), it strengthens its case.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_impact_of_reclassification, empirical, 'Uncertainty about the economic consequences of reclassification.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of platform worker organizing structural (legal barriers, algorithmic control) or internalized (fear of deactivation, lack of collective identity)?',
    'Post-reclassification organizing trajectory: if organizing increases significantly after legal barriers are removed, it suggests structural suppression was dominant. If it remains low, internalized suppression is more significant.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, requiring different interventions (e.g., community building, awareness campaigns).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for platform workers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(employment_boundary__substantive_employment_reading, 2010, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(empl_be_t2010, employment_boundary__substantive_employment_reading, base_extractiveness, 2010, 0.5).
narrative_ontology:measurement(empl_be_t2014, employment_boundary__substantive_employment_reading, base_extractiveness, 2014, 0.55).
narrative_ontology:measurement(empl_be_t2018, employment_boundary__substantive_employment_reading, base_extractiveness, 2018, 0.6).
narrative_ontology:measurement(empl_be_t2022, employment_boundary__substantive_employment_reading, base_extractiveness, 2022, 0.63).
narrative_ontology:measurement(empl_be_t2024, employment_boundary__substantive_employment_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(empl_su_t2010, employment_boundary__substantive_employment_reading, suppression_requirement, 2010, 0.55).
narrative_ontology:measurement(empl_su_t2014, employment_boundary__substantive_employment_reading, suppression_requirement, 2014, 0.6).
narrative_ontology:measurement(empl_su_t2018, employment_boundary__substantive_employment_reading, suppression_requirement, 2018, 0.65).
narrative_ontology:measurement(empl_su_t2022, employment_boundary__substantive_employment_reading, suppression_requirement, 2022, 0.68).
narrative_ontology:measurement(empl_su_t2024, employment_boundary__substantive_employment_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(employment_boundary__substantive_employment_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(employment_boundary__substantive_employment_reading, employment_boundary__formalist_employment_reading).
narrative_ontology:affects_constraint(employment_boundary__substantive_employment_reading, employment_boundary__hybrid_security_reading).
narrative_ontology:affects_constraint(employment_boundary__substantive_employment_reading, social_safety_net_eligibility).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'employment_boundary' kernel, each representing a distinct structural claim about the nature of employment in the platform economy. This reading directly challenges the formalist interpretation and offers an alternative to the hybrid security model.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
