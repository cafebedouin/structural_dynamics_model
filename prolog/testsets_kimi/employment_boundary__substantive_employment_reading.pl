% ============================================================================
% CONSTRAINT STORY: employment_boundary__substantive_employment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: employment_boundary__substantive_employment_reading
 *   human_readable: Substantive Employment Test for Platform Workers
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   This constraint instantiates the substantive_employment_reading of the
 *   contested employment_boundary kernel. It defines employment status by
 *   economic dependence and algorithmic control rather than contractual form,
 *   reclassifying platform workers as employees entitled to full social
 *   insurance and job security. The constraint operates as a legal
 *   enforcement mechanism that reallocates the costs of labor protection from
 *   workers and public systems to digital labor platforms. It is claimed as a
 *   tangled_rope because it combines genuine protective coordination for
 *   workers with asymmetric extraction from platforms, requiring active state
 *   enforcement to maintain against platform resistance.
 *
 * KEY AGENTS:
 *   - platform_workers: Primary beneficiary (powerless/constrained) â gain employment protections and social insurance.
 *   - digital_labor_platforms: Primary payer/target (powerful/constrained) â bear employer obligations and compliance costs, actively resist.
 *   - state_labor_authorities: Agenda setter (institutional/analytical) â defines and enforces the substantive test.
 *   - gig_economy_advocates: Excluded voice (moderate/constrained) â argues for contractual flexibility, structurally absent from rulemaking.
 *   - traditional_employers: Observer (organized/mobile) â faces precedent risk from boundary expansion.
 *   - labor_economists: Analytical observer (analytical/analytical) â provides empirical evidence on dependence and control.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(employment_boundary__substantive_employment_reading, 0.55).
domain_priors:suppression_score(employment_boundary__substantive_employment_reading, 0.45).
domain_priors:theater_ratio(employment_boundary__substantive_employment_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(employment_boundary__substantive_employment_reading, tangled_rope).
narrative_ontology:human_readable(employment_boundary__substantive_employment_reading, "Substantive Employment Test for Platform Workers").
narrative_ontology:topic_domain(employment_boundary__substantive_employment_reading, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(employment_boundary__substantive_employment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(employment_boundary__substantive_employment_reading, 'b096aef4-ba94-4135-8e70-ae03f5175ae6').
narrative_ontology:cs_kernel_codification('b096aef4-ba94-4135-8e70-ae03f5175ae6', formalized).
narrative_ontology:cs_authority_grounding('b096aef4-ba94-4135-8e70-ae03f5175ae6', lineage).
narrative_ontology:cs_interpretation_layer_present('b096aef4-ba94-4135-8e70-ae03f5175ae6').
narrative_ontology:cs_reading_relation('b096aef4-ba94-4135-8e70-ae03f5175ae6', employment_boundary__formalist_employment_reading, coexists_with).
narrative_ontology:cs_reading_relation('b096aef4-ba94-4135-8e70-ae03f5175ae6', employment_boundary__hybrid_security_reading, influences).
narrative_ontology:cs_axiom('b096aef4-ba94-4135-8e70-ae03f5175ae6', foundational, economic_substance_over_legal_form).
narrative_ontology:cs_axiom_status(economic_substance_over_legal_form, holdable).
narrative_ontology:cs_axiom_grounding('b096aef4-ba94-4135-8e70-ae03f5175ae6', economic_substance_over_legal_form, conventional).
narrative_ontology:cs_axiom('b096aef4-ba94-4135-8e70-ae03f5175ae6', foundational, algorithmic_control_as_supervision).
narrative_ontology:cs_axiom_status(algorithmic_control_as_supervision, holdable).
narrative_ontology:cs_axiom_grounding('b096aef4-ba94-4135-8e70-ae03f5175ae6', algorithmic_control_as_supervision, empirically_contingent).
narrative_ontology:cs_reference_frame('b096aef4-ba94-4135-8e70-ae03f5175ae6', substantive_employment_framework).
narrative_ontology:cs_drift_state('b096aef4-ba94-4135-8e70-ae03f5175ae6', platform_economy_contested_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b096aef4-ba94-4135-8e70-ae03f5175ae6', '').
narrative_ontology:cs_kernel_id(employment_boundary__substantive_employment_reading, employment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(employment_boundary__substantive_employment_reading, platform_workers).
narrative_ontology:constraint_victim(employment_boundary__substantive_employment_reading, digital_labor_platforms).
narrative_ontology:constraint_vindicates(employment_boundary__substantive_employment_reading, substance_over_form_doctrine).
narrative_ontology:constraint_vindicates(employment_boundary__substantive_employment_reading, algorithmic_management_as_control).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Perform task-based labor mediated by digital platforms. Their work is allocated, monitored, and evaluated through algorithmic systems they do not control. Under the substantive employment reading, they gain employee status, minimum wage guarantees, social insurance coverage, and collective bargaining rights regardless of contractual labels. Their economic dependence on platform income and the platform's control over work process are the criteria triggering classification.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, platform_workers, beneficiary,
    powerless, biographical, constrained, national).

% Operate two-sided digital markets connecting workers to end-users. Under this reading, they must reclassify workers as employees and assume full employer obligations including social insurance contributions, minimum wage top-ups, job security provisions, and compliance costs. They resist through strategic litigation, regulatory lobbying, and business model restructuring because the transfer substantially increases labor costs and reduces operational flexibility.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, digital_labor_platforms, payer,
    powerful, biographical, constrained, global).

% Legislate and adjudicate the substantive employment test, evaluating economic dependence indicators and algorithmic control mechanisms to determine employment status. They design inspection protocols, assess misclassification penalties, and set precedents that expand or contract the boundary's reach. Their enforcement capacity and legal interpretation determine whether the constraint operates as protective coordination or symbolic declaration.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, state_labor_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Argue that flexible independent contracting benefits workers and consumers and that substantive reclassification destroys viable business models. They are structurally excluded from standard-setting processes dominated by state regulators and incumbent platform negotiations, and their preferred outcomeâcontractual freedomâis overridden by the substantive test.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, gig_economy_advocates, excluded,
    moderate, biographical, constrained, national).

% Observe the expansion of the employment boundary because precedents established for platform workers may cascade to franchising, subcontracting, and other contingent work arrangements. They have not been directly reclassified but face precedent risk that could extend employer obligations throughout supply chains.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, traditional_employers, observer,
    organized, biographical, mobile, national).

% Study the employment boundary's effects on labor market structure, welfare distribution, and platform viability. They provide empirical evidence on economic dependence, algorithmic management intensity, and the incidence of social insurance costs, informing adjudication without directly benefiting from or paying for the constraint.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, labor_economists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(employment_boundary__substantive_employment_reading, platform_workers).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the collective-action problem of social insurance funding and worker protection in platform labor markets by preventing regulatory arbitrage through contractual labeling, ensuring that economically dependent workers are captured by protective frameworks regardless of contractual form.
% TRANSFER_FUNCTION: Moves the cost of social insurance contributions, minimum wage guarantees, job security obligations, and compliance overhead from workers and unfunded public systems to digital labor platforms.
% ABSENT_VOICES: Gig economy advocates arguing for contractual flexibility and workers who prefer independent contractor status are structurally underrepresented in enforcement design. Small platforms with thin margins lack voice in standard-setting dominated by large incumbents and state authorities.
% DISAPPEARANCE_RATIONALE: If the substantive employment test vanished overnight, platforms would reclassify workers as independent contractors, social insurance contributions would drop from the gig economy, worker protections would retract to contractual labels, and the regulatory architecture of labor protection would unravel in the platform sector.
% FOUNDING_PROBLEM: The formalist employment boundary failed to cover platform workers who were economically dependent and algorithmically controlled but contractually labeled independent, leaving them without social insurance, minimum wage protection, or collective bargaining rights while platforms externalized labor costs to public systems.
% FOUNDING_PROBLEM_CORROBORATION: Labor unions and academic researchers attest that platform workers lack social insurance and earn below minimum wage without the substantive reading. Platform industry associations contest the problem's severity, arguing workers prefer flexibility. Independent court findings in multiple jurisdictions have corroborated the precarity claim.
narrative_ontology:disappearance_verdict(employment_boundary__substantive_employment_reading, world_rearranges).
narrative_ontology:founding_problem_status(employment_boundary__substantive_employment_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(employment_boundary__substantive_employment_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(employment_boundary__substantive_employment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(employment_boundary__substantive_employment_reading, 0.55, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is moderate (0.55) because the constraint imposes substantial social insurance and wage obligations on platforms but does not extract monopoly rentâits transfer funds genuine worker protections. Suppression is moderate (0.45) because enforcement requires state inspection and penalty systems, but platforms can partially evade through restructuring and jurisdictional arbitrage. Theater_ratio is moderate-low (0.32) because performative compliance exists (platforms adopting cosmetic changes to avoid classification) but real economic transfers do occur where enforced. Accessibility_collapse is moderate (0.35) because the formalist and hybrid readings remain live alternatives in other jurisdictions and policy discourse. Resistance is substantial (0.65) because platforms mount sustained legal and political opposition to reclassification. The temporal series shows extractiveness and suppression rising together as enforcement machinery matures from 2015 to 2035, while theater_ratio grows as platforms develop sophisticated avoidance strategies.
 *
 * PERSPECTIVAL GAP:
 *   From the platform_workers' seat, the constraint is protective coordination that corrects a power imbalance. From the digital_labor_platforms' seat, it is regulatory extraction that imposes inflexible costs on a dynamic business model. From the state_labor_authorities' seat, it is the enforcement of a protective legal tradition adapting to new work forms. The engine computes these divergent classifications from the same structural data: beneficiaries with constrained exit and low power versus powerful payers with constrained but resourceful exit.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform_workers are declared beneficiaries because they receive employment protections, minimum wage guarantees, and social insurance coverageâflows that structurally subsidize their position (low d, damped Ï). Digital_labor_platforms are declared victims because they bear the full cost of employer obligations without receiving compensatory transfers from the constraint (high d, amplified Ï). State_labor_authorities administer the constraint but neither collect from it nor pay its costs; they are agenda_setters with analytical exit. The automatic derivation produces the correct directionalities without override.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not a rope because the transfer is asymmetric: platforms pay, workers receive, and the platform resistance demonstrates the cost is not voluntarily borne. It is not a snare because the coordination function is genuine and structurally separableâworker precarity under the formalist reading is a documented market failure that social insurance systems are designed to correct. The risk of mandatrophy arises if the substantive test becomes so rigid that it forces platform withdrawal, converting a protective coordination mechanism into a destructive extraction device. The temporal measurements track this risk: if base_extractiveness rises while theater_ratio remains high, the constraint is capturing without coordinating.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This constraint is the substantive_employment_reading of the employment_boundary kernel. How would the victim-beneficiary structure invert under the formalist_employment_reading, and would the hybrid_security_reading produce a distinct extraction profile?',
    'Comparative jurisdictional analysis mapping classification regime to worker welfare outcomes and platform cost structures across sibling readings.',
    'If the formalist reading produces superior worker welfare, this reading''s extraction is unjustified; if the hybrid reading resolves precarity with lower platform exit, this reading''s binary classification may be overreaching.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Structural uncertainty about which kernel reading correctly models the employment boundary.').

omega_variable(
    substantive_reclassification_efficacy,
    'Does substantive reclassification actually resolve platform worker precarity, or does it persist through enforcement gaps, hour reductions, and platform market withdrawal?',
    'Longitudinal income and coverage studies in jurisdictions with mature substantive employment tests for platform workers.',
    'If precarity persists despite reclassification, the worker beneficiary seat may be illusoryâextraction from platforms would then flow partially to state bureaucracy rather than worker welfare, shifting classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substantive_reclassification_efficacy, empirical, 'Whether the constraint''s coordination function delivers its promised protections.').

omega_variable(
    algorithmic_control_falsifiability,
    'Is algorithmic control an empirically robust criterion for employment classification, or does its measurement rely on contested definitions of control that vary across platforms?',
    'Standardized audit protocols measuring algorithmic scheduling, rating, and pricing autonomy across major platforms.',
    'If algorithmic control cannot be measured consistently, the constraint''s enforcement becomes arbitrary, increasing theater_ratio and suppressing legitimate business models; if robust, the substantive reading is vindicated as a genuine coordination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_control_falsifiability, empirical, 'Empirical stability of the algorithmic control criterion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(employment_boundary__substantive_employment_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(empl_tr_t0, employment_boundary__substantive_employment_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(empl_tr_t5, employment_boundary__substantive_employment_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement(empl_tr_t10, employment_boundary__substantive_employment_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(empl_tr_t15, employment_boundary__substantive_employment_reading, theater_ratio, 15, 0.3).
narrative_ontology:measurement(empl_tr_t20, employment_boundary__substantive_employment_reading, theater_ratio, 20, 0.32).

% Extraction over time
narrative_ontology:measurement(empl_be_t0, employment_boundary__substantive_employment_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(empl_be_t5, employment_boundary__substantive_employment_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(empl_be_t10, employment_boundary__substantive_employment_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(empl_be_t15, employment_boundary__substantive_employment_reading, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(empl_be_t20, employment_boundary__substantive_employment_reading, base_extractiveness, 20, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(empl_su_t0, employment_boundary__substantive_employment_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(empl_su_t5, employment_boundary__substantive_employment_reading, suppression_requirement, 5, 0.35).
narrative_ontology:measurement(empl_su_t10, employment_boundary__substantive_employment_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(empl_su_t15, employment_boundary__substantive_employment_reading, suppression_requirement, 15, 0.52).
narrative_ontology:measurement(empl_su_t20, employment_boundary__substantive_employment_reading, suppression_requirement, 20, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(employment_boundary__substantive_employment_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(employment_boundary__substantive_employment_reading, formalist_employment_reading).
narrative_ontology:affects_constraint(employment_boundary__substantive_employment_reading, hybrid_security_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the employment_boundary kernel, decomposed from the natural-language concept of 'employment status' into three structurally distinct claims per the Îµ-invariance principle. The substantive reading (this file) asserts that economic dependence and algorithmic control determine employment status. The formalist reading holds that contractual form and direct supervision are dispositive. The hybrid reading proposes a third protective category. Each reading has distinct beneficiary/victim structures and Îµ profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
