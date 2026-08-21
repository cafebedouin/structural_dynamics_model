% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__pragmatic_openness_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_control_legitimacy__pragmatic_openness_reading, []).

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
 *   constraint_id: software_control_legitimacy__pragmatic_openness_reading
 *   human_readable: Software Control Legitimacy: Pragmatic Openness Reading
 *   domain: software_engineering/political_economy_of_technology/intellectual_property
 *
 * SUMMARY:
 *   This constraint represents the 'pragmatic openness' reading of software
 *   control legitimacy, which views both open-source and proprietary
 *   development models as legitimate choices, each with its own merits. It
 *   emphasizes that open source often produces better software through peer
 *   review and collaboration, but acknowledges proprietary models as valid
 *   alternatives for commercial sustainability and specific use cases. This
 *   reading aims to foster a diverse and innovative software ecosystem rather
 *   than an ideologically pure one. It is a 'rope' because it facilitates
 *   coordination and benefits all parties by reducing ideological conflict
 *   and promoting choice.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__pragmatic_openness_reading, 0.15).
domain_priors:suppression_score(software_control_legitimacy__pragmatic_openness_reading, 0.05).
domain_priors:theater_ratio(software_control_legitimacy__pragmatic_openness_reading, 0.02).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 0.02).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, accessibility_collapse, 0.1).
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__pragmatic_openness_reading, rope).
narrative_ontology:human_readable(software_control_legitimacy__pragmatic_openness_reading, "Software Control Legitimacy: Pragmatic Openness Reading").
narrative_ontology:topic_domain(software_control_legitimacy__pragmatic_openness_reading, "software_engineering/political_economy_of_technology/intellectual_property").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__pragmatic_openness_reading, 'acc1f410-ec4f-4692-ab2e-2694ed0ba5a8').
narrative_ontology:cs_kernel_codification('acc1f410-ec4f-4692-ab2e-2694ed0ba5a8', distributed).
narrative_ontology:cs_authority_grounding('acc1f410-ec4f-4692-ab2e-2694ed0ba5a8', practice).
narrative_ontology:cs_interpretation_layer_present('acc1f410-ec4f-4692-ab2e-2694ed0ba5a8').
narrative_ontology:cs_reading_relation('acc1f410-ec4f-4692-ab2e-2694ed0ba5a8', software_control_legitimacy__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('acc1f410-ec4f-4692-ab2e-2694ed0ba5a8', software_control_legitimacy__property_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('acc1f410-ec4f-4692-ab2e-2694ed0ba5a8', software_control_legitimacy__commons_reading, coexists_with).
narrative_ontology:cs_axiom('acc1f410-ec4f-4692-ab2e-2694ed0ba5a8', foundational, methodological_pluralism_optimizes_quality).
narrative_ontology:cs_axiom_status(methodological_pluralism_optimizes_quality, holdable).
narrative_ontology:cs_axiom_grounding('acc1f410-ec4f-4692-ab2e-2694ed0ba5a8', methodological_pluralism_optimizes_quality, instrumental).
narrative_ontology:cs_axiom('acc1f410-ec4f-4692-ab2e-2694ed0ba5a8', foundational, commercial_sustainability_is_a_legitimate_goal).
narrative_ontology:cs_axiom_status(commercial_sustainability_is_a_legitimate_goal, holdable).
narrative_ontology:cs_axiom_grounding('acc1f410-ec4f-4692-ab2e-2694ed0ba5a8', commercial_sustainability_is_a_legitimate_goal, conventional).
narrative_ontology:cs_reference_frame('acc1f410-ec4f-4692-ab2e-2694ed0ba5a8', diverse_software_ecosystem).
narrative_ontology:cs_drift_state('acc1f410-ec4f-4692-ab2e-2694ed0ba5a8', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('acc1f410-ec4f-4692-ab2e-2694ed0ba5a8', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__pragmatic_openness_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, software_developers).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, software_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the flexibility to choose between open-source and proprietary models based on project needs and business goals, optimizing for quality, collaboration, or commercial viability. They are not forced into one model but can leverage the strengths of both.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, software_developers, beneficiary,
    organized, biographical, mobile, global).

% Benefit from a diverse software ecosystem that offers both high-quality open-source solutions and commercially supported proprietary products. They can choose software based on features, support, and cost, rather than being restricted by a single ideological stance.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, software_users, beneficiary,
    moderate, biographical, constrained, global).

% Observe and promote the benefits of open source, but acknowledge the legitimacy of proprietary alternatives as a pragmatic reality. They focus on demonstrating the superior quality and collaborative advantages of open source rather than condemning proprietary models outright.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, open_source_advocates, observer,
    organized, generational, analytical, global).

% Operate within the acknowledged legitimacy of their business model, competing on features, support, and market value. They are not challenged on the fundamental right to exist, but on the merits of their products and practices.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, proprietary_software_companies, observer,
    institutional, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the software development ecosystem by establishing a shared understanding that both open-source and proprietary models are legitimate and can coexist, allowing for diverse innovation and market competition.
% TRANSFER_FUNCTION: Facilitates the transfer of knowledge and best practices across different development models, and allows for the free flow of talent and investment between open and closed projects, optimizing overall software quality and utility for users.
% ABSENT_VOICES: Hardline ideological advocates for either absolute software freedom or absolute property rights might object, as this reading prioritizes pragmatic coexistence over their exclusive claims. They are not absent from the broader debate, but their absolutist positions are not central to this pragmatic framing.
% DISAPPEARANCE_RATIONALE: If this pragmatic understanding vanished, the software world would polarize into ideological camps, leading to increased conflict, reduced collaboration, and potentially stifled innovation as each side sought to delegitimize the other. The current diverse ecosystem would be severely disrupted.
% FOUNDING_PROBLEM: The early software industry faced a fundamental tension between the desire for free sharing and the need for commercial sustainability, leading to ideological battles over software control.
% FOUNDING_PROBLEM_CORROBORATION: Industry analysts and academic researchers outside of either the open-source or proprietary camps corroborate that the tension between sharing and commercialization remains a live issue, requiring ongoing pragmatic solutions for a healthy ecosystem.
narrative_ontology:disappearance_verdict(software_control_legitimacy__pragmatic_openness_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__pragmatic_openness_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__pragmatic_openness_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(software_control_legitimacy__pragmatic_openness_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__pragmatic_openness_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_control_legitimacy__pragmatic_openness_reading_tests).
:- end_tests(software_control_legitimacy__pragmatic_openness_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low because this reading does not impose a single model or extract rents; it merely legitimizes coexistence. Suppression is very low as it actively discourages coercive enforcement of one model over another. Theater ratio is negligible as the constraint's function is genuinely about fostering a balanced ecosystem. Accessibility collapse is low because it explicitly supports multiple alternatives. Resistance is low because it is a widely accepted, pragmatic stance within the industry.
 *
 * PERSPECTIVAL GAP:
 *   This reading itself represents a 'middle ground' perspective, so significant perspectival gaps are less pronounced than in more ideologically charged readings. The primary 'gap' is between this pragmatic view and the more absolutist positions, which would experience this constraint as a dilution of their core principles.
 *
 * DIRECTIONALITY LOGIC:
 *   Both software developers and users are beneficiaries, as they gain from the flexibility and diversity this reading promotes. There are no direct victims, as no party is structurally disadvantaged by the acceptance of multiple legitimate models. Hardline advocates for either extreme (absolute freedom or absolute property rights) might be considered 'excluded' in the sense that their absolutist views are not centered, but they are not victims of extraction.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    boundary_of_legitimacy,
    'At what point does a proprietary model''s practices (e.g., anti-repair, vendor lock-in) cross the line from ''legitimate alternative'' to ''harmful extraction'' within this pragmatic framework?',
    'Ongoing legal challenges, regulatory interventions, and shifts in public perception regarding fair competition and consumer rights. The boundary is empirically contested and shifts over time.',
    'If the line is crossed, specific proprietary practices would be reclassified as extractive snares, requiring active intervention rather than mere coexistence. This would increase the effective extractiveness of the overall software ecosystem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_of_legitimacy, empirical, 'The dynamic boundary between legitimate proprietary practices and harmful extraction.').

omega_variable(
    balance_of_innovation,
    'Does the pragmatic coexistence of models truly optimize overall software quality and innovation, or does one model (e.g., open source) consistently outperform the other in key areas, suggesting a need for re-evaluation of ''legitimacy''?',
    'Longitudinal studies comparing innovation rates, security vulnerabilities, and adoption across open-source and proprietary ecosystems, disaggregated by software category and market segment.',
    'If one model consistently demonstrates superior outcomes, the ''legitimacy'' of the other might be conceptually challenged, leading to policy shifts favoring the demonstrably better model and potentially increasing resistance from the disadvantaged side.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balance_of_innovation, empirical, 'Whether the pragmatic balance truly optimizes innovation or if one model is inherently superior.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__pragmatic_openness_reading, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t1990, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 1990, 0.05).
narrative_ontology:measurement(soft_tr_t2000, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 2000, 0.03).
narrative_ontology:measurement(soft_tr_t2010, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 2010, 0.02).
narrative_ontology:measurement(soft_tr_t2024, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 2024, 0.02).

% Extraction over time
narrative_ontology:measurement(soft_be_t1990, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 1990, 0.2).
narrative_ontology:measurement(soft_be_t2000, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 2000, 0.18).
narrative_ontology:measurement(soft_be_t2010, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 2010, 0.16).
narrative_ontology:measurement(soft_be_t2024, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t1990, software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 1990, 0.1).
narrative_ontology:measurement(soft_su_t2000, software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 2000, 0.08).
narrative_ontology:measurement(soft_su_t2010, software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 2010, 0.06).
narrative_ontology:measurement(soft_su_t2024, software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 2024, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__pragmatic_openness_reading, information_standard).
narrative_ontology:affects_constraint(software_control_legitimacy__pragmatic_openness_reading, software_control_legitimacy__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__pragmatic_openness_reading, software_control_legitimacy__property_rights_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__pragmatic_openness_reading, software_control_legitimacy__commons_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the 'software_control_legitimacy' kernel. This 'pragmatic openness' reading emphasizes coexistence and quality optimization, influencing the operating environment for the other, more ideologically driven readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
