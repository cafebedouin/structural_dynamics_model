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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: software_control_legitimacy__pragmatic_openness_reading
 *   human_readable: Pragmatic Openness Reading of Software Control Legitimacy
 *   domain: software_engineering/political_economy_of_technology/intellectual_property
 *
 * SUMMARY:
 *   This constraint represents the 'pragmatic openness' reading of software
 *   control legitimacy, which views both open source and proprietary
 *   development models as legitimate methodological choices. It emphasizes
 *   that open source often produces better software through peer review and
 *   collaboration, but acknowledges that proprietary models are valid
 *   alternatives. The core of this reading is a focus on practical outcomes
 *   and quality optimization, rather than ideological purity or absolute
 *   rights. It seeks to reduce conflict and foster collaboration across
 *   different licensing models.
 *
 * KEY AGENTS:
 *   - Developers: Primary beneficiaries, gain from choice and cross-pollination.
 *   - Users: Primary beneficiaries, gain from higher quality software.
 *   - Open Source Advocates: Agenda-setters, promote open source pragmatically.
 *   - Proprietary Software Companies: Agenda-setters, operate within this framework.
 *   - Software Engineers: Observers, practice both methodologies.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__pragmatic_openness_reading, 0.15).
domain_priors:suppression_score(software_control_legitimacy__pragmatic_openness_reading, 0.1).
domain_priors:theater_ratio(software_control_legitimacy__pragmatic_openness_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__pragmatic_openness_reading, rope).
narrative_ontology:human_readable(software_control_legitimacy__pragmatic_openness_reading, "Pragmatic Openness Reading of Software Control Legitimacy").
narrative_ontology:topic_domain(software_control_legitimacy__pragmatic_openness_reading, "software_engineering/political_economy_of_technology/intellectual_property").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__pragmatic_openness_reading, 'b2ae11b9-230e-4aa1-a599-62835a493e73').
narrative_ontology:cs_kernel_codification('b2ae11b9-230e-4aa1-a599-62835a493e73', distributed).
narrative_ontology:cs_authority_grounding('b2ae11b9-230e-4aa1-a599-62835a493e73', diffuse_epistemic).
narrative_ontology:cs_reading_relation('b2ae11b9-230e-4aa1-a599-62835a493e73', software_control_legitimacy__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('b2ae11b9-230e-4aa1-a599-62835a493e73', software_control_legitimacy__property_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('b2ae11b9-230e-4aa1-a599-62835a493e73', software_control_legitimacy__commons_reading, coexists_with).
narrative_ontology:cs_axiom('b2ae11b9-230e-4aa1-a599-62835a493e73', foundational, methodological_pluralism_optimizes_quality).
narrative_ontology:cs_axiom_status(methodological_pluralism_optimizes_quality, holdable).
narrative_ontology:cs_axiom_grounding('b2ae11b9-230e-4aa1-a599-62835a493e73', methodological_pluralism_optimizes_quality, instrumental).
narrative_ontology:cs_axiom('b2ae11b9-230e-4aa1-a599-62835a493e73', foundational, proprietary_and_open_source_are_legitimate).
narrative_ontology:cs_axiom_status(proprietary_and_open_source_are_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('b2ae11b9-230e-4aa1-a599-62835a493e73', proprietary_and_open_source_are_legitimate, conventional).
narrative_ontology:cs_reference_frame('b2ae11b9-230e-4aa1-a599-62835a493e73', quality_driven_coexistence).
narrative_ontology:cs_drift_state('b2ae11b9-230e-4aa1-a599-62835a493e73', contemporary_software_ecosystem, gap(stable, minor, true)).
narrative_ontology:cs_created_at('b2ae11b9-230e-4aa1-a599-62835a493e73', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__pragmatic_openness_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, developers).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the recognition of diverse development models, allowing them to choose the best approach for a given project without ideological pressure. They gain from cross-pollination of ideas and tools.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, developers, beneficiary,
    organized, biographical, mobile, global).

% Benefit from higher quality software, regardless of its licensing model, as developers are encouraged to focus on practical outcomes and leverage the strengths of both open and proprietary approaches.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, users, beneficiary,
    organized, biographical, mobile, global).

% Promote open source as a superior methodology for quality and collaboration, but pragmatically accept proprietary models as legitimate alternatives, focusing on practical benefits rather than ideological purity.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, open_source_advocates, agenda_setter,
    organized, generational, mobile, global).

% Develop and sell proprietary software, benefiting from the recognition of their model's legitimacy. They engage with open source where it offers practical advantages, such as leveraging community contributions or using open standards.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, proprietary_software_companies, agenda_setter,
    institutional, generational, mobile, global).

% As practitioners, they observe and implement both open source and proprietary methodologies, valuing the flexibility and practical benefits of choosing the right tool for the job rather than adhering to a single ideology.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, software_engineers, observer,
    moderate, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_control_legitimacy__pragmatic_openness_reading, diffuse).
narrative_ontology:fixing_cost_class(software_control_legitimacy__pragmatic_openness_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the recognition of diverse software development methodologies (open source and proprietary) as legitimate, fostering a focus on quality, practical outcomes, and reducing ideological conflict within the software ecosystem.
% TRANSFER_FUNCTION: Facilitates the transfer of knowledge, best practices, and innovation across different development models, ultimately benefiting users through higher quality and more diverse software solutions.
% ABSENT_VOICES: This reading aims to be inclusive, so there are no structurally absent voices. While some might hold more extreme ideological positions, this pragmatic view seeks to integrate rather than exclude.
% DISAPPEARANCE_RATIONALE: If this pragmatic view disappeared, the software ecosystem would likely revert to more polarized, ideological conflicts between open source and proprietary camps. This would hinder collaboration, reduce cross-pollination of ideas, and potentially lead to a less innovative and higher-friction development landscape, forcing developers and users to make rigid 'either/or' choices.
% FOUNDING_PROBLEM: The historical tension and ideological conflict between open source and proprietary software development models, often framed as an 'either/or' choice, leading to fragmentation and missed opportunities for collaboration.
% FOUNDING_PROBLEM_CORROBORATION: Industry analysts, cross-platform developers, and academic studies in software engineering consistently highlight the benefits of both models and the ongoing need for pragmatic coexistence to drive innovation and quality. This perspective is widely attested by those outside the most ideologically committed groups.
narrative_ontology:disappearance_verdict(software_control_legitimacy__pragmatic_openness_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__pragmatic_openness_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__pragmatic_openness_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The metrics reflect a low-extraction, low-suppression 'Rope' classification. Extractiveness is low (0.15) because this reading does not impose costs or extract rents; instead, it promotes a framework for coexistence that benefits all. Suppression is low (0.10) as it actively legitimizes alternatives rather than suppressing them. Theater ratio is minimal (0.05) because the constraint is genuinely about practical methodology and quality, not performative maintenance of a defunct function. Accessibility collapse and resistance are low because the reading acknowledges and supports multiple viable paths.
 *
 * PERSPECTIVAL GAP:
 *   This reading inherently aims to bridge perspectival gaps by legitimizing diverse approaches. Therefore, significant divergence in perceived constraint type across stakeholders is not expected. All parties, even those with strong preferences for one model, benefit from the reduced ideological friction and increased focus on quality that this pragmatic view promotes.
 *
 * DIRECTIONALITY LOGIC:
 *   Developers and users are clear beneficiaries (d=0.0-0.2) as they gain from the flexibility, choice, and quality improvements fostered by this pragmatic approach. There are no identifiable victims (d=1.0) because the reading explicitly legitimizes both open source and proprietary models, ensuring no party is structurally targeted for extraction. Agenda-setters (advocates and companies) benefit from a more stable and less conflict-ridden ecosystem, allowing them to pursue their goals more effectively.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy. Its mandate is to foster pragmatic coexistence and quality optimization, which remains a live and relevant problem in the software industry. The constraint's function is actively pursued and its benefits are continuously realized, preventing any atrophy of its core purpose.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pragmatism_vs_ideology_drift,
    'Will the pragmatic acceptance of both open source and proprietary models hold, or will renewed ideological pressures push the debate back to an ''either/or'' framing?',
    'Longitudinal analysis of industry discourse, policy debates, and developer community sentiment. Observe whether calls for absolute freedom or absolute property rights gain significant traction over pragmatic coexistence.',
    'If ideological pressures increase, the constraint''s extractiveness and suppression could rise (e.g., if one model attempts to delegitimize or suppress the other), potentially shifting its classification towards a Tangled Rope or Snare. The benefits of collaboration and quality optimization would diminish.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pragmatism_vs_ideology_drift, empirical, 'The stability of pragmatic coexistence against ideological polarization in software development.').

omega_variable(
    quality_metric_objectivity,
    'Are the ''better software'' claims (e.g., via peer review) objectively measurable and consistently attributable to open source, or are they subject to selection bias and specific project contexts?',
    'Systematic meta-analysis of software engineering studies comparing quality metrics (e.g., defect rates, security vulnerabilities) across open source and proprietary projects, controlling for project size, complexity, and team structure.',
    'If quality claims are found to be highly context-dependent or biased, the ''instrumental'' grounding of the foundational axiom (''methodological_pluralism_optimizes_quality'') would weaken, potentially reducing the perceived legitimacy of this reading''s core argument and making it more vulnerable to ideological challenges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quality_metric_objectivity, empirical, 'The empirical grounding of quality claims for open source software.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__pragmatic_openness_reading, 2000, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t2000, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 2000, 0.08).
narrative_ontology:measurement(soft_tr_t2005, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 2005, 0.06).
narrative_ontology:measurement(soft_tr_t2010, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 2010, 0.05).
narrative_ontology:measurement(soft_tr_t2015, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 2015, 0.04).
narrative_ontology:measurement(soft_tr_t2020, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 2020, 0.04).
narrative_ontology:measurement(soft_tr_t2025, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 2025, 0.05).

% Extraction over time
narrative_ontology:measurement(soft_be_t2000, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 2000, 0.2).
narrative_ontology:measurement(soft_be_t2005, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 2005, 0.18).
narrative_ontology:measurement(soft_be_t2010, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 2010, 0.16).
narrative_ontology:measurement(soft_be_t2015, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 2015, 0.15).
narrative_ontology:measurement(soft_be_t2020, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 2020, 0.14).
narrative_ontology:measurement(soft_be_t2025, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 2025, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t2000, software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 2000, 0.15).
narrative_ontology:measurement(soft_su_t2005, software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 2005, 0.12).
narrative_ontology:measurement(soft_su_t2010, software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 2010, 0.1).
narrative_ontology:measurement(soft_su_t2015, software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 2015, 0.09).
narrative_ontology:measurement(soft_su_t2020, software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 2020, 0.09).
narrative_ontology:measurement(soft_su_t2025, software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 2025, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__pragmatic_openness_reading, information_standard).
narrative_ontology:affects_constraint(software_control_legitimacy__pragmatic_openness_reading, software_control_legitimacy__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__pragmatic_openness_reading, software_control_legitimacy__property_rights_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__pragmatic_openness_reading, software_control_legitimacy__commons_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the 'software_control_legitimacy' kernel. Each reading represents a distinct structural claim about the nature of software control, with different beneficiaries, victims, and metric profiles. They are linked to show their interrelationship within the broader debate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
