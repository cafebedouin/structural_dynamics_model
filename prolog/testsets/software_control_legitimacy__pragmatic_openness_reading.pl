% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__pragmatic_openness_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: software_control_legitimacy__pragmatic_openness_reading
 *   human_readable: Software Control as Pragmatic Development Methodology (Openness Reading)
 *   domain: technology/intellectual_property/software_engineering
 *
 * SUMMARY:
 *   The pragmatic openness reading frames software control as a development
 *   methodology choice where both open peer review and proprietary models are
 *   legitimate alternatives. Open source produces measurably better software
 *   through collaboration and transparency; proprietary models deliver
 *   sustainable funding and vendor accountability. This reading rejects
 *   framing software control as a zero-sum property vs. freedom contest.
 *   Instead, it accepts coexistence and mutual benefit. The constraint's low
 *   extractiveness (0.22) reflects the absence of a victim set — neither
 *   model is treated as parasitic on the other. This is deliberately one
 *   reading of a contested kernel; sibling readings (freedom imperative,
 *   property rights, commons governance) instantiate different ε values,
 *   beneficiary structures, and classification outcomes from the same domain.
 *
 * KEY AGENTS:
 *   - open_source_developers: organize around peer review, gain reputation and skill, benefit from transparency constraint
 *   - proprietary_software_developers: choose vendor models, gain investment protection and licensing revenue, coexist as legitimate alternative
 *   - end_users (both models): gain quality via competing methodologies, benefit from ecosystem diversity
 *   - freedom_imperative_advocates: excluded because they reject proprietary legitimacy; would argue this reading ignores ethical foundations
 *   - commons_governance_advocates: excluded because they reject property/freedom framing; would argue this reading misses shared infrastructure questions
 *   - software_engineering_research_community: provides external corroboration for quality claims and founding-problem status
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__pragmatic_openness_reading, 0.22).
domain_priors:suppression_score(software_control_legitimacy__pragmatic_openness_reading, 0.15).
domain_priors:theater_ratio(software_control_legitimacy__pragmatic_openness_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__pragmatic_openness_reading, rope).
narrative_ontology:human_readable(software_control_legitimacy__pragmatic_openness_reading, "Software Control as Pragmatic Development Methodology (Openness Reading)").
narrative_ontology:topic_domain(software_control_legitimacy__pragmatic_openness_reading, "technology/intellectual_property/software_engineering").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__pragmatic_openness_reading, '30154624-0b47-4956-b817-2e41fcff0ae1').
narrative_ontology:cs_kernel_codification('30154624-0b47-4956-b817-2e41fcff0ae1', distributed).
narrative_ontology:cs_authority_grounding('30154624-0b47-4956-b817-2e41fcff0ae1', distributed).
narrative_ontology:cs_reading_relation('30154624-0b47-4956-b817-2e41fcff0ae1', software_control_legitimacy__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('30154624-0b47-4956-b817-2e41fcff0ae1', software_control_legitimacy__property_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('30154624-0b47-4956-b817-2e41fcff0ae1', software_control_legitimacy__commons_reading, coexists_with).
narrative_ontology:cs_axiom('30154624-0b47-4956-b817-2e41fcff0ae1', foundational, software_control_is_methodology_choice).
narrative_ontology:cs_axiom_status(software_control_is_methodology_choice, holdable).
narrative_ontology:cs_axiom_grounding('30154624-0b47-4956-b817-2e41fcff0ae1', software_control_is_methodology_choice, instrumental).
narrative_ontology:cs_axiom('30154624-0b47-4956-b817-2e41fcff0ae1', foundational, both_open_and_proprietary_models_deliver_legitimate_benefits).
narrative_ontology:cs_axiom_status(both_open_and_proprietary_models_deliver_legitimate_benefits, holdable).
narrative_ontology:cs_axiom_grounding('30154624-0b47-4956-b817-2e41fcff0ae1', both_open_and_proprietary_models_deliver_legitimate_benefits, empirically_contingent).
narrative_ontology:cs_reference_frame('30154624-0b47-4956-b817-2e41fcff0ae1', coexistent_model_legitimacy).
narrative_ontology:cs_drift_state('30154624-0b47-4956-b817-2e41fcff0ae1', contemporary_cloud_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('30154624-0b47-4956-b817-2e41fcff0ae1', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__pragmatic_openness_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, software_developers).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, software_users).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, quality_optimized_ecosystem).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__pragmatic_openness_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(software_control_legitimacy__pragmatic_openness_reading, 'none', 1).

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
 *   Extractiveness is low (0.22) because the reading posits mutual benefit and no victim set — open developers get quality and reputation, proprietary developers get sustainability, users get choices. Suppression is low (0.15) because the constraint persists through voluntary participation in chosen models, not through coercion of a trapped actor. Theater is minimal (0.08) because both models deliver on their claimed functions: open projects do produce peer-reviewed code, proprietary vendors do deliver integrated systems with accountability. The measurement series spans 1985–2025 to capture the coevolution of open and proprietary dominance. The slight rise in extractiveness 1985–2015 reflects growing incumbent proprietary vendor market power (not a constraint property, but market concentration in the implementing industry), which stabilizes 2015–2025 as open infrastructure (cloud, containers, AI) redistributes power. Suppression rises modestly (1985–2015, flattens 2015–2025) reflecting intermittent regulatory examination of open-source commons licensing, not enforcement machinery against proprietary models.
 *
 * PERSPECTIVAL GAP:
 *   From a proprietary vendor's seat, this is genuine rope: voluntary adoption of their products, quality delivered as promised, fair commercial exchange. From a free-software advocate's seat, the same arrangement is false legitimacy masking user constraint — they would compute a different directionality (victim seat, not beneficiary). The engine computes per-seat types from beneficiary/victim declarations and structural data. The pragmatic reading declares both as beneficiaries (coexistence premise); the freedom reading would declare users as victims of proprietary constraint. This gap — the same arrangement yielding different d values from different readings — is the kernel contest.
 *
 * DIRECTIONALITY LOGIC:
 *   Open-source developers (d ≈ 0.2, slight beneficiary): they gain from the openness norm but retain full exit (can fork, can choose proprietary work, can switch communities). Proprietary developers (d ≈ 0.25, slight beneficiary): they gain from intellectual property protection but property rights are a domain-wide consensus, not extraction specific to this constraint. End users (d ≈ 0.4–0.5, near symmetric): they benefit from quality improvements and choice, but experience vendor lock-in costs from their chosen model. No actor sits at d > 0.7 (target) because the reading rejects victim classification. Quality_optimized_ecosystem (non-agent, vindicated proposition): benefits from both models without extraction because the proposition collects nothing.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved in this reading by the living founding problem: the dual pressure on sustainability (open needs funding, proprietary needs quality assurance) remains active. If the founding problem died (if, say, proprietary software achieved perfect peer-review integration and open source solved sustainable funding), the constraint would persist as zombie cover for property rights or freedom doctrine. The reading avoids this by grounding legitimacy in pragmatic quality outcomes, not in either doctrine. As long as measurable quality differences persist between methodologies and developers choose based on those differences, the founding problem stays live. The founding_problem_status is 'live' not 'contested' because the material engineering constraint (quality trade-offs between models) is observable, not interpretive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pragmatic_vs_ideological_framing,
    'Does grounding legitimacy in quality outcomes (pragmatic) adequately address concerns about user freedom and property rights that the excluded readings raise, or does it evade fundamental ethical questions?',
    'Examine cases where quality outcomes (open-source superiority) conflict with rights claims (proprietary property protection). If outcomes alone determine legitimacy, rights-based objections should carry no weight; if they do, the framing is incomplete.',
    'If pragmatic outcomes are insufficient and ethical/rights framing is necessary, the reading becomes untenable and one of the sibling readings is correct. If rights-based objections are empirically unfounded or merely ideological, the pragmatic framing stands. This is the core ambiguity of the reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pragmatic_vs_ideological_framing, conceptual, 'Whether pragmatic quality optimization is an adequate legitimacy ground independent of freedom and property-rights arguments.').

omega_variable(
    proprietary_quality_measurement,
    'How are quality improvements in proprietary software measured and verified when code is not open to independent peer review?',
    'Compare audit-certified quality metrics (from independent security firms, regulatory compliance reports, liability litigation) against open-source equivalents in the same domain. If proprietary metrics are consistently lower or unverifiable, the quality claim fails.',
    'If proprietary models cannot deliver measurable quality parity, the reading''s coexistence premise fails because one model demonstrably cannot achieve the founding problem''s stated solution. This would force reclassification to a constraint where proprietary legitimacy is derivative, not equal.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proprietary_quality_measurement, empirical, 'Whether proprietary software quality is measurably equivalent to open-source peer-reviewed quality.').

omega_variable(
    commons_invisibility_in_proprietary_models,
    'Does proprietary software use (or depend on) open-source commons infrastructure in ways that would be recognized as extraction by the commons-reading (sibling)?',
    'Audit proprietary projects for open-source dependencies, cloud infrastructure reliance, protocol standards that originated in commons projects. If substantial, the commons reading would argue the proprietary model is parasitic on undeclared commons.',
    'If proprietary models substantially depend on commons infrastructure without equivalent contribution back, the pragmatic reading''s claim of ''coexistence and mutual benefit'' is false from the commons perspective. This would support reclassification toward the commons reading and implicate hidden extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(commons_invisibility_in_proprietary_models, empirical, 'Whether proprietary models extract commons value without equivalent reciprocity.').

omega_variable(
    coexistence_vs_hidden_hierarchy,
    'Is the reading''s claim of coexistent models (''both legitimate'') masking a hidden hierarchy where one model (currently open) subsidizes the other (currently proprietary)?',
    'Track resource flows and infrastructure dependencies: does open-source development subsidize proprietary software through public funding, university research, or commons infrastructure that proprietary vendors use without contribution?',
    'If a hierarchy exists, the constraint is not coexistence (rope) but a tangled rope or snare where proprietary benefit from open subsidy without reciprocal obligation. The low ε would be false.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coexistence_vs_hidden_hierarchy, empirical, 'Whether claimed coexistence masks asymmetric subsidy or dependency.').

omega_variable(
    reading_vs_sibling_empirical_vulnerability,
    'Which reading is most empirically vulnerable to refutation by new evidence?',
    'The pragmatic reading rests on measurable quality differences. The freedom reading rests on rights claims (not empirically falsifiable). The property reading rests on incentive claims (partially empirical). The commons reading rests on shared-resource governance claims (partially empirical). Identify which empirical prediction, if falsified, would collapse which reading.',
    'A reading that rests on a single empirical hypothesis (pragmatic: quality differences) is more falsifiable than one grounded in rights doctrine. This asymmetry affects which reading should be treated as the null hypothesis versus alternative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_sibling_empirical_vulnerability, conceptual, 'Comparative falsifiability of the four kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__pragmatic_openness_reading, 1985, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t1985, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 1985, 0.05).
narrative_ontology:measurement(soft_tr_t1995, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 1995, 0.06).
narrative_ontology:measurement(soft_tr_t2005, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 2005, 0.08).
narrative_ontology:measurement(soft_tr_t2015, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 2015, 0.09).
narrative_ontology:measurement(soft_tr_t2025, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 2025, 0.08).

% Extraction over time
narrative_ontology:measurement(soft_be_t1985, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 1985, 0.18).
narrative_ontology:measurement(soft_be_t1995, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 1995, 0.19).
narrative_ontology:measurement(soft_be_t2005, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 2005, 0.21).
narrative_ontology:measurement(soft_be_t2015, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 2015, 0.23).
narrative_ontology:measurement(soft_be_t2025, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 2025, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t1985, software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 1985, 0.1).
narrative_ontology:measurement(soft_su_t1995, software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 1995, 0.12).
narrative_ontology:measurement(soft_su_t2005, software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 2005, 0.14).
narrative_ontology:measurement(soft_su_t2015, software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 2015, 0.15).
narrative_ontology:measurement(soft_su_t2025, software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 2025, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__pragmatic_openness_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(software_control_legitimacy__pragmatic_openness_reading, 0.12).
narrative_ontology:affects_constraint(software_control_legitimacy__pragmatic_openness_reading, software_control_legitimacy__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__pragmatic_openness_reading, software_control_legitimacy__property_rights_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__pragmatic_openness_reading, software_control_legitimacy__commons_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__pragmatic_openness_reading, open_source_sustainability_constraint).
narrative_ontology:affects_constraint(software_control_legitimacy__pragmatic_openness_reading, proprietary_software_quality_accountability).

% DUAL FORMULATION NOTE:
% The software_control_legitimacy kernel admits four distinct readings: pragmatic_openness_reading (this file), freedom_imperative_reading, property_rights_reading, and commons_reading. Each reading instantiates a different constraint with different ε, beneficiary/victim sets, and classification outcomes. The readings coexist in software-governance discourse and are held by different communities simultaneously. No single reading forecloses the others within the broader epistemic landscape, though each constrains what the others can say within specific institutional contexts (e.g., a court system grounded in property law would adopt the property reading; a free-software foundation would adopt the freedom reading). This file represents the pragmatic reading, which accepts coexistence and grounds legitimacy in measurable quality outcomes rather than rights doctrine or commons theory.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(software_control_legitimacy__pragmatic_openness_reading, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
