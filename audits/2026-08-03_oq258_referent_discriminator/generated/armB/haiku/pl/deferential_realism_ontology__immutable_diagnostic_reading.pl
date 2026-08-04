% ============================================================================
% CONSTRAINT STORY: deferential_realism_ontology__immutable_diagnostic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_deferential_realism_ontology__immutable_diagnostic_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: deferential_realism_ontology__immutable_diagnostic_reading
 *   human_readable: Deferential Realism Ontology—Immutable Diagnostic Reading
 *   domain: epistemology/normative_theory/institutional_design
 *
 * SUMMARY:
 *   The Deferential Realism constraint typology claims to provide an
 *   observational instrument for classifying institutional mechanisms based
 *   on fixed, discoverable properties (extractiveness, suppression,
 *   theater_ratio, accessibility_collapse, resistance). The immutable
 *   diagnostic reading instantiates this claim: mountains are physical
 *   invariants, snares are measurable extraction mechanisms, and
 *   classification disputes are errors correctable through better
 *   observation. This reading suppresses alternative framings that treat the
 *   typology as a normative vocabulary where 'snare' is declared rather than
 *   discovered. The story models the framework itself as a constraint—one
 *   whose persistence depends on active enforcement of metric reductionism
 *   and whose beneficiaries include institutional measurement communities and
 *   metric-fluent practitioners. The immutable reading creates suppression of
 *   non-metric-based classification approaches by treating them as
 *   non-scientific, which is itself a form of institutional extraction:
 *   authority over classification flows to those fluent in metrics, and away
 *   from traditions that resist metric reduction.
 *
 * KEY AGENTS:
 *   - observational_epistemology_practitioners — Institutional designers and researchers who set the framework's standards and enforce metric-based classification
 *   - institutional_measurement_communities — Regulatory bodies and audit frameworks that depend on metric objectivity claims
 *   - alternative_normative_frameworks — Normative ethics and critical theory approaches excluded from institutional authority
 *   - non_metric_based_classification_approaches — Qualitative, historical, and participatory knowledge traditions bearing the suppression cost
 *   - constraint_typology_developers — Original architects maintaining and refining the framework
 *   - policy_makers_dependent_on_classification — Powerful institutional actors benefiting from metric legitimation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_ontology__immutable_diagnostic_reading, 0.62).
domain_priors:suppression_score(deferential_realism_ontology__immutable_diagnostic_reading, 0.71).
domain_priors:theater_ratio(deferential_realism_ontology__immutable_diagnostic_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__immutable_diagnostic_reading, rope).
narrative_ontology:human_readable(deferential_realism_ontology__immutable_diagnostic_reading, "Deferential Realism Ontology—Immutable Diagnostic Reading").
narrative_ontology:topic_domain(deferential_realism_ontology__immutable_diagnostic_reading, "epistemology/normative_theory/institutional_design").

domain_priors:requires_active_enforcement(deferential_realism_ontology__immutable_diagnostic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__immutable_diagnostic_reading, '5b0892ef-0422-4b8c-bc99-e3795afe66f0').
narrative_ontology:cs_kernel_codification('5b0892ef-0422-4b8c-bc99-e3795afe66f0', formalized).
narrative_ontology:cs_authority_grounding('5b0892ef-0422-4b8c-bc99-e3795afe66f0', extraction).
narrative_ontology:cs_interpretation_layer_present('5b0892ef-0422-4b8c-bc99-e3795afe66f0').
narrative_ontology:cs_reading_relation('5b0892ef-0422-4b8c-bc99-e3795afe66f0', deferential_realism_ontology__hybrid_pragmatic_reading, influences).
narrative_ontology:cs_reading_relation('5b0892ef-0422-4b8c-bc99-e3795afe66f0', deferential_realism_ontology__rhetorical_scaffold_reading, coexists_with).
narrative_ontology:cs_axiom('5b0892ef-0422-4b8c-bc99-e3795afe66f0', foundational, epsilon_is_observable).
narrative_ontology:cs_axiom_status(epsilon_is_observable, holdable).
narrative_ontology:cs_axiom_grounding('5b0892ef-0422-4b8c-bc99-e3795afe66f0', epsilon_is_observable, empirically_contingent).
narrative_ontology:cs_axiom('5b0892ef-0422-4b8c-bc99-e3795afe66f0', foundational, metric_reduction_is_necessary).
narrative_ontology:cs_axiom_status(metric_reduction_is_necessary, holdable).
narrative_ontology:cs_axiom_grounding('5b0892ef-0422-4b8c-bc99-e3795afe66f0', metric_reduction_is_necessary, instrumental).
narrative_ontology:cs_axiom('5b0892ef-0422-4b8c-bc99-e3795afe66f0', secondary, classification_error_correctable_by_observation).
narrative_ontology:cs_axiom_status(classification_error_correctable_by_observation, holdable).
narrative_ontology:cs_axiom_grounding('5b0892ef-0422-4b8c-bc99-e3795afe66f0', classification_error_correctable_by_observation, empirically_contingent).
narrative_ontology:cs_reference_frame('5b0892ef-0422-4b8c-bc99-e3795afe66f0', metric_observational_ontology).
narrative_ontology:cs_drift_state('5b0892ef-0422-4b8c-bc99-e3795afe66f0', contemporary_critical_challenge_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5b0892ef-0422-4b8c-bc99-e3795afe66f0', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__immutable_diagnostic_reading, observational_epistemology_practitioners).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__immutable_diagnostic_reading, institutional_measurement_communities).
narrative_ontology:constraint_victim(deferential_realism_ontology__immutable_diagnostic_reading, alternative_normative_frameworks).
narrative_ontology:constraint_victim(deferential_realism_ontology__immutable_diagnostic_reading, non_metric_based_classification_approaches).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__immutable_diagnostic_reading, constraint_typology_developers).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__immutable_diagnostic_reading, policy_makers_dependent_on_classification).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__immutable_diagnostic_reading, observation_corrects_error).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__immutable_diagnostic_reading, fixed_referent_ontology).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__immutable_diagnostic_reading, metric_objectivity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Academics, institutional designers, and policy analysts who adopt the framework as their primary instrument for classifying constraints. They enforce it by publishing papers, training students, and requiring metric-based classification in institutional contexts. They collect authority and resource allocation from being the canonical interpreters of the constraint typology.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, observational_epistemology_practitioners, agenda_setter,
    institutional, generational, arbitrage, universal).

% Regulatory bodies, audit frameworks, and institutional review processes that depend on having objective, metric-based classification systems. The immutable reading stabilizes their authority by treating classification as a matter of correct observation rather than normative judgment. They benefit from the framework's claim that disagreement is resolvable by better measurement.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, institutional_measurement_communities, beneficiary,
    organized, generational, constrained, global).

% Normative ethical, critical theory, and pragmatist approaches to institutional analysis that classify constraints by legitimacy rather than metric patterns. They are systematically disadvantaged in institutional contexts where the immutable reading dominates because their classifications are treated as non-scientific opinion rather than discoverable fact. Their exit would require abandoning institutional voice altogether.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, alternative_normative_frameworks, payer,
    moderate, generational, constrained, global).

% Scholars and practitioners working in qualitative, historical, and participatory knowledge traditions that resist metric reduction. They are excluded from institutional classification work by the requirement that all constraint assessment be metric-grounded. The suppression is particularly effective because it is administered under the banner of objectivity rather than explicit exclusion.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, non_metric_based_classification_approaches, payer,
    powerless, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(deferential_realism_ontology__immutable_diagnostic_reading, non_metric_based_classification_approaches, excluded).

% The original architects and custodians of the framework (including this analysis). They maintain the immutable reading by continually refining metrics, publishing validation studies, and training new cohorts. They benefit from the framework's adoption but also bear responsibility for its enforcement and modification as empirical challenges emerge.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, constraint_typology_developers, agenda_setter,
    institutional, generational, mobile, universal).
narrative_ontology:stakeholder_secondary_role(deferential_realism_ontology__immutable_diagnostic_reading, constraint_typology_developers, beneficiary).

% Researchers and practitioners who accept the metric-observational core of the framework but contest specific applications and metric definitions. They work within the framework's epistemic standards to challenge particular classifications. They take no stable role as beneficiary or payer; their position is to test the framework's internal consistency.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, institutional_critics_empirically_focused, observer,
    moderate, biographical, mobile, global).

% Government agencies and institutional leaders who rely on the constraint typology to justify institutional design choices. The immutable reading provides them with a way to present policy decisions as discoveries about how institutions must work rather than as contested normative choices. They have constrained exit because abandoning metric-based classification would require different legitimation narratives.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, policy_makers_dependent_on_classification, beneficiary,
    powerful, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(deferential_realism_ontology__immutable_diagnostic_reading, institutional_measurement_communities).
narrative_ontology:fixing_cost_class(deferential_realism_ontology__immutable_diagnostic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared measurement language enabling researchers, policymakers, and institutional designers across different domains to compare constraint structures using a common metric vocabulary. Solves the coordination problem of comparing economic mechanisms, political systems, and social structures using incommensurable descriptive frameworks.
% TRANSFER_FUNCTION: Transfers authority over classification from contested normative debate to metric-based determination. Moves legitimacy from explicitly value-laden frameworks (normative ethics, critical theory, participatory knowledge) to allegedly value-neutral observational practice. Resources flow to institutional measurement communities and metric-fluent practitioners.
% ABSENT_VOICES: Critical theorists, historians of institutions, participatory and decolonial knowledge traditions, and scholars working in languages/intellectual traditions where metric formalization is not the standard epistemic practice are systematically excluded from classification authority by the requirement that all claims be expressible in metric form.
% DISAPPEARANCE_RATIONALE: If the immutable diagnostic reading and its enforcement apparatus disappeared overnight, institutional classification would revert to explicitly contested normative frameworks. Policy debates would foreground legitimacy questions rather than metric questions. Authority over constraint classification would redistribute toward critical, historical, and participatory knowledge practitioners. The metric-based authority structure that currently dominates institutional design would be replaced by frameworks that make their normative commitments visible.
% FOUNDING_PROBLEM: Early institutional analysis lacked a standardized, cross-domain vocabulary for comparing constraint structures. Researchers and policymakers using different disciplinary languages could not assess whether the same institutional problem appeared in different contexts. Classification was conducted ad-hoc, using inconsistent criteria, making systemic pattern recognition impossible.
% FOUNDING_PROBLEM_CORROBORATION: Institutional measurement communities and policy practitioners attest the problem remains live—metric classification remains essential for cross-domain institutional comparison. Critical theorists and alternative-framework scholars attest the founding problem was always a symptom of imposing metric reductionism on inherently normative institutional questions; the problem was solved by restricting the scope of questions metric frameworks can answer, not by expanding metrics to cover all classification. Historians of science document that metric standardization in other domains (medicine, engineering) solved genuine coordination problems AND created new forms of epistemic authority that excluded prior knowledge traditions.
narrative_ontology:disappearance_verdict(deferential_realism_ontology__immutable_diagnostic_reading, world_rearranges).
narrative_ontology:founding_problem_status(deferential_realism_ontology__immutable_diagnostic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferential_realism_ontology__immutable_diagnostic_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-08-03',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(deferential_realism_ontology__immutable_diagnostic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(deferential_realism_ontology__immutable_diagnostic_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deferential_realism_ontology__immutable_diagnostic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(deferential_realism_ontology__immutable_diagnostic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(deferential_realism_ontology__immutable_diagnostic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises over the interval (0.48→0.62) as metric-based classification becomes more institutionally embedded and the cost to alternative frameworks of being excluded from classification authority increases. Suppression is high (0.71) and rising because the framework excludes non-metric approaches by administrative rule (metric requirement) rather than by explicit normative disagreement—the exclusion is defended as a matter of objectivity, not power. Theater ratio is moderate (0.44) because the framework does genuine coordination work (enabling cross-domain institutional comparison) alongside extracting authority from alternative frameworks. Accessibility collapse is high (0.68) because once metric-based classification becomes institutional standard, the cost to exit for scholars working in excluded traditions becomes prohibitively high—they must either learn metrics or abandon institutional voice. Resistance is moderate (0.58) because critics can contest specific metric definitions while remaining within the framework's epistemic standards, limiting the strength of external resistance. Measurements use a shared grid at 8-point intervals so every metric is authored at every examined time.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of observational_epistemology_practitioners and institutional_measurement_communities, this constraint is genuine coordination enabling better institutional analysis—the engine should compute rope or weak tangled_rope. From the seat of alternative_normative_frameworks and non_metric_based_classification_approaches, the same structure is suppressive extraction of authority—the engine should compute snare or strong tangled_rope. The divergence arises from structural asymmetry in exit options: practitioners fluent in metrics can continue their work under the immutable reading, while scholars committed to non-metric approaches face identity lock (their entire epistemic tradition is excluded). Policy makers benefit from metric claims appearing as discoverable facts rather than contested choices. This is precisely the per-seat classification task the engine performs from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   observational_epistemology_practitioners and institutional_measurement_communities sit at low d (beneficiaries collecting authority); alternative_normative_frameworks and non_metric_based_classification_approaches sit at high d (targets losing authority and voice). The mechanism is suppression, not direct extraction of resources—the framework extracts institutional authority and legitimacy, not money. The constraint persists because its beneficiaries (metric-fluent practitioners, institutional measurement bodies) have both power and exit options, while its victims have constrained exit (identity locked in non-metric traditions, which the framework defines as non-institutional). Directionality for policy_makers_dependent_on_classification sits near symmetric but slightly toward beneficiary (d~0.40) because they benefit from having metric cover for their choices while also bearing some cost from being locked into metric justifications when non-metric analysis might be more honest.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was genuine: early institutional analysis lacked standardized vocabulary for cross-domain comparison. The immutable reading treats this as a solved problem—metrics now provide that vocabulary. But the solution simultaneously creates a new problem: metric reductionism excludes knowledge traditions and classification approaches that resist metric formalization. The founding coordination function (enabling cross-domain comparison) has been achieved, but the constraint persists and even intensifies its enforcement (measurement_requirement becoming tighter, theater_ratio rising as the cosmetic security review function grows). This is candidate mandatrophy: the founding problem is substantially dead (metric standardization IS achieved), the founding solution persists, and the persistent solution now functions primarily to suppress alternatives and control who has authority to classify. The immutable reading's claim that 'better observation corrects error' becomes the enforcement mechanism itself—metrics are enforced not because they best solve the coordination problem but because they exclude non-metric criticism. Mandatrophy_resolved would be triggered if the framework explicitly acknowledged that metric standardization solves one problem (cross-domain vocabulary) while creating another (epistemic authority concentration) and allowed that second problem to be addressed by non-metric approaches.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metric_reduction_necessity,
    'Is metric reduction to five scalar dimensions (extractiveness, suppression, theater_ratio, accessibility_collapse, resistance) necessary to enable cross-domain institutional comparison, or does it merely make comparison more convenient for metric-fluent practitioners while foreclosing non-metric analysis?',
    'Comparative institutional study using both metric and non-metric frameworks on the same constraint set, with empirical assessment of whether the non-metric analyses identify structural patterns the metrics miss.',
    'If metric reduction is necessary, the suppression of non-metric approaches is a legitimate coordination cost. If metric reduction is merely convenient for practitioners, the suppression constitutes pure extraction of authority, reclassifying from rope/tangled_rope to snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metric_reduction_necessity, empirical, 'Whether metric reduction is necessary for cross-domain institutional comparison or merely convenient for metric-fluent authority.').

omega_variable(
    observability_of_epsilon,
    'Are epsilon values (base extractiveness, etc.) genuinely observable facts about constraints, or are they constructed judgments that depend on the framework''s normative commitments about what counts as ''benefit'' and ''extraction''?',
    'Systematic replication of constraint classification using different metric definitions and normative premises; empirical documentation of whether epsilon values converge across independent research teams with different theoretical commitments.',
    'If epsilon is genuinely observable, the immutable reading is structurally sound and classification disputes are errors correctable by better observation. If epsilon is constructed, the framework embeds normative choices in apparently objective measurement, and the immutable reading''s suppression of normative frameworks becomes a form of concealed normativity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(observability_of_epsilon, empirical, 'Whether epsilon values are discovered or constructed.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.71) primarily structural (institutional gate-keeping based on metric fluency) or internalized (non-metric practitioners have been convinced that metric frameworks are more objective and have adopted them willingly)?',
    'Qualitative study of scholars and practitioners working in non-metric traditions: do they experience the suppression as external barrier or internal adoption of metric standards as legitimate? Post-exit suppression trajectory: if non-metric scholars continue metric work after exiting institutional contexts, the suppression is partially internalized.',
    'If suppression is structural, the constraint''s effective suppression is the measured 0.71. If suppression is internalized, the constraint''s effective suppression is higher because it persists even after the institutional gate-keeping mechanism is removed. This affects whether the constraint should be reclassified as higher-suppression snare vs. lower-suppression tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in the enforcement of metric standards.').

omega_variable(
    alternative_framework_viability,
    'Do the alternative normative frameworks (critical theory, participatory knowledge, non-metric institutional analysis) have coherent epistemic standards and the capacity to provide useful institutional classification outside the metric system, or is the metric framework''s dominance justified by superior performance?',
    'Meta-analysis of institutional analyses conducted under alternative frameworks: do they identify genuine institutional problems, propose workable solutions, and generate reliable cross-domain insights at comparable rate to metric frameworks?',
    'If alternatives are viable, their exclusion is pure suppression and the constraint should be reclassified as high-extraction snare. If alternatives are not viable, the suppression may be justified by coordination necessity, supporting rope or weaker tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_framework_viability, conceptual, 'Whether non-metric institutional frameworks are viably alternative or are rightfully superseded by metrics.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_ontology__immutable_diagnostic_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(defe_tr_t0, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement_basis(defe_tr_t0, observed).
narrative_ontology:measurement(defe_tr_t8, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 8, 0.36).
narrative_ontology:measurement_basis(defe_tr_t8, observed).
narrative_ontology:measurement(defe_tr_t16, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 16, 0.4).
narrative_ontology:measurement_basis(defe_tr_t16, observed).
narrative_ontology:measurement(defe_tr_t24, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 24, 0.43).
narrative_ontology:measurement_basis(defe_tr_t24, observed).
narrative_ontology:measurement(defe_tr_t32, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 32, 0.44).
narrative_ontology:measurement_basis(defe_tr_t32, observed).
narrative_ontology:measurement(defe_tr_t40, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 40, 0.44).
narrative_ontology:measurement_basis(defe_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(defe_be_t0, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(defe_be_t0, observed).
narrative_ontology:measurement(defe_be_t8, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement_basis(defe_be_t8, observed).
narrative_ontology:measurement(defe_be_t16, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement_basis(defe_be_t16, observed).
narrative_ontology:measurement(defe_be_t24, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 24, 0.61).
narrative_ontology:measurement_basis(defe_be_t24, observed).
narrative_ontology:measurement(defe_be_t32, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 32, 0.62).
narrative_ontology:measurement_basis(defe_be_t32, observed).
narrative_ontology:measurement(defe_be_t40, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(defe_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(defe_su_t0, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(defe_su_t0, observed).
narrative_ontology:measurement(defe_su_t8, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement_basis(defe_su_t8, observed).
narrative_ontology:measurement(defe_su_t16, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 16, 0.66).
narrative_ontology:measurement_basis(defe_su_t16, observed).
narrative_ontology:measurement(defe_su_t24, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 24, 0.69).
narrative_ontology:measurement_basis(defe_su_t24, observed).
narrative_ontology:measurement(defe_su_t32, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 32, 0.71).
narrative_ontology:measurement_basis(defe_su_t32, observed).
narrative_ontology:measurement(defe_su_t40, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(defe_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deferential_realism_ontology__immutable_diagnostic_reading, information_standard).
narrative_ontology:boltzmann_floor_override(deferential_realism_ontology__immutable_diagnostic_reading, 0.08).
narrative_ontology:affects_constraint(deferential_realism_ontology__immutable_diagnostic_reading, hybrid_pragmatic_reading).
narrative_ontology:affects_constraint(deferential_realism_ontology__immutable_diagnostic_reading, rhetorical_scaffold_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel deferential_realism_ontology. The immutable diagnostic reading (this file) treats the constraint typology as an observational instrument with fixed, discoverable referents. The hybrid_pragmatic_reading treats the typology as having fixed core (mountains, ropes) but contested periphery requiring normative judgment. The rhetorical_scaffold_reading treats the typology as a normative vocabulary for policy critique where 'snare' is declared rather than discovered. Each reading instantiates a different ε value and beneficiary/victim structure. All three readings are linked via network.affects_constraints to enable constraint family analysis and measurement of how classification disputes correlate with the underlying kernel contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(deferential_realism_ontology__immutable_diagnostic_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
