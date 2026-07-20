% ============================================================================
% CONSTRAINT STORY: coercion_legitimacy_boundary__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coercion_legitimacy_boundary__proportionality_reading, []).

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
 *   constraint_id: coercion_legitimacy_boundary__proportionality_reading
 *   human_readable: Proportionality-Based Public Health Coercion Boundary
 *   domain: public health policy / medical ethics / constitutional law
 *
 * SUMMARY:
 *   This constraint instantiates the proportionality_reading of the
 *   coercion_legitimacy_boundary kernel. It represents the legal-ethical
 *   position that state coercion for medical intervention is legitimate only
 *   when disease severity and transmission dynamics exceed a threshold (e.g.,
 *   measles justifies mandates; seasonal influenza does not). The constraint
 *   coordinates epidemic prevention while extracting bodily autonomy from
 *   targeted individuals. As a contested kernel reading, its structural
 *   classification depends on whether the severity threshold is interpreted
 *   genuinely (tangled rope) or manipulated to expand state power (snare).
 *   The claim and metrics are authored independently: the constraint is
 *   claimed as tangled_rope while the metrics capture moderate extractiveness
 *   that spikes during emergency expansions such as COVID-19.
 *
 * KEY AGENTS:
 *   - public_health_authorities: Agenda setter (institutional/constrained) â administers proportionality test and triggers mandates.
 *   - mandated_individuals: Primary target (powerless/trapped) â bear bodily autonomy cost when threshold is met.
 *   - general_public: Primary beneficiary (organized/constrained) â receive herd immunity benefit without direct cost.
 *   - judiciary: Analytical observer (institutional/analytical) â adjudicates proportionality and sets legal boundaries.
 *   - bodily_autonomy_advocates: Excluded voice (moderate/constrained) â object to coercion but are overridden in proportionality calculus.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coercion_legitimacy_boundary__proportionality_reading, 0.42).
domain_priors:suppression_score(coercion_legitimacy_boundary__proportionality_reading, 0.48).
domain_priors:theater_ratio(coercion_legitimacy_boundary__proportionality_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coercion_legitimacy_boundary__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(coercion_legitimacy_boundary__proportionality_reading, "Proportionality-Based Public Health Coercion Boundary").
narrative_ontology:topic_domain(coercion_legitimacy_boundary__proportionality_reading, "public health policy / medical ethics / constitutional law").

domain_priors:requires_active_enforcement(coercion_legitimacy_boundary__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coercion_legitimacy_boundary__proportionality_reading, '8b58a983-d2f1-43c7-a1c6-e48b2aceae3a').
narrative_ontology:cs_kernel_codification('8b58a983-d2f1-43c7-a1c6-e48b2aceae3a', formalized).
narrative_ontology:cs_authority_grounding('8b58a983-d2f1-43c7-a1c6-e48b2aceae3a', lineage).
narrative_ontology:cs_interpretation_layer_present('8b58a983-d2f1-43c7-a1c6-e48b2aceae3a').
narrative_ontology:cs_reading_relation('8b58a983-d2f1-43c7-a1c6-e48b2aceae3a', coercion_legitimacy_boundary__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('8b58a983-d2f1-43c7-a1c6-e48b2aceae3a', coercion_legitimacy_boundary__public_health_primary, influences).
narrative_ontology:cs_axiom('8b58a983-d2f1-43c7-a1c6-e48b2aceae3a', foundational, coercion_legitimacy_proportional_to_severity).
narrative_ontology:cs_axiom_status(coercion_legitimacy_proportional_to_severity, holdable).
narrative_ontology:cs_axiom_grounding('8b58a983-d2f1-43c7-a1c6-e48b2aceae3a', coercion_legitimacy_proportional_to_severity, empirically_contingent).
narrative_ontology:cs_axiom('8b58a983-d2f1-43c7-a1c6-e48b2aceae3a', foundational, mandate_necessity_requires_high_transmission_or_mortality).
narrative_ontology:cs_axiom_status(mandate_necessity_requires_high_transmission_or_mortality, holdable).
narrative_ontology:cs_axiom_grounding('8b58a983-d2f1-43c7-a1c6-e48b2aceae3a', mandate_necessity_requires_high_transmission_or_mortality, empirically_contingent).
narrative_ontology:cs_reference_frame('8b58a983-d2f1-43c7-a1c6-e48b2aceae3a', constitutional_proportionality_framework).
narrative_ontology:cs_drift_state('8b58a983-d2f1-43c7-a1c6-e48b2aceae3a', post_covid_legal_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8b58a983-d2f1-43c7-a1c6-e48b2aceae3a', '').
narrative_ontology:cs_kernel_id(coercion_legitimacy_boundary__proportionality_reading, coercion_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__proportionality_reading, general_public).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__proportionality_reading, mandated_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer epidemic response and invoke legal coercion when epidemiological thresholds for severity and transmission are met. Justify mandates through constitutional proportionality and police-power lineage. Bound by judicial review and statutory limits; cannot coerce arbitrarily.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Subject to mandatory vaccination or quarantine orders when the proportionality threshold is triggered. Bear the direct cost of bodily autonomy infringement, medical risk, and legal penalties for refusal. Exit is legally blocked during active mandates.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, mandated_individuals, payer,
    powerless, immediate, trapped, local).

% Receive herd immunity and outbreak-prevention benefits when mandates are triggered. Do not bear the direct coercion cost, though they live under the same legal framework. Their exit options are constrained by the same public health rules but they are not the primary targets.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, general_public, beneficiary,
    organized, biographical, constrained, national).

% Adjudicate whether coercion meets constitutional proportionality. Review severity evidence, balance individual rights against collective harm, and set precedents that raise or lower the threshold for future mandates. Neither collects from nor pays into the constraint.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, judiciary, observer,
    institutional, generational, analytical, national).

% Advance categorical objections to medical coercion on ethical and constitutional grounds. Their arguments appear in amicus briefs and dissenting opinions but are systematically overridden in proportionality analysis during emergencies. Structurally sidelined from the threshold-calculus.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, bodily_autonomy_advocates, excluded,
    moderate, biographical, constrained, national).

narrative_ontology:fixing_cost_class(coercion_legitimacy_boundary__proportionality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents epidemic disease transmission by enabling state coercion to achieve herd immunity or quarantine containment when voluntary compliance is insufficient and pathogen severity is high.
% TRANSFER_FUNCTION: Transfers bodily autonomy, compliance cost, and medical risk from targeted individuals to the broader community in the form of reduced disease transmission and outbreak avoidance.
% ABSENT_VOICES: Bodily autonomy advocates and vaccine-refusing minorities are heard in exemptions and dissent but are excluded from the proportionality threshold calculus itself; their categorical objections are treated as legally subordinate to severity metrics.
% DISAPPEARANCE_RATIONALE: If the proportionality-based coercion boundary vanished, public health authorities would lose the primary legal mechanism for mandatory vaccination and quarantine. Outbreak response would shift to voluntary measures, legal precedent would rearrange toward strict bodily autonomy or broad police power, and the state-individual balance in epidemics would reset.
% FOUNDING_PROBLEM: How to prevent epidemic disease transmission and protect community health when voluntary individual compliance fails to achieve herd immunity and non-compliant individuals endanger others.
% FOUNDING_PROBLEM_CORROBORATION: Epidemiologists and historians attest that pre-vaccine-era outbreaks caused mass mortality. Independent civil liberties scholars and medical ethicists outside the public health beneficiary set attest that the problem is real but that the proportionality threshold and its application are contested; no neutral arbiter confirms the state's framing uncontestedly.
narrative_ontology:disappearance_verdict(coercion_legitimacy_boundary__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(coercion_legitimacy_boundary__proportionality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(coercion_legitimacy_boundary__proportionality_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(coercion_legitimacy_boundary__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(coercion_legitimacy_boundary__proportionality_reading, 0.42, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coercion_legitimacy_boundary__proportionality_reading_tests).
:- end_tests(coercion_legitimacy_boundary__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.42) is moderate because the constraint is dormant for low-severity diseases and activates only above a threshold; however, the COVID-19 period (interval 30-40) shows extraction spiking to 0.68 when the threshold was contested. Suppression (0.48) reflects legal enforcement during active mandate periods. Theater ratio (0.20 baseline) rose to 0.38 during COVID as political performative compliance partially displaced epidemiological targeting. Accessibility collapse (0.65) is high because once a mandate is legally authorized, alternatives narrow sharply. Resistance (0.50) captures persistent anti-mandate mobilization that varies with perceived severity. The cyclical measurement pattern reflects tension-buildup, emergency expansion, and partial post-crisis relaxation.
 *
 * PERSPECTIVAL GAP:
 *   The mandated_individuals seat experiences high directionality (full target): they bear the autonomy cost and face trapped exit during mandates. The general_public seat experiences low directionality (beneficiary): they gain protection without direct cost. The public_health_authorities seat sits near symmetric because they administer coercion but are themselves constrained by judicial review and legal precedent. The engine will compute divergent per-seat types: the payer seat likely computes as snare during active mandate phases, while the beneficiary seat computes as rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declaration (general_public) drives directionality toward the beneficiary end for that seat. Victim declaration (mandated_individuals) drives directionality toward the target end for that seat. Public_health_authorities lack explicit beneficiary or victim tagging and will receive the canonical fallback for institutional power, placing them near symmetric. No directionality override is needed because the structural derivation matches the actual relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The proportionality reading prevents mandatrophy by tying coercion to a live founding problem (epidemic prevention) and a severity condition that functions as a contingent sunset (mandates lift when severity drops). However, if the severity threshold is manipulated or the definition of high severity expands without empirical grounding, the constraint slides toward snare. The temporal measurements show a post-COVID partial relaxation, suggesting the mandate authority has not fully atrophied into piton; it retains genuine coordination function but carries elevated theater from the emergency period.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    severity_threshold_ambiguity,
    'Where exactly does the proportionality threshold lie between low-severity influenza and high-severity measles, and did COVID-19 fall on the legitimate or illegitimate side of this reading?',
    'Comparative case-law analysis of judicial proportionality rulings across jurisdictions combined with meta-analysis of R0 and infection-fatality data for contested pathogens.',
    'If COVID-19 is judged below the classical threshold, the post-COVID mandate surge represents a snare-like expansion; if above, it vindicates the proportionality frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(severity_threshold_ambiguity, empirical, 'Ambiguity in the severity threshold for legitimate coercion.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of vaccine refusal structural (legal penalties, school and employment exclusion) or internalized (social stigma, professional identity fusion with compliance)?',
    'Post-mandate trajectory studies measuring refusal persistence and social cost after legal penalties are formally removed.',
    'If internalized, the constraint''s effective suppression exceeds structural measures and may function as identity_coordination rather than pure enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism.').

omega_variable(
    proportionality_reading_kernel_location,
    'This constraint is the proportionality_reading of the coercion_legitimacy_boundary kernel. Would adopting the bodily_autonomy_primary reading (categorical prohibition) or the public_health_primary reading (broad utilitarian balance) yield a structurally different classification?',
    'Cross-reading comparison of victim sets, epsilon values, and coordination functions across the kernel family.',
    'If bodily_autonomy_primary is correct, this constraint misclassifies legitimate autonomy violation as extraction; if public_health_primary is correct, the severity floor is too high and the constraint undercounts extractive scope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proportionality_reading_kernel_location, conceptual, 'Commitment-system kernel reading contest for coercion legitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coercion_legitimacy_boundary__proportionality_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coercion_legitimacy_boundary__proportionality_reading_tr_t0, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(coercion_legitimacy_boundary__proportionality_reading_tr_t15, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 15, 0.12).
narrative_ontology:measurement(coercion_legitimacy_boundary__proportionality_reading_tr_t30, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement(coercion_legitimacy_boundary__proportionality_reading_tr_t40, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement(coercion_legitimacy_boundary__proportionality_reading_tr_t50, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 50, 0.28).

% Extraction over time
narrative_ontology:measurement(coercion_legitimacy_boundary__proportionality_reading_be_t0, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(coercion_legitimacy_boundary__proportionality_reading_be_t15, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 15, 0.35).
narrative_ontology:measurement(coercion_legitimacy_boundary__proportionality_reading_be_t30, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 30, 0.6).
narrative_ontology:measurement(coercion_legitimacy_boundary__proportionality_reading_be_t40, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(coercion_legitimacy_boundary__proportionality_reading_be_t50, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 50, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(coercion_legitimacy_boundary__proportionality_reading_su_t0, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(coercion_legitimacy_boundary__proportionality_reading_su_t15, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 15, 0.35).
narrative_ontology:measurement(coercion_legitimacy_boundary__proportionality_reading_su_t30, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 30, 0.75).
narrative_ontology:measurement(coercion_legitimacy_boundary__proportionality_reading_su_t40, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 40, 0.82).
narrative_ontology:measurement(coercion_legitimacy_boundary__proportionality_reading_su_t50, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 50, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coercion_legitimacy_boundary__proportionality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__proportionality_reading, bodily_autonomy_primary).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__proportionality_reading, public_health_primary).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the coercion_legitimacy_boundary kernel. The proportionality_reading coexists with and influences the bodily_autonomy_primary and public_health_primary readings within the same legal-cultural space. See the kernel triplet decomposition in the constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
