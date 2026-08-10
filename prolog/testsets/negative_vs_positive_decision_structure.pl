% ============================================================================
% CONSTRAINT STORY: negative_vs_positive_decision_structure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_negative_vs_positive_decision_structure, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: negative_vs_positive_decision_structure
 *   human_readable: Attractive-Option vs. Least-Bad-Option Decision Structure
 *   domain: applied_philosophy/ethics/self_help
 *
 * SUMMARY:
 *   Popular libertarian self-help literature (exemplified by Browne-style
 *   'boxes' rhetoric) treats every constrained life as secretly a life of
 *   attractive options obscured by fear or bad framing. Critics from
 *   capabilities theory and relational ethics respond that some choice-sets
 *   genuinely contain only least-bad options, and no amount of internal
 *   reorientation adds an attractive option that structurally does not exist.
 *   This story treats the underlying observable — the actual ratio of
 *   attractive-option decisions to aversive-option decisions in a life — as a
 *   rope: a real, useful coordination device for diagnosing a life's choice
 *   architecture, usable across every freedom_locus_kernel reading without
 *   needing to resolve the metaphysical dispute about what freedom ultimately
 *   consists in. The metaphysical dispute over the kernel is a separate,
 *   higher-variance argument; the observable delta itself is comparatively
 *   low-extraction because it is a measurement tool, not an enforced
 *   arrangement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(negative_vs_positive_decision_structure, 0.18).
domain_priors:suppression_score(negative_vs_positive_decision_structure, 0.12).
domain_priors:theater_ratio(negative_vs_positive_decision_structure, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(negative_vs_positive_decision_structure, extractiveness, 0.18).
narrative_ontology:constraint_metric(negative_vs_positive_decision_structure, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(negative_vs_positive_decision_structure, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(negative_vs_positive_decision_structure, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(negative_vs_positive_decision_structure, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(negative_vs_positive_decision_structure, rope).
narrative_ontology:human_readable(negative_vs_positive_decision_structure, "Attractive-Option vs. Least-Bad-Option Decision Structure").
narrative_ontology:topic_domain(negative_vs_positive_decision_structure, "applied_philosophy/ethics/self_help").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(negative_vs_positive_decision_structure, '6dd57822-b595-4e80-9b30-2ccf7f7f7296').
narrative_ontology:cs_kernel_codification('6dd57822-b595-4e80-9b30-2ccf7f7f7296', distributed).
narrative_ontology:cs_authority_grounding('6dd57822-b595-4e80-9b30-2ccf7f7f7296', distributed).
narrative_ontology:cs_reference_frame('6dd57822-b595-4e80-9b30-2ccf7f7f7296', measured_option_set_composition).
narrative_ontology:cs_drift_state('6dd57822-b595-4e80-9b30-2ccf7f7f7296', contemporary_self_help_commercialization, gap(stable, minor, true)).
narrative_ontology:cs_created_at('6dd57822-b595-4e80-9b30-2ccf7f7f7296', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(negative_vs_positive_decision_structure, individuals_with_resourced_option_sets).
narrative_ontology:constraint_beneficiary(negative_vs_positive_decision_structure, self_help_authors_and_coaches).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(negative_vs_positive_decision_structure, structurally_constrained_individuals).
narrative_ontology:constraint_vindicates(negative_vs_positive_decision_structure, internal_locus_of_control_efficacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% People whose material, social, and legal position means most of their decisions are genuinely between attractive options (which job offer, which city to move to) rather than between a bad option and a worse one. They can act on reframing advice and see real gains because their option-sets already contain viable exits.
narrative_ontology:constraint_stakeholder(negative_vs_positive_decision_structure, individuals_with_resourced_option_sets, beneficiary,
    moderate, biographical, mobile, local).

% People whose choice-sets are dominated by least-bad options — stay in an abusive household or become homeless, keep an exploitative job or lose healthcare access. Self-reframing changes how the choice feels but does not add an attractive option to the set. They bear the cost when the observable difference between the two life-shapes is collapsed into a single narrative of 'mindset.'
narrative_ontology:constraint_stakeholder(negative_vs_positive_decision_structure, structurally_constrained_individuals, payer,
    powerless, biographical, trapped, local).

% Produce and monetize the sovereign-agency framing (books, courses, coaching) in which the ratio of attractive-to-aversive decisions is presented as a function of internal reorientation. They benefit commercially from a universal narrative that applies regardless of a reader's actual option-set composition, and have no stake in distinguishing structurally trapped readers from resourced ones.
narrative_ontology:constraint_stakeholder(negative_vs_positive_decision_structure, self_help_authors_and_coaches, beneficiary,
    organized, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(negative_vs_positive_decision_structure, self_help_authors_and_coaches, agenda_setter).

% Bear costs when someone acts on a 'just leave the box' framing without accounting for relational obligations — children, aging parents, co-signed debts. Their stake in how the option-set is assessed is not represented in the sovereign-agency reading's ledger; they would object that some exits are not free even when they are internally available.
narrative_ontology:constraint_stakeholder(negative_vs_positive_decision_structure, dependents_and_partners, excluded,
    powerless, biographical, trapped, local).

% Study and measure the actual composition of people's choice-sets (capabilities approach empirical work) and can behaviorally infer the attractive/aversive ratio independent of self-report, which is what makes the negative-vs-positive structure an observable delta rather than a purely rhetorical dispute.
narrative_ontology:constraint_stakeholder(negative_vs_positive_decision_structure, capabilities_researchers, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Distinguishing attractive-option decisions from least-bad-option decisions gives individuals and researchers a real, non-question-begging way to describe the shape of a life's choice architecture without first resolving what freedom metaphysically consists in — it is a measurable delta usable by every reading of the freedom_locus_kernel.
% TRANSFER_FUNCTION: The distinction itself transfers nothing materially; what it enables is diagnostic — it can direct attention (and therefore resources, sympathy, or blame) either toward internal reframing interventions or toward structural/relational remediation, depending on which kernel reading captures the diagnosis.
% ABSENT_VOICES: Structurally trapped individuals and dependents bearing externalized exit costs are rarely the ones producing the theory; the sovereign-agency framing that dominates commercially is authored largely by people whose own option-sets were already resourced when they wrote it.
% DISAPPEARANCE_RATIONALE: The observable ratio (attractive vs. aversive decisions) is a descriptive fact about a life's actual choice architecture, not an enforced arrangement — if no one ever theorized or measured it, people's real option-sets would be exactly as constrained or open as they already are. What would disappear is only the vocabulary for naming the difference, not the difference itself.
% FOUNDING_PROBLEM: People conflate 'I have choices' with 'I have good choices,' which obscures whether a felt sense of agency reflects an actually open option-set or a narrowed one being coped with well; the distinction was drawn to make that conflation visible.
% FOUNDING_PROBLEM_CORROBORATION: Capabilities-approach researchers (Nussbaum/Sen tradition), operating outside the self-help commercial ecosystem and outside the sovereign-agency lineage, independently corroborate that option-set composition (not merely felt agency) is the operative variable — their empirical capability measures exist precisely because self-report of felt freedom is known to diverge from actual option-set structure.
narrative_ontology:disappearance_verdict(negative_vs_positive_decision_structure, world_unchanged).
narrative_ontology:founding_problem_status(negative_vs_positive_decision_structure, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(negative_vs_positive_decision_structure, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-10',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(negative_vs_positive_decision_structure, 'none', 1).
narrative_ontology:epsilon_provenance(negative_vs_positive_decision_structure, 0.18, 'claude-sonnet-5', 'harry_browne_freedom_kernel_2026_20260810_020156', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(negative_vs_positive_decision_structure_tests).
:- end_tests(negative_vs_positive_decision_structure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because the observable itself — counting/inferring attractive vs. aversive decision ratios — does not by itself transfer resources from anyone to anyone; it only becomes extractive downstream, when a particular kernel reading (sovereign_agency) is imposed on someone whose actual ratio is skewed toward aversive options, denying them recognition of that fact. Suppression is low (0.12): no one is coerced into accepting the measurement, though commercial self-help discourse exerts soft pressure to interpret one's own ratio favorably. Resistance is moderate (0.25) because capabilities researchers and relational ethicists actively contest the sovereign-agency gloss on the same observable.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of someone whose option-set is already resourced, the sovereign-agency reading and the observable delta line up neatly — reorientation really does raise their felt agency because their ratio was never that bad. From the seat of someone structurally trapped, the same observable delta reveals the gap between the theory's promise and their measured reality; the engine would compute a materially different profile for that seat even though the underlying observable (the ratio itself) is identical across both.
 *
 * DIRECTIONALITY LOGIC:
 *   Individuals with resourced option-sets and self-help producers sit near the beneficiary end: the former because their actual ratios are already favorable and the framing flatters their situation, the latter because they monetize a universal narrative regardless of any given reader's true ratio. Structurally constrained individuals sit near the target end: their aversive-option-heavy ratio is real and behaviorally measurable, and a totalizing sovereign-agency reading denies them recognition of that structural fact, extracting dignity/attention rather than money. Dependents are excluded from the ledger entirely under the dominant reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The 'founding problem' — conflating felt agency with actual option-set breadth — remains live; the observable ratio survives as a genuinely useful diagnostic tool (a rope) independent of which kernel reading eventually wins the argument about what freedom fundamentally is. This prevents the classification from collapsing into either 'pure ideology defending itself' (snare) or 'settled fact' (mountain): the tool for measuring the delta is not the same object as any single reading's verdict about what the delta means.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    self_report_vs_behavioral_ratio_divergence,
    'Does self-reported attractive/aversive decision ratio track the behaviorally inferred ratio, or do people systematically misreport their own option-set composition (in either direction)?',
    'Compare self-report surveys against behavioral/capabilities-style measures (actual documented alternatives available at decision points) for the same population and decision history.',
    'If self-report diverges systematically upward (people believe they have more attractive options than they behaviorally had), sovereign-agency-style interventions may be measuring felt agency rather than actual freedom, inflating apparent success. If it diverges downward, structural readings may be overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_report_vs_behavioral_ratio_divergence, empirical, 'Whether self-report is a reliable proxy for the behaviorally inferred ratio.').

omega_variable(
    kernel_reading_determines_remediation_not_measurement,
    'Is the freedom_locus_kernel dispute actually about what the observable ratio IS, or only about what should be done in response to a given ratio (remediation vs. reframing)?',
    'Trace whether any of the four readings dispute the measured ratio itself in a given case study, versus disputing only the appropriate response (internal reorientation vs. structural remedy vs. relational renegotiation vs. bounded mixed intervention).',
    'If the dispute is purely about remediation and never about the measured ratio, this story''s rope classification is stable and the four readings should be authored as four separate downstream constraint stories about REMEDIATION, not about the observable itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_determines_remediation_not_measurement, conceptual, 'Whether the kernel dispute is located in measurement or in prescribed response.').

omega_variable(
    commercial_capture_of_the_measurement_tool,
    'Does the self-help industry''s commercial incentive to universalize the sovereign-agency reading degrade the observable tool itself over time (e.g., by discouraging honest behavioral measurement in favor of flattering self-report)?',
    'Track whether commercially-produced self-help materials increasingly substitute self-report inventories for any behavioral or structural cross-check, over successive editions/products.',
    'If commercial capture is rising, the theater_ratio and extractiveness of THIS constraint (the measurement tool) would need to rise correspondingly in a future revision — the tool would no longer be neutral across readings but would be drifting toward serving one reading''s commercial interest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(commercial_capture_of_the_measurement_tool, empirical, 'Whether commercial self-help incentives are degrading the neutrality of the measurement tool over time.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(negative_vs_positive_decision_structure, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nega_tr_t0, negative_vs_positive_decision_structure, theater_ratio, 0, 0.1).
narrative_ontology:measurement(nega_tr_t4, negative_vs_positive_decision_structure, theater_ratio, 4, 0.11).
narrative_ontology:measurement(nega_tr_t8, negative_vs_positive_decision_structure, theater_ratio, 8, 0.12).
narrative_ontology:measurement(nega_tr_t12, negative_vs_positive_decision_structure, theater_ratio, 12, 0.13).
narrative_ontology:measurement(nega_tr_t16, negative_vs_positive_decision_structure, theater_ratio, 16, 0.14).
narrative_ontology:measurement(nega_tr_t20, negative_vs_positive_decision_structure, theater_ratio, 20, 0.14).
narrative_ontology:measurement(nega_tr_t24, negative_vs_positive_decision_structure, theater_ratio, 24, 0.15).

% Extraction over time
narrative_ontology:measurement(nega_be_t0, negative_vs_positive_decision_structure, base_extractiveness, 0, 0.14).
narrative_ontology:measurement(nega_be_t4, negative_vs_positive_decision_structure, base_extractiveness, 4, 0.15).
narrative_ontology:measurement(nega_be_t8, negative_vs_positive_decision_structure, base_extractiveness, 8, 0.16).
narrative_ontology:measurement(nega_be_t12, negative_vs_positive_decision_structure, base_extractiveness, 12, 0.17).
narrative_ontology:measurement(nega_be_t16, negative_vs_positive_decision_structure, base_extractiveness, 16, 0.17).
narrative_ontology:measurement(nega_be_t20, negative_vs_positive_decision_structure, base_extractiveness, 20, 0.18).
narrative_ontology:measurement(nega_be_t24, negative_vs_positive_decision_structure, base_extractiveness, 24, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(negative_vs_positive_decision_structure, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(negative_vs_positive_decision_structure, information_standard).
narrative_ontology:boltzmann_floor_override(negative_vs_positive_decision_structure, 0.03).
narrative_ontology:affects_constraint(negative_vs_positive_decision_structure, sovereign_agency_freedom_reading).
narrative_ontology:affects_constraint(negative_vs_positive_decision_structure, structural_conditions_freedom_reading).
narrative_ontology:affects_constraint(negative_vs_positive_decision_structure, relational_obligation_freedom_reading).
narrative_ontology:affects_constraint(negative_vs_positive_decision_structure, negotiated_agency_freedom_reading).

% DUAL FORMULATION NOTE:
% This story authors the shared, kernel-agnostic observable (the attractive/aversive decision ratio) that all four freedom_locus_kernel readings measure against. Each reading is a separate constraint story classifying what that measured ratio implies and what ought to follow from it; ε differs sharply across those downstream stories (sovereign_agency_reading treats the same ratio's low value as near-costless because the frame denies structural constraint counts as a cost-bearer, while structural_conditions_reading treats an identical low ratio as evidence of substantial extraction from a real victim set). This story is the upstream Mountain/Rope-adjacent measurement layer; the four readings are downstream and more contested, per the BGS decomposition pattern.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
