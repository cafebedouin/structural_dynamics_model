% ============================================================================
% CONSTRAINT STORY: deferential_realism_ontology__rhetorical_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_deferential_realism_ontology__rhetorical_scaffold_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: deferential_realism_ontology__rhetorical_scaffold_reading
 *   human_readable: Constraint Typology as Persuasive Advocacy Vocabulary (Rhetorical Scaffold Reading)
 *   domain: epistemology/normative_theory/institutional_design
 *
 * SUMMARY:
 *   This story instantiates the rhetorical-scaffold reading of the
 *   deferential_realism_ontology kernel: the six-category typology
 *   (mountain/rope/tangled_rope/snare/scaffold/piton) is not a discovery
 *   procedure but a normative vocabulary whose classifications are declared,
 *   not measured, and whose value lies in persuasive force. On this reading,
 *   calling an arrangement a 'snare' is an act of critique dressed in the
 *   syntax of diagnosis — the epsilon and suppression values authors assign
 *   are constructed judgments about which beneficiaries count as
 *   illegitimate, not readings off an independent instrument. This is a
 *   distinct constraint from the sibling readings, not a different observable
 *   of the same one: the immutable_diagnostic_reading holds the typology has
 *   fixed referents and misclassification is correctable error (a different
 *   epsilon story entirely — near-mountain for the apparatus itself), and the
 *   hybrid_pragmatic_reading holds the core categories are fixed while only
 *   the periphery (tangled_rope/snare) is contested. Each reading is authored
 *   as its own file per the epsilon-invariance principle; this file's epsilon
 *   (0.42, moderate, rising) describes only how much the rhetorical-scaffold
 *   use of the typology itself extracts — reputational cost imposed on
 *   labeled institutions via apparently-neutral machinery — not the
 *   object-level policy disputes the typology is used to adjudicate.
 *
 * KEY AGENTS:
 *   - critical_policy_advocates: primary beneficiary of the typology's persuasive credibility, deploys the vocabulary strategically
 *   - framework_authoring_analysts: builds and maintains the apparatus, benefits from its apparent objectivity
 *   - institutions_labeled_snares: bears reputational and political cost of a normative verdict presented as measurement
 *   - incumbent_mechanism_defenders: cannot rebut on evidentiary grounds because there is, on this reading, no independent evidentiary layer to rebut
 *   - immutable_diagnostic_reading_holders: analytical observer whose competing reading is excluded from this reading's self-description
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_ontology__rhetorical_scaffold_reading, 0.42).
domain_priors:suppression_score(deferential_realism_ontology__rhetorical_scaffold_reading, 0.18).
domain_priors:theater_ratio(deferential_realism_ontology__rhetorical_scaffold_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__rhetorical_scaffold_reading, tangled_rope).
narrative_ontology:human_readable(deferential_realism_ontology__rhetorical_scaffold_reading, "Constraint Typology as Persuasive Advocacy Vocabulary (Rhetorical Scaffold Reading)").
narrative_ontology:topic_domain(deferential_realism_ontology__rhetorical_scaffold_reading, "epistemology/normative_theory/institutional_design").

domain_priors:requires_active_enforcement(deferential_realism_ontology__rhetorical_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__rhetorical_scaffold_reading, '61c01de9-a03f-4d93-8bea-f045d1896491').
narrative_ontology:cs_kernel_codification('61c01de9-a03f-4d93-8bea-f045d1896491', distributed).
narrative_ontology:cs_authority_grounding('61c01de9-a03f-4d93-8bea-f045d1896491', practice).
narrative_ontology:cs_interpretation_layer_present('61c01de9-a03f-4d93-8bea-f045d1896491').
narrative_ontology:cs_reading_relation('61c01de9-a03f-4d93-8bea-f045d1896491', deferential_realism_ontology__immutable_diagnostic_reading, forecloses).
narrative_ontology:cs_reading_relation('61c01de9-a03f-4d93-8bea-f045d1896491', deferential_realism_ontology__hybrid_pragmatic_reading, influences).
narrative_ontology:cs_axiom('61c01de9-a03f-4d93-8bea-f045d1896491', foundational, classification_is_normative_declaration_not_discovery).
narrative_ontology:cs_axiom_status(classification_is_normative_declaration_not_discovery, holdable).
narrative_ontology:cs_axiom_grounding('61c01de9-a03f-4d93-8bea-f045d1896491', classification_is_normative_declaration_not_discovery, conventional).
narrative_ontology:cs_axiom('61c01de9-a03f-4d93-8bea-f045d1896491', secondary, framework_value_is_persuasive_not_evidentiary).
narrative_ontology:cs_axiom_status(framework_value_is_persuasive_not_evidentiary, holdable).
narrative_ontology:cs_axiom_grounding('61c01de9-a03f-4d93-8bea-f045d1896491', framework_value_is_persuasive_not_evidentiary, instrumental).
narrative_ontology:cs_reference_frame('61c01de9-a03f-4d93-8bea-f045d1896491', advocacy_vocabulary_baseline).
narrative_ontology:cs_drift_state('61c01de9-a03f-4d93-8bea-f045d1896491', contemporary_corpus_generation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('61c01de9-a03f-4d93-8bea-f045d1896491', '').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__rhetorical_scaffold_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__rhetorical_scaffold_reading, critical_policy_advocates).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__rhetorical_scaffold_reading, framework_authoring_analysts).
narrative_ontology:constraint_victim(deferential_realism_ontology__rhetorical_scaffold_reading, institutions_labeled_snares).
narrative_ontology:constraint_victim(deferential_realism_ontology__rhetorical_scaffold_reading, incumbent_mechanism_defenders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Uses the typology's vocabulary — especially the word 'snare' — to build a persuasive case against a mechanism they already oppose on other grounds. The label does rhetorical work: it recasts a contested policy dispute as a discovered structural fact, lending the critique borrowed authority. They can deploy or withhold the label depending on which framing serves the current campaign.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, critical_policy_advocates, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(deferential_realism_ontology__rhetorical_scaffold_reading, critical_policy_advocates, agenda_setter).

% Write and maintain the classification apparatus itself, including the six categories and the associated metrics (epsilon, suppression, theater ratio). On this reading, they are declaring normative verdicts through a vocabulary dressed as measurement, and they benefit from the framework's credibility as apparently-objective analysis — a credibility earned through its resemblance to empirical science rather than through actual discovery procedure.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, framework_authoring_analysts, agenda_setter,
    moderate, biographical, mobile, global).

% Have their arrangement classified as a 'snare' by an advocate wielding the typology. On this reading, the label was authored, not measured — but it still lands with the social force of a diagnostic finding, and the institution bears real reputational and political costs from a classification that is, in this reading, a normative declaration rather than a discovery.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, institutions_labeled_snares, payer,
    powerful, generational, constrained, national).

% Try to contest a 'snare' classification and find they are arguing against something that presents itself as an empirical verdict (extraction measured, victims named) when, on this reading, the classification was reached by normative judgment about which beneficiaries count as illegitimate. They cannot rebut it on measurement grounds because there is, on this reading, no independent measurement to rebut — only a persuasive framing to out-argue.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, incumbent_mechanism_defenders, payer,
    powerful, biographical, constrained, national).

% Receive the typology's verdicts secondhand, through advocacy campaigns and journalism, without visibility into whether 'snare' was applied through a stable evaluative procedure or through the persuasive judgment of whoever authored the story. They would want to know which reading of the typology is operative before trusting a verdict, but that question is rarely posed to them.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, policy_publics, excluded,
    powerless, immediate, trapped, national).

% Hold that the typology has fixed referents and that misclassification is an observational error, not a normative choice. They are structurally excluded from this reading's own self-description — this reading treats their position as a naive realism about what is, in this reading's own terms, an advocacy instrument.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, immutable_diagnostic_reading_holders, excluded,
    moderate, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(deferential_realism_ontology__rhetorical_scaffold_reading, critical_policy_advocates).
narrative_ontology:fixing_cost_class(deferential_realism_ontology__rhetorical_scaffold_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The typology gives critics of extractive arrangements a shared vocabulary to organize otherwise-scattered objections into a coherent structural critique, which lets disparate stakeholders converge on a common framing faster than they could build the case from first principles each time.
% TRANSFER_FUNCTION: Moves persuasive authority — the appearance of discovered structural fact — from the framework's scientific-sounding apparatus (epsilon, suppression, theater ratio) to whichever advocate deploys the 'snare' label, and moves reputational cost from that authority onto whatever institution the label lands on.
% ABSENT_VOICES: Institutions labeled snares rarely get to contest the classification on its own terms, because the classification presents as measured rather than argued; holders of the immutable_diagnostic_reading would object that treating epsilon as constructed rather than discovered concedes too much and undermines the framework's evidentiary claims generally, but this reading does not give them a seat inside its own self-understanding.
% DISAPPEARANCE_RATIONALE: If the rhetorical-scaffold reading of the typology vanished, advocates would lose a persuasively efficient vocabulary but could likely reconstruct similar critiques through ordinary policy argument; framework-authoring analysts dispute this, holding that the specific credibility conferred by apparent-measurement is not easily replaced by plain argument — hence contested rather than a clean verdict either way.
% FOUNDING_PROBLEM: Advocates needed a way to name extractive arrangements that resist plain description because their proponents frame them as neutral coordination — the typology was built to give critique a vocabulary with enough apparent rigor to counter that framing.
% FOUNDING_PROBLEM_CORROBORATION: Incumbent mechanism defenders, from outside the advocacy coalition, attest that the founding problem persists in the sense that contested arrangements are still routinely defended as neutral coordination — but they dispute that the typology's answer to it is itself neutral, arguing instead that 'snare' functions exactly as the framing tool this reading claims it is, which is corroboration of the reading's own self-description even from an adversarial seat.
narrative_ontology:disappearance_verdict(deferential_realism_ontology__rhetorical_scaffold_reading, contested).
narrative_ontology:founding_problem_status(deferential_realism_ontology__rhetorical_scaffold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferential_realism_ontology__rhetorical_scaffold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(deferential_realism_ontology__rhetorical_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(deferential_realism_ontology__rhetorical_scaffold_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deferential_realism_ontology__rhetorical_scaffold_reading_tests).
:- end_tests(deferential_realism_ontology__rhetorical_scaffold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) rather than high because the coordination function is genuine — the typology really does let scattered critics converge on a shared vocabulary faster than ad hoc argument, and that coordination benefit is real even under this reading. But extraction rises over the interval as the label's persuasive currency compounds: each successful 'snare' classification increases the term's rhetorical weight for the next deployment, without any corresponding increase in verification rigor, which is exactly the dynamic that produces rising theater_ratio (0.22 to 0.40) — the apparatus increasingly performs objectivity it does not, on this reading, possess. Suppression is deliberately low (0.18): this reading explicitly holds that alternative framings (the immutable_diagnostic_reading, ordinary policy argument without the typology) are not suppressed, merely out-competed rhetorically. Accessibility_collapse is correspondingly low (0.25) — institutions labeled snares retain the option to contest the framing in ordinary political and argumentative terms; what they cannot do is contest it as a measurement, because on this reading there is no independent measurement layer underneath the label.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setting seat (framework_authoring_analysts), the apparatus is coordination infrastructure that happens to also carry persuasive force as a byproduct of clarity. From the payer seats (institutions_labeled_snares, incumbent_mechanism_defenders), the same apparatus operates as an extraction mechanism that borrows the social authority of measurement to win arguments that would otherwise have to be won on the merits. The engine should compute these as structurally different experiences of the same authored data — the divergence is the finding, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Critical policy advocates and framework-authoring analysts sit near the beneficiary end: they gain persuasive leverage and apparatus credibility respectively, at low structural cost to themselves. Institutions labeled snares and incumbent mechanism defenders sit near the target end: they bear reputational cost from a verdict that, on this reading, was never independently verified against them, and their exit options are constrained precisely because contesting a classification that presents as objective is harder than contesting an argument presented as an argument. Policy publics are the excluded seat — they receive verdicts without visibility into which reading of the typology produced them.
 *
 * MANDATROPHY ANALYSIS:
 *   The rhetorical-scaffold reading resists mislabeling the typology's coordination function as pure extraction: it explicitly credits the genuine convergence benefit the shared vocabulary provides to scattered critics (this is why the story is authored as tangled_rope and not snare — a snare would require the coordination story to be pure cover, and this reading holds the coordination function is real). It equally resists treating the typology as a neutral discovery instrument whose verdicts require no normative defense — that is precisely the immutable_diagnostic_reading this story is NOT authoring. The tangled_rope classification captures the hybrid: real coordination value, riding alongside asymmetric extraction (reputational cost concentrated on labeled institutions) that requires active maintenance (continued deployment and re-deployment of the vocabulary) to persist.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_selection_is_itself_a_move,
    'Is choosing the rhetorical-scaffold reading over the immutable-diagnostic or hybrid-pragmatic readings itself a normative act with the same structure the rhetorical-scaffold reading attributes to ''snare'' classifications — i.e., is meta-level reading-selection subject to the same critique this reading makes of object-level classification?',
    'No empirical resolution is available; this is a reflexivity question about whether the framework''s self-description can escape its own critique. Progress would come from examining whether the rhetorical-scaffold reading can non-circularly justify its own selection without appeal to persuasive force.',
    'If reading-selection is itself ungrounded normative declaration, the rhetorical-scaffold reading is self-undermining in the same way it claims object-level classification is; if reading-selection can be grounded in something other than persuasion (coherence, predictive success, corpus fit), the reading survives its own test.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_is_itself_a_move, conceptual, 'Whether the kernel-reading choice is itself subject to the rhetorical-scaffold reading''s own critique.').

omega_variable(
    engine_computation_vs_reading_claim,
    'The engine computes per-seat classifications from authored structural data using a fixed formula (chi from epsilon, directionality, scope) — does this computational procedure itself constitute a ''measurement'' that the rhetorical-scaffold reading''s core claim (classification is declared, not discovered) would have to deny is happening, even though the formula is applied uniformly and mechanically once inputs are fixed?',
    'Examine whether the mechanical application of a fixed formula to authored inputs counts as ''discovery'' in the sense the immutable_diagnostic_reading intends, or whether the authoring step (choosing epsilon, beneficiaries, victims) is where all the normative work happens, making the downstream computation a formal artifact rather than an independent check.',
    'If the authoring step is where the normativity lives and the computation is merely formal propagation, this reading''s core claim holds even for engine-computed classifications; if the computation itself constrains or falsifies authored claims in ways authors cannot fully anticipate, the framework has more discovery-like structure than this reading credits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(engine_computation_vs_reading_claim, conceptual, 'Whether the engine''s mechanical computation from authored inputs undermines or preserves the declared-not-discovered claim.').

omega_variable(
    persuasive_success_as_evidence,
    'Does the empirical fact that ''snare'' classifications successfully persuade audiences (measurable via campaign outcomes, policy changes, public opinion shift) count as evidence FOR the rhetorical-scaffold reading, or is persuasive success orthogonal to which reading is correct?',
    'Track correlation between successful snare-labeling campaigns and subsequent independent audits of the labeled arrangement''s actual extraction profile; if labels track independently-verified extraction well, that favors the immutable_diagnostic_reading; if labels track rhetorical skill and audience predisposition independent of subsequent verification, that favors this reading.',
    'Directly bears on which reading of the kernel is closer to the operative mechanism — this is the empirical wedge between readings that are otherwise conceptually distinguished.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(persuasive_success_as_evidence, empirical, 'Whether persuasive success tracks independently verifiable extraction or tracks something else.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_ontology__rhetorical_scaffold_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(defe_tr_t0, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(defe_tr_t4, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 4, 0.26).
narrative_ontology:measurement(defe_tr_t8, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(defe_tr_t12, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 12, 0.33).
narrative_ontology:measurement(defe_tr_t16, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement(defe_tr_t20, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(defe_tr_t24, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 24, 0.4).

% Extraction over time
narrative_ontology:measurement(defe_be_t0, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(defe_be_t4, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 4, 0.33).
narrative_ontology:measurement(defe_be_t8, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 8, 0.36).
narrative_ontology:measurement(defe_be_t12, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 12, 0.38).
narrative_ontology:measurement(defe_be_t16, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 16, 0.4).
narrative_ontology:measurement(defe_be_t20, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(defe_be_t24, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 24, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(deferential_realism_ontology__rhetorical_scaffold_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deferential_realism_ontology__rhetorical_scaffold_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(deferential_realism_ontology__rhetorical_scaffold_reading, 0.1).
narrative_ontology:affects_constraint(deferential_realism_ontology__rhetorical_scaffold_reading, immutable_diagnostic_reading).
narrative_ontology:affects_constraint(deferential_realism_ontology__rhetorical_scaffold_reading, hybrid_pragmatic_reading).

% DUAL FORMULATION NOTE:
% This file is one of three constraint stories decomposing the natural-language claim 'the constraint typology is X' into structurally distinct readings, per the epsilon-invariance principle: rhetorical_scaffold_reading (this file, epsilon 0.42, tangled_rope — persuasive-vocabulary claim), immutable_diagnostic_reading (separate file — fixed-referent-instrument claim, expected near-mountain epsilon for the apparatus itself), and hybrid_pragmatic_reading (separate file — fixed-core/contested-periphery claim, expected mixed epsilon by category). All three share the kernel_id deferential_realism_ontology and are linked via affects_constraints; none averages or hedges across the others. Each reading's epsilon describes a different claim about what the typology IS, not a different observable of the same claim.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
