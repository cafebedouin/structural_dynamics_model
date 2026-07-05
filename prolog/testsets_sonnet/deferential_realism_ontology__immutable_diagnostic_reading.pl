% ============================================================================
% CONSTRAINT STORY: deferential_realism_ontology__immutable_diagnostic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: deferential_realism_ontology__immutable_diagnostic_reading
 *   human_readable: Constraint Typology as Fixed-Referent Diagnostic Instrument
 *   domain: epistemology/normative_theory/institutional_design
 *
 * SUMMARY:
 *   This story instantiates one specific reading of a contested kernel about
 *   what the constraint typology itself is. Under the immutable-diagnostic
 *   reading, mountains, ropes, snares, and their hybrids are treated as
 *   observational categories with fixed referents in the world —
 *   extractiveness, suppression, and theater_ratio are discoverable
 *   quantities, and disagreement about which type applies is an error
 *   correctable by better measurement, not a normative dispute about
 *   legitimate beneficiaries. This reading is itself a constraint: it
 *   coordinates institutions around a shared vocabulary (genuine coordination
 *   gain) but does so by suppressing the two sibling readings — the
 *   rhetorical-scaffold reading (typology as persuasive normative vocabulary)
 *   and the hybrid-pragmatic reading (fixed core, contested normative
 *   periphery) — treating both as category confusions rather than live
 *   alternatives. The extraction is the interpretive authority transferred to
 *   those empowered to declare a measurement 'settled,' at the cost of
 *   disputants and theorists who are foreclosed from making the normative
 *   argument the immutable-diagnostic frame denies is available.
 *
 * KEY AGENTS:
 *   - framework_engineers: primary agenda_setter (institutional/arbitrage) — build and defend the fixed-referent framing
 *   - credentialed_classifiers: beneficiary (organized/constrained) — professional authority depends on measurement-not-values framing
 *   - contested_edge_case_disputants: primary payer (moderate/trapped) — told disagreement is measurement error
 *   - alternative_framing_theorists: excluded (moderate/constrained) — structurally locked out of adjudicating the kernel dispute
 *   - meta_theoretical_observers: analytical observer (analytical/analytical) — sees the reading itself as a normative achievement, not a discovery
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_ontology__immutable_diagnostic_reading, 0.58).
domain_priors:suppression_score(deferential_realism_ontology__immutable_diagnostic_reading, 0.71).
domain_priors:theater_ratio(deferential_realism_ontology__immutable_diagnostic_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__immutable_diagnostic_reading, tangled_rope).
narrative_ontology:human_readable(deferential_realism_ontology__immutable_diagnostic_reading, "Constraint Typology as Fixed-Referent Diagnostic Instrument").
narrative_ontology:topic_domain(deferential_realism_ontology__immutable_diagnostic_reading, "epistemology/normative_theory/institutional_design").

domain_priors:requires_active_enforcement(deferential_realism_ontology__immutable_diagnostic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__immutable_diagnostic_reading, 'd3e91545-ad4e-49d0-b215-128a88c12412').
narrative_ontology:cs_kernel_codification('d3e91545-ad4e-49d0-b215-128a88c12412', distributed).
narrative_ontology:cs_authority_grounding('d3e91545-ad4e-49d0-b215-128a88c12412', expertise).
narrative_ontology:cs_interpretation_layer_present('d3e91545-ad4e-49d0-b215-128a88c12412').
narrative_ontology:cs_reading_relation('d3e91545-ad4e-49d0-b215-128a88c12412', deferential_realism_ontology__rhetorical_scaffold_reading, influences).
narrative_ontology:cs_reading_relation('d3e91545-ad4e-49d0-b215-128a88c12412', deferential_realism_ontology__hybrid_pragmatic_reading, coexists_with).
narrative_ontology:cs_axiom('d3e91545-ad4e-49d0-b215-128a88c12412', foundational, epsilon_is_a_discoverable_property_of_the_world).
narrative_ontology:cs_axiom_status(epsilon_is_a_discoverable_property_of_the_world, holdable).
narrative_ontology:cs_axiom_grounding('d3e91545-ad4e-49d0-b215-128a88c12412', epsilon_is_a_discoverable_property_of_the_world, empirically_contingent).
narrative_ontology:cs_axiom('d3e91545-ad4e-49d0-b215-128a88c12412', foundational, misclassification_is_always_a_measurement_error_not_a_value_choice).
narrative_ontology:cs_axiom_status(misclassification_is_always_a_measurement_error_not_a_value_choice, holdable).
narrative_ontology:cs_axiom_grounding('d3e91545-ad4e-49d0-b215-128a88c12412', misclassification_is_always_a_measurement_error_not_a_value_choice, empirically_contingent).
narrative_ontology:cs_reference_frame('d3e91545-ad4e-49d0-b215-128a88c12412', typology_as_calibrated_measuring_instrument).
narrative_ontology:cs_drift_state('d3e91545-ad4e-49d0-b215-128a88c12412', contested_periphery_cases_proliferate, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d3e91545-ad4e-49d0-b215-128a88c12412', '').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__immutable_diagnostic_reading, framework_engineers).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__immutable_diagnostic_reading, credentialed_classifiers).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__immutable_diagnostic_reading, institutions_seeking_dispositive_verdicts).
narrative_ontology:constraint_victim(deferential_realism_ontology__immutable_diagnostic_reading, contested_edge_case_disputants).
narrative_ontology:constraint_victim(deferential_realism_ontology__immutable_diagnostic_reading, alternative_framing_theorists).
narrative_ontology:constraint_victim(deferential_realism_ontology__immutable_diagnostic_reading, affected_parties_denied_recourse_by_misclassification).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(deferential_realism_ontology__immutable_diagnostic_reading, institutions_seeking_dispositive_verdicts).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build and maintain the classification engine, define the metric thresholds (extractiveness, suppression, theater_ratio) and the gating logic that turns those metrics into a discrete type. Treat epsilon and the other metrics as discoverable facts about the world rather than choices they made, and enforce this framing by rejecting alternative vocabularies as category errors rather than as competing normative claims.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, framework_engineers, agenda_setter,
    institutional, generational, arbitrage, global).

% Apply the typology professionally — as auditors, policy analysts, expert witnesses — and derive authority from being the ones who can correctly measure epsilon and suppression. Their standing depends on the typology being read as measurement rather than as one normative vocabulary among several; a shift to the rhetorical-scaffold reading would flatten their expertise into advocacy.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, credentialed_classifiers, beneficiary,
    organized, biographical, constrained, national).

% Courts, regulators, and boards that want a closed answer — 'is this a snare or a rope' — rather than an open contest of values. They benefit from the immutable-diagnostic reading's promise of finality, but pay when the instrument's fixed-referent framing forecloses legitimate normative argument they might otherwise have won.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, institutions_seeking_dispositive_verdicts, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(deferential_realism_ontology__immutable_diagnostic_reading, institutions_seeking_dispositive_verdicts, payer).

% Parties whose situation sits at a tangled_rope/snare boundary — where the classification genuinely turns on a judgment about which beneficiaries are legitimate. Under the immutable-diagnostic reading, they are told their disagreement is a measurement error to be corrected by 'better observation,' which forecloses the normative argument they actually want to make about who deserves to benefit.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, contested_edge_case_disputants, payer,
    moderate, biographical, trapped, national).

% Scholars and practitioners advancing the rhetorical-scaffold or hybrid-pragmatic readings. Under this reading's suppression regime, their position is treated as a category confusion — a failure to grasp that the typology has fixed referents — rather than as a live competing account of what the typology is for. They are structurally locked out of the adjudication because the reading they'd need to argue from is precisely what this reading denies exists.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, alternative_framing_theorists, excluded,
    moderate, generational, constrained, global).

% People harmed by a constraint that the instrument classifies as a mountain or rope when their lived experience is closer to snare or tangled_rope. Because the immutable-diagnostic reading treats misclassification as correctable by better measurement rather than as possibly reflecting a contested value judgment, their recourse is limited to petitioning for re-measurement rather than contesting the framework's normative premises.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, affected_parties_denied_recourse_by_misclassification, payer,
    powerless, biographical, trapped, local).

% Watch the kernel dispute itself — the fact that three incompatible readings of what the typology IS coexist and cannot all be true simultaneously without equivocation. They note that this reading's claim to be pure observation is itself a normative and institutional achievement, not a discovery, and that the suppression of alternative framings is the load-bearing mechanism keeping the diagnostic reading dominant.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, meta_theoretical_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, apparently non-arbitrary vocabulary so that disparate institutions (courts, auditors, policy bodies) can converge on classification outcomes without each re-litigating first principles — a genuine coordination gain when disputants actually agree on the underlying facts and disagree only about measurement.
% TRANSFER_FUNCTION: Moves interpretive authority from disputants and alternative-framing theorists to credentialed classifiers and framework engineers, and moves the burden of proof from 'justify your beneficiary structure' to 'produce a better measurement' — a transfer of who gets to close an argument, not merely of resources.
% ABSENT_VOICES: Alternative-framing theorists (rhetorical-scaffold and hybrid-pragmatic advocates) are structurally excluded from adjudicating disputes under this reading, because the reading's own premise is that their position is a category error rather than a competing account. Powerless affected parties whose situations sit on contested normative boundaries are also absent from the room where 'better observation' is defined.
% DISAPPEARANCE_RATIONALE: Framework engineers and institutions seeking dispositive verdicts would say the world rearranges badly — classification disputes would become openly normative and unresolvable by measurement alone, destabilizing every downstream legal and regulatory use of the typology. Alternative-framing theorists and contested-edge-case disputants would say the world barely changes for the underlying facts, only the vocabulary used to argue about them — the normative disagreement was always there, just suppressed by the fixed-referent framing.
% FOUNDING_PROBLEM: Early users of the constraint typology needed a way to stop classification debates from collapsing into pure political assertion — a shared observational vocabulary that could, in principle, be checked against measurable features (extraction rates, suppression levels, enforcement patterns) rather than settled by whoever argued loudest.
% FOUNDING_PROBLEM_CORROBORATION: Framework engineers and credentialed classifiers attest the founding problem remains live — measurement disputes are real and metrics genuinely discriminate cases. Independent commentary from meta-theoretical observers and from hybrid-pragmatic-reading theorists (outside the beneficiary set) attests that the founding problem was only ever partially solved: the core (mountain/rope) is measurement-tractable, but the periphery (snare/tangled_rope boundary) was never purely observational, and treating it as such is itself an unacknowledged normative choice that benefits those empowered to declare 'the measurement is now settled.'
narrative_ontology:disappearance_verdict(deferential_realism_ontology__immutable_diagnostic_reading, contested).
narrative_ontology:founding_problem_status(deferential_realism_ontology__immutable_diagnostic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferential_realism_ontology__immutable_diagnostic_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(deferential_realism_ontology__immutable_diagnostic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(deferential_realism_ontology__immutable_diagnostic_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.58) and theater_ratio (0.42) reflect that the immutable-diagnostic reading genuinely coordinates classification at the mountain/rope core (low-friction, low-extraction), but the periphery — where snare/tangled_rope boundaries turn on contested beneficiary legitimacy — is where the reading's 'it's just measurement' claim does real extractive work: it closes down normative argument by re-describing it as an observational error. Suppression (0.71) is high and rising over the interval because maintaining the fixed-referent framing requires actively rejecting the sibling readings as confusions rather than engaging them as competing accounts — that rejection is an enforcement cost that grows as more edge cases surface and the periphery problem becomes harder to paper over with 'better observation.' All three temporal series share one time grid (T=0 to T=24) as required.
 *
 * PERSPECTIVAL GAP:
 *   From the framework-engineer/credentialed-classifier seats, this reading is simply correct — the typology measures real structural features and disputes are resolved by refining the measurement. From the contested-edge-case-disputant and alternative-framing-theorist seats, the same structure operates as a closure mechanism: their normative disagreement is re-described as their failure to observe correctly, which is a transfer of the argument's terms, not a resolution of it. The engine should compute these seats differently given the divergent power/exit profiles — the point of this story is that seat divergence tracks who is empowered to say 'the measurement is settled.'
 *
 * DIRECTIONALITY LOGIC:
 *   Framework engineers and credentialed classifiers sit near the beneficiary end: they collect interpretive authority and professional standing from the fixed-referent framing being accepted, and their exit options (arbitrage, constrained-but-institutionally-protected) are strong. Contested edge-case disputants and powerless affected parties sit near the target end: trapped exit, no recourse except petitioning for re-measurement within a frame that denies their real objection is available. Alternative-framing theorists are excluded rather than coordinated — their exclusion from adjudication is what maintains the reading's dominance, structurally analogous to how a snare's persistence depends on suppressing alternatives.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (stopping classification debates from collapsing into pure assertion) is only partially live: the core measurement function still works for physically or coordination-grounded cases (true mountains, true ropes). But for the tangled_rope/snare periphery, the founding problem's originally intended solution — shared observation — has been extended by inertia into domains where no purely observational resolution exists, and the 'error correctable by better observation' framing is doing normative work while denying that it is normative work. This is exactly the divergence the corpus exists to surface: claimed_type is tangled_rope (a real coordination function plus real extraction, requiring active enforcement to suppress the sibling readings) rather than mountain or rope, because the reading's persistence depends on active suppression of competing accounts, not on its self-description as pure measurement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    epsilon_discoverability_vs_construction,
    'Are the epsilon (extractiveness), suppression, and theater_ratio values this reading treats as discoverable actually independent of the normative framework used to select and weight the observables that produce them?',
    'Cross-framework audit: apply the hybrid-pragmatic and rhetorical-scaffold readings'' own metric-construction procedures to the same underlying cases and compare resulting classifications. Convergence across frameworks on the periphery cases would support discoverability; divergence would support the constructed-metric hypothesis.',
    'If metrics diverge across frameworks for periphery cases (tangled_rope/snare boundary), the immutable-diagnostic reading''s core claim — that misclassification is a correctable observational error — is falsified for that region, and the reading''s suppression of alternative framings there becomes indistinguishable from enforced normative closure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_discoverability_vs_construction, conceptual, 'Whether ε and related metrics are theory-independent facts or constructions of the immutable-diagnostic frame itself.').

omega_variable(
    kernel_reading_adjudication_authority,
    'Who has standing to adjudicate between the three sibling readings of the deferential_realism_ontology kernel, and does the immutable-diagnostic reading''s dominance reflect a resolved epistemic question or an unresolved distribution of institutional power?',
    'Trace the historical adoption pattern of the typology across institutions: did the immutable-diagnostic framing win because it produced more accurate, checkable predictions, or because institutions preferred its finality and were the ones positioned to enforce its use?',
    'If adoption tracked institutional convenience rather than predictive success, the reading''s dominance is itself evidence for the rhetorical-scaffold reading''s claim that ''snare'' and similar categories are declared rather than discovered — which would not refute this reading''s classification here, but would corroborate the founding_problem_status of ''contested'' rather than ''live.''',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_adjudication_authority, conceptual, 'Whether the reading''s dominance is epistemically earned or institutionally imposed.').

omega_variable(
    false_summit_of_the_typology_itself,
    'Is the immutable-diagnostic reading a genuine mountain-like claim about the typology''s core (mountains, ropes truly are physical/coordination invariants) that has been over-extended into a false summit for the periphery (treating contested normative classification as if it were equally invariant)?',
    'Decompose the typology''s own claim per the ε-invariance principle: test whether core-case classifications (mountain, rope) remain stable under a metric-construction audit while periphery-case classifications (snare, tangled_rope) do not.',
    'Confirms or disconfirms the claimed_type of tangled_rope for THIS reading — if the core is genuinely invariant and only the periphery is constructed-and-enforced, that is precisely the tangled_rope structure (real coordination + real extraction via suppression) authored here rather than a pure mountain or pure snare reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_of_the_typology_itself, empirical, 'Whether this reading over-generalizes a genuine core invariance into a false claim of invariance at the contested periphery.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_ontology__immutable_diagnostic_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(defe_tr_t0, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(defe_tr_t4, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 4, 0.25).
narrative_ontology:measurement(defe_tr_t8, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement(defe_tr_t12, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement(defe_tr_t16, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement(defe_tr_t20, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(defe_tr_t24, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 24, 0.42).

% Extraction over time
narrative_ontology:measurement(defe_be_t0, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(defe_be_t4, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 4, 0.4).
narrative_ontology:measurement(defe_be_t8, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(defe_be_t12, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 12, 0.5).
narrative_ontology:measurement(defe_be_t16, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 16, 0.53).
narrative_ontology:measurement(defe_be_t20, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(defe_be_t24, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(defe_su_t0, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(defe_su_t4, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 4, 0.53).
narrative_ontology:measurement(defe_su_t8, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(defe_su_t12, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 12, 0.64).
narrative_ontology:measurement(defe_su_t16, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 16, 0.67).
narrative_ontology:measurement(defe_su_t20, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement(defe_su_t24, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 24, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deferential_realism_ontology__immutable_diagnostic_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology__rhetorical_scaffold_reading).
narrative_ontology:affects_constraint(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology__hybrid_pragmatic_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the deferential_realism_ontology kernel. The immutable_diagnostic_reading (this file) claims the typology is pure measurement; the rhetorical_scaffold_reading claims it is persuasive normative vocabulary; the hybrid_pragmatic_reading claims a fixed core with a contested normative periphery. Each reading is authored as its own ε-invariant constraint with its own stakeholders, metrics, and type — they are not averaged or blended. The immutable_diagnostic_reading structurally influences (does not foreclose) the rhetorical_scaffold_reading: by claiming the periphery is measurement-tractable, it changes the resource and legitimacy conditions under which rhetorical-scaffold advocacy must operate (it must first dislodge the discoverability claim before its normative-vocabulary claim can be heard). It coexists with the hybrid_pragmatic_reading in ongoing institutional practice — different institutions hold each reading simultaneously without resolution.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
