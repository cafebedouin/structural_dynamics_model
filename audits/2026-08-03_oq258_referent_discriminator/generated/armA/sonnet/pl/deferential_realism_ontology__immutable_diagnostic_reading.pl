% ============================================================================
% CONSTRAINT STORY: deferential_realism_ontology__immutable_diagnostic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: The Constraint Typology as Fixed-Referent Diagnostic Instrument
 *   domain: epistemology/normative_theory/institutional_design
 *
 * SUMMARY:
 *   This constraint is the 'immutable diagnostic' reading of a kernel contest
 *   over what the constraint typology itself IS — an instrument (this
 *   reading), a normative vocabulary (rhetorical_scaffold_reading), or a
 *   hybrid with a fixed core and contested periphery
 *   (hybrid_pragmatic_reading). Under this reading, mountains are physical
 *   invariants and snares are measurable extraction mechanisms;
 *   classification disputes are, in principle, resolvable by better
 *   observation, and epsilon is treated as discovered rather than authored.
 *   The reading's own operation is what is under evaluation here: treating
 *   classification as pure observation is itself doing normative work — it
 *   forecloses the argument that a verdict is partly a choice about which
 *   beneficiaries count as legitimate. The story authors this reading's own
 *   epsilon for the standing arrangement (the typology as currently practiced
 *   by its certifying institutions), not for any endorsed alternative.
 *
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
narrative_ontology:human_readable(deferential_realism_ontology__immutable_diagnostic_reading, "The Constraint Typology as Fixed-Referent Diagnostic Instrument").
narrative_ontology:topic_domain(deferential_realism_ontology__immutable_diagnostic_reading, "epistemology/normative_theory/institutional_design").

domain_priors:requires_active_enforcement(deferential_realism_ontology__immutable_diagnostic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__immutable_diagnostic_reading, '28d5f714-a0ea-4baa-8352-b95fd4677c5d').
narrative_ontology:cs_kernel_codification('28d5f714-a0ea-4baa-8352-b95fd4677c5d', formalized).
narrative_ontology:cs_authority_grounding('28d5f714-a0ea-4baa-8352-b95fd4677c5d', expertise).
narrative_ontology:cs_interpretation_layer_present('28d5f714-a0ea-4baa-8352-b95fd4677c5d').
narrative_ontology:cs_reading_relation('28d5f714-a0ea-4baa-8352-b95fd4677c5d', deferential_realism_ontology__rhetorical_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('28d5f714-a0ea-4baa-8352-b95fd4677c5d', deferential_realism_ontology__hybrid_pragmatic_reading, influences).
narrative_ontology:cs_axiom('28d5f714-a0ea-4baa-8352-b95fd4677c5d', foundational, epsilon_is_discoverable_not_constructed).
narrative_ontology:cs_axiom_status(epsilon_is_discoverable_not_constructed, holdable).
narrative_ontology:cs_axiom_grounding('28d5f714-a0ea-4baa-8352-b95fd4677c5d', epsilon_is_discoverable_not_constructed, empirically_contingent).
narrative_ontology:cs_axiom('28d5f714-a0ea-4baa-8352-b95fd4677c5d', secondary, misclassification_is_correctable_observation_error).
narrative_ontology:cs_axiom_status(misclassification_is_correctable_observation_error, holdable).
narrative_ontology:cs_axiom_grounding('28d5f714-a0ea-4baa-8352-b95fd4677c5d', misclassification_is_correctable_observation_error, instrumental).
narrative_ontology:cs_reference_frame('28d5f714-a0ea-4baa-8352-b95fd4677c5d', fixed_referent_measurement_regime).
narrative_ontology:cs_drift_state('28d5f714-a0ea-4baa-8352-b95fd4677c5d', contemporary_contested_application, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('28d5f714-a0ea-4baa-8352-b95fd4677c5d', '').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__immutable_diagnostic_reading, framework_certifying_analysts).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__immutable_diagnostic_reading, institutions_seeking_naturalized_legitimacy).
narrative_ontology:constraint_victim(deferential_realism_ontology__immutable_diagnostic_reading, contested_classification_disputants).
narrative_ontology:constraint_victim(deferential_realism_ontology__immutable_diagnostic_reading, alternative_framing_proponents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Apply the typology's metrics (extractiveness, suppression, accessibility_collapse) as if reading off a thermometer, adjudicating classification disputes by appeal to 'better observation.' Their authority rests on the claim that epsilon is discovered, not constructed — a claim that, if it held, would make their diagnostic role uncontestable. They administer the instrument and gain standing whenever a dispute is resolved by appeal to the metrics rather than to values.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, framework_certifying_analysts, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(deferential_realism_ontology__immutable_diagnostic_reading, framework_certifying_analysts, beneficiary).

% Institutions accused of running snares or tangled ropes benefit when the typology is treated as a fixed-referent instrument, because a 'mountain' or 'rope' verdict forecloses further normative argument — the classification becomes a fact rather than a contested judgment. They can commission or cite 'measurement' to close down critique.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, institutions_seeking_naturalized_legitimacy, beneficiary,
    powerful, generational, mobile, global).

% Parties who believe a given arrangement is extractive but are told the classification is a matter of observation, not values, and that their disagreement reflects measurement error rather than a legitimate normative dispute. They must argue on the instrument's terms (produce better metrics) rather than contest the framing itself, which is costly and often technically inaccessible to them.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, contested_classification_disputants, payer,
    moderate, biographical, constrained, national).

% Scholars and advocates who hold that classification is partly constructed (the rhetorical_scaffold_reading or hybrid_pragmatic_reading) are treated as making a category error rather than a competing epistemic claim. Their framing is not refuted on its merits within this reading; it is excluded from the space of legitimate disagreement by definitional fiat — the instrument's fixed-referent premise makes their position appear simply mistaken rather than contestable.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, alternative_framing_proponents, excluded,
    moderate, biographical, trapped, global).

% Meta-theorists studying how the typology itself functions — including this very story — who note that treating epsilon as discoverable is itself a normative commitment with distributive consequences, not a neutral empirical stance.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, epistemic_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared vocabulary and metric surface so that disputes about institutional arrangements can be adjudicated by appeal to observable structural properties rather than by raw assertion of interest — in principle, this reduces pure rhetorical warfare by giving all parties a common measurement language.
% TRANSFER_FUNCTION: Moves argumentative burden from institutions accused of extraction onto disputants, who must now produce competing measurements rather than competing values; it also moves legitimacy from contested political judgment toward the analysts who administer the instrument.
% ABSENT_VOICES: Proponents of the rhetorical_scaffold_reading and hybrid_pragmatic_reading are structurally present as sibling constraints but are not admitted as co-equal epistemic partners within this reading — their claim that classification is partly declared rather than discovered is treated as a failure to observe carefully, not as a live alternative account of what classification IS.
% DISAPPEARANCE_RATIONALE: If the fixed-referent premise were abandoned overnight, every existing classification verdict issued under this reading would become contestable again on normative grounds; institutions currently shielded by a 'mountain' or 'rope' verdict would face renewed argument about legitimate beneficiaries, and the certifying-analyst role would lose its distinctive authority (a role grounded in adjudicating values, rather than reading instruments, is a different institutional position).
% FOUNDING_PROBLEM: Constraint disputes were previously settled by whoever had more rhetorical or political power, with no shared vocabulary for distinguishing genuine coordination problems from extraction dressed as coordination — the typology was built to give analysts a common structural language for that distinction.
% FOUNDING_PROBLEM_CORROBORATION: Analysts within the framework attest the founding problem remains live (disputes still need adjudication) and that the fixed-referent reading solves it by removing observer bias. Meta-theorists and proponents of the sibling readings — outside the certifying-analyst beneficiary group — attest that the 'discoverable epsilon' premise itself encodes a contestable metaethical choice, and that this reading's own successful operation (resolving disputes definitively) is evidence FOR the constructedness the reading denies, not against it.
narrative_ontology:disappearance_verdict(deferential_realism_ontology__immutable_diagnostic_reading, world_rearranges).
narrative_ontology:founding_problem_status(deferential_realism_ontology__immutable_diagnostic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferential_realism_ontology__immutable_diagnostic_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-04',
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
 *   Extractiveness (0.58) reflects that treating classification as pure measurement transfers real argumentative and legitimating power to whoever administers the instrument, while suppressing the normative-construction critique as a category error rather than answering it. Suppression (0.71) is high and rising over the interval because the reading's persistence depends on actively excluding the constructedness argument from the space of live disagreement — this is a raw structural property, not scaled by scope in the authored value. Theater ratio (0.42) captures that a meaningful share of 'we measured it' activity is performative: appeals to observable metrics function partly to end debate rather than to actually update belief, and this share has grown as the framework has been more widely cited to close down critique.
 *
 * PERSPECTIVAL GAP:
 *   From the certifying-analyst seat, this reading is coordination: a shared, falsifiable vocabulary that prevents disputes from collapsing into naked power contests. From the excluded alternative-framing-proponent seat, the same structure operates as extraction of legitimacy — a discovered-not-constructed premise that happens to always resolve in favor of whoever controls the instrument's application. The engine should compute these as different per-seat types from the same structural data; the divergence is exactly the phenomenon this story is documenting, one level up, about the typology itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Framework-certifying analysts sit at the beneficiary end: they gain standing and reduced argumentative burden whenever a dispute resolves by appeal to metrics they administer. Institutions accused of extraction likewise benefit — a naturalized 'mountain' or 'rope' verdict forecloses further normative challenge, which is a direct subsidy relative to having to defend the arrangement on the merits. Contested classification disputants and alternative-framing proponents bear the cost: their normative objections are recast as observational error, raising the cost of dissent (they must produce competing measurements, not competing values) and, for the framing proponents, excluding their account of what classification IS from serious consideration.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — settling disputes without pure rhetorical warfare — remains partly live, which is why this reading cannot be dismissed as pure zombie extraction; a genuine coordination function (a shared vocabulary reducing arbitrary power contests) is real. But the specific claim that epsilon is DISCOVERED rather than authored has outlived whatever epistemic warrant it once had, given that the sibling readings persist as live, reasoned positions held by serious parties rather than as refuted errors. Classifying this as tangled_rope rather than mountain or rope captures both halves: real coordination function, plus asymmetric extraction (legitimacy accrues to the instrument's administrators and to institutions it happens to naturalize) sustained by active suppression of the constructedness critique.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    epsilon_discovered_or_authored,
    'Is epsilon (base extractiveness) for any given constraint a discoverable physical/economic fact, or is it partly constituted by the observer''s choice of referent, beneficiary boundary, and time horizon?',
    'Examine whether independent analysts, given the same raw structural data but different beneficiary-boundary assumptions, converge on the same epsilon. Convergence would support the discoverable-fact reading; systematic divergence tracking analyst interest or framing would support the constructed reading.',
    'If epsilon is substantially observer-relative, this reading''s core premise (fixed referents, correctable misclassification) is itself a normative choice masquerading as measurement, which strengthens the case for classifying this reading''s own operation as tangled_rope rather than mountain — exactly the classification authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_discovered_or_authored, conceptual, 'Whether the typology''s central metric is discovered or constructed is the load-bearing uncertainty for the whole kernel contest.').

omega_variable(
    committer_structure_kernel_disagreement,
    'Where exactly do the three sibling readings of deferential_realism_ontology locate their disagreement — is it about which constraints have fixed referents (hybrid_pragmatic vs. immutable_diagnostic) or about whether ANY constraint has a fixed referent independent of declared beneficiary legitimacy (rhetorical_scaffold vs. the other two)?',
    'Formal comparison of each reading''s axiom set against specific contested classification cases (e.g., a real tangled_rope dispute) to see which axioms actually do the adjudicating work in practice versus which are rhetorical framing.',
    'If the disagreement is narrower than it appears (all three agree mountains are fixed, disagree only about snares/tangled_ropes), the immutable_diagnostic_reading''s distinctive claim collapses toward the hybrid_pragmatic_reading, weakening its independent standing as a reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_structure_kernel_disagreement, conceptual, 'Locates precisely where this reading''s premise diverges from its siblings, per Rule 2 (committer content routed to omega).').

omega_variable(
    false_summit_of_the_instrument_itself,
    'Does the immutable_diagnostic_reading''s own claim to be a neutral instrument function as a false-summit mountain — presenting a constructed, beneficiary-laden classification practice as natural/discovered fact?',
    'Track whether classification verdicts issued under this reading systematically favor institutions with resources to commission favorable ''measurement,'' controlling for actual structural extraction levels.',
    'A systematic favorability pattern would confirm the tangled_rope classification authored here and would suggest the reading itself is a textbook instance of the false-summit pattern the broader framework is designed to detect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_of_the_instrument_itself, empirical, 'Tests whether this reading''s naturalization move is itself an instance of the phenomenon (false summit) the typology exists to catch.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_ontology__immutable_diagnostic_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(defe_tr_t0, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(defe_tr_t4, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 4, 0.27).
narrative_ontology:measurement(defe_tr_t8, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement(defe_tr_t12, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 12, 0.34).
narrative_ontology:measurement(defe_tr_t16, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement(defe_tr_t20, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(defe_tr_t24, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 24, 0.42).

% Extraction over time
narrative_ontology:measurement(defe_be_t0, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 0, 0.44).
narrative_ontology:measurement(defe_be_t4, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(defe_be_t8, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 8, 0.51).
narrative_ontology:measurement(defe_be_t12, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 12, 0.53).
narrative_ontology:measurement(defe_be_t16, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(defe_be_t20, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(defe_be_t24, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(defe_su_t0, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(defe_su_t4, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 4, 0.56).
narrative_ontology:measurement(defe_su_t8, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 8, 0.61).
narrative_ontology:measurement(defe_su_t12, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 12, 0.65).
narrative_ontology:measurement(defe_su_t16, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(defe_su_t20, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(defe_su_t24, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 24, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology__rhetorical_scaffold_reading).
narrative_ontology:affects_constraint(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology__hybrid_pragmatic_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the deferential_realism_ontology kernel. immutable_diagnostic_reading (this file) claims fixed referents and discoverable epsilon; rhetorical_scaffold_reading claims classification is normatively declared rather than discovered; hybrid_pragmatic_reading claims a fixed core (mountain/rope) with a contested periphery (tangled_rope/snare). Each reading is authored as its own ε-invariant constraint with its own stakeholders and metrics per the ε-invariance principle; they are linked here rather than merged because measuring 'the constraint typology' by this reading's lights versus the rhetorical_scaffold lights yields structurally different epsilon and beneficiary sets.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
