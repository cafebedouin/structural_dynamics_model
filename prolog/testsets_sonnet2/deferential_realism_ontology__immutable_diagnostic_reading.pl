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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: deferential_realism_ontology__immutable_diagnostic_reading
 *   human_readable: Immutable-Diagnostic Reading of the Constraint Typology
 *   domain: epistemology/normative_theory/institutional_design
 *
 * SUMMARY:
 *   The Deferential Realism framework classifies constraints into six types
 *   using authored metrics (extractiveness, suppression, theater_ratio, etc.)
 *   that the engine computes into per-seat classifications. The
 *   immutable_diagnostic_reading treats this entire apparatus as if it were a
 *   physical measuring instrument: mountains are discovered, snares are
 *   discovered, and any disagreement about a given case's type is evidence of
 *   measurement error — bad metrics, incomplete data, insufficient
 *   observation — never evidence that classification itself requires a
 *   normative judgment about which beneficiary structures are legitimate.
 *   This reading is itself a constraint on discourse: it authorizes
 *   credentialed classifiers and engine maintainers to resolve disputes by
 *   appeal to 'the metrics,' while normative critics who argue the metrics
 *   themselves encode value judgments are recast as making a category error
 *   rather than raising a live methodological objection.
 *
 * KEY AGENTS:
 *   - engine_maintainers: primary agenda-setters (institutional/arbitrage) — administer the schema and metrics
 *   - credentialed_classifiers: primary beneficiaries (organized/mobile) — professional standing rests on the reading's technical framing
 *   - institutions_seeking_neutral_cover: secondary beneficiaries (powerful/constrained) — use the reading to defer contested classification
 *   - normative_critics: primary targets (moderate/constrained) — their normative arguments are recast as measurement errors
 *   - contested_framing_advocates: primary targets (powerless/trapped) — must contest on technical terrain they cannot access
 *   - affected_communities_outside_metric_scope: excluded (powerless/trapped) — harms outside metric scope are read as non-extraction
 *   - rival_reading_advocates: excluded (organized/constrained) — hybrid and rhetorical readings treated as category errors, not live alternatives
 *   - framework_auditors: analytical observer — notes the reading's own tension with the framework's stated claim/metric independence principle
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_ontology__immutable_diagnostic_reading, 0.58).
domain_priors:suppression_score(deferential_realism_ontology__immutable_diagnostic_reading, 0.79).
domain_priors:theater_ratio(deferential_realism_ontology__immutable_diagnostic_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__immutable_diagnostic_reading, tangled_rope).
narrative_ontology:human_readable(deferential_realism_ontology__immutable_diagnostic_reading, "Immutable-Diagnostic Reading of the Constraint Typology").
narrative_ontology:topic_domain(deferential_realism_ontology__immutable_diagnostic_reading, "epistemology/normative_theory/institutional_design").

domain_priors:requires_active_enforcement(deferential_realism_ontology__immutable_diagnostic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__immutable_diagnostic_reading, 'f7a5309f-ded5-42d1-b178-6270f93d89fc').
narrative_ontology:cs_kernel_codification('f7a5309f-ded5-42d1-b178-6270f93d89fc', formalized).
narrative_ontology:cs_authority_grounding('f7a5309f-ded5-42d1-b178-6270f93d89fc', expertise).
narrative_ontology:cs_interpretation_layer_present('f7a5309f-ded5-42d1-b178-6270f93d89fc').
narrative_ontology:cs_reading_relation('f7a5309f-ded5-42d1-b178-6270f93d89fc', deferential_realism_ontology__rhetorical_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('f7a5309f-ded5-42d1-b178-6270f93d89fc', deferential_realism_ontology__hybrid_pragmatic_reading, influences).
narrative_ontology:cs_axiom('f7a5309f-ded5-42d1-b178-6270f93d89fc', foundational, epsilon_is_discoverable_not_constructed).
narrative_ontology:cs_axiom_status(epsilon_is_discoverable_not_constructed, holdable).
narrative_ontology:cs_axiom_grounding('f7a5309f-ded5-42d1-b178-6270f93d89fc', epsilon_is_discoverable_not_constructed, empirically_contingent).
narrative_ontology:cs_axiom('f7a5309f-ded5-42d1-b178-6270f93d89fc', secondary, misclassification_is_always_measurement_error).
narrative_ontology:cs_axiom_status(misclassification_is_always_measurement_error, holdable).
narrative_ontology:cs_axiom_grounding('f7a5309f-ded5-42d1-b178-6270f93d89fc', misclassification_is_always_measurement_error, empirically_contingent).
narrative_ontology:cs_reference_frame('f7a5309f-ded5-42d1-b178-6270f93d89fc', instrument_as_pure_measurement_device).
narrative_ontology:cs_drift_state('f7a5309f-ded5-42d1-b178-6270f93d89fc', contemporary_contested_boundary_cases, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f7a5309f-ded5-42d1-b178-6270f93d89fc', '').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__immutable_diagnostic_reading, engine_maintainers).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__immutable_diagnostic_reading, credentialed_classifiers).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__immutable_diagnostic_reading, institutions_seeking_neutral_cover).
narrative_ontology:constraint_victim(deferential_realism_ontology__immutable_diagnostic_reading, normative_critics).
narrative_ontology:constraint_victim(deferential_realism_ontology__immutable_diagnostic_reading, contested_framing_advocates).
narrative_ontology:constraint_victim(deferential_realism_ontology__immutable_diagnostic_reading, affected_communities_outside_metric_scope).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define and administer the six-category schema and the metrics (extractiveness, suppression, theater_ratio) that adjudicate classification disputes. Treat contested cases as measurement problems to be resolved by refining the instrument, not as sites of irreducible normative judgment. Their authority rests on the claim that the categories track discoverable structure rather than encode a choice among rival framings.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, engine_maintainers, agenda_setter,
    institutional, civilizational, arbitrage, universal).

% Apply the typology professionally — in policy analysis, institutional audits, academic classification work. Benefit from the reading's insistence that disputes are resolved by better observation: this grounds their expertise as technical rather than political, converts contested judgment calls into measurement tasks they are credentialed to perform, and insulates their verdicts from being read as advocacy.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, credentialed_classifiers, beneficiary,
    organized, generational, mobile, global).

% Institutions accused of running extractive arrangements can invoke the immutable-diagnostic reading to demand 'better observation' rather than contest the normative premise that some beneficiary structures are illegitimate. If the metrics can be gamed, delayed, or contested on technical grounds, classification as 'snare' can be indefinitely deferred without conceding the underlying normative dispute exists.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, institutions_seeking_neutral_cover, beneficiary,
    powerful, biographical, constrained, national).

% Argue that classification of tangled_rope and snare cases turns on contested judgments about legitimate beneficiaries — who counts as coordinated versus extracted-from is not settled by measurement alone. Under this reading, their normative arguments are recast as measurement errors or category confusion, foreclosing the venue in which they could contest the underlying value judgment. They bear the cost of having their critique reframed as a technical mistake.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, normative_critics, payer,
    moderate, biographical, constrained, national).

% Communities or advocates who experience an arrangement as extractive but lack the credentialing or resources to contest metric operationalization. The reading requires them to win on the terrain of 'better observation' — instrument calibration, data access, metric design — rather than on the terrain of contested value ('who deserves to benefit'), a terrain shift that systematically disadvantages resource-poor challengers.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, contested_framing_advocates, payer,
    powerless, biographical, trapped, regional).

% Bear costs from arrangements whose extraction is real but not captured by the metrics as currently operationalized (diffuse harms, slow-accumulating costs, harms outside the observed variables). Under the immutable-diagnostic reading their exclusion from the metric surface is read as absence of extraction rather than as a limit of the instrument — they have no standing to argue the instrument itself is incomplete without being told to wait for better measurement.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, affected_communities_outside_metric_scope, excluded,
    powerless, immediate, trapped, local).

% Proponents of the rhetorical_scaffold_reading and hybrid_pragmatic_reading argue classification disputes at the tangled_rope/snare boundary are irreducibly normative or partly so. Under the immutable-diagnostic reading their position is treated as a category error — mistaking a discoverable fact for a policy stance — rather than as a live alternative account of what the typology is doing.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, rival_reading_advocates, excluded,
    organized, generational, constrained, global).

% Study the typology's own operation across readings — including this one — asking whether the immutable-diagnostic reading's confidence in fixed referents is itself a defensible epistemic position or a structural convenience for those who administer and apply the instrument.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, framework_auditors, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, allegedly-objective vocabulary so that disputants across institutions can classify arrangements (mountain/rope/snare/etc.) using common metrics rather than ad hoc, incommensurable moral language — genuinely useful when parties disagree about facts, not values.
% TRANSFER_FUNCTION: Moves the burden of proof in classification disputes from 'is this arrangement's beneficiary structure legitimate' to 'is this measurement accurate' — shifting authority and standing from normative critics and resource-poor challengers toward credentialed measurers and the institutions who can afford to contest metric operationalization.
% ABSENT_VOICES: Advocates of the rhetorical_scaffold_reading and hybrid_pragmatic_reading are excluded from adjudicating boundary cases on their own terms; communities harmed in ways the current metric surface does not capture are excluded from registering that harm as extraction at all, since under this reading absence-from-metric reads as absence-of-fact.
% DISAPPEARANCE_RATIONALE: Engine maintainers and credentialed classifiers would say the world barely changes — the underlying arrangements and their true classifications persist independent of anyone's belief about them, so removing this reading just removes a convenient (if correct) description. Normative critics and rival-reading advocates would say the world rearranges substantially: classification disputes would openly become contests over legitimate beneficiaries rather than contests over instrument calibration, reopening cases this reading currently treats as settled.
% FOUNDING_PROBLEM: Early users of the constraint typology needed a way to distinguish genuine structural analysis from bare accusation — a vocabulary that could not be dismissed as 'just your opinion' when applied to contested institutional arrangements.
% FOUNDING_PROBLEM_CORROBORATION: Engine maintainers and credentialed classifiers attest the problem (distinguishing analysis from accusation) remains live and the fixed-referent framing solves it. Framework auditors — an analytical seat outside the beneficiary set — note that the typology's own documentation (the ε-invariance principle, the explicit claim/metric independence rule, the requirement that divergence between claim and computed type is itself data) presupposes that classification is not simply measurement of a pre-existing fact, which is in tension with this reading's own premise; no source entirely outside the reading's beneficiaries straightforwardly corroborates the 'error correctable by better observation' framing as opposed to the hybrid or rhetorical readings.
narrative_ontology:disappearance_verdict(deferential_realism_ontology__immutable_diagnostic_reading, contested).
narrative_ontology:founding_problem_status(deferential_realism_ontology__immutable_diagnostic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferential_realism_ontology__immutable_diagnostic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extraction (0.58) is moderate-high: the reading's genuine coordination value (a shared vocabulary preventing bare accusation) is real, but its insistence that classification disputes are purely empirical systematically shifts the burden of proof onto normatively-motivated challengers, particularly those without resources to contest metric design. Suppression is high and rising (0.58 → 0.79) because maintaining the 'discoverable fact' framing against two live sibling readings requires increasingly active defense — dismissing rival readings as category errors, treating unresolved boundary disputes as calibration problems rather than open normative questions. Theater ratio is moderate and rising (0.22 → 0.42): as boundary disputes accumulate (tangled_rope/snare cases turning on beneficiary legitimacy), more of the reading's activity becomes performative reassertion that 'better observation' will resolve what is, on the hybrid and rhetorical readings, an irreducibly normative question. Accessibility collapse (0.71) reflects how thoroughly the immutable-diagnostic framing forecloses the venue for advocates to contest the metrics on normative grounds once the framing is accepted. Resistance (0.62) reflects that normative critics and rival-reading advocates continue to press the point rather than accepting the categorical dismissal.
 *
 * DIRECTIONALITY LOGIC:
 *   Engine maintainers and credentialed classifiers sit near the beneficiary end: they administer or apply the instrument and derive authority from its 'discoverable fact' framing. Institutions seeking neutral cover benefit indirectly by using the reading to defer contested classification without conceding the normative point. Normative critics and contested_framing_advocates are targets: their arguments are structurally recast as errors rather than live objections, and the trapped/constrained exit options reflect that they cannot simply opt out of a discourse regime that determines whether their harm is legible as extraction at all. Affected communities outside metric scope are the most severely targeted — their harm literally does not register under the instrument's current operationalization, and this reading treats that non-registration as evidence of non-extraction rather than evidence of an incomplete instrument.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — distinguishing structural analysis from bare accusation — was genuinely live when the typology was introduced and remains partially live today (some classification disputes really are factual: whether enforcement is 'active,' whether a sunset clause exists). But the immutable_diagnostic_reading over-generalizes this genuine core to the contested periphery (tangled_rope/snare boundary cases), where the hybrid_pragmatic_reading holds classification depends partly on normative judgment about legitimate beneficiaries. Treating the whole instrument as measurement-only is a mandatrophy risk: a genuinely useful epistemic discipline (don't just assert 'extraction,' show your metrics) calcifies into a discourse-control mechanism (any objection to a classification must be phrased as a measurement dispute, foreclosing normative objection as such). The classification here as tangled_rope (rather than rope or mountain) registers exactly this: real coordination function (shared vocabulary, defense against bare accusation) coexists with asymmetric extraction (burden-of-proof shift disadvantaging under-resourced normative challengers), sustained by active enforcement (dismissal of rival readings as category errors).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    measurement_vs_construction_of_epsilon,
    'Are the ε values the engine computes genuinely discoverable properties of pre-existing arrangements, or are they constructed by the choice of which metrics to operationalize and which beneficiaries/victims to name in the first place?',
    'Compare classification outcomes across independently-constructed metric sets for the same underlying arrangement; if outcomes are stable across reasonable metric variations, the discoverable-fact reading gains support. If small, defensible variations in metric operationalization flip classification (especially at the tangled_rope/snare boundary), the constructed reading gains support.',
    'If ε is substantially construction-sensitive at contested boundaries, this reading''s core claim — that misclassification is always an error correctable by better observation — is false for exactly the cases (tangled_rope, snare) where it matters most, and the hybrid_pragmatic_reading''s bounded claim becomes the more defensible account.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_vs_construction_of_epsilon, conceptual, 'Whether ε is discovered or constructed at the contested classification boundary.').

omega_variable(
    sibling_reading_foreclosure_scope,
    'Does the immutable_diagnostic_reading''s insistence on fixed referents genuinely foreclose the rhetorical_scaffold_reading''s normative-vocabulary claim, or can both be held by different parties without contradiction (e.g., ''the mountain/rope core is discovered; the snare/tangled_rope periphery is declared'')?',
    'Examine whether any single institutional framework has successfully operated with both readings simultaneously applied to different parts of the typology without internal contradiction — this is effectively the hybrid_pragmatic_reading''s own claim, so its coherent operation would demonstrate coexistence is possible.',
    'If coexistence is demonstrated, the immutable_diagnostic_reading''s totalizing claim (ALL categories are fixed referents) is overreach, and the reading''s suppression of the rhetorical and hybrid readings loses justification, likely reclassifying part of this constraint''s extraction as pure enforcement overreach rather than genuine epistemic defense.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_scope, conceptual, 'Whether the fixed-referent claim can coexist with contested-periphery or normative-vocabulary readings, or genuinely forecloses them.').

omega_variable(
    credentialing_capture_risk,
    'To what extent does the professional benefit accruing to credentialed_classifiers from the fixed-referent framing bias their assessment of whether classification disputes are genuinely resolvable by measurement?',
    'Compare classification judgments made by credentialed classifiers against blinded judgments from parties without a professional stake in the instrument''s objectivity claim, on a matched set of contested boundary cases.',
    'A significant divergence would suggest the immutable-diagnostic reading is partly self-serving rather than epistemically neutral, strengthening the case that this reading is itself an instance of a tangled_rope (genuine coordination function riding alongside asymmetric professional benefit).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(credentialing_capture_risk, empirical, 'Whether professional stake in the fixed-referent framing biases classifiers'' resolution of contested cases.').


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
narrative_ontology:measurement(defe_be_t0, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 0, 0.41).
narrative_ontology:measurement(defe_be_t4, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 4, 0.46).
narrative_ontology:measurement(defe_be_t8, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(defe_be_t12, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 12, 0.53).
narrative_ontology:measurement(defe_be_t16, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(defe_be_t20, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(defe_be_t24, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(defe_su_t0, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(defe_su_t4, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 4, 0.64).
narrative_ontology:measurement(defe_su_t8, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 8, 0.68).
narrative_ontology:measurement(defe_su_t12, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 12, 0.71).
narrative_ontology:measurement(defe_su_t16, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 16, 0.74).
narrative_ontology:measurement(defe_su_t20, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 20, 0.77).
narrative_ontology:measurement(defe_su_t24, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 24, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deferential_realism_ontology__immutable_diagnostic_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(deferential_realism_ontology__immutable_diagnostic_reading, 0.1).
narrative_ontology:affects_constraint(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology__rhetorical_scaffold_reading).
narrative_ontology:affects_constraint(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology__hybrid_pragmatic_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the deferential_realism_ontology kernel. immutable_diagnostic_reading (this file) treats the typology as a fixed-referent measuring instrument; rhetorical_scaffold_reading treats it as a declared, persuasive normative vocabulary; hybrid_pragmatic_reading treats mountains/ropes as fixed and tangled_ropes/snares as normatively contested. Each carries its own ε, beneficiary/victim structure, and classification per the ε-invariance principle — they are not the same constraint viewed three ways, but three structurally distinct claims about what the typology is doing, sharing only the kernel identity and the disputants who hold each position.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
