% ============================================================================
% CONSTRAINT STORY: deferential_realism_ontology__immutable_diagnostic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   human_readable: Immutable Diagnostic Reading of Deferential Realism Ontology
 *   domain: epistemology/normative_theory/institutional_design
 *
 * SUMMARY:
 *   This is the immutable diagnostic reading of the deferential realism
 *   ontology kernel. It treats the six-category constraint typology as an
 *   observational instrument with fixed referents: mountains are physical
 *   invariants, snares are measurable extraction mechanisms, and any
 *   misclassification is an error correctable through better observation.
 *   This reading functions as an epistemic constraint on institutional design
 *   and policy critique by delegitimizing normative contestation over
 *   classification. It is one of three contested readings of the DR ontology,
 *   alongside a rhetorical scaffold reading and a hybrid pragmatic reading.
 *
 * KEY AGENTS:
 *   - Metric authority (agenda_setter / institutional / identity_locked): Adjudicates disputes by appealing to metrics; their authority rests on the ontology's objectivity.
 *   - Institutional adopters (beneficiary / organized / constrained): Gain coordination and legitimacy from the shared framework without bearing administrative costs.
 *   - Policy critics (payer / moderate / constrained): Bear the cost of having normative arguments dismissed as observational error.
 *   - Constructivist scholars (excluded / moderate / trapped): Completely excluded from legitimate dispute resolution because the framework rejects their core methodological premise.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_ontology__immutable_diagnostic_reading, 0.68).
domain_priors:suppression_score(deferential_realism_ontology__immutable_diagnostic_reading, 0.82).
domain_priors:theater_ratio(deferential_realism_ontology__immutable_diagnostic_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__immutable_diagnostic_reading, tangled_rope).
narrative_ontology:human_readable(deferential_realism_ontology__immutable_diagnostic_reading, "Immutable Diagnostic Reading of Deferential Realism Ontology").
narrative_ontology:topic_domain(deferential_realism_ontology__immutable_diagnostic_reading, "epistemology/normative_theory/institutional_design").

domain_priors:requires_active_enforcement(deferential_realism_ontology__immutable_diagnostic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__immutable_diagnostic_reading, '67576f23-ce86-48dc-abf1-7a936434a3a6').
narrative_ontology:cs_kernel_codification('67576f23-ce86-48dc-abf1-7a936434a3a6', formalized).
narrative_ontology:cs_authority_grounding('67576f23-ce86-48dc-abf1-7a936434a3a6', expertise).
narrative_ontology:cs_interpretation_layer_present('67576f23-ce86-48dc-abf1-7a936434a3a6').
narrative_ontology:cs_reading_relation('67576f23-ce86-48dc-abf1-7a936434a3a6', deferential_realism_ontology__rhetorical_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('67576f23-ce86-48dc-abf1-7a936434a3a6', deferential_realism_ontology__hybrid_pragmatic_reading, forecloses).
narrative_ontology:cs_axiom('67576f23-ce86-48dc-abf1-7a936434a3a6', foundational, constraint_kinds_are_natural_kinds).
narrative_ontology:cs_axiom_status(constraint_kinds_are_natural_kinds, holdable).
narrative_ontology:cs_axiom_grounding('67576f23-ce86-48dc-abf1-7a936434a3a6', constraint_kinds_are_natural_kinds, empirically_contingent).
narrative_ontology:cs_axiom('67576f23-ce86-48dc-abf1-7a936434a3a6', foundational, metric_dispute_resolution_is_sufficient).
narrative_ontology:cs_axiom_status(metric_dispute_resolution_is_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('67576f23-ce86-48dc-abf1-7a936434a3a6', metric_dispute_resolution_is_sufficient, instrumental).
narrative_ontology:cs_reference_frame('67576f23-ce86-48dc-abf1-7a936434a3a6', immutable_diagnostic_reference).
narrative_ontology:cs_drift_state('67576f23-ce86-48dc-abf1-7a936434a3a6', contemporary_epistemic_practice, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('67576f23-ce86-48dc-abf1-7a936434a3a6', '').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__immutable_diagnostic_reading, metric_authority).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__immutable_diagnostic_reading, institutional_adopters).
narrative_ontology:constraint_victim(deferential_realism_ontology__immutable_diagnostic_reading, policy_critics).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the constraint typology and adjudicates classification disputes by appealing to observable metrics. Their professional standing depends on the claim that the framework is an immutable diagnostic instrument with fixed referents. They resolve contested classifications by demanding better measurement rather than entertaining normative disagreement.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, metric_authority, agenda_setter,
    institutional, generational, identity_locked, universal).

% Adopt the framework to lend empirical rigor to policy or institutional analysis. They benefit from the coordination and legitimacy of a shared vocabulary without administering the classification apparatus. Switching to an alternative framework would require retraining and discarding accumulated institutional capital.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, institutional_adopters, beneficiary,
    organized, biographical, constrained, global).

% Argue that classifying a mechanism as a snare or tangled rope requires normative judgment about legitimate beneficiaries. Their arguments are treated as misclassification errors correctable through better observation rather than as valid political critique. They bear the cost of having their normative contributions delegitimized within the disciplinary conversation.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, policy_critics, payer,
    moderate, biographical, constrained, national).

% Hold that the six categories are constructed vocabularies rather than natural kinds. They are structurally excluded from classification dispute resolution because the framework only recognizes metric adjudication. Their framing is suppressed as methodological error, leaving them outside the legitimate bounds of the discourse.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, constructivist_scholars, excluded,
    moderate, biographical, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(deferential_realism_ontology__immutable_diagnostic_reading, metric_authority).
narrative_ontology:fixing_cost_class(deferential_realism_ontology__immutable_diagnostic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, transferable vocabulary for classifying social constraints across institutional contexts, enabling cumulative research and comparative analysis without perpetual definitional regress.
% TRANSFER_FUNCTION: Moves epistemic authority from normative deliberation to metric adjudication: classification disputes are resolved by appealing to observable metrics rather than negotiated political judgment, transferring interpretive power to those who control the measurement apparatus and the typology's maintenance.
% ABSENT_VOICES: Constructivist scholars and rhetorical-framework advocates are excluded from dispute resolution; their insistence that 'snare' is a normative declaration is treated as a failure of observation rather than a legitimate competing framing.
% DISAPPEARANCE_RATIONALE: If the immutable diagnostic reading vanished, the field would lose its central arbitration mechanism. Classification disputes would shift to normative negotiation, the authority of metric authorities would dissolve, and institutional adopters would face fragmentation of the shared vocabulary into contested local readings.
% FOUNDING_PROBLEM: The absence of a rigorous, transferable framework for distinguishing extraction from coordination in social mechanisms, leading to inconsistent critique and difficulty building cumulative institutional knowledge.
% FOUNDING_PROBLEM_CORROBORATION: Critical political economists and early institutional designers attest the need for extraction-detection tools from outside the metric-authority seat. However, the claim that immutable diagnostic fixed referents are the only valid solution is corroborated primarily by the metric authorities themselves; normative theorists contest both the exclusivity and the immutability of the proposed solution.
narrative_ontology:disappearance_verdict(deferential_realism_ontology__immutable_diagnostic_reading, world_rearranges).
narrative_ontology:founding_problem_status(deferential_realism_ontology__immutable_diagnostic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferential_realism_ontology__immutable_diagnostic_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-08-04',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(deferential_realism_ontology__immutable_diagnostic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(deferential_realism_ontology__immutable_diagnostic_reading, 0.68, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.68) is high because the reading systematically transfers interpretive authority from normative deliberation to metric adjudication. Suppression (0.82) is higher still: the claim that epsilon is discoverable rather than constructed requires active exclusion of constructivist and rhetorical alternatives. Theater ratio (0.48) reflects the performative aspect of treating contested social classifications as if they were natural-kind diagnoses. Accessibility collapse (0.72) is significant because once the framework is accepted, disagreement appears as ignorance rather than politics. Resistance (0.55) is moderate because the hybrid and rhetorical readings constitute an active oppositional literature.
 *
 * PERSPECTIVAL GAP:
 *   From the metric authority's seat, the arrangement is genuine coordination: a stable vocabulary prevents endless definitional squabble and enables cumulative science. From the policy critic's seat, the same structure operates as extraction: normative disagreement is translated into error and delegitimized. The engine computes this divergence from the structural data; the authored claim does not resolve it.
 *
 * DIRECTIONALITY LOGIC:
 *   Metric authorities and institutional adopters are declared beneficiaries: they receive epistemic authority and coordination benefits respectively, yielding low directionality. Policy critics are declared victims: they bear the cost of delegitimization, yielding high directionality. Constructivist scholars are excluded entirely, sitting at the extreme target end because the constraint is designed to make their exit from suppression impossible.
 *
 * MANDATROPHY ANALYSIS:
 *   The risk of mandatrophy is present but not terminal. The founding problem—distinguishing extraction from coordination—remains live, which prevents pure piton classification. However, the immutable diagnostic reading layers a second extraction mechanism atop the genuine coordination function by forbidding normative contestation of the periphery. That is why the classification is tangled rope rather than rope or snare: the coordination is real, but the enforcement of immutability is asymmetrically extractive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    epsilon_constructed_vs_discovered,
    'Is the base extractiveness of a constraint invariant to the choice of measurement protocol, or is it constructed by the selection of observables?',
    'Empirical test: apply multiple measurement protocols to the same contested constraint; if epsilon varies structurally with the observable chosen, the immutable diagnostic claim is falsified.',
    'If epsilon is constructed, the immutable diagnostic reading collapses toward the hybrid or rhetorical readings, and the typology loses its claim to fixed referents.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(epsilon_constructed_vs_discovered, empirical, 'Whether epsilon values are measurement-invariant or protocol-dependent.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternative framings structural (enforced by institutional gatekeeping) or internalized (adopters believe the framework is objectively true and self-censor)?',
    'Comparative study of dissent expression across institutional contexts with and without formal DR adoption; if suppression persists in informal settings, it is internalized.',
    'If internalized, the constraint''s effective suppression exceeds the structural measure, pushing the computed type closer to snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression of alternative readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_ontology__immutable_diagnostic_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(defe_tr_t0, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(defe_tr_t4, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 4, 0.22).
narrative_ontology:measurement(defe_tr_t8, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(defe_tr_t12, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 12, 0.38).
narrative_ontology:measurement(defe_tr_t16, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 16, 0.43).
narrative_ontology:measurement(defe_tr_t20, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(defe_be_t0, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(defe_be_t4, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(defe_be_t8, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(defe_be_t12, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 12, 0.6).
narrative_ontology:measurement(defe_be_t16, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 16, 0.65).
narrative_ontology:measurement(defe_be_t20, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(defe_su_t0, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(defe_su_t4, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 4, 0.62).
narrative_ontology:measurement(defe_su_t8, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 8, 0.7).
narrative_ontology:measurement(defe_su_t12, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 12, 0.75).
narrative_ontology:measurement(defe_su_t16, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 16, 0.8).
narrative_ontology:measurement(defe_su_t20, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 20, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deferential_realism_ontology__immutable_diagnostic_reading, information_standard).
narrative_ontology:affects_constraint(deferential_realism_ontology__immutable_diagnostic_reading, rhetorical_scaffold_reading).
narrative_ontology:affects_constraint(deferential_realism_ontology__immutable_diagnostic_reading, hybrid_pragmatic_reading).

% DUAL FORMULATION NOTE:
% The deferential_realism_ontology kernel decomposes into three structurally distinct readings because the epsilon value of the ontology itself changes depending on whether it is treated as natural kind, normative vocabulary, or hybrid. This reading (immutable diagnostic) suppresses the others by claiming fixed referents for the entire typology.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
