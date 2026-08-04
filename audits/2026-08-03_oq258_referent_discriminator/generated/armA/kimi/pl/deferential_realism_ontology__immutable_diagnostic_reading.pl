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
    narrative_ontology:constraint_vindicates/2,
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
 *   human_readable: Deferential Realism Ontology — Immutable Diagnostic Reading
 *   domain: epistemology/normative_theory/institutional_design
 *
 * SUMMARY:
 *   The Deferential Realism ontology, under the immutable diagnostic reading,
 *   asserts that its six constraint types map onto fixed structural features
 *   of social reality. Mountains are physical or logical invariants; snares
 *   are measurable extraction mechanisms. This reading treats epsilon as a
 *   discoverable property of constraints and frames classification disputes
 *   as empirical errors resolvable through better observation. The reading
 *   thereby suppresses alternative framings—such as the rhetorical scaffold
 *   reading or hybrid pragmatic reading—by treating their normative claims as
 *   misclassifications. The constraint story models this reading itself as an
 *   active constraint on epistemic practice: it coordinates analysts around a
 *   shared vocabulary while extracting autonomy from dissenting voices and
 *   delegitimizing contestation over the framework's own normative
 *   loadedness.
 *
 * KEY AGENTS:
 *   - framework_architects (agenda_setter / institutional / analytical exit)
 *   - diagnostic_analysts (beneficiary/payer / organized / constrained exit)
 *   - institutional_adopters (beneficiary / powerful / mobile exit)
 *   - constructivist_critics (excluded / moderate / constrained exit)
 *   - pragmatic_reformers (payer / moderate / constrained exit)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_ontology__immutable_diagnostic_reading, 0.62).
domain_priors:suppression_score(deferential_realism_ontology__immutable_diagnostic_reading, 0.78).
domain_priors:theater_ratio(deferential_realism_ontology__immutable_diagnostic_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__immutable_diagnostic_reading, tangled_rope).
narrative_ontology:human_readable(deferential_realism_ontology__immutable_diagnostic_reading, "Deferential Realism Ontology — Immutable Diagnostic Reading").
narrative_ontology:topic_domain(deferential_realism_ontology__immutable_diagnostic_reading, "epistemology/normative_theory/institutional_design").

domain_priors:requires_active_enforcement(deferential_realism_ontology__immutable_diagnostic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__immutable_diagnostic_reading, '2042e75e-3be5-4388-a4cc-d94f26686d03').
narrative_ontology:cs_kernel_codification('2042e75e-3be5-4388-a4cc-d94f26686d03', formalized).
narrative_ontology:cs_authority_grounding('2042e75e-3be5-4388-a4cc-d94f26686d03', expertise).
narrative_ontology:cs_interpretation_layer_present('2042e75e-3be5-4388-a4cc-d94f26686d03').
narrative_ontology:cs_reading_relation('2042e75e-3be5-4388-a4cc-d94f26686d03', deferential_realism_ontology__rhetorical_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('2042e75e-3be5-4388-a4cc-d94f26686d03', deferential_realism_ontology__hybrid_pragmatic_reading, forecloses).
narrative_ontology:cs_axiom('2042e75e-3be5-4388-a4cc-d94f26686d03', foundational, epsilon_is_discoverable).
narrative_ontology:cs_axiom_status(epsilon_is_discoverable, holdable).
narrative_ontology:cs_axiom_grounding('2042e75e-3be5-4388-a4cc-d94f26686d03', epsilon_is_discoverable, empirically_contingent).
narrative_ontology:cs_axiom('2042e75e-3be5-4388-a4cc-d94f26686d03', foundational, typology_has_fixed_referents).
narrative_ontology:cs_axiom_status(typology_has_fixed_referents, holdable).
narrative_ontology:cs_axiom_grounding('2042e75e-3be5-4388-a4cc-d94f26686d03', typology_has_fixed_referents, empirically_contingent).
narrative_ontology:cs_reference_frame('2042e75e-3be5-4388-a4cc-d94f26686d03', objective_diagnostic_framework).
narrative_ontology:cs_drift_state('2042e75e-3be5-4388-a4cc-d94f26686d03', post_constructivist_challenge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2042e75e-3be5-4388-a4cc-d94f26686d03', '').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__immutable_diagnostic_reading, diagnostic_analysts).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__immutable_diagnostic_reading, institutional_adopters).
narrative_ontology:constraint_victim(deferential_realism_ontology__immutable_diagnostic_reading, constructivist_critics).
narrative_ontology:constraint_victim(deferential_realism_ontology__immutable_diagnostic_reading, pragmatic_reformers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(deferential_realism_ontology__immutable_diagnostic_reading, diagnostic_analysts).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__immutable_diagnostic_reading, ontological_realism_about_types).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__immutable_diagnostic_reading, value_free_diagnostic_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the schema, validation rules, and compiler; enforce that classification disputes are resolved by appealing to observable metrics and that epsilon values are treated as discoverable. They define what counts as valid observation and correct classification.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, framework_architects, agenda_setter,
    institutional, generational, analytical, global).

% Apply the DR framework to classify constraints, gaining professional credibility and epistemic authority from the claim that their classifications are objective discoveries. They pay through constrained interpretive latitude: deviation from the diagnostic protocol is treated as methodological error.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, diagnostic_analysts, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(deferential_realism_ontology__immutable_diagnostic_reading, diagnostic_analysts, payer).

% Adopt DR classifications to justify policy positions or institutional designs, leveraging the immutable diagnostic framing to insulate decisions from normative critique and claim scientific grounding.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, institutional_adopters, beneficiary,
    powerful, generational, mobile, national).

% Argue that constraint types and epsilon values are constructed through normative judgment and positional interests rather than discovered. Their framings are excluded from the canonical corpus and treated as failures of observation or methodological competence.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, constructivist_critics, excluded,
    moderate, biographical, constrained, global).

% Seek to adapt the typology or epsilon thresholds based on contextual utility or evolving domains, but are constrained by the immutable claim that types have fixed referents and that proposed deviations represent misclassification rather than legitimate evolution.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, pragmatic_reformers, payer,
    moderate, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, stable typology for classifying social constraints, enabling coordinated analysis across disparate domains and reducing transaction costs in institutional diagnosis.
% TRANSFER_FUNCTION: Moves epistemic authority from contested normative deliberation to appeals to observable metrics; transfers autonomy from dissenting voices to the framework's diagnostic apparatus.
% ABSENT_VOICES: Constructivist critics, pragmatic reformers, and situated communities whose experiences resist fixed categorization are excluded; they would argue that typology construction is inherently political and that epsilon encodes positional interests.
% DISAPPEARANCE_RATIONALE: If the immutable diagnostic claim vanished, the DR community would lose its central authority mechanism; classifications would revert to normative contestation, institutional adopters would lose objective cover, and the shared vocabulary would fragment into competing readings.
% FOUNDING_PROBLEM: The problem of distinguishing genuine coordination from extraction in social and institutional analysis without collapsing into purely subjective normative assertion or unactionable relativism.
% FOUNDING_PROBLEM_CORROBORATION: The problem of coordination-extraction distinction is attested by political economists and institutional designers outside the DR framework's direct beneficiary set; however, the immutable diagnostic resolution is primarily attested by framework architects and analysts who benefit from its objective status, with no independent corroboration that epsilon is fully discoverable.
narrative_ontology:disappearance_verdict(deferential_realism_ontology__immutable_diagnostic_reading, world_rearranges).
narrative_ontology:founding_problem_status(deferential_realism_ontology__immutable_diagnostic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferential_realism_ontology__immutable_diagnostic_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-04',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(deferential_realism_ontology__immutable_diagnostic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(deferential_realism_ontology__immutable_diagnostic_reading, 0.62, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.62) is substantial because the diagnostic claim systematically transfers the burden of proof onto dissenters: to challenge a classification is to assert a failure of observation rather than a legitimate normative disagreement. Suppression (0.78) is high because the framework actively excludes voices that treat epsilon as constructed. Theater ratio (0.45) reflects the growing performative dimension of appeals to observable metrics in disputes where the metrics themselves are theory-laden. Accessibility collapse (0.70) captures how difficult it becomes to articulate alternative framings once the diagnostic vocabulary is entrenched. Resistance (0.55) is moderate because excluded voices persist but are marginalized. The temporal series trace the framework's maturation from a provisional analytical tool (T=0) to a hardened epistemic gate (T=25).
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter and beneficiary seats, the framework appears as a genuine scientific instrument resolving prior anarchy of classification; from the excluded and payer seats, it appears as an epistemic mechanism that preempts normative argument by laundering it into technical error. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Framework architects and diagnostic analysts sit near the beneficiary end: they collect epistemic authority and professional coordination from the constraint's operation. Constructivist critics and pragmatic reformers sit near the target end: they bear the cost of suppressed voice and constrained reform. Institutional adopters are diffuse beneficiaries who leverage the framework's objective aura for policy insulation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—distinguishing coordination from extraction—remains live. The immutable diagnostic reading risks mandatrophy if it persists despite accumulating evidence that contested classifications embed normative judgment (e.g., the epsilon-invariance principle itself requiring decomposition by observable). The temporal measurements show extraction and suppression rising over the interval, suggesting the diagnostic apparatus is hardening against drift rather than correcting it. The status is contested because the problem is genuine but the solution is disputed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    epistemic_status_of_epsilon,
    'Is epsilon genuinely a discoverable property of constraints, or does its measurement necessarily embed the observer''s normative and positional commitments?',
    'Cross-reading inter-rater reliability studies: if analysts with identical data but different normative commitments converge on epsilon, it is discoverable; if convergence tracks normative alignment, it is constructed.',
    'If constructed, the immutable diagnostic reading is a false summit and should reclassify as snare or tangled_rope; if discoverable, the suppression of alternatives is justified as error-correction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_status_of_epsilon, empirical, 'Whether epsilon is observer-independent or constructed').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternative framings structural (exclusion from validation, schema enforcement, canonical corpus) or internalized (analysts self-censor to preserve objective credibility)?',
    'Audit of rejected submissions and analyst interviews post-exit from the framework.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, and the constraint operates partly through identity_coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism').

omega_variable(
    foreclosure_legitimacy,
    'Does the immutable reading''s foreclosure of its siblings rest on a genuine logical contradiction or on a rhetorical assertion of exclusivity?',
    'Formal analysis of the core premises for consistency; assess whether a single framework can hold that mountains are fixed while accepting that snare-classification is normative.',
    'If the foreclosure is rhetorical rather than logical, the reading''s authority rests on extraction rather than expertise.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(foreclosure_legitimacy, conceptual, 'Logical versus rhetorical foreclosure of sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_ontology__immutable_diagnostic_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(defe_tr_t0, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(defe_tr_t5, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 5, 0.24).
narrative_ontology:measurement(defe_tr_t10, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(defe_tr_t15, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement(defe_tr_t20, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(defe_tr_t25, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 25, 0.45).

% Extraction over time
narrative_ontology:measurement(defe_be_t0, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(defe_be_t5, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(defe_be_t10, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(defe_be_t15, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 15, 0.55).
narrative_ontology:measurement(defe_be_t20, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(defe_be_t25, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 25, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(defe_su_t0, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(defe_su_t5, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(defe_su_t10, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(defe_su_t15, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(defe_su_t20, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(defe_su_t25, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 25, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(deferential_realism_ontology__immutable_diagnostic_reading, rhetorical_scaffold_reading).
narrative_ontology:affects_constraint(deferential_realism_ontology__immutable_diagnostic_reading, hybrid_pragmatic_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the deferential_realism_ontology kernel. The kernel decomposes into three structurally distinct constraints: immutable_diagnostic_reading (fixed referents, discoverable epsilon), hybrid_pragmatic_reading (fixed core, contested periphery), and rhetorical_scaffold_reading (normative vocabulary). Each reading carries a different epsilon and stakeholder structure. This story links to its siblings via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
