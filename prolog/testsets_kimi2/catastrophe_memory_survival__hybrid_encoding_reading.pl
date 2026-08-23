% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_survival__hybrid_encoding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_survival__hybrid_encoding_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: catastrophe_memory_survival__hybrid_encoding_reading
 *   human_readable: Catastrophe Memory Survival via Hybrid Ritual Encoding
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint instantiates the hybrid_encoding_reading of the
 *   catastrophe_memory_survival kernel. The claim is that ritual operates on
 *   dual registersâsymbolic boundary-maintenance and embedded practical
 *   knowledgeâwith group survival depending on maintaining both. The
 *   constraint is presented as a structural feature of ritual practice
 *   itself: an irreducible hybridity that resists analytical separation.
 *   Communities maintaining both registers benefit from the redundancy and
 *   cohesion it provides; analysts forcing binary classification bear the
 *   epistemic cost of theoretical misfit. The authored metrics are
 *   deliberately low-extraction and low-suppression, consistent with a
 *   mountain claim, while beneficiary and victim declarations are present to
 *   trigger false-summit evaluationâdocumenting the tension between
 *   naturality and constructed coordination.
 *
 * KEY AGENTS:
 *   - communities_maintaining_registers: Primary beneficiary (organized/identity_locked) â sustained by dual-register ritual performance and generational memory
 *   - analysts_forcing_binary: Primary target (moderate/constrained) â bear epistemic costs of theoretical frameworks that mispredict hybrid practice
 *   - ethnographic_observers: Analytical observer (analytical/analytical) â document the constraint without enacting it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_survival__hybrid_encoding_reading, 0.16).
domain_priors:suppression_score(catastrophe_memory_survival__hybrid_encoding_reading, 0.1).
domain_priors:theater_ratio(catastrophe_memory_survival__hybrid_encoding_reading, 0.06).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, extractiveness, 0.16).
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 0.06).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, accessibility_collapse, 0.86).
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_survival__hybrid_encoding_reading, mountain).
narrative_ontology:human_readable(catastrophe_memory_survival__hybrid_encoding_reading, "Catastrophe Memory Survival via Hybrid Ritual Encoding").
narrative_ontology:topic_domain(catastrophe_memory_survival__hybrid_encoding_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:emerges_naturally(catastrophe_memory_survival__hybrid_encoding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_survival__hybrid_encoding_reading, 'b8894dc7-a3f8-49fb-bd47-9614a013fbe1').
narrative_ontology:cs_kernel_codification('b8894dc7-a3f8-49fb-bd47-9614a013fbe1', distributed).
narrative_ontology:cs_authority_grounding('b8894dc7-a3f8-49fb-bd47-9614a013fbe1', distributed).
narrative_ontology:cs_reading_relation('b8894dc7-a3f8-49fb-bd47-9614a013fbe1', catastrophe_memory_survival__symbol_survival_reading, forecloses).
narrative_ontology:cs_reading_relation('b8894dc7-a3f8-49fb-bd47-9614a013fbe1', catastrophe_memory_survival__competence_transmission_reading, influences).
narrative_ontology:cs_axiom('b8894dc7-a3f8-49fb-bd47-9614a013fbe1', foundational, dual_register_necessity).
narrative_ontology:cs_axiom_status(dual_register_necessity, holdable).
narrative_ontology:cs_axiom_grounding('b8894dc7-a3f8-49fb-bd47-9614a013fbe1', dual_register_necessity, empirically_contingent).
narrative_ontology:cs_axiom('b8894dc7-a3f8-49fb-bd47-9614a013fbe1', foundational, non_reducibility_to_single_register).
narrative_ontology:cs_axiom_status(non_reducibility_to_single_register, holdable).
narrative_ontology:cs_axiom_grounding('b8894dc7-a3f8-49fb-bd47-9614a013fbe1', non_reducibility_to_single_register, conventional).
narrative_ontology:cs_reference_frame('b8894dc7-a3f8-49fb-bd47-9614a013fbe1', dual_register_integrity).
narrative_ontology:cs_drift_state('b8894dc7-a3f8-49fb-bd47-9614a013fbe1', modernization_pressure, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b8894dc7-a3f8-49fb-bd47-9614a013fbe1', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_memory_survival).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__hybrid_encoding_reading, communities_maintaining_registers).
narrative_ontology:constraint_victim(catastrophe_memory_survival__hybrid_encoding_reading, analysts_forcing_binary).
narrative_ontology:constraint_vindicates(catastrophe_memory_survival__hybrid_encoding_reading, dual_register_anthropology).
narrative_ontology:constraint_vindicates(catastrophe_memory_survival__hybrid_encoding_reading, ritual_holism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communities that sustain ritual traditions encoding both symbolic boundary norms and practical survival knowledge; their collective identity, social cohesion, and catastrophe preparedness depend on preserving both registers without theoretically separating one from the other.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, communities_maintaining_registers, beneficiary,
    organized, generational, identity_locked, regional).

% Academic analysts trained in theoretical frameworks that demand classifying ritual into either symbolic or functional categories; their models systematically mispredict or flatten hybrid ritual practice, generating professional friction and epistemic costs when confronted with dual-register phenomena.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, analysts_forcing_binary, payer,
    moderate, biographical, constrained, global).

% Anthropologists and religious studies scholars who observe and document hybrid ritual encoding without imposing binary classification; they occupy an analytical seat outside the register tension and serve as witnesses to the constraint's structural persistence.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, ethnographic_observers, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Transmits collective memory and survival-relevant knowledge across generations by embedding both symbolic identity-markers and practical adaptation protocols in unified ritual performance, solving the archive problem for non-literate or displaced societies.
% TRANSFER_FUNCTION: Moves cognitive effort and theoretical risk from communities (who sustain the hybrid encoding through repeated performance) to analysts (who expend labor trying to separate the registers), while preserving redundancy in social memory within the community.
% ABSENT_VOICES: Communities that have already lost either the symbolic or practical register due to colonial disruption or modernization are absent from the survival narrative; pure symbolic or pure competence theorists are present in the discipline but structurally dominate discourse.
% DISAPPEARANCE_RATIONALE: If hybrid ritual encoding vanished, some communities would lose the redundancy that buffers catastrophic memory loss, but modernization advocates argue that literacy and formal institutions have superseded the need; the parties dispute whether the dual-register structure is necessary or replaceable.
% FOUNDING_PROBLEM: How to preserve actionable survival knowledge and social cohesion across generations without stable archives, literacy, or centralized institutions, especially under conditions of catastrophe or displacement.
% FOUNDING_PROBLEM_CORROBORATION: Ethnographic and disaster-studies researchers outside the benefiting communities attest that ritual performance correlates with survival outcomes in catastrophe contexts; modernization theorists and development economists attest the problem is superseded by contemporary institutions, providing external corroboration for both live and dead readings.
narrative_ontology:disappearance_verdict(catastrophe_memory_survival__hybrid_encoding_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_memory_survival__hybrid_encoding_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_survival__hybrid_encoding_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_survival__hybrid_encoding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_survival__hybrid_encoding_reading, 0.16, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_survival__hybrid_encoding_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, ExtMetricName, E),
    domain_priors:suppression_score(catastrophe_memory_survival__hybrid_encoding_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(catastrophe_memory_survival__hybrid_encoding_reading),
    narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(catastrophe_memory_survival__hybrid_encoding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.16) because the constraint's primary operation is informational coordination, not rent extraction. Suppression is low (0.10) because the constraint persists through embodied performance and identity fusion rather than active coercion. Accessibility collapse is high (0.86): once the dual-register structure is recognized, reductionist alternatives lose explanatory power. Resistance is low (0.18): reductionist academics resist the framing, but the constraint does not require their assent to persist. Theater ratio is minimal (0.06): there is little performative maintenance separate from function. Suppression is authored as a raw structural property; only extractiveness is scaled by directionality and scope in the engine.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat (communities) experiences the constraint as lived identity and survival infrastructure; the payer seat (analysts) experiences it as theoretical obstruction. The engine will compute divergent classifications: communities near dâ0.2 (subsidized by the constraint's coordination), analysts near dâ0.8 (targeted by its epistemic friction). The divergence is structural, not perspectival illusion.
 *
 * DIRECTIONALITY LOGIC:
 *   Communities are beneficiaries because the constraint directly subsidizes their collective memory and cohesion; their identity_locked exit amplifies the subsidy effect, pulling d toward the beneficiary pole. Analysts are victims because their constrained disciplinary frameworks force them to pay a cognitive cost; the extraction is epistemic rather than material, but the structural derivation still places them near the target end. Observers sit near symmetric with analytical exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The low theater ratio and low suppression argue against piton classification. The founding problemâpreserving survival knowledge without archivesâis contested but not dead, and the constraint's persistence is tied to ongoing ritual practice rather than institutional inertia. The presence of beneficiaries on a mountain claim triggers false-summit evaluation, ensuring that a merely naturalized coordination does not escape extractive scrutiny.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_hybridity,
    'Is the dual-register nature of ritual a fixed structural feature of collective memory, or a constructed coordination arrangement that benefits communities at the expense of analytical clarity?',
    'Comparative ethnography of communities with varying register separation: if communities that separate symbolic and practical functions show equivalent survival outcomes, the hybridity is constructed; if separation consistently correlates with memory loss or cohesion breakdown, it is structural.',
    'If constructed, FSM reclassification to tangled_rope or rope is warranted; if structural, the mountain classification holds despite beneficiary presence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_hybridity, conceptual, 'Ambiguity between natural-law and constructed coordination in ritual hybrid encoding').

omega_variable(
    register_separability_framing,
    'Can symbolic boundary-maintenance and practical knowledge be analytically separated without destroying the ritual''s survival function, or are they constitutively entangled?',
    'Intervention studies or historical natural experiments where one register was suppressed (e.g., colonial prohibitions on symbolic practice while retaining practical knowledge, or vice versa) and subsequent survival outcomes observed.',
    'If separable without loss, the constraint is weaker than claimed (lower accessibility_collapse); if entangled, the dual-register claim is vindicated as a genuine structural limit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(register_separability_framing, empirical, 'Whether the two ritual registers are constitutively entangled or merely correlated').

omega_variable(
    analyst_extraction_or_cognitive_friction,
    'Do analysts forcing binary classification suffer genuine extraction (career costs, funding constraints) from the hybrid constraint, or merely cognitive friction?',
    'Trace funding and citation patterns: do hybrid-framed studies face systematic exclusion from top journals or grants compared to binary-framed studies?',
    'If extraction is real, the victim declaration is structurally grounded; if mere friction, the victim set should be reclassified as excluded or observer.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(analyst_extraction_or_cognitive_friction, empirical, 'Whether analyst victimhood is extractive or purely epistemic').

omega_variable(
    kernel_codification_ambiguity,
    'Should the catastrophe_memory_survival kernel be read as distributed (competing scholarly and community readings) or implicit (the kernel is whatever ritual practice does, without codified adjudication)?',
    'Historical sociology of the field: trace whether the kernel emerged as an explicit scholarly hypothesis or was retroactively imposed on observed practice.',
    'If implicit, the commitment system framing itself may be an analyst imposition, weakening the CS classification; if distributed, the sibling readings are genuinely competing codifications.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_codification_ambiguity, conceptual, 'Ambiguity in how the kernel itself is codified and whether CS framing is warranted').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_survival__hybrid_encoding_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(catastrophe_hybrid_tr_t0, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 0, 0.04).
narrative_ontology:measurement(catastrophe_hybrid_tr_t10, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 10, 0.05).
narrative_ontology:measurement(catastrophe_hybrid_tr_t20, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 20, 0.05).
narrative_ontology:measurement(catastrophe_hybrid_tr_t30, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 30, 0.06).
narrative_ontology:measurement(catastrophe_hybrid_tr_t40, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 40, 0.07).
narrative_ontology:measurement(catastrophe_hybrid_tr_t50, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 50, 0.08).

% Extraction over time
narrative_ontology:measurement(catastrophe_hybrid_be_t0, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(catastrophe_hybrid_be_t10, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 10, 0.13).
narrative_ontology:measurement(catastrophe_hybrid_be_t20, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 20, 0.14).
narrative_ontology:measurement(catastrophe_hybrid_be_t30, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 30, 0.15).
narrative_ontology:measurement(catastrophe_hybrid_be_t40, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 40, 0.16).
narrative_ontology:measurement(catastrophe_hybrid_be_t50, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 50, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(catastrophe_hybrid_su_t0, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 0, 0.06).
narrative_ontology:measurement(catastrophe_hybrid_su_t10, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 10, 0.07).
narrative_ontology:measurement(catastrophe_hybrid_su_t20, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 20, 0.08).
narrative_ontology:measurement(catastrophe_hybrid_su_t30, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 30, 0.09).
narrative_ontology:measurement(catastrophe_hybrid_su_t40, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 40, 0.1).
narrative_ontology:measurement(catastrophe_hybrid_su_t50, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 50, 0.11).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(catastrophe_memory_survival__hybrid_encoding_reading, symbol_survival_reading).
narrative_ontology:affects_constraint(catastrophe_memory_survival__hybrid_encoding_reading, competence_transmission_reading).

% DUAL FORMULATION NOTE:
% The catastrophe_memory_survival kernel decomposes into three structurally distinct claims: symbol_survival_reading (symbolic continuity alone explains survival), competence_transmission_reading (practical knowledge transmission alone explains survival), and hybrid_encoding_reading (both registers are necessary and inseparable). Each has distinct epsilon, beneficiary structures, and victim sets.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
