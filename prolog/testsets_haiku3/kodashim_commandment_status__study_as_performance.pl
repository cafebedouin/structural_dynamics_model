% ============================================================================
% CONSTRAINT STORY: kodashim_commandment_status__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_commandment_status__study_as_performance, []).

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
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: kodashim_commandment_status__study_as_performance
 *   human_readable: Study-as-Performance Fulfillment of Kodashim Commandment
 *   domain: religious/halakhic
 *
 * SUMMARY:
 *   Under this reading of the kodashim (sacrifice laws) commandment, the
 *   obligation to study the laws of Temple sacrifice fulfills the commandment
 *   itself. After the Temple's destruction (70 CE), when physical sacrifice
 *   became impossible, the halakhic system generated an equivalence:
 *   intensive study of the sacrificial laws is not a substitute for or a
 *   consolation about the commandment, but constitutes direct fulfillment.
 *   The kernel under contest is the status of the commandment
 *   post-destruction: does it remain fully operative (via study), is it
 *   suspended pending Messianic restoration, or does it become a husk (no
 *   performance possible, no fulfillment available)? This constraint story
 *   instantiates the 'study-as-performance' reading, which asserts that the
 *   commandment kernel is continuously occupied and its obligation is fully
 *   maintained through intellectual engagement with the textual tradition.
 *
 * KEY AGENTS:
 *   - torah_scholars: Primary agents who study the laws; through their practice, the reading claims the commandment is fulfilled. Structurally positioned as beneficiaries because the reading assigns them the role of maintaining the commandment's force through their interpretive work.
 *   - jewish_community: The broader community whose religious obligation is said to be satisfied by the scholarly class's study. Structurally positioned as secondary beneficiary (indirect satisfaction through representatives).
 *   - actual_performance: Not an agent, but the absent capacity. The reading's power lies in converting absence (no Temple, no altar) into equivalence (study substitutes nothing — it IS the performance now).
 *   - messianic_expectation: The eschatological horizon. Under this reading, restoration is not required for the commandment to have force now; under the messianic_deferral reading, the commandment is suspended pending restoration. This reading asserts the commandment is present and active, which influences (but does not necessarily foreclose) the messianic reading.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_commandment_status__study_as_performance, 0.0).
domain_priors:suppression_score(kodashim_commandment_status__study_as_performance, 0.0).
domain_priors:theater_ratio(kodashim_commandment_status__study_as_performance, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, extractiveness, 0.0).
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_commandment_status__study_as_performance, mountain).
narrative_ontology:human_readable(kodashim_commandment_status__study_as_performance, "Study-as-Performance Fulfillment of Kodashim Commandment").
narrative_ontology:topic_domain(kodashim_commandment_status__study_as_performance, "religious/halakhic").

domain_priors:emerges_naturally(kodashim_commandment_status__study_as_performance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_commandment_status__study_as_performance, '55dcdb6e-d305-4d81-b56a-1bf20c06ca98').
narrative_ontology:cs_kernel_codification('55dcdb6e-d305-4d81-b56a-1bf20c06ca98', fixed_text).
narrative_ontology:cs_authority_grounding('55dcdb6e-d305-4d81-b56a-1bf20c06ca98', lineage).
narrative_ontology:cs_interpretation_layer_present('55dcdb6e-d305-4d81-b56a-1bf20c06ca98').
narrative_ontology:cs_reading_relation('55dcdb6e-d305-4d81-b56a-1bf20c06ca98', kodashim_commandment_status__performance_only, forecloses).
narrative_ontology:cs_reading_relation('55dcdb6e-d305-4d81-b56a-1bf20c06ca98', kodashim_commandment_status__messianic_deferral, influences).
narrative_ontology:cs_axiom('55dcdb6e-d305-4d81-b56a-1bf20c06ca98', foundational, study_constitutes_fulfillment).
narrative_ontology:cs_axiom_status(study_constitutes_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('55dcdb6e-d305-4d81-b56a-1bf20c06ca98', study_constitutes_fulfillment, deontological).
narrative_ontology:cs_axiom('55dcdb6e-d305-4d81-b56a-1bf20c06ca98', foundational, continuous_present_force).
narrative_ontology:cs_axiom_status(continuous_present_force, holdable).
narrative_ontology:cs_axiom_grounding('55dcdb6e-d305-4d81-b56a-1bf20c06ca98', continuous_present_force, deontological).
narrative_ontology:cs_reference_frame('55dcdb6e-d305-4d81-b56a-1bf20c06ca98', commandment_fully_operative_through_study).
narrative_ontology:cs_created_at('55dcdb6e-d305-4d81-b56a-1bf20c06ca98', '2026-06-12T14:37:00Z').
narrative_ontology:cs_kernel_id(kodashim_commandment_status__study_as_performance, kodashim_commandment_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__study_as_performance, torah_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__study_as_performance, jewish_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Engage in intensive study of the sacrifice laws (kodashim), particularly through Talmudic analysis and legal reasoning. Under this reading, their intellectual work constitutes the fulfillment of the commandment itself. They are structurally the seat through which the community's obligation is satisfied. Their practice is both their primary identity (Torah scholar) and the mechanism by which the reading's claim is grounded.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, torah_scholars, beneficiary,
    organized, generational, identity_locked, global).

% The broader Jewish religious community whose obligation to the sacrifice commandment is met (under this reading) through the scholarly class's study. They do not directly perform sacrifice; instead, they benefit from the scholars' intellectual engagement, which maintains the commandment's force and continuity. They are constrained by the halakhic framework and by communal religious identity.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, jewish_community, beneficiary,
    organized, generational, constrained, global).

% The eschatological horizon that the sibling readings reference differently. Under study_as_performance, Messianic restoration is not required for the commandment to have present force; under messianic_deferral, restoration is the expected moment of re-activation. This reading treats restoration as not necessary for commandment fulfillment, which influences (does not foreclose) the deferral reading's eschatological framing.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, messianic_expectation, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(kodashim_commandment_status__study_as_performance, messianic_expectation).

% The historical event (70 CE) that triggered the kernel contest. The reading interprets this as a transformation of the commandment's mechanism, not as creating an obligation-gap or a suspension. Recorded here as a non-agent entity that frames the halakhic problem.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, temple_destruction, observer,
    analytical, civilizational, analytical, local).
narrative_ontology:stakeholder_non_agent(kodashim_commandment_status__study_as_performance, temple_destruction).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains the intellectual and spiritual continuity of the Jewish covenant relationship and divine law after the loss of the Temple's sacrificial mechanism. Coordinates the community's religious identity around intensive engagement with halakhic texts, preventing the loss of the Temple from becoming a rupture in the covenant tradition.
% TRANSFER_FUNCTION: Transfers the locus of commandment fulfillment from physical performance (sacrifice at the altar) to intellectual performance (study and reasoning about the laws). No material goods move; instead, the obligation's site and mechanism are reconfigured. What flows is the community's religious obligation from the material altar to the scholar's engagement with the textual tradition.
% ABSENT_VOICES: Those who maintain the performance_only reading (for whom the commandment has become void) are not directly seated in the community that accepts study_as_performance. They would argue that intellectual study, however rigorous, does not constitute actual fulfillment and that the commandment remains suspended/voided. Also absent are those who advocate messianic_deferral but reject the claim that study provides full present fulfillment — they see study as maintenance of readiness, not as satisfaction of the commandment itself.
% DISAPPEARANCE_RATIONALE: From the study_as_performance perspective, if the reading vanished and the performance_only reading prevailed, the commandment would become a void obligation with no mechanism for fulfillment — the world would rearrange such that the community has an unrealizable obligation and scholars' study would become consolation rather than fulfillment. From the messianic_deferral perspective, if this reading vanished, the commandment would remain suspended and waiting for restoration — a different world organization (deferred hope rather than present fulfillment). From within this reading's own frame, the disappearance of study_as_performance would mean the loss of a real and present way of fulfilling a central commandment.
% FOUNDING_PROBLEM: After the Temple's destruction in 70 CE, the commandment to study and perform the sacrifice laws became seemingly impossible to fulfill in its original form (no Temple, no altar, no capacity for actual sacrifice). The halakhic system faced a coherence problem: either the commandment became void (leaving a gap in the covenant), or it needed reinterpretation to remain active. The founding problem is: how can the commandment maintain force and meaning when its primary mechanism has been permanently lost?
% FOUNDING_PROBLEM_CORROBORATION: The founding problem remains live in contemporary Jewish religious thought and scholarship. The Talmudic sources (Megillah 31b, Arakhin 3a, and others) directly address the question and generate the study_as_performance equivalence as a way of maintaining the commandment's present force. Contemporary halakhic authorities and scholars attest to the ongoing vitality of this reading and to the continued study of sacrifice laws as religiously meaningful. The problem is not merely historical; it is structurally present because the Temple remains destroyed and the question of how the commandment has force remains live in Jewish legal reasoning.
narrative_ontology:disappearance_verdict(kodashim_commandment_status__study_as_performance, contested).
narrative_ontology:founding_problem_status(kodashim_commandment_status__study_as_performance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_commandment_status__study_as_performance, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kodashim_commandment_status__study_as_performance, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_commandment_status__study_as_performance, 0.0, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_commandment_status__study_as_performance_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, ExtMetricName, E),
    domain_priors:suppression_score(kodashim_commandment_status__study_as_performance, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(kodashim_commandment_status__study_as_performance),
    narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(kodashim_commandment_status__study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is zero because the reading asserts no gap, no deficit, no extraction from anyone. The claim is that study fulfills the commandment; therefore no one is harmed by non-performance (there is no non-performance under this reading). Suppression is zero because the reading permits open debate and textual engagement — the mechanism of fulfillment is intellectual, not enforced. Theater_ratio is zero because there is no performative husk; the practice is the real fulfillment. Accessibility_collapse is high (0.95) because once the equivalence is accepted within this reading's framework, alternatives (waiting for Messiah, treating the commandment as void) are nearly unreachable — the reading's own logic forecloses them. Resistance is minimal (0.05) because the reading comes from within the halakhic tradition and is authored by its practitioners; it meets no external resistance from those who accept its premises. The measurement series is flat across the interval (0-2000 years of halakhic history) because the reading asserts that under its interpretation, extractiveness has always been zero — this is not a constraint that accumulates or decays, but one whose character is constant once adopted.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between this reading and its siblings is maximal at the logic level: performance_only reads the Temple's destruction as creating an obligation-gap (victim set = those who cannot fulfill); messianic_deferral reads it as a suspension pending restoration (the commandment has deferred force). This reading (study_as_performance) reads the destruction as a transformation of the obligation itself, not a gap or suspension. From the study_as_performance seat, the other readings either miss the halakhic logic (performance_only) or posit an unnecessary eschatological condition (messianic_deferral). From the messianic_deferral seat, this reading pre-empts the ultimate restoration by treating current study as already sufficient. The engine computes the per-seat types from the structural data; this is where the divergence lives.
 *
 * DIRECTIONALITY LOGIC:
 *   The reading offers beneficiaries (torah_scholars) full coherence and meaningful work: the study they already do is declared to constitute commandment fulfillment, not a consolation prize or a deferral. The Jewish community benefits indirectly: their religious obligation is continuously satisfied. There are no victims under this reading because there is no extraction, no gap between obligation and fulfillment, no performance deficit. The reading's strength lies in dissolving the apparent harm (loss of Temple, inability to sacrifice) by reframing the obligation itself. Directionality for torah_scholars approaches 0.0 (full beneficiary) because the reading elevates their practice to commandment-fulfilling status; directionality for the broader community approaches 0.0 as well (indirect beneficiary through scholarly representation). There are no payer seats because the reading asserts no asymmetric cost structure.
 *
 * MANDATROPHY ANALYSIS:
 *   There is no mandatrophy under this reading because the founding problem — maintaining the commandment's force post-destruction — is asserted to be solved by the reading itself. The commandment was founded to regulate sacrifice; under this reading, the mechanism changed (from physical to intellectual) but the commandment itself did not become obsolete. The reading prevents mandatrophy by asserting functional continuity: the purpose (honoring God, maintaining the covenant relationship, engaging with divine will as expressed in law) is continuous, even though the form changed. A mandatrophy analysis would arise only if the reading were adopted but then treated as a temporary expedient awaiting Messianic restoration (at which point the real performance would resume and study would become secondary) — but that combination would be the messianic_deferral reading, not this one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_reading,
    'Is the claim that study fulfills the commandment a natural feature of the halakhic system itself, or a constructed interpretive reading that benefits scholars who practice it?',
    'Genealogical analysis: does the halakhic tradition itself generate this equivalence from its core premises, or is it a late-period stabilization that benefits the interpretive community? Examine Talmudic and geonic sources for the emergence and consolidation of this equivalence.',
    'If the equivalence is a natural derivation from halakhic logic (logical necessity, not institutional choice), the constraint is a mountain and beneficiaries are incidental. If it is a stabilized interpretive choice that benefits scholars, it may reclassify as false summit (tangled rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_reading, empirical, 'Whether study-as-performance is a logical necessity of halakhic structure or a constructed reading.').

omega_variable(
    kernel_occupancy_semantics,
    'What precisely does it mean that ''the kernel remains occupied'' through intellectual engagement? Does study literally fulfill the commandment, or does it preserve readiness/constitute an alternative form of obligation?',
    'Close textual reading of the sources that ground this reading (likely Talmud Megillah 31b, Arakhin 3a, and interpretive traditions): does the language assert equivalence (''study IS performance''), preservation (''study MAINTAINS the commandment''), or something else?',
    'A strict equivalence reading supports zero extractiveness and victim-free classification. A preservation reading might leave a gap between study and full commandment-fulfillment, potentially creating a victim set of those who rely on actual performance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_occupancy_semantics, conceptual, 'The semantic content of study-as-fulfillment in the source tradition.').

omega_variable(
    sibling_reading_empirical_challenge,
    'Does the messianic deferral reading actually coexist with this reading, or do they foreclose each other at the logical level?',
    'Examine whether a single halakhic authority can hold both (study maintains commandment force now AND the commandment is temporally suspended pending restoration) without contradiction. If the same school holds both, they coexist; if they are contradictory, one forecloses the other.',
    'If messianic_deferral forecloses this reading''s core claim, the relation changes from coexists_with to forecloses, and this reading''s axiom about immediate commandment force would be overridden by eschatological suspension in the messianic_deferral frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_empirical_challenge, empirical, 'Logical compatibility of this reading with the messianic deferral alternative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_commandment_status__study_as_performance, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_commandment_status__study_as_performance, theater_ratio, 0, 0.0).
narrative_ontology:measurement(koda_tr_t500, kodashim_commandment_status__study_as_performance, theater_ratio, 500, 0.0).
narrative_ontology:measurement(koda_tr_t1000, kodashim_commandment_status__study_as_performance, theater_ratio, 1000, 0.0).
narrative_ontology:measurement(koda_tr_t1500, kodashim_commandment_status__study_as_performance, theater_ratio, 1500, 0.0).
narrative_ontology:measurement(koda_tr_t2000, kodashim_commandment_status__study_as_performance, theater_ratio, 2000, 0.0).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_commandment_status__study_as_performance, base_extractiveness, 0, 0.0).
narrative_ontology:measurement(koda_be_t500, kodashim_commandment_status__study_as_performance, base_extractiveness, 500, 0.0).
narrative_ontology:measurement(koda_be_t1000, kodashim_commandment_status__study_as_performance, base_extractiveness, 1000, 0.0).
narrative_ontology:measurement(koda_be_t1500, kodashim_commandment_status__study_as_performance, base_extractiveness, 1500, 0.0).
narrative_ontology:measurement(koda_be_t2000, kodashim_commandment_status__study_as_performance, base_extractiveness, 2000, 0.0).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(kodashim_commandment_status__study_as_performance, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_commandment_status__study_as_performance, identity_coordination).
narrative_ontology:boltzmann_floor_override(kodashim_commandment_status__study_as_performance, 0.0).
narrative_ontology:affects_constraint(kodashim_commandment_status__study_as_performance, kodashim_commandment_status__performance_only).
narrative_ontology:affects_constraint(kodashim_commandment_status__study_as_performance, kodashim_commandment_status__messianic_deferral).

% DUAL FORMULATION NOTE:
% The 'kodashim_commandment_status' kernel admits three structurally distinct constraint readings. This story instantiates the study_as_performance reading, which asserts that intellectual engagement with sacrifice laws fulfills the commandment directly, maintaining its force post-destruction without extraction, deferral, or gap. The performance_only reading treats the commandment as suspended/voided by Temple loss. The messianic_deferral reading treats it as deferred but not void. Each reading has its own ε, beneficiary/victim structure, and claimed type. The three stories form a constraint family, linked by network.affects_constraints, documenting the halakhic kernel's contested interpretations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
