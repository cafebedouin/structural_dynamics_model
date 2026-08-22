% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_continuity__performance_only, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: sacrifice_obligation_continuity__performance_only
 *   human_readable: Sacrifice Obligation Requires Physical Performance (Performance-Only Reading)
 *   domain: religious/ritual/textual
 *
 * SUMMARY:
 *   A covenant community faces the foundational theological problem: after
 *   Temple destruction, sacrifice commandments remain standing in sacred
 *   text, but physical performance is structurally impossible. This
 *   constraint embodies ONE READING of how to maintain the obligation's
 *   standing: performance alone fulfills; study is preparation for future
 *   restoration, not satisfaction. Under this reading, the current generation
 *   is locked in mandatory unfulfillment—the obligation persists, study
 *   offers no discharge, and guilt is a structural feature of existence under
 *   the covenant. The alternative readings (study-as-performance,
 *   messianic-suspension, archival-preservation) would relieve this burden
 *   but are actively suppressed by the rabbinic authority structure that
 *   maintains the performance-only interpretation. This is a kernel reading,
 *   not a single discovered fact about sacrifice law.
 *
 * KEY AGENTS:
 *   - current_generation_practitioners: identity-locked victims bearing the burden of unfulfillment; cannot exit without apostasy
 *   - post_temple_destruction_community: structurally unable to perform; trapped between obligation and impossibility
 *   - rabbinic_authority_structure: agenda-setter maintaining the performance-only reading and suppressing alternatives
 *   - textual_tradition_transmitters: beneficiaries sustaining institutional role through perpetual study requirement
 *   - study_as_performance_advocates: excluded but live alternative reading that would dissolve the guilt structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__performance_only, 0.89).
domain_priors:suppression_score(sacrifice_obligation_continuity__performance_only, 0.76).
domain_priors:theater_ratio(sacrifice_obligation_continuity__performance_only, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, extractiveness, 0.89).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, accessibility_collapse, 0.81).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__performance_only, snare).
narrative_ontology:human_readable(sacrifice_obligation_continuity__performance_only, "Sacrifice Obligation Requires Physical Performance (Performance-Only Reading)").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__performance_only, "religious/ritual/textual").

domain_priors:requires_active_enforcement(sacrifice_obligation_continuity__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__performance_only, '921ca05e-79b8-4d77-8106-fc90461e06f6').
narrative_ontology:cs_kernel_codification('921ca05e-79b8-4d77-8106-fc90461e06f6', fixed_text).
narrative_ontology:cs_authority_grounding('921ca05e-79b8-4d77-8106-fc90461e06f6', lineage).
narrative_ontology:cs_interpretation_layer_present('921ca05e-79b8-4d77-8106-fc90461e06f6').
narrative_ontology:cs_reading_relation('921ca05e-79b8-4d77-8106-fc90461e06f6', sacrifice_obligation_continuity__study_as_performance, forecloses).
narrative_ontology:cs_reading_relation('921ca05e-79b8-4d77-8106-fc90461e06f6', sacrifice_obligation_continuity__messianic_suspension, coexists_with).
narrative_ontology:cs_reading_relation('921ca05e-79b8-4d77-8106-fc90461e06f6', sacrifice_obligation_continuity__archival_preservation, coexists_with).
narrative_ontology:cs_axiom('921ca05e-79b8-4d77-8106-fc90461e06f6', foundational, performance_constitutive_of_obligation_fulfillment).
narrative_ontology:cs_axiom_status(performance_constitutive_of_obligation_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('921ca05e-79b8-4d77-8106-fc90461e06f6', performance_constitutive_of_obligation_fulfillment, deontological).
narrative_ontology:cs_axiom('921ca05e-79b8-4d77-8106-fc90461e06f6', foundational, obligation_binding_across_eras).
narrative_ontology:cs_axiom_status(obligation_binding_across_eras, holdable).
narrative_ontology:cs_axiom_grounding('921ca05e-79b8-4d77-8106-fc90461e06f6', obligation_binding_across_eras, deontological).
narrative_ontology:cs_reference_frame('921ca05e-79b8-4d77-8106-fc90461e06f6', temple_standing_obligation_performable).
narrative_ontology:cs_drift_state('921ca05e-79b8-4d77-8106-fc90461e06f6', post_temple_destruction_indefinite_deferral, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('921ca05e-79b8-4d77-8106-fc90461e06f6', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_victim(sacrifice_obligation_continuity__performance_only, current_generation_practitioners).
narrative_ontology:constraint_victim(sacrifice_obligation_continuity__performance_only, post_temple_destruction_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__performance_only, textual_tradition_transmitters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bound by the commandment to perform sacrifice but structurally unable to do so—the Temple is destroyed and cannot be rebuilt in the current era. They bear guilt for non-fulfillment under this reading because the obligation remains intact and only its performance modality is impossible. Their identity as covenant-bound practitioners requires them to hold the obligation as standing and unfulfillable simultaneously. Exit means apostasy or dissolution of covenantal identity.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, current_generation_practitioners, payer,
    powerless, generational, identity_locked, global).

% Lives with the interpretive burden that study is preparation for future restoration but NOT fulfillment of the present obligation. They must maintain textual knowledge and ritual readiness while accepting that the obligation cannot be discharged in the current world order. Study provides meaning and continuity but not absolution.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, post_temple_destruction_community, payer,
    moderate, biographical, constrained, regional).

% Interprets and enforces this reading of the sacrifice law: maintains that physical performance is the only fulfillment modality, study is preparation, and the current generation exists in a state of mandatory unfulfillment. Derives authority from textual lineage and the claim that the obligation's structure is immutable even if its performance is temporarily impossible. Resists alternative readings that would dissolve the obligation or treat study as constitutive satisfaction.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, rabbinic_authority_structure, agenda_setter,
    institutional, generational, mobile, regional).

% Would argue that textual engagement with sacrifice law constitutes fulfillment of the commandment, dissolving the guilt structure this reading imposes. Their alternative framing is actively contested by the agenda-setter; they are excluded from the authority structure that adjudicates which reading governs practice.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, study_as_performance_advocates, excluded,
    organized, generational, constrained, regional).

% Argue that the obligation is suspended—not violated, not fulfilled, but held in abeyance—pending messianic restoration. This reading would relieve the guilt structure; their framework is alternative and competing, kept from dominance by the rabbinic authority that upholds the performance-only reading.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, messianic_suspension_proponents, excluded,
    moderate, civilizational, constrained, regional).

% Benefit from the maintenance of study as a primary activity—their institutional role as interpreters, teachers, and authorities derives authority and resources from the perpetuation of textual engagement. The reading that study is preparatory (not constitutive) keeps study perpetually necessary, sustaining their interpretive and institutional function.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, textual_tradition_transmitters, beneficiary,
    organized, generational, mobile, global).

% The abstract future condition this reading defers to. The performance-only reading vindicates the claim that restoration will occur and obligations will be redeemed; maintaining the obligation's standing now underwrites the hope that fulfillment remains possible.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, messianic_future_restoration, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(sacrifice_obligation_continuity__performance_only, messianic_future_restoration).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading provides no real-time coordination function. It maintains the standing of sacrifice obligation across generations with no modality for present discharge—the function is purely deferred (preservation of obligation for future restoration).
% TRANSFER_FUNCTION: Transfers guilt, interpretive responsibility, and the burden of unfulfillment to each current generation while also transferring authority to rabbinic interpreters to maintain the obligation's boundaries and refuse alternative readings. The current generation receives no absolution; the textual authorities receive sustained interpretive jurisdiction.
% ABSENT_VOICES: Practitioners who have left the tradition; rival interpretive communities that adopt study-as-performance or messianic-suspension readings; those who believe the obligation should be declared void. They are structurally excluded from adjudicating this reading because the rabbinic authority structure maintains the performance-only interpretation against alternatives.
% DISAPPEARANCE_RATIONALE: If this constraint vanished—if the reading that physical performance alone fulfills the obligation were abandoned—the guilt structure would collapse, study practices might dissolve or transform in meaning, alternative readings (study as performance, suspension) would become live options, and the community's relationship to the unfulfillable obligation would reorganize around one of the alternative framings. The maintenance of this reading shapes interpretive practice, psychological disposition toward the tradition, and the authority structure that enforces the boundaries.
% FOUNDING_PROBLEM: The foundational problem this reading addresses: after Temple destruction, how does a covenant community maintain the binding force of sacrifice commandments when physical performance is structurally impossible? The performance-only reading holds the obligation as standing—unfulfilled, not discharged, not suspended—requiring each generation to live with its mandatory incompleteness.
% FOUNDING_PROBLEM_CORROBORATION: The reading is attested by rabbinic textual tradition and contemporary interpreters who hold it. However, the alternative readings (study-as-performance, messianic-suspension, archival-preservation) are also attested and contested within the same tradition. No consensus corroboration from outside the benefiting parties (textual authorities and those who claim authority from this reading); the contest itself is the evidence that status is disputed.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__performance_only, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__performance_only, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sacrifice_obligation_continuity__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_continuity__performance_only, 0.89, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_continuity__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_obligation_continuity__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sacrifice_obligation_continuity__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is extremely high (0.89) because the constraint creates unfulfillable obligation without remedy or release mechanism—practitioners bear guilt that cannot be discharged through the only mode the reading permits (physical performance). Suppression is substantial (0.76) because alternative readings that would relieve guilt are actively kept from dominance by institutional authority. Theater is elevated (0.62) because study, styled as 'preparation for restoration,' functions partly as justification for the unfulfilled obligation rather than as genuine readiness—the theatrical dimension grows as time extends and messianic restoration remains deferred. Accessibility collapse is high (0.81) because the obligation is written in sacred text (impossible to deny) and identity-fusion makes exit unthinkable. Resistance is moderate (0.58) because practitioners live within the framework and organized resistance to alternative readings exists but faces institutional suppression. The measurement trajectory shows extractiveness and theater rising over the 2000-unit interval (approximating the post-Temple era through medieval period)—as generations accumulate without restoration, the burden deepens and the study function drifts from genuine preparation toward theatrical maintenance.
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinic authority seat: this reading preserves the obligation's integrity, honors the covenant's standing, and maintains readiness for restoration—it is a coherent interpretation of textual commitment. From the current-generation victim seat: this reading imposes perpetual guilt, refuses any discharge mechanism, and uses study as a justification for unfulfillment rather than a path toward fulfillment. The two seats see the SAME constraint structure (unfulfilled obligation + study as preparation) but experience it entirely differently—one as interpretive fidelity, the other as institutional extraction of guilt and compliance without relief. The engine's per-seat computation will capture this gap: the agenda-setter's seat may compute a different type (perhaps tangled_rope from the authority perspective, if it sees genuine interpretive coordination) while the victim seat computes snare (structure without exit, guilt without remedy).
 *
 * DIRECTIONALITY LOGIC:
 *   See directionality_logic above.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (maintaining obligation after Temple destruction) was live at the reading's inception. However, over 2000 years, the messianic restoration premise has not materialized. The founding problem has moved from 'live' toward 'dead' (the restoration the obligation anticipates has not occurred). Simultaneously, the obligation's status shifts: if restoration is indefinitely deferred, the obligation becomes a permanent state, not a temporary suspension pending restoration. At this point, the reading transitions into a zombie state—it maintains the obligation formally but has lost its forward-looking justification. The theater_ratio rising to 0.62 reflects this shift: study is increasingly performance of readiness rather than genuine preparation for imminent restoration. The constraint should declare mandatrophy_resolved if the tradition has formally acknowledged that restoration is not imminent, making the preparation framing obsolete. The fact that this reading persists despite the founding problem's apparent death suggests the constraint is now maintained by institutional inertia and identity-lock rather than by coherent justification—a signature piton or late-snare dynamic. However, within the tradition, the messianic framework officially remains live (restoration is still expected, just on an indefinite timeline), so mandatrophy_resolved is technically false. The mismatch between formal assertion (restoration is coming, study prepares for it) and lived reality (restoration is not coming, study justifies unfulfillment indefinitely) is the constraint's current tension.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_identity_fusion,
    'Is the performance-only reading held as a structural truth about the obligation, or is it maintained primarily because accepting alternative readings would dissolve the psychological and institutional identity of interpreters and practitioners bound to unfulfillment?',
    'Historical analysis of counterfactual adoption of study-as-performance or messianic-suspension readings and tracking of institutional/psychological consequences. Examination of whether practitioners express relief or loss when briefly exposed to alternative framings.',
    'If held as structural truth: the reading is a genuine interpretation of the obligation''s nature. If maintained primarily for identity-lock and institutional authority: the reading is better classified as a snare using identity fusion as its suppression mechanism—practitioners are trapped not by external barriers but by the fusion of their identity with the unfulfillment obligation itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_identity_fusion, empirical, 'Whether the reading is a structural truth or an identity-locked institutional extraction.').

omega_variable(
    study_preparatory_boundary_ambiguity,
    'What empirically distinguishes study as ''preparation for restoration'' from study as ''evasion of the obligation''s unfulfillment''? At what point does maintenance of textual knowledge become theatrical performance of preparedness rather than genuine readiness?',
    'Ethnographic observation of study practices and their stated justifications; comparison of study intensity and focus in communities adopting this reading vs. alternative readings; measurement of whether practitioners report psychological tension (unfulfillment) or psychological coherence (readiness) from study engagement.',
    'High theater_ratio (0.62 at interval end) already reflects this ambiguity. If study is genuinely preparatory, theater should be lower and driven by incidental performance. If study is primarily theatrical justification for unfulfillment, theater is structural and the constraint''s character shifts toward pure psychological extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_preparatory_boundary_ambiguity, empirical, 'Whether study maintains real readiness or performs readiness to justify unfulfillment.').

omega_variable(
    alternative_reading_suppression_mechanism,
    'Why have alternative readings (study-as-performance, messianic-suspension) not displaced the performance-only reading despite their psychological advantages (absolution, coherence, reduced guilt)? Is suppression of alternatives enforced structurally (institutional power, textual argument) or internalized (practitioners have fused identity with unfulfillment and resist relief)?',
    'Analysis of how alternative readings are textually refuted vs. institutionally excluded. Survey of practitioners who encounter alternative readings: do they resist intellectually (structural argument) or emotionally/identity-defensively (internalized suppression)?',
    'If structural suppression: the rabbinic authority structure is the enforcing agent and the constraint''s persistence depends on institutional power. If internalized: practitioners carry the suppression of alternatives within themselves even if institutional enforcement relaxed; exit would require identity reconstruction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_reading_suppression_mechanism, empirical, 'Whether suppression of alternative readings is structural or internalized.').

omega_variable(
    kernel_reading_under_determination,
    'This constraint instantiates the ''performance-only'' reading of the sacrifice-obligation-continuity kernel. The kernel itself is under-determined: the standing commitment to sacrifice obligations is ambiguous about what constitutes fulfillment, what study''s role is, and whether the obligation persists in the post-Temple era. Multiple coherent readings exist. This reading selects ONE framing (performance is constitutive, study is preparatory). An alternative reading would frame study itself as constitutive, dissolving the unfulfillment burden. Are both framings equally supported by the kernel text, or does the text favor one reading?',
    'Textual hermeneutics: close reading of the kernel sources (Talmudic discussions, early halakhic texts, tradition-continuity arguments) to identify whether the text itself supports or underdetermines the choice between readings.',
    'If the kernel text underdetermines the reading: the choice to adopt performance-only is a reading (not a discovery), and its persistence depends on institutional authority and identity-lock, not textual force—the constraint''s classification as snare is strengthened. If the text favors performance-only: the constraint''s extraction is grounded in valid textual interpretation, not institutional capture—the classification might shift toward tangled_rope (the reading solves a genuine textual ambiguity while extracting guilt from the current generation as a structural consequence, not as institutional rent-seeking).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_under_determination, conceptual, 'Whether the performance-only reading is textually determined or institutionally selected from a text that underdetermines the choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__performance_only, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_continuity__performance_only, theater_ratio, 0, 0.48).
narrative_ontology:measurement(sacr_tr_t200, sacrifice_obligation_continuity__performance_only, theater_ratio, 200, 0.51).
narrative_ontology:measurement(sacr_tr_t500, sacrifice_obligation_continuity__performance_only, theater_ratio, 500, 0.55).
narrative_ontology:measurement(sacr_tr_t1000, sacrifice_obligation_continuity__performance_only, theater_ratio, 1000, 0.58).
narrative_ontology:measurement(sacr_tr_t1500, sacrifice_obligation_continuity__performance_only, theater_ratio, 1500, 0.61).
narrative_ontology:measurement(sacr_tr_t2000, sacrifice_obligation_continuity__performance_only, theater_ratio, 2000, 0.62).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_continuity__performance_only, base_extractiveness, 0, 0.78).
narrative_ontology:measurement(sacr_be_t200, sacrifice_obligation_continuity__performance_only, base_extractiveness, 200, 0.81).
narrative_ontology:measurement(sacr_be_t500, sacrifice_obligation_continuity__performance_only, base_extractiveness, 500, 0.84).
narrative_ontology:measurement(sacr_be_t1000, sacrifice_obligation_continuity__performance_only, base_extractiveness, 1000, 0.87).
narrative_ontology:measurement(sacr_be_t1500, sacrifice_obligation_continuity__performance_only, base_extractiveness, 1500, 0.88).
narrative_ontology:measurement(sacr_be_t2000, sacrifice_obligation_continuity__performance_only, base_extractiveness, 2000, 0.89).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_obligation_continuity__performance_only, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(sacr_su_t200, sacrifice_obligation_continuity__performance_only, suppression_requirement, 200, 0.7).
narrative_ontology:measurement(sacr_su_t500, sacrifice_obligation_continuity__performance_only, suppression_requirement, 500, 0.72).
narrative_ontology:measurement(sacr_su_t1000, sacrifice_obligation_continuity__performance_only, suppression_requirement, 1000, 0.74).
narrative_ontology:measurement(sacr_su_t1500, sacrifice_obligation_continuity__performance_only, suppression_requirement, 1500, 0.75).
narrative_ontology:measurement(sacr_su_t2000, sacrifice_obligation_continuity__performance_only, suppression_requirement, 2000, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__performance_only, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_obligation_continuity__performance_only, 0.12).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity__study_as_performance).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity__messianic_suspension).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity__archival_preservation).

% DUAL FORMULATION NOTE:
% This constraint family decomposes a single contested kernel (sacrifice_obligation_continuity) into four structurally distinct constraints, each instantiating a different reading of the post-Temple sacrifice obligation. They differ in ε values (obligation binding vs. dissolved), victim/beneficiary structures (current generation locked in unfulfillment vs. relieved), and foundational axioms (performance constitutive vs. study constitutive vs. obligation suspended vs. law obsolete). The readings are linked by network.affects_constraints; each story is a clean, ε-invariant constraint with its own cs_structure.reading_relations and axioms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
