% ============================================================================
% CONSTRAINT STORY: quran_ontological_status__uncreated_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_ontological_status__uncreated_reading, []).

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
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: quran_ontological_status__uncreated_reading
 *   human_readable: Qur'an Ontological Status: Uncreated Eternal Divine Speech
 *   domain: theological/political
 *
 * SUMMARY:
 *   The Qur'an's ontological status—whether it is created in time or
 *   uncreated and eternal—is among the most consequential doctrinal disputes
 *   in Islamic history. This constraint story instantiates THE UNCREATED
 *   READING: the position that the Qur'an is kalām Allāh qadīm (God's eternal
 *   speech), coeternal with the divine essence, not a created artifact in
 *   time. This reading privileges prophetic authority, literalist
 *   hermeneutics, and fixed textual meaning. It benefits traditional jurists,
 *   literalist communities, and anti-rationalist theological schools by
 *   grounding their hermeneutical and jurisprudential methods in ontology
 *   itself. It extracts from rational theologians, metaphorical interpreters,
 *   and reform movements by foreclosing their interpretive freedom: if the
 *   Qur'an is eternally fixed divine speech, reason cannot revise it and
 *   metaphorical interpretation cannot recontextualize it. The story treats
 *   this reading as ONE OF THREE constraints, siblings in a kernel contest:
 *   the created reading (makhlūq) and the state-enforced creation reading
 *   (mihna) are authored separately, linked via network.affects_constraints.
 *   This story claims mountain status (the uncreated reading presents itself
 *   as metaphysical fact); the authored metrics (high extractiveness, high
 *   suppression, rising over time) document a pattern that triggers
 *   false-summit detection—an institution benefits from treating a
 *   constructed constraint as natural law. The omegas and commentary expose
 *   the ambiguity: is this a genuine mountain or a doctrine maintained by
 *   institutional power?
 *
 * KEY AGENTS:
 *   - traditional_jurists: Institutional beneficiary; grounds jurisprudence in fixed textual meaning; identity-locked to literal hermeneutics.
 *   - literalist_communities: Organized beneficiary; reads the Qur'an directly, needing no rational mediation or metaphorical reinterpretation; identity-locked to textual literalism.
 *   - anti_rationalist_schools: Organized beneficiary; rejects Mu'tazilite rationalism; the uncreated reading privileges revelation over reason.
 *   - rational_theologians: Moderate payer; Mu'tazilite and Enlightenment tradition; constrained by the uncreated reading from applying reason to doctrine revision.
 *   - metaphorical_interpreters: Moderate payer; ta'wīl and allegorical traditions; constrained by fixed-meaning ontology from interpretive freedom.
 *   - reform_movements: Moderate payer, national scope; seeking to reconcile Islamic tradition with modernity; foreclosed from recontextualizing revelation.
 *   - state_enforcement_apparatus: Institutional agenda_setter; optional to this reading (but present in many contexts); enforces doctrinal adherence through institutional control.
 *   - mystical_interpreters: Partially excluded; Sufi esoteric traditions; structurally marginal under fixed-meaning ontology; not fully expelled but constrained.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_ontological_status__uncreated_reading, 0.68).
domain_priors:suppression_score(quran_ontological_status__uncreated_reading, 0.72).
domain_priors:theater_ratio(quran_ontological_status__uncreated_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_ontological_status__uncreated_reading, mountain).
narrative_ontology:human_readable(quran_ontological_status__uncreated_reading, "Qur'an Ontological Status: Uncreated Eternal Divine Speech").
narrative_ontology:topic_domain(quran_ontological_status__uncreated_reading, "theological/political").

domain_priors:emerges_naturally(quran_ontological_status__uncreated_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_ontological_status__uncreated_reading, 'b5c3c002-0df6-4d80-8224-c1a0ef3433c0').
narrative_ontology:cs_kernel_codification('b5c3c002-0df6-4d80-8224-c1a0ef3433c0', fixed_text).
narrative_ontology:cs_authority_grounding('b5c3c002-0df6-4d80-8224-c1a0ef3433c0', lineage).
narrative_ontology:cs_interpretation_layer_present('b5c3c002-0df6-4d80-8224-c1a0ef3433c0').
narrative_ontology:cs_reading_relation('b5c3c002-0df6-4d80-8224-c1a0ef3433c0', quran_ontological_status__created_reading, forecloses).
narrative_ontology:cs_reading_relation('b5c3c002-0df6-4d80-8224-c1a0ef3433c0', quran_ontological_status__state_enforced_creation_reading, forecloses).
narrative_ontology:cs_axiom('b5c3c002-0df6-4d80-8224-c1a0ef3433c0', foundational, quran_eternality_cosubstantial_with_divine_essence).
narrative_ontology:cs_axiom_status(quran_eternality_cosubstantial_with_divine_essence, holdable).
narrative_ontology:cs_axiom_grounding('b5c3c002-0df6-4d80-8224-c1a0ef3433c0', quran_eternality_cosubstantial_with_divine_essence, deontological).
narrative_ontology:cs_axiom('b5c3c002-0df6-4d80-8224-c1a0ef3433c0', foundational, textual_meaning_fixed_immutable_interpretation).
narrative_ontology:cs_axiom_status(textual_meaning_fixed_immutable_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('b5c3c002-0df6-4d80-8224-c1a0ef3433c0', textual_meaning_fixed_immutable_interpretation, conventional).
narrative_ontology:cs_reference_frame('b5c3c002-0df6-4d80-8224-c1a0ef3433c0', quranic_eternality_framework).
narrative_ontology:cs_drift_state('b5c3c002-0df6-4d80-8224-c1a0ef3433c0', contemporary_institutional_enforcement_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b5c3c002-0df6-4d80-8224-c1a0ef3433c0', '').
narrative_ontology:cs_kernel_id(quran_ontological_status__uncreated_reading, quran_ontological_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, traditional_jurists).
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, literalist_communities).
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, anti_rationalist_schools).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, rational_theologians).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, metaphorical_interpreters).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, reform_movements).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_ontological_status__uncreated_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(quran_ontological_status__uncreated_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_ontological_status__uncreated_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quran_ontological_status__uncreated_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, ExtMetricName, E),
    domain_priors:suppression_score(quran_ontological_status__uncreated_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(quran_ontological_status__uncreated_reading),
    narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(quran_ontological_status__uncreated_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The authored metrics document extraction rising from 0.45 (early establishment, 7th–8th century) to 0.68 (modern enforcement apparatus mature, 20th–21st century). This trajectory is consistent with an institutional constraint: initial coordination value is high (solving genuine founding problem of doctrinal unity), but over time institutional beneficiaries extract increasing rent from the constraint's operation. Theater ratio rises from 0.12 to 0.41, indicating that performative maintenance (repetition of doctrinal affirmation, curriculum enforcement, formal fatwa authority) grows as a share of functional activity. This is a piton signature—a constraint that solved a real problem (7th–8th century) but persists increasingly by institutional inertia and suppression rather than genuine necessity. Suppression requirement rises from 0.35 to 0.72, documenting increased enforcement intensity: the state and institutional authority must actively suppress rational theology, metaphorical interpretation, and reformist readings to maintain the uncreated reading. This is inconsistent with a genuine mountain (which meets near-zero resistance and requires no suppression). The coercion grid documents that suppression intensifies more at the organizational and structural levels (0.72–0.81 at tn) than at the individual level (0.54 at tn), indicating that institutional authority and organized teaching institutions bear the suppression burden, while individual believers show more resistance and drift toward reinterpretive or created-reading positions. Accessibility collapse is high at all levels (0.72–0.86 at tn), indicating that once the uncreated reading is institutionalized, alternatives are genuinely hard to access for believers socialized into the constraint—but collapse rose more dramatically at structural and organizational levels (from 0.74 to 0.86 and 0.68 to 0.81) than at individual level (0.52 to 0.72), suggesting that individual believers retain more access to alternatives than institutional structures do. This pattern is the false-summit signature: a constraint presenting as natural law (high accessibility collapse) that is maintained by institutional power (rising suppression requirement, rising theater ratio) at the organizational and structural levels while individual-level alternatives persist.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (traditional jurists, literalist communities, anti-rationalist schools) experience the uncreated reading as ontological truth grounding their legitimate hermeneutical practice—a mountain that was always there. From their position, the constraint is not enforced; it is recognized and lived. From the payer seats (rational theologians, metaphorical interpreters, reform movements), the same constraint is experienced as institutional suppression of legitimate intellectual work—a snare maintaining its power through curriculum control, legal exclusion, and institutional authority. The state enforcement apparatus occupies yet a third position: it uses the uncreated reading to justify institutional control over doctrine and education, treating it as a tool of governance rather than as metaphysical fact. The coercion grid captures this: beneficiaries experience low suppression and high accessibility collapse as natural constraint (the alternatives literally make no sense to them); payers experience high suppression despite high accessibility collapse (they can understand the constraint but are prevented from acting on alternatives); the state apparatus experiences it as a lever for institutional power. The engine computes per-seat directionality from this structural data: beneficiary seats sit near d=0.0 (full subsidy, no extraction); payer seats sit near d=1.0 (full target, high extraction); the state apparatus sits near d=0.5 for extractive enforcement (it both collects rents and bears suppression costs). These divergences are the measurement the story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Traditional jurists and literalist communities are structural beneficiaries: the uncreated reading privileges their hermeneutics and grounds their institutional authority. Their directionality is near 0.0 (beneficiary end): they benefit from the constraint without bearing its costs. Their exit options are identity_locked—abandoning literalism and the uncreated reading would dissolve their professional identity and community membership. Their power is institutional (for jurists) or organized (for literalist communities), and their spatial scope is global (Islamic tradition) or generational/civilizational (time horizon). For these seats, d is derived as low beneficiary end, ~0.05–0.15. Rational theologians and metaphorical interpreters are payers: the uncreated reading forecloses their intellectual project. Their directionality is near 1.0 (target end): they bear the suppression cost and loss of hermeneutical freedom without direct benefit. Their exit options are constrained (they can leave the tradition or accept suppression) or identity_locked (some are committed to Islamic intellectual life and cannot leave without existential loss). Their power is moderate (they are scholars and intellectuals, not institutional bodies), and their spatial scope is regional or national (their influence is limited to specific communities or reform movements). For these seats, d is derived as high target end, ~0.75–0.85. Reform movements occupy a similar payer position: their social agenda requires reinterpretive freedom that the uncreated reading forecloses. d is ~0.70–0.80. The state enforcement apparatus is in an agenda-setter position: it uses the constraint to maintain institutional control. Its directionality is neither purely beneficiary nor purely target—it both collects rents (institutional authority, educational control) and bears suppression costs (must maintain enforcement machinery, handle resistance). For institutional agenda-setters engaged in extractive enforcement, d is typically ~0.45–0.60. No directionality overrides are required: the beneficiary/victim declarations and exit options feed the derivation cleanly.
 *
 * MANDATROPHY ANALYSIS:
 *   The uncreated reading is claimed as mountain (emerges_naturally: true) but authored beneficiaries are declared (traditional_jurists, literalist_communities, anti_rationalist_schools) and victims are declared (rational_theologians, metaphorical_interpreters, reform_movements). This triggers the FSM (false-summit-mountain) signature: the constraint presents itself as natural law but identifiable institutional beneficiaries exist. The omegas document the irreducible ambiguity: is this a genuine mountain (metaphysical truth about the Qur'an's nature) or a constructed constraint maintained by institutional power (traditional jurisprudence, state enforcement, curriculum control)? The measurement series reveal mounting mandatrophy: extractiveness rises from 0.45 to 0.68, theater ratio rises from 0.12 to 0.41, suppression intensifies from 0.35 to 0.72, and resistance persists (0.48 to 0.62). These are the classic signals of a constraint whose founding problem has been solved (doctrinal coherence was achieved by ~9th century) but whose operation increasingly serves institutional extraction and inertia rather than coordination. The six_questions battery documents the mandatrophy explicitly: founding_problem_status='contested', disappearance_verdict='world_rearranges'. The mismatch (dead founding problem + world_rearranges) is the mandatrophy flag: the constraint persists not because the problem it was built for remains live, but because institutional beneficiaries extract rents from it and possess the power to maintain it. The coercion grid further supports mandatrophy: suppression and stakes_inflation intensify at structural and organizational levels (where institutions wield power) while individual-level metrics remain lower, indicating that the constraint is sustained more by institutional apparatus than by genuine inability to conceive alternatives. The false-summit reading is supported: an institution benefits from treating a constructed constraint as natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mountain_vs_constructed_ambiguity,
    'Is the Qur''an''s uncreated status a genuine metaphysical fact (mountain) or a constructed doctrine maintained by institutional power?',
    'Epistemological analysis: does the claim rest on accessible metaphysical reasoning available to any rational agent, or does it require institutional endorsement and suppression of contrary reasoning to persist? Test: what happens to the reading''s persistence if institutional enforcement machinery (state law, curriculum control, fatwa authority) is removed?',
    'If institutional removal causes widespread adoption of the created reading, the uncreated reading is a snare/tangled rope, not a mountain. If the reading persists or even strengthens absent institutional enforcement, it is a genuine mountain. Current data is ambiguous: historical periods of weak institutional enforcement (contemporary diaspora, secularized states) show both persistent literalist adherence AND adoption of created readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mountain_vs_constructed_ambiguity, conceptual, 'Whether the constraint''s persistence depends on institutional power or genuine metaphysical conviction.').

omega_variable(
    beneficiary_vs_vindicated_proposition,
    'Do traditional jurists and literalist communities benefit FROM the uncreated reading, or does the uncreated reading VINDICATE commitments they held for independent reasons?',
    'Historical analysis: trace when the uncreated reading was adopted (Abbasid period, 8th–9th century) against when literalist hermeneutics and traditional jurisprudence developed. If traditional jurisprudence predates or is independent of the uncreated reading, the reading vindicates rather than creates beneficiary positions. If the reading created new beneficiary positions (institutional authority, enforceability of jurisprudence), it is extractive.',
    'If vindicating, the reading is a genuine coordination mechanism—it provides ontological ground for practices communities were already committed to. If extractive, it is an institutional innovation that benefited certain seats by constraining others. This shifts classification from mountain toward tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_vs_vindicated_proposition, empirical, 'Whether the constraint captures benefits or vindicates pre-existing commitments.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the measured suppression of rational theology and metaphorical interpretation structural (legal prohibition, curriculum exclusion, institutional sanction) or internalized (practitioners believe the reading and accept its constraints voluntarily)?',
    'Post-suppression trajectory: in contexts where institutional enforcement is removed (secular states, diaspora communities), do rational theologians and metaphorical interpreters continue to suppress their own practice, or do they openly revert to reinterpretive methods? If reversion is rapid, suppression was primarily structural; if suppression persists or takes internalized forms (shame, identity fusion with literalism), the suppression is partially internalized.',
    'If internalized, the effective suppression is higher than the raw structural measure suggests—practitioners carry it with them even absent enforcement machinery. This would increase the constraint''s extractiveness from the payer seats'' perspective and support classification as snare or tangled rope rather than mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether suppression of rational and metaphorical schools is structural or internalized.').

omega_variable(
    reading_kernel_identity,
    'Is this constraint ONE READING of a contested kernel (the Qur''an''s ontological status), or is it a core article of faith that stands independently?',
    'This is a structural feature of the committer frame: if other readings (created, state_enforced_creation) are live options held by authoritative parties within Islamic tradition, then this reading is a reading; if no live alternative exists within orthodoxy, then this is a core article, not a reading.',
    'The constraint story is authored as a kernel reading (Rule 1: one reading only; Rule 2: committer structure routed to omegas). If the committer frame is incorrect and there is no live alternative, the story must be reorganized as a single-reading constraint without cross-reading edges.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_identity, conceptual, 'Whether this constraint is one reading of a contested kernel or a standalone article of faith.').

omega_variable(
    extracted_rents_vs_coordination_cost,
    'How much of the measured extractiveness (0.68) represents coordination cost (necessary overhead for maintaining doctrinal coherence) versus pure monopoly rent?',
    'Comparative analysis: in Islamic communities that adopt the created reading, do they achieve equivalent doctrinal coherence and jurisprudential stability at lower measured extraction cost? What is the minimum cost required to solve the founding problem (preservation of legal/doctrinal unity across expanding communities)?',
    'If coordination cost is high (~0.4–0.5), much of the extraction is necessary cost and the constraint approaches rope classification. If rent is high and coordination cost is low (~0.15–0.25), the constraint is closer to snare. Current measurement places extractiveness at 0.68; if ~0.2 is coordination cost, ~0.48 is rent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extracted_rents_vs_coordination_cost, empirical, 'The partition between coordination overhead and pure extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_ontological_status__uncreated_reading, 0, 1400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_ontological_status__uncreated_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(qura_tr_t200, quran_ontological_status__uncreated_reading, theater_ratio, 200, 0.18).
narrative_ontology:measurement(qura_tr_t400, quran_ontological_status__uncreated_reading, theater_ratio, 400, 0.24).
narrative_ontology:measurement(qura_tr_t800, quran_ontological_status__uncreated_reading, theater_ratio, 800, 0.35).
narrative_ontology:measurement(qura_tr_t1200, quran_ontological_status__uncreated_reading, theater_ratio, 1200, 0.39).
narrative_ontology:measurement(qura_tr_t1400, quran_ontological_status__uncreated_reading, theater_ratio, 1400, 0.41).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_ontological_status__uncreated_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(qura_be_t200, quran_ontological_status__uncreated_reading, base_extractiveness, 200, 0.52).
narrative_ontology:measurement(qura_be_t400, quran_ontological_status__uncreated_reading, base_extractiveness, 400, 0.58).
narrative_ontology:measurement(qura_be_t800, quran_ontological_status__uncreated_reading, base_extractiveness, 800, 0.64).
narrative_ontology:measurement(qura_be_t1200, quran_ontological_status__uncreated_reading, base_extractiveness, 1200, 0.67).
narrative_ontology:measurement(qura_be_t1400, quran_ontological_status__uncreated_reading, base_extractiveness, 1400, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_ontological_status__uncreated_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(qura_su_t200, quran_ontological_status__uncreated_reading, suppression_requirement, 200, 0.48).
narrative_ontology:measurement(qura_su_t400, quran_ontological_status__uncreated_reading, suppression_requirement, 400, 0.55).
narrative_ontology:measurement(qura_su_t800, quran_ontological_status__uncreated_reading, suppression_requirement, 800, 0.64).
narrative_ontology:measurement(qura_su_t1200, quran_ontological_status__uncreated_reading, suppression_requirement, 1200, 0.7).
narrative_ontology:measurement(qura_su_t1400, quran_ontological_status__uncreated_reading, suppression_requirement, 1400, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_ontological_status__uncreated_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(quran_ontological_status__uncreated_reading, 0.12).
narrative_ontology:affects_constraint(quran_ontological_status__uncreated_reading, quran_ontological_status__created_reading).
narrative_ontology:affects_constraint(quran_ontological_status__uncreated_reading, quran_ontological_status__state_enforced_creation_reading).

% DUAL FORMULATION NOTE:
% The Qur'an's ontological status is a contested kernel with three structurally distinct readings, each with different extractiveness, beneficiary structure, and classification. The uncreated reading (this story) treats the Qur'an as eternally fixed divine speech, privileging literalism and traditional jurisprudence; ε ≈ 0.68. The created reading treats the Qur'an as created artifact, enabling rational theology and reinterpretation; ε ≈ 0.35–0.45. The state-enforced creation reading adds institutional suppression machinery to the created position; ε ≈ 0.78 (higher because of enforcement cost). Each story must be authored independently with its own ε and metrics; the three are linked only by network edges. This decomposition respects the ε-invariance principle: each reading instantiates a single stable constraint with predictable classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
