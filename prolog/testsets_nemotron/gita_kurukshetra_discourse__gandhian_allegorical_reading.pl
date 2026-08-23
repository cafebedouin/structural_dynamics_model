% ============================================================================
% CONSTRAINT STORY: gita_kurukshetra_discourse__gandhian_allegorical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gita_kurukshetra_discourse__gandhian_allegorical_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: gita_kurukshetra_discourse__gandhian_allegorical_reading
 *   human_readable: Gandhian Allegorical Reading of Kurukshetra as Internal Struggle
 *   domain: religious_studies/textual_hermeneutics/ethical_philosophy
 *
 * SUMMARY:
 *   This constraint story captures the Gandhian allegorical reading of the
 *   Bhagavad Gita's Kurukshetra battlefield as a metaphor for internal
 *   spiritual struggle, repudiating physical violence and caste hierarchy.
 *   The reading emerged in the early 20th century as Gandhi's hermeneutic
 *   strategy for mobilizing mass nonviolent resistance while remaining within
 *   the Hindu textual tradition. It functions as a coordination mechanism:
 *   communities sharing this reading can oppose structural violence and caste
 *   oppression without abandoning their scripture. The constraint is claimed
 *   as a rope — genuine coordination with minimal coercion — and the metrics
 *   reflect low extractiveness and suppression. The reading's persistence
 *   depends on voluntary adoption and moral resonance, not enforcement.
 *
 * KEY AGENTS:
 *   - oppressed_caste_groups: Primary beneficiary (powerless/constrained) — caste hierarchy loses textual warrant
 *   - individual_moral_agents: Primary beneficiary (moderate/mobile) — conscience becomes interpretive seat
 *   - nonviolent_activists: Beneficiary (organized/mobile) — gain textual resource for ahimsa praxis
 *   - brahminical_scholarly_tradition: Excluded (institutional/identity_locked) — interpretive monopoly dissolved
 *   - orthodox_literalist_practitioners: Payer (organized/identity_locked) — reading marginalized in progressive discourse
 *   - analytical_hermeneut: Observer (analytical/analytical) — tracks structural transformation across readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.08).
domain_priors:suppression_score(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.12).
domain_priors:theater_ratio(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gita_kurukshetra_discourse__gandhian_allegorical_reading, rope).
narrative_ontology:human_readable(gita_kurukshetra_discourse__gandhian_allegorical_reading, "Gandhian Allegorical Reading of Kurukshetra as Internal Struggle").
narrative_ontology:topic_domain(gita_kurukshetra_discourse__gandhian_allegorical_reading, "religious_studies/textual_hermeneutics/ethical_philosophy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gita_kurukshetra_discourse__gandhian_allegorical_reading, 'c83e8127-dcd8-4869-bf88-61064bbcc4b8').
narrative_ontology:cs_kernel_codification('c83e8127-dcd8-4869-bf88-61064bbcc4b8', fixed_text).
narrative_ontology:cs_authority_grounding('c83e8127-dcd8-4869-bf88-61064bbcc4b8', lineage).
narrative_ontology:cs_interpretation_layer_present('c83e8127-dcd8-4869-bf88-61064bbcc4b8').
narrative_ontology:cs_reading_relation('c83e8127-dcd8-4869-bf88-61064bbcc4b8', gita_kurukshetra_discourse__orthodox_literal_reading, forecloses).
narrative_ontology:cs_reading_relation('c83e8127-dcd8-4869-bf88-61064bbcc4b8', gita_kurukshetra_discourse__universalist_devotional_reading, coexists_with).
narrative_ontology:cs_axiom('c83e8127-dcd8-4869-bf88-61064bbcc4b8', foundational, violence_never_dharmically_justified).
narrative_ontology:cs_axiom_status(violence_never_dharmically_justified, holdable).
narrative_ontology:cs_axiom_grounding('c83e8127-dcd8-4869-bf88-61064bbcc4b8', violence_never_dharmically_justified, deontological).
narrative_ontology:cs_axiom('c83e8127-dcd8-4869-bf88-61064bbcc4b8', foundational, caste_hierarchy_has_no_divine_mandate).
narrative_ontology:cs_axiom_status(caste_hierarchy_has_no_divine_mandate, holdable).
narrative_ontology:cs_axiom_grounding('c83e8127-dcd8-4869-bf88-61064bbcc4b8', caste_hierarchy_has_no_divine_mandate, deontological).
narrative_ontology:cs_axiom('c83e8127-dcd8-4869-bf88-61064bbcc4b8', foundational, individual_conscience_is_supreme_hermeneutic_authority).
narrative_ontology:cs_axiom_status(individual_conscience_is_supreme_hermeneutic_authority, holdable).
narrative_ontology:cs_axiom_grounding('c83e8127-dcd8-4869-bf88-61064bbcc4b8', individual_conscience_is_supreme_hermeneutic_authority, deontological).
narrative_ontology:cs_reference_frame('c83e8127-dcd8-4869-bf88-61064bbcc4b8', classical_dharmic_warrior_ethic).
narrative_ontology:cs_drift_state('c83e8127-dcd8-4869-bf88-61064bbcc4b8', gandhian_allegorical_turn, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('c83e8127-dcd8-4869-bf88-61064bbcc4b8', '').
narrative_ontology:cs_kernel_id(gita_kurukshetra_discourse__gandhian_allegorical_reading, gita_kurukshetra_discourse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__gandhian_allegorical_reading, oppressed_caste_groups).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__gandhian_allegorical_reading, individual_moral_agents).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__gandhian_allegorical_reading, nonviolent_activists).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__gandhian_allegorical_reading, interfaith_dialogue_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__gandhian_allegorical_reading, orthodox_literalist_practitioners).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__gandhian_allegorical_reading, ahimsa_as_supreme_principle).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__gandhian_allegorical_reading, moral_conscience_as_interpretive_authority).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__gandhian_allegorical_reading, textual_allegoresis_as_liberation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Historically subjected to caste-based structural violence justified by literalist readings of dharmic texts. This reading removes divine sanction from caste hierarchy, creating hermeneutic space for dignity claims. Exit from caste oppression remains constrained by material structures but the textual warrant is severed.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, oppressed_caste_groups, beneficiary,
    powerless, generational, constrained, national).

% Gain interpretive authority over the text — conscience becomes the hermeneutic seat rather than priestly mediation. The constraint coordinates a reading practice where each person wrestles with the text directly. No coercion enforces this reading; adoption is voluntary.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, individual_moral_agents, beneficiary,
    moderate, biographical, mobile, global).

% Receive a powerful textual resource for ahimsa-based resistance. The reading converts a war-text into a nonviolence manual, providing strategic legitimacy. They deploy this reading in political struggles but do not enforce it on others.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, nonviolent_activists, beneficiary,
    organized, biographical, mobile, global).

% Use this reading to build bridges across traditions — the internalized battlefield becomes a universal human structure. The reading's portability across contexts makes it a coordination tool for pluralistic discourse.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, interfaith_dialogue_practitioners, beneficiary,
    moderate, generational, mobile, global).

% Historically held interpretive monopoly over the Gita. This reading displaces their authority by centering individual conscience over lineage transmission. They are not prevented from speaking but their structural gatekeeping role is hermeneutically dissolved.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, brahminical_scholarly_tradition, excluded,
    institutional, civilizational, identity_locked, national).

% Bear the cost of having their reading marginalized in liberal and progressive discourse. The allegorical reading treats their literal war-duty framework as morally superseded. They experience this as extraction of textual authority, though no material coercion is applied.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, orthodox_literalist_practitioners, payer,
    organized, generational, identity_locked, national).

% Observes the contest between readings from outside the commitment. Sees the structural transformation: caste hierarchy exits the constraint set, violence is repudiated, interpretive authority redistributes. Tracks how the reading functions as coordination without enforcement.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, analytical_hermeneut, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gita_kurukshetra_discourse__gandhian_allegorical_reading, diffuse).
narrative_ontology:fixing_cost_class(gita_kurukshetra_discourse__gandhian_allegorical_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared hermeneutic framework that converts a text historically used to legitimize caste hierarchy and righteous violence into a resource for nonviolent moral self-examination and collective liberation. Solves the coordination problem of how communities rooted in this textual tradition can reject structural violence without abandoning their scripture.
% TRANSFER_FUNCTION: Moves interpretive authority from Brahminical scholarly lineage to individual moral conscience; moves the text's legitimating force from caste duty and war to ahimsa and internal struggle. No material transfer — purely epistemic and moral authority redistribution.
% ABSENT_VOICES: Those killed in literal wars justified by the orthodox reading — their voices are absent by definition. Also absent: traditional communities who experience this reading as colonial imposition or cultural erasure rather than liberation. They would object to the universalization of an internalized battlefield that erases the material history of dharmic warfare.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the text would revert to its dominant literalist and devotional frames in most institutional settings. Caste-justifying and war-legitimating readings would regain unchallenged hermeneutic ground. The coordination of nonviolent resistance within the tradition would lose its primary textual anchor. The world of Hindu moral discourse would rearrange toward orthodoxy.
% FOUNDING_PROBLEM: How to remain within the Hindu textual tradition while rejecting the caste system and the legitimization of violence that dominant readings of the Gita authorize. Gandhi confronted this as a practitioner who needed the text's authority for mass mobilization but could not accept its surface meaning.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by Ambedkar's parallel critique (though he rejected the text entirely), by the Indian independence movement's actual deployment of this reading, and by contemporary Dalit and feminist scholars who engage the Gita allegorically while documenting the founding problem's persistence. The problem remains live because caste violence and religious nationalism continue to invoke the literal reading.
narrative_ontology:disappearance_verdict(gita_kurukshetra_discourse__gandhian_allegorical_reading, world_rearranges).
narrative_ontology:founding_problem_status(gita_kurukshetra_discourse__gandhian_allegorical_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gita_kurukshetra_discourse__gandhian_allegorical_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(gita_kurukshetra_discourse__gandhian_allegorical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.08, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gita_kurukshetra_discourse__gandhian_allegorical_reading_tests).
:- end_tests(gita_kurukshetra_discourse__gandhian_allegorical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near-zero (0.08) because the reading extracts nothing material — it redistributes interpretive authority voluntarily. Suppression is low (0.12) because no coercion enforces adoption; the slight suppression reflects social marginalization of the orthodox reading in liberal spaces. Theater ratio is low (0.15) — some performative deployment occurs in interfaith and academic settings, but the reading's core function (moral coordination for nonviolent resistance) remains substantive. Accessibility collapse is low (0.25) — alternative readings remain fully available and widely held. Resistance is moderate-high (0.65) — the reading faces sustained opposition from orthodox traditions and Hindu nationalist movements that treat it as distortion.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute different effective extractions per seat: for oppressed castes, χ is negative (subsidy — the reading removes a legitimating structure of their oppression). For individual moral agents, χ ≈ 0 (symmetric — they gain interpretive freedom at no cost). For orthodox practitioners, χ is positive but small (epistemic displacement). For the analytical observer, χ = 0 (no stake). The seat divergence is the measurement: a reading that is rope for the liberated and mildly extractive for the displaced.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (oppressed castes, individual moral agents, nonviolent activists, interfaith practitioners) experience the reading as coordination gain — they receive interpretive resources without paying extraction. The orthodox literalist practitioners are payers in a limited sense: they lose hermeneutic dominance in progressive discourse, but this is epistemic displacement not material extraction. The brahminical tradition is excluded — its gatekeeping role dissolves but it retains freedom to teach. Directionality derives from beneficiary/victim declarations: no victims declared, four beneficiary groups, one payer group experiencing epistemic marginalization.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling textual fidelity with caste/violence rejection) remains live — caste violence and religious nationalism persist. The reading has not atrophied into piton because its coordination function is actively deployed in resistance movements. Theater ratio has crept up slightly (0.05→0.15) as academic and interfaith deployment sometimes performs the reading without existential stakes — but the core coordination function remains genuine. No mandatrophy resolution needed; the constraint's mandate is still operative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    allegorical_reading_as_colonial_imposition,
    'Is the Gandhian allegorical reading experienced by some traditional communities as a colonial/modernist imposition that erases indigenous hermeneutics rather than liberating them?',
    'Ethnographic study of traditional communities'' reception of allegorical vs. literal readings; analysis of whether the reading''s spread correlates with Western academic influence or indigenous reform movements.',
    'If experienced as imposition, the reading''s coordination function carries extractive overhead for those communities — their interpretive sovereignty is displaced. This would raise effective extraction for the excluded seat and potentially shift classification toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(allegorical_reading_as_colonial_imposition, empirical, 'Whether the reading''s universalization functions as epistemic extraction for traditional communities').

omega_variable(
    ahimsa_supremacy_vs_textual_fidelity,
    'Does elevating ahimsa as the supreme hermeneutic principle require systematic distortion of the text''s pluralistic dharma framework (which includes ksatriya duty, bhakti, jnana as co-equal paths)?',
    'Philological analysis of whether the allegorical reading can be sustained without suppressing the text''s explicit endorsement of varna-dharma and righteous war in chapters 2, 11, 18.',
    'If the reading requires textual suppression to maintain ahimsa supremacy, its low suppression metric is misleading — the suppression is displaced onto the text itself. This would indicate hidden extraction (the text pays the cost of the reading''s coherence).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ahimsa_supremacy_vs_textual_fidelity, conceptual, 'Whether the reading''s coherence depends on suppressing textual elements that contradict ahimsa supremacy').

omega_variable(
    kernel_framing_underdetermination,
    'Is the kernel correctly framed as ''the Gita''s teaching on Kurukshetra'' or should it be framed as ''the Mahabharata''s teaching on war and dharma'' — where the Gita is one voice in a polyphonic epic that already contains its own critique?',
    'Comparative analysis of whether the Mahabharata''s framing of the Gita (as Krishna''s battlefield counsel to a reluctant warrior) already contains the tension between allegory and literal duty that the kernel contest reproduces.',
    'If the kernel framing is too narrow (Gita-only), the contest between readings misses the epic''s own internal dialectic. The orthodox reading may be more faithful to the Gita but less faithful to the Mahabharata. This reframes the structural relationships: the universalist reading might be the epic''s own synthesis, not a third competitor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel boundary (Gita vs. Mahabharata) determines the reading relations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gita_kurukshetra_discourse__gandhian_allegorical_reading, 1920, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gita_kurukshetra_discourse__gandhian_allegorical_reading_tr_t1920, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 1920, 0.05).
narrative_ontology:measurement(gita_kurukshetra_discourse__gandhian_allegorical_reading_tr_t1947, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 1947, 0.08).
narrative_ontology:measurement(gita_kurukshetra_discourse__gandhian_allegorical_reading_tr_t1970, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 1970, 0.12).
narrative_ontology:measurement(gita_kurukshetra_discourse__gandhian_allegorical_reading_tr_t1990, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 1990, 0.14).
narrative_ontology:measurement(gita_kurukshetra_discourse__gandhian_allegorical_reading_tr_t2010, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(gita_kurukshetra_discourse__gandhian_allegorical_reading_tr_t2025, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 2025, 0.15).

% Extraction over time
narrative_ontology:measurement(gita_kurukshetra_discourse__gandhian_allegorical_reading_be_t1920, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 1920, 0.05).
narrative_ontology:measurement(gita_kurukshetra_discourse__gandhian_allegorical_reading_be_t1947, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 1947, 0.06).
narrative_ontology:measurement(gita_kurukshetra_discourse__gandhian_allegorical_reading_be_t1970, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 1970, 0.07).
narrative_ontology:measurement(gita_kurukshetra_discourse__gandhian_allegorical_reading_be_t1990, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 1990, 0.07).
narrative_ontology:measurement(gita_kurukshetra_discourse__gandhian_allegorical_reading_be_t2010, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 2010, 0.08).
narrative_ontology:measurement(gita_kurukshetra_discourse__gandhian_allegorical_reading_be_t2025, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 2025, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(gita_kurukshetra_discourse__gandhian_allegorical_reading_su_t1920, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 1920, 0.05).
narrative_ontology:measurement(gita_kurukshetra_discourse__gandhian_allegorical_reading_su_t1947, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 1947, 0.08).
narrative_ontology:measurement(gita_kurukshetra_discourse__gandhian_allegorical_reading_su_t1970, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 1970, 0.1).
narrative_ontology:measurement(gita_kurukshetra_discourse__gandhian_allegorical_reading_su_t1990, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 1990, 0.11).
narrative_ontology:measurement(gita_kurukshetra_discourse__gandhian_allegorical_reading_su_t2010, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 2010, 0.12).
narrative_ontology:measurement(gita_kurukshetra_discourse__gandhian_allegorical_reading_su_t2025, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 2025, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gita_kurukshetra_discourse__gandhian_allegorical_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.08).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__gandhian_allegorical_reading, gita_kurukshetra_discourse__orthodox_literal_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__gandhian_allegorical_reading, gita_kurukshetra_discourse__universalist_devotional_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the gita_kurukshetra_discourse kernel. The three readings form a constraint family linked by network.affects_constraints. The epsilon values differ substantially: this reading (ε≈0.08) vs orthodox (ε≈0.65, legitimates caste/violence) vs universalist (ε≈0.15, devolves authority but retains devotional hierarchy). The decomposition follows the BGS pattern: same label ('the Gita's teaching'), structurally distinct constraints with different extraction profiles and victim structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
