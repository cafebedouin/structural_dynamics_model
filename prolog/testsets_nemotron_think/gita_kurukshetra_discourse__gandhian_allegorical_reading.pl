% ============================================================================
% CONSTRAINT STORY: gita_kurukshetra_discourse__gandhian_allegorical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Gandhian Allegorical Reading of the Kurukshetra Discourse
 *   domain: religious_studies/textual_hermeneutics/ethical_philosophy
 *
 * SUMMARY:
 *   The Gandhian allegorical reading of the Bhagavad Gita's Kurukshetra
 *   discourse claims the battlefield is a metaphor for the internal struggle
 *   between higher and lower impulses, and that the text's true teaching is
 *   absolute non-violence (ahimsa). This reading emerged in Gandhi's
 *   engagement with the Gita (1920s-1940s) as a direct counter to the
 *   orthodox literalist reading that mandated caste duty (varnashrama dharma)
 *   and legitimated righteous violence (dharmic war). The reading
 *   structurally transforms the constraint set: caste hierarchy loses divine
 *   mandate, physical violence is repudiated, interpretive authority shifts
 *   from Brahminical scholars to individual moral conscience. The constraint
 *   story assesses the STANDING ARRANGEMENT UNDER CONTEST — the orthodox
 *   literalist reading's constraint — from the Gandhian reading's lights,
 *   finding it extractive toward lower castes and war victims. The Gandhian
 *   reading itself operates as a coordinating constraint (claimed as rope)
 *   with moderate suppression of the literalist alternative.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.35).
domain_priors:suppression_score(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.45).
domain_priors:theater_ratio(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gita_kurukshetra_discourse__gandhian_allegorical_reading, rope).
narrative_ontology:human_readable(gita_kurukshetra_discourse__gandhian_allegorical_reading, "Gandhian Allegorical Reading of the Kurukshetra Discourse").
narrative_ontology:topic_domain(gita_kurukshetra_discourse__gandhian_allegorical_reading, "religious_studies/textual_hermeneutics/ethical_philosophy").

domain_priors:requires_active_enforcement(gita_kurukshetra_discourse__gandhian_allegorical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gita_kurukshetra_discourse__gandhian_allegorical_reading, 'c3f99ee9-00eb-4ed5-8cf1-9642142121e1').
narrative_ontology:cs_kernel_codification('c3f99ee9-00eb-4ed5-8cf1-9642142121e1', fixed_text).
narrative_ontology:cs_authority_grounding('c3f99ee9-00eb-4ed5-8cf1-9642142121e1', lineage).
narrative_ontology:cs_interpretation_layer_present('c3f99ee9-00eb-4ed5-8cf1-9642142121e1').
narrative_ontology:cs_reading_relation('c3f99ee9-00eb-4ed5-8cf1-9642142121e1', gita_kurukshetra_discourse__orthodox_literal_reading, forecloses).
narrative_ontology:cs_reading_relation('c3f99ee9-00eb-4ed5-8cf1-9642142121e1', gita_kurukshetra_discourse__universalist_devotional_reading, coexists_with).
narrative_ontology:cs_axiom('c3f99ee9-00eb-4ed5-8cf1-9642142121e1', foundational, violence_is_always_adharmic).
narrative_ontology:cs_axiom_status(violence_is_always_adharmic, holdable).
narrative_ontology:cs_axiom_grounding('c3f99ee9-00eb-4ed5-8cf1-9642142121e1', violence_is_always_adharmic, deontological).
narrative_ontology:cs_axiom('c3f99ee9-00eb-4ed5-8cf1-9642142121e1', foundational, caste_hierarchy_has_no_divine_mandate).
narrative_ontology:cs_axiom_status(caste_hierarchy_has_no_divine_mandate, holdable).
narrative_ontology:cs_axiom_grounding('c3f99ee9-00eb-4ed5-8cf1-9642142121e1', caste_hierarchy_has_no_divine_mandate, deontological).
narrative_ontology:cs_axiom('c3f99ee9-00eb-4ed5-8cf1-9642142121e1', secondary, text_serves_liberation_not_legitimation).
narrative_ontology:cs_axiom_status(text_serves_liberation_not_legitimation, holdable).
narrative_ontology:cs_axiom_grounding('c3f99ee9-00eb-4ed5-8cf1-9642142121e1', text_serves_liberation_not_legitimation, instrumental).
narrative_ontology:cs_reference_frame('c3f99ee9-00eb-4ed5-8cf1-9642142121e1', gandhian_allegorical_framework).
narrative_ontology:cs_drift_state('c3f99ee9-00eb-4ed5-8cf1-9642142121e1', contemporary_postcolonial_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('c3f99ee9-00eb-4ed5-8cf1-9642142121e1', '').
narrative_ontology:cs_kernel_id(gita_kurukshetra_discourse__gandhian_allegorical_reading, gita_kurukshetra_discourse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__gandhian_allegorical_reading, lower_castes).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__gandhian_allegorical_reading, pacifists_conscientious_objectors).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__gandhian_allegorical_reading, individual_conscience_practitioners).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__gandhian_allegorical_reading, brahminical_scholarly_authority).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__gandhian_allegorical_reading, literalist_traditionalists).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__gandhian_allegorical_reading, ahimsa_as_supreme_dharma).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__gandhian_allegorical_reading, caste_hierarchy_lacks_divine_mandate).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__gandhian_allegorical_reading, text_serves_liberation_not_legitimation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Propagate and enforce the allegorical reading through ashrams, publications, and political practice. Gandhi himself authored the reading; his followers maintain it as a living hermeneutic and political program. They actively contest literalist interpretations in public discourse.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, gandhian_practitioners, agenda_setter,
    organized, generational, mobile, global).

% Freed from the divine mandate of caste hierarchy that the literalist reading imposes. The allegorical reading removes scriptural justification for their subordination. Their exit from caste oppression is constrained by material structures the reading alone cannot dismantle.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, lower_castes, beneficiary,
    powerless, biographical, constrained, national).

% Gain a canonical Hindu text that authorizes absolute non-violence rather than legitimating righteous war. The reading provides spiritual resources for conscientious objection. They can adopt or leave the reading without structural penalty.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, pacifists_conscientious_objectors, beneficiary,
    moderate, biographical, mobile, global).

% Interpretive authority shifts to individual moral conscience — each person becomes the arbiter of the text's meaning for their own life. They benefit from the reading's anti-authoritarian hermeneutic. High exit options: they can engage multiple readings simultaneously.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, individual_conscience_practitioners, beneficiary,
    moderate, biographical, arbitrage, global).

% Lose their monopoly as authorized interpreters of the Gita. The allegorical reading dissolves the textual basis for their custodial authority over caste order and ritual orthodoxy. They resist through institutional control of seminaries, temple networks, and academic positions. Exit means abandoning their institutional identity.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, brahminical_scholarly_authority, payer,
    institutional, generational, constrained, national).

% Their hermeneutic framework — that the Gita mandates caste duty and righteous violence — is structurally delegitimated by the allegorical reading. The reading treats their core commitments as category errors. Their identity is fused to the literalist reading; exit would constitute apostasy.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, literalist_traditionalists, payer,
    organized, generational, identity_locked, national).

% Analyze the contest between readings as a historical and philological problem. They do not collect rents from either reading nor bear its costs. Their exit is analytical — they can evaluate all readings without commitment.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, secular_scholars, observer,
    analytical, civilizational, analytical, global).

% Would object to the allegorical reading's rejection of righteous violence (kshatriya dharma) and its anti-caste implications. They are structurally excluded from the Gandhian hermeneutic community but contest it politically. Their exclusion is enforced by the reading's own commitment to non-violence, which refuses to engage them on their terms.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, hindu_nationalists, excluded,
    powerful, generational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gita_kurukshetra_discourse__gandhian_allegorical_reading, diffuse).
narrative_ontology:fixing_cost_class(gita_kurukshetra_discourse__gandhian_allegorical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a spiritual community around non-violent resistance and anti-caste ethics using a shared canonical text. Solves the problem of how to claim Hindu tradition for liberation rather than oppression.
% TRANSFER_FUNCTION: Moves interpretive authority from Brahminical scholars to individual conscience; moves spiritual legitimacy from caste hierarchy and righteous violence to ahimsa and equality. The transfer is from institutional custodians to dispersed practitioners.
% ABSENT_VOICES: Hindu nationalists and traditional kshatriya lineages are excluded — they would argue the reading emasculates the text's martial spirit and dissolves social order. They are kept out by the reading's own non-violent commitments, which refuse the agonistic terms of their challenge.
% DISAPPEARANCE_RATIONALE: If the allegorical reading vanished, the literalist reading would reclaim unchallenged dominance in Hindu public discourse. Caste hierarchy would regain scriptural mandate. Non-violent resistance would lose its strongest textual anchor in the tradition. The moral economy of Hindu public life would shift toward hierarchical duty and legitimated violence.
% FOUNDING_PROBLEM: The literalist Gita was weaponized to justify caste oppression and colonial collaboration. Gandhi needed a reading that could claim the tradition's authority for anti-colonial non-violence and anti-caste reform.
% FOUNDING_PROBLEM_CORROBORATION: Gandhi's own writings (Autobiography, Gita commentaries) attest the founding problem. Ambedkar's critique corroborates from outside the beneficiary set — he agreed the literalist reading oppressed lower castes but rejected the allegorical reading as insufficient, arguing the text itself must be rejected. Postcolonial scholars (e.g., J.L. Mehta, Akeel Bilgrami) corroborate the reading's political genesis.
narrative_ontology:disappearance_verdict(gita_kurukshetra_discourse__gandhian_allegorical_reading, world_rearranges).
narrative_ontology:founding_problem_status(gita_kurukshetra_discourse__gandhian_allegorical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gita_kurukshetra_discourse__gandhian_allegorical_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gita_kurukshetra_discourse__gandhian_allegorical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.35) reflects that the Gandhian reading extracts interpretive authority from Brahminical scholars and delegitimates the literalist framework — a real but non-material transfer. Suppression (0.45) is moderate: the reading actively contests and marginalizes the literalist reading in Gandhian spaces but does not use state power to ban it. Theater ratio (0.15) is low: the reading's non-violent practice (satyagraha) is functionally integrated with its hermeneutic. Accessibility collapse (0.4) is moderate: the literalist reading remains widely available and culturally dominant, but the allegorical reading makes it hermeneutically unstable. Resistance (0.7) is high: orthodox institutions, Hindu nationalist movements, and traditional scholars vigorously contest the reading.
 *
 * PERSPECTIVAL GAP:
 *   From the Brahminical scholar seat, the constraint appears as a snare: it suppresses their authoritative reading without offering replacement coordination, using the text's own authority against them. From the lower caste seat, it appears as a rope: genuine coordination around liberation, net benefit, alternatives (literalist reading) not suppressed in wider society. From the Gandhian practitioner seat, it appears as a scaffold: transitional support for a non-violent social order that should eventually become self-sustaining. The engine computes these per-seat classifications from the structural data; the authored claim (rope) reflects the agenda-setter's self-understanding.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (lower castes, pacifists, individual conscience practitioners) gain interpretive freedom and spiritual resources for liberation — directionality near 0.0 (full beneficiary). Payers (Brahminical scholars, literalist traditionalists) lose interpretive monopoly and face hermeneutic delegitimation — directionality near 1.0 (full target). Brahminical scholars are constrained exit (institutional identity at stake); literalist traditionalists are identity-locked (apostasy cost). The agenda_setter (Gandhian practitioners) sits near beneficiary end (d ~ 0.15) — they propagate the reading but do not extract material rents. Observers (secular scholars) sit at analytical (d=0.5). Excluded (Hindu nationalists) are trapped — they cannot exit the discourse because it shapes the public sphere they contest.
 *
 * MANDATROPHY ANALYSIS:
 *   The reading was founded to solve colonial-era literalism that justified collaboration and caste oppression. That founding problem is CONTESTED: colonial rule ended, but caste oppression and militarized Hinduism persist. The reading's mandate has not atrophied — it faces renewed literalist dominance under Hindu nationalism. Mandatrophy is unresolved: the reading persists as active resistance, not inertial performance. Theater ratio remains low because the reading's practice (non-violent resistance) is still functionally necessary against actual violence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    allegorical_vs_literal_hermeneutic_boundary,
    'Is the allegorical reading a genuine recovery of the text''s original intent, or a modern projection onto an intrinsically violent text?',
    'Philological analysis of the Gita''s compositional layers, historical reception history, and comparative study of early commentaries (Shankara, Ramanuja, Abhinavagupta) on Kurukshetra''s meaning.',
    'If the allegorical reading is philologically untenable, its claim to be a ''rope'' (genuine coordination with tradition) weakens — it becomes a scaffold imposed on the text. If it has textual warrant, the literalist reading''s claim to authenticity is undermined.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(allegorical_vs_literal_hermeneutic_boundary, empirical, 'Whether the allegorical reading has textual-historical warrant or is a modern ethical projection.').

omega_variable(
    suppression_mechanism_hermeneutic,
    'Is the suppression of the literalist reading by the Gandhian reading structural (institutional marginalization) or internalized (literalist practitioners absorbing the critique)?',
    'Sociological study of traditional Vedantic institutions: do they engage the allegorical reading on its terms, or has it been internally neutralized through compartmentalization?',
    'If internalized, the effective suppression is higher than structural measures suggest — the literalist reading persists but in a hollowed form. If structural, the contest remains open and the suppression is contestable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_hermeneutic, conceptual, 'Whether hermeneutic suppression operates through external marginalization or internal absorption.').

omega_variable(
    kernel_reading_foreclosure_structure,
    'Does the Gandhian reading''s core premise (violence is always adharmic) logically foreclose the orthodox literal reading (righteous violence is dharmic) within any single framework, or do they merely coexist as rival interpretations?',
    'Logical analysis of the two readings'' axiom sets: if one asserts ''all violence is adharmic'' and the other asserts ''some violence is dharmic'', they are contradictory. Test whether any framework (e.g., contextualist, metaphorical) can hold both without equivocation.',
    'If forecloses, the kernel has a genuine fault line — the readings cannot be reconciled. If coexists_with, the kernel sustains pluralism. The relation choice affects contamination propagation in the network.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_structure, conceptual, 'Structural relationship between the allegorical and literalist readings of the Kurukshetra discourse.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gita_tr_t0, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(gita_tr_t20, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 20, 0.08).
narrative_ontology:measurement(gita_tr_t40, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(gita_tr_t60, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 60, 0.12).
narrative_ontology:measurement(gita_tr_t80, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 80, 0.14).
narrative_ontology:measurement(gita_tr_t100, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(gita_be_t0, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(gita_be_t20, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 20, 0.25).
narrative_ontology:measurement(gita_be_t40, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 40, 0.3).
narrative_ontology:measurement(gita_be_t60, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 60, 0.32).
narrative_ontology:measurement(gita_be_t80, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 80, 0.34).
narrative_ontology:measurement(gita_be_t100, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 100, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(gita_su_t0, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(gita_su_t20, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 20, 0.35).
narrative_ontology:measurement(gita_su_t40, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 40, 0.4).
narrative_ontology:measurement(gita_su_t60, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 60, 0.42).
narrative_ontology:measurement(gita_su_t80, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 80, 0.44).
narrative_ontology:measurement(gita_su_t100, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 100, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gita_kurukshetra_discourse__gandhian_allegorical_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.08).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__gandhian_allegorical_reading, gita_kurukshetra_discourse__orthodox_literal_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__gandhian_allegorical_reading, gita_kurukshetra_discourse__universalist_devotional_reading).

% DUAL FORMULATION NOTE:
% This story decomposes the 'Gita Kurukshetra discourse' kernel into three structurally distinct constraint stories. The orthodox literal reading (high extraction, caste/violence mandate) and universalist devotional reading (moderate extraction, accessibility focus) are separate constraints with different ε values and beneficiary/victim structures. The Gandhian reading's ε assesses the literalist arrangement as extractive; the literalist reading's ε would assess the Gandhian arrangement as suppressive. They are not the same constraint viewed differently — they are different constraints linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gita_kurukshetra_discourse__gandhian_allegorical_reading, institutional, 0.85).
constraint_indexing:directionality_override(gita_kurukshetra_discourse__gandhian_allegorical_reading, organized, 0.75).
constraint_indexing:directionality_override(gita_kurukshetra_discourse__gandhian_allegorical_reading, powerless, 0.1).
constraint_indexing:directionality_override(gita_kurukshetra_discourse__gandhian_allegorical_reading, moderate, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
