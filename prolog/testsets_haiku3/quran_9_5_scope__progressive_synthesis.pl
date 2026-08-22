% ============================================================================
% CONSTRAINT STORY: quran_9_5_scope__progressive_synthesis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_9_5_scope__progressive_synthesis, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: quran_9_5_scope__progressive_synthesis
 *   human_readable: Quranic 9:5 Progressive Ethical Trajectory Reading
 *   domain: religious/political/hermeneutics
 *
 * SUMMARY:
 *   Qur'an 9:5, revealed in 7th-century Medina, directs Muslims to 'slay the
 *   polytheists wherever ye find them.' Islamic jurisprudence has produced
 *   three structurally distinct readings of this verse's scope and binding
 *   force: (1) Abrogating/Universal reading treats the verse as superseding
 *   (nasikh) all prior peaceful verses and establishing a standing universal
 *   obligation for offensive jihad; (2) Contextual/Defensive reading situates
 *   the verse in its specific Medinan context (treaty-breaking tribes,
 *   defensive necessity) and denies it abrogates peaceful verses; (3)
 *   Progressive/Synthesis reading treats the verse as a time-bound
 *   7th-century political directive whose Qur'anic ethical trajectory
 *   (compassion, mercy, justice) constitutes the deeper binding principle,
 *   leaving the literal directive historically contingent. This JSON
 *   instantiates ONLY the progressive synthesis reading as a single
 *   constraint with stable ε, stable beneficiary/victim structure, and stable
 *   epistemic referent. The other readings are separate constraint stories in
 *   the family. The constraint described here is not the verse itself, but
 *   the hermeneutical move—the frame adoption—that reads the verse as
 *   time-bound rather than eternal.
 *
 * KEY AGENTS:
 *   - Progressive Muslim communities (beneficiary, institutional adoption of reading)
 *   - Secular pluralist frameworks (institutional beneficiary, weakened literal-textual-authority claims)
 *   - Textualist authority structures (payer, institutional authority undermined by historical-contextual shift)
 *   - Abrogating reading adherents (payer, their interpretation loses institutional ground when progressive reading dominates)
 *   - Islamic legal scholars (agenda-setter, canonize or reject the reading in curricula and fatwas)
 *   - Contextual reading adherents (allied beneficiary, progressive reading reinforces their contextual emphasis)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_9_5_scope__progressive_synthesis, 0.15).
domain_priors:suppression_score(quran_9_5_scope__progressive_synthesis, 0.08).
domain_priors:theater_ratio(quran_9_5_scope__progressive_synthesis, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, extractiveness, 0.15).
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_9_5_scope__progressive_synthesis, rope).
narrative_ontology:human_readable(quran_9_5_scope__progressive_synthesis, "Quranic 9:5 Progressive Ethical Trajectory Reading").
narrative_ontology:topic_domain(quran_9_5_scope__progressive_synthesis, "religious/political/hermeneutics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_9_5_scope__progressive_synthesis, '3bc9cc23-429d-4034-8c92-377fd0e029c3').
narrative_ontology:cs_kernel_codification('3bc9cc23-429d-4034-8c92-377fd0e029c3', fixed_text).
narrative_ontology:cs_authority_grounding('3bc9cc23-429d-4034-8c92-377fd0e029c3', lineage).
narrative_ontology:cs_interpretation_layer_present('3bc9cc23-429d-4034-8c92-377fd0e029c3').
narrative_ontology:cs_reading_relation('3bc9cc23-429d-4034-8c92-377fd0e029c3', quran_9_5_scope__abrogating_universal, forecloses).
narrative_ontology:cs_reading_relation('3bc9cc23-429d-4034-8c92-377fd0e029c3', quran_9_5_scope__contextual_defensive, coexists_with).
narrative_ontology:cs_axiom('3bc9cc23-429d-4034-8c92-377fd0e029c3', foundational, historical_contingency_of_specific_directives).
narrative_ontology:cs_axiom_status(historical_contingency_of_specific_directives, holdable).
narrative_ontology:cs_axiom_grounding('3bc9cc23-429d-4034-8c92-377fd0e029c3', historical_contingency_of_specific_directives, empirically_contingent).
narrative_ontology:cs_axiom('3bc9cc23-429d-4034-8c92-377fd0e029c3', foundational, quranic_ethical_trajectory_supersedes_literalism).
narrative_ontology:cs_axiom_status(quranic_ethical_trajectory_supersedes_literalism, holdable).
narrative_ontology:cs_axiom_grounding('3bc9cc23-429d-4034-8c92-377fd0e029c3', quranic_ethical_trajectory_supersedes_literalism, deontological).
narrative_ontology:cs_reference_frame('3bc9cc23-429d-4034-8c92-377fd0e029c3', quranic_ethical_coherence_through_trajectory).
narrative_ontology:cs_drift_state('3bc9cc23-429d-4034-8c92-377fd0e029c3', contemporary_pluralist_islam, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3bc9cc23-429d-4034-8c92-377fd0e029c3', '').
narrative_ontology:cs_kernel_id(quran_9_5_scope__progressive_synthesis, quran_9_5_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_9_5_scope__progressive_synthesis, secular_pluralist_frameworks).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__progressive_synthesis, progressive_muslim_communities).
narrative_ontology:constraint_victim(quran_9_5_scope__progressive_synthesis, textualist_authority_structures).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__progressive_synthesis, contextual_defensive_adherents).
narrative_ontology:constraint_victim(quran_9_5_scope__progressive_synthesis, abrogating_universal_adherents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adopt an interpretive frame that treats Qur'anic ethical trajectory (compassion, justice, mercy over the verses) as superseding literalist applications of specific historical directives. This reading allows them to inhabit Islamic tradition while rejecting universalized warfare obligations and maintaining theological coherence with pluralist modern contexts. They argue the verse's historical specificity, once acknowledged, frees Islamic theology from defensive postures on this text.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, progressive_muslim_communities, beneficiary,
    organized, generational, mobile, global).

% Gain intellectual and political space when authoritative Islamic voices adopt historical-contextual readings that exit the verse from active legal/ethical obligation. The reading weakens claims that Islam mandates or endorses offensive warfare; it shifts the burden of proof onto those who would universalize a 7th-century directive. Secularists do not author this reading but benefit strategically from its adoption.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, secular_pluralist_frameworks, beneficiary,
    institutional, generational, arbitrage, global).

% Hold interpretive authority grounded in the claim that the Qur'an's text states universal ethical directives that remain binding across time unless explicitly abrogated. They bear the cost of this reading's adoption: their claimed authority over textual meaning is diminished; the hermeneutical ground shifts from 'what does the text say universally' to 'what is the ethical trajectory we discern'. Their institutional position depends partly on the authority of literal textual reading.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, textualist_authority_structures, payer,
    institutional, generational, constrained, global).

% Adopt the abrogating reading (nasikh doctrine: verse 9:5 supersedes prior peaceful verses as a universal standing obligation). This reading's authority is damaged when the progressive reading gains traction in scholarly/public discourse—the historical-contextual frame directly undercuts the claim that the verse establishes eternal law. They are partially excluded from the conversation when the progressive reading dominates in certain institutional contexts (academic theology, interfaith dialogue), but retain authority in more literalist institutional spaces.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, abrogating_universal_adherents, payer,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(quran_9_5_scope__progressive_synthesis, abrogating_universal_adherents, excluded).

% Adopt the contextual reading (verse 9:5 addresses specific 7th-century Medinan context, defensive only, does not abrogate). The progressive reading does not foreclose this position—both agree the verse is contextually bound—but the progressive reading's emphasis on ethical trajectory and historical contingency creates allied interpretive pressure. Contextual adherents benefit from the progressive reading's institutional success because both undercut the abrogating reading's claim to universal binding force.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, contextual_defensive_adherents, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(quran_9_5_scope__progressive_synthesis, contextual_defensive_adherents, observer).

% Exist outside any Islamic legal framework; the verse's directives do not constrain them directly under any reading. However, they have an interest in how Muslims read the verse: the progressive reading's assertion that verse 9:5 is not a standing constraint removes one theological basis for targeting them specifically. They are analytical observers of an intra-Islamic hermeneutical dispute.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, polytheist_communities, observer,
    organized, biographical, mobile, global).

% Adjudicate which reading gains recognition as authoritative within Islamic tradition. The progressive reading's adoption depends on their institutional endorsement—textbooks, fatwas, seminary curricula. They exercise agenda-setting power by choosing which interpretations to teach and canonize. This reading represents one trajectory of their professional deliberation, not a constraint they created.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, islamic_legal_scholars, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(quran_9_5_scope__progressive_synthesis, islamic_legal_scholars, observer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_9_5_scope__progressive_synthesis, secular_pluralist_frameworks).
narrative_ontology:fixing_cost_class(quran_9_5_scope__progressive_synthesis, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the theological coherence crisis by providing a frame that allows Islamic communities to maintain theological integrity while rejecting literalist warfare obligations and coexisting in pluralist societies.
% TRANSFER_FUNCTION: Transfers hermeneutical authority from literal textual command to ethical-trajectory inference; redistributes interpretive power from textualist institutional authorities to progressive scholars and Muslim communities engaged in historical-critical reasoning.
% ABSENT_VOICES: Conservative textualist authorities in majority-Muslim contexts and institutionalist Islamic legal scholars tied to traditional authority structures are partially excluded from dominant scholarly discourse (especially academic, interfaith, and progressive institutional venues) when the progressive reading becomes canonical; they retain voices in traditional legal institutions but lose hegemony in publicly-prominent Islamic theology.

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_9_5_scope__progressive_synthesis, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quran_9_5_scope__progressive_synthesis, 'none', 1).
narrative_ontology:epsilon_provenance(quran_9_5_scope__progressive_synthesis, 0.15, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_9_5_scope__progressive_synthesis_tests).
:- end_tests(quran_9_5_scope__progressive_synthesis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the progressive reading does not extract material value from any party—it redistributes hermeneutical authority and interpretive frameworks, not rents. Suppression is minimal (0.08) because adoption of this reading is not coerced; it spreads through intellectual persuasion in scholarly and religious communities. Theater is low (0.12) because the interpretive work is genuine—scholars study Qur'anic ethics, historical context, and textual coherence—not performative maintenance of a function that has atrophied. Accessibility collapse is very low (0.25) because alternatives (abrogating, contextual readings) remain live options held by significant institutional constituencies; the progressive reading has not foreclosed exit from literalism, only offered an alternative. Resistance is high (0.72) because literalist and textualist authorities actively resist the frame shift; they defend the universal-binding-force interpretation. The measurement series shows low monotonic rise in extractiveness (institutional adoption slowly diffuses through academic and progressive Islamic institutions) and slight rises in theater and suppression as the reading becomes canonized in curricula and fatwas—increased institutional formalization creates minor performative and enforcement overhead, but the core activity remains hermeneutical work. By time-point 35 the metrics flatten, projecting stable institutional adoption without further rise.
 *
 * PERSPECTIVAL GAP:
 *   Progressive Muslim scholars and secular pluralist institutions perceive this reading as liberation of ethical coherence and intellectual flexibility; textualist authorities perceive it as erosion of textual authority and theological precision. From the progressive seat, the verse exits constraint-space entirely (it becomes illustrative of ethical trajectory, not a standing obligation). From the textualist seat, the reading is an attempt to rewrite the text's meaning—a violation of textual authority. The engine computes per-seat directionality from beneficiary/victim declarations: progressive communities and secularists sit near d=0.0 (beneficiaries), textualists sit near d=1.0 (bear the cost of eroded authority). The divergence is structural, not a measurement error.
 *
 * DIRECTIONALITY LOGIC:
 *   This reading's primary beneficiaries are (1) progressive Muslim communities seeking theological coherence without warfare obligations, and (2) secular pluralist frameworks that strategically benefit from weakened literalist authority. The primary victim is textualist authority structures (Islamic legal institutions, conservative scholarship) whose claimed authority over universal textual meaning is diminished when the frame shifts to historical contingency and ethical trajectory. Textualist authority is not destroyed—literalist readings persist in conservative institutions—but is de-centered in interfaith, academic, and progressive contexts. The reading requires no active enforcement (it is adopted voluntarily through scholarly persuasion), so d derivation is from the beneficiary/victim asymmetry alone. Progressive communities have high mobility (they can adopt the reading or not; it does not trap them); textualist structures have constrained exit (their institutional authority depends on textual literal-binding claims, so the reading's success threatens their institutional position). This asymmetry in exit pushes the directionality divergence.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live: Islamic theology must achieve coherence between warfare texts and ethical texts. The progressive reading solves (or attempts to solve) this genuine coordination problem—it restores coherence by invoking ethical trajectory as the deeper binding principle. This is not a dead mandate maintained by inertia. The reading requires active scholarly engagement (it is not theater). However, mandatrophy emerges at the victim seat (textualist authority): they maintain the claim that the verse is eternally binding partly from institutional inertia and partly from genuine theological conviction. If the founding problem were solved (Islamic communities universally adopted progressive readings), textualist authority claims would atrophy into ceremonial roles, making the reading itself move from rope (genuine coordination function) to piton (maintained by institutional momentum with diminished functional need). The story is authored at a moment when the founding problem is still live and the reading's coordination function (restore theological coherence) is still active, so mandatrophy_resolved is false. A future state where the reading is universalized would require re-analysis as piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ethical_trajectory_discernment_ambiguity,
    'What constitutes a ''Qur''anic ethical trajectory'' that supersedes literal text? On what grounds do we identify which principles are the deeper ethical commitments vs. which are specific historical applications?',
    'Inter-textual analysis of Qur''anic ethics across revealed phases (Meccan, early Medinan, late Medinan); scholarly consensus on which principles appear foundational vs. circumstantial; whether trajectory is grounded in revelation history or in contemporary ethical reasoning applied retrospectively to the text.',
    'If the trajectory is grounded in revelation history, the progressive reading has strong epistemic grounding. If trajectory is retrospectively imposed from contemporary values, the reading may collapse into ad hoc modern revisionism—a vulnerability textualists exploit. This affects whether the reading persists as scholarly consensus or remains contestable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ethical_trajectory_discernment_ambiguity, conceptual, 'Whether ''Qur''anic ethical trajectory'' is discovered from the text''s own development or imposed from external ethical frameworks.').

omega_variable(
    authority_grounding_under_drift,
    'Does the shift from literal textual authority to ethical-trajectory authority represent a genuine drift in Islamic scholarly consensus, or a contestable hermeneutical choice that cannot claim institutional incumbency?',
    'Historical documentation of which readings held majority scholar support in each century; whether progressive reading gains canonical status in Islamic seminaries and official fatwas, or remains a minority scholarly position.',
    'If progressive reading achieves canonical status, it becomes the new textual-authority frame and textualists become defenders of a competing reading. If it remains minority scholarship, textualist authority retains institutional dominance and the progressive reading''s extractiveness remains low (minimal institutional adoption). The reading''s power depends partly on institutional canonization, which is not guaranteed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_grounding_under_drift, empirical, 'Whether the progressive reading achieves sufficient institutional adoption to shift Islamic jurisprudential authority frames.').

omega_variable(
    identity_locked_textualist_exit,
    'To what extent are textualist Islamic scholars identity-locked into textual-literalism, vs. able to adopt progressive readings without professional/identity-coherence collapse?',
    'Biographical and sociological study of textualist-to-progressive conversions (or resistance to conversion) among Islamic scholars; whether the shift is framed as intellectual growth or identity betrayal.',
    'High identity-lock increases textualist resistance to the reading and raises the effective cost to them (exit is not mobile, it is identity-threatening). Low identity-lock suggests textualist adherents can shift without structural cost, reducing the reading''s extractiveness. This affects whether the reading''s adoption proceeds by persuasion or requires generational replacement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_textualist_exit, empirical, 'Whether textualist professional identity is separable from literal-textual-authority claims.').

omega_variable(
    secular_benefit_causal_direction,
    'Does the progressive reading''s adoption occur because secular pluralist interests drive it (the reading is instrumentally chosen), or does it emerge organically from Islamic theological reflection and happen to benefit secular frameworks (the reading is driven by internal Islamic reasoning)?',
    'Historical tracing of where the progressive reading originated (Islamic scholar initiatives vs. secular academic influence vs. political pressure); whether Muslim scholars report their reasoning as theological-internal or responsive to external pressure.',
    'If secular influence drove the reading, it may be vulnerable to critique as externally imposed (orientalist contamination of Islamic thought) and textualists can frame it as loss of Islamic authenticity. If the reading emerged from internal Islamic reasoning, its legitimacy is stronger within Islamic tradition. This affects whether the reading''s institutional adoption is framed as intellectual independence or capitulation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secular_benefit_causal_direction, empirical, 'Whether the progressive reading is organically Islamic-theologized or instrumentally adopted to serve secular interests.').

omega_variable(
    kernel_vs_reading_identity,
    'Is the progressive reading a stable constraint instantiation of the kernel, or is the reading so different from the kernel''s literal text that we have actually created a new constraint entirely (the kernel does not permit this reading, we have replaced it)?',
    'Comparison of the reading''s referent (what it is about) with other readings'' referents: do all three readings interpret the same text, or does the progressive reading interpret something different (ethical trajectory, not the verse)? If the progressive reading''s referent is ''what the Qur''an''s ethical arc implies should override this specific verse,'' the referent is not the verse itself, it is the ethical arc—a different constraint.',
    'If the progressive reading has a genuinely different referent, it should be listed as a separate kernel in constraint taxonomy, not a reading of quran_9_5_scope. If it shares the kernel''s referent (the verse''s binding force), it is a valid reading. The distinction affects how the reading is classified in broader Islamic jurisprudential framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_vs_reading_identity, conceptual, 'Whether the progressive reading''s referent is the verse''s scope or the Qur''an''s ethical trajectory (and thus whether it is truly a reading of the kernel or a new constraint).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_9_5_scope__progressive_synthesis, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_9_5_scope__progressive_synthesis, theater_ratio, 0, 0.08).
narrative_ontology:measurement(qura_tr_t7, quran_9_5_scope__progressive_synthesis, theater_ratio, 7, 0.09).
narrative_ontology:measurement(qura_tr_t14, quran_9_5_scope__progressive_synthesis, theater_ratio, 14, 0.1).
narrative_ontology:measurement(qura_tr_t21, quran_9_5_scope__progressive_synthesis, theater_ratio, 21, 0.11).
narrative_ontology:measurement(qura_tr_t28, quran_9_5_scope__progressive_synthesis, theater_ratio, 28, 0.11).
narrative_ontology:measurement(qura_tr_t35, quran_9_5_scope__progressive_synthesis, theater_ratio, 35, 0.12).
narrative_ontology:measurement(qura_tr_t42, quran_9_5_scope__progressive_synthesis, theater_ratio, 42, 0.12).
narrative_ontology:measurement(qura_tr_t50, quran_9_5_scope__progressive_synthesis, theater_ratio, 50, 0.12).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_9_5_scope__progressive_synthesis, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(qura_be_t7, quran_9_5_scope__progressive_synthesis, base_extractiveness, 7, 0.1).
narrative_ontology:measurement(qura_be_t14, quran_9_5_scope__progressive_synthesis, base_extractiveness, 14, 0.12).
narrative_ontology:measurement(qura_be_t21, quran_9_5_scope__progressive_synthesis, base_extractiveness, 21, 0.13).
narrative_ontology:measurement(qura_be_t28, quran_9_5_scope__progressive_synthesis, base_extractiveness, 28, 0.14).
narrative_ontology:measurement(qura_be_t35, quran_9_5_scope__progressive_synthesis, base_extractiveness, 35, 0.15).
narrative_ontology:measurement(qura_be_t42, quran_9_5_scope__progressive_synthesis, base_extractiveness, 42, 0.15).
narrative_ontology:measurement(qura_be_t50, quran_9_5_scope__progressive_synthesis, base_extractiveness, 50, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_9_5_scope__progressive_synthesis, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(qura_su_t7, quran_9_5_scope__progressive_synthesis, suppression_requirement, 7, 0.06).
narrative_ontology:measurement(qura_su_t14, quran_9_5_scope__progressive_synthesis, suppression_requirement, 14, 0.07).
narrative_ontology:measurement(qura_su_t21, quran_9_5_scope__progressive_synthesis, suppression_requirement, 21, 0.07).
narrative_ontology:measurement(qura_su_t28, quran_9_5_scope__progressive_synthesis, suppression_requirement, 28, 0.08).
narrative_ontology:measurement(qura_su_t35, quran_9_5_scope__progressive_synthesis, suppression_requirement, 35, 0.08).
narrative_ontology:measurement(qura_su_t42, quran_9_5_scope__progressive_synthesis, suppression_requirement, 42, 0.08).
narrative_ontology:measurement(qura_su_t50, quran_9_5_scope__progressive_synthesis, suppression_requirement, 50, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_9_5_scope__progressive_synthesis, identity_coordination).
narrative_ontology:boltzmann_floor_override(quran_9_5_scope__progressive_synthesis, 0.12).
narrative_ontology:affects_constraint(quran_9_5_scope__progressive_synthesis, quran_9_5_scope__abrogating_universal).
narrative_ontology:affects_constraint(quran_9_5_scope__progressive_synthesis, quran_9_5_scope__contextual_defensive).

% DUAL FORMULATION NOTE:
% The kernel quran_9_5_scope instantiates three structurally distinct constraints: (1) abrogating_universal: ε≈0.85, victims include peaceist Islamic voices, beneficiaries include offensive-jihad institutional structures; (2) contextual_defensive: ε≈0.35, beneficiaries include defensive-warrior legitimation, victims include abrogating-reading adherents; (3) progressive_synthesis (this reading): ε≈0.15, beneficiaries include progressive Muslims and secular pluralists, victims include textualist authority structures. The three readings decompose the kernel because their ε values differ dramatically (85% vs 35% vs 15%), they have different beneficiary/victim structures (completely disjoint sets), and they make different claims about the verse's binding force. They are not the same constraint viewed from three perspectives—they are three different constraints whose common referent (the verse's text) masks underlying structural incommensurability. All three readings link via network.affects_constraints to show the kernel family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quran_9_5_scope__progressive_synthesis, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
