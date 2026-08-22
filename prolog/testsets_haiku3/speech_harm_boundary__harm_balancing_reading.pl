% ============================================================================
% CONSTRAINT STORY: speech_harm_boundary__harm_balancing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_harm_boundary__harm_balancing_reading, []).

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
 *   constraint_id: speech_harm_boundary__harm_balancing_reading
 *   human_readable: Speech Protection with Demonstrated-Harm Override (Proportionality Balancing)
 *   domain: constitutional/political philosophy
 *
 * SUMMARY:
 *   The harm-balancing reading of the speech-protection kernel positions
 *   speech protection as a strong but rebuttable presumption: speech is
 *   presumptively free unless the speaker can be shown to have caused
 *   concrete, severe, proximately-caused harm to identifiable persons or
 *   groups, and the restriction is proportional to that harm. This reading
 *   sits between two contested siblings: the absolutist reading (speech
 *   nearly always protected, harm threshold near-impossible to meet) and the
 *   dignity reading (speech that denies personhood is categorically
 *   unprotected, no case-by-case balancing required). The harm-balancing
 *   reading claims to be a middle ground, combining speech protection with
 *   accountability; critics argue it is either too permissive (leaving
 *   harmful speech in place) or too vague (inviting inconsistent
 *   weaponization).
 *
 * KEY AGENTS:
 *   - Judicial interpreters: set the precedent that defines what 'demonstrated harm' means and how severe it must be
 *   - General and protected-speech beneficiaries: those who gain from presumptive protection
 *   - Targeted harm-bearers and restricted speakers: those who pay when the harm threshold is crossed
 *   - Platform operators: enforce the boundary at scale and navigate both protection and restriction pressure
 *   - Absolutist and dignity advocates: contest the reading from excluded positions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_harm_boundary__harm_balancing_reading, 0.52).
domain_priors:suppression_score(speech_harm_boundary__harm_balancing_reading, 0.48).
domain_priors:theater_ratio(speech_harm_boundary__harm_balancing_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_harm_boundary__harm_balancing_reading, tangled_rope).
narrative_ontology:human_readable(speech_harm_boundary__harm_balancing_reading, "Speech Protection with Demonstrated-Harm Override (Proportionality Balancing)").
narrative_ontology:topic_domain(speech_harm_boundary__harm_balancing_reading, "constitutional/political philosophy").

domain_priors:requires_active_enforcement(speech_harm_boundary__harm_balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_harm_boundary__harm_balancing_reading, '79406d1c-7421-41d2-8ebb-46b79a83539a').
narrative_ontology:cs_kernel_codification('79406d1c-7421-41d2-8ebb-46b79a83539a', formalized).
narrative_ontology:cs_authority_grounding('79406d1c-7421-41d2-8ebb-46b79a83539a', lineage).
narrative_ontology:cs_interpretation_layer_present('79406d1c-7421-41d2-8ebb-46b79a83539a').
narrative_ontology:cs_reading_relation('79406d1c-7421-41d2-8ebb-46b79a83539a', speech_harm_boundary__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('79406d1c-7421-41d2-8ebb-46b79a83539a', speech_harm_boundary__dignity_reading, influences).
narrative_ontology:cs_axiom('79406d1c-7421-41d2-8ebb-46b79a83539a', foundational, speech_protection_presumptive).
narrative_ontology:cs_axiom_status(speech_protection_presumptive, holdable).
narrative_ontology:cs_axiom_grounding('79406d1c-7421-41d2-8ebb-46b79a83539a', speech_protection_presumptive, deontological).
narrative_ontology:cs_axiom('79406d1c-7421-41d2-8ebb-46b79a83539a', foundational, demonstrated_harm_override_justified).
narrative_ontology:cs_axiom_status(demonstrated_harm_override_justified, holdable).
narrative_ontology:cs_axiom_grounding('79406d1c-7421-41d2-8ebb-46b79a83539a', demonstrated_harm_override_justified, empirically_contingent).
narrative_ontology:cs_reference_frame('79406d1c-7421-41d2-8ebb-46b79a83539a', speech_protection_presumptive_with_harm_override).
narrative_ontology:cs_drift_state('79406d1c-7421-41d2-8ebb-46b79a83539a', contemporary_digital_scale, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('79406d1c-7421-41d2-8ebb-46b79a83539a', '').
narrative_ontology:cs_kernel_id(speech_harm_boundary__harm_balancing_reading, speech_harm_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_harm_boundary__harm_balancing_reading, general_speech_practitioners).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__harm_balancing_reading, protected_speech_beneficiaries).
narrative_ontology:constraint_victim(speech_harm_boundary__harm_balancing_reading, targeted_harm_bearers).
narrative_ontology:constraint_victim(speech_harm_boundary__harm_balancing_reading, speakers_restricted_by_harm_threshold).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(speech_harm_boundary__harm_balancing_reading, platform_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Journalists, academics, artists, activists, and everyday citizens benefit from the presumption that speech is protected unless demonstrated harm can be shown to be severe and proximately caused. They operate within a framework where restriction requires a showing, not a suppressed alternative.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, general_speech_practitioners, beneficiary,
    organized, generational, constrained, national).

% Marginalized speakers, dissident movements, and politically disfavored groups benefit from the presumption because it protects their ability to articulate grievances without prior censorship. The harm threshold prevents majoritarian silencing of minority voice.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, protected_speech_beneficiaries, beneficiary,
    moderate, generational, constrained, national).

% Individuals and groups subject to hate speech, targeted harassment, defamation, and group libel incur psychological, reputational, and sometimes material injury. The harm-balancing reading acknowledges this cost but requires it to be demonstrated, measured, and proportional to override the speaker's presumptive protection.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, targeted_harm_bearers, payer,
    moderate, biographical, constrained, national).

% Speakers whose utterances cross the demonstrated-harm threshold bear the restriction cost: removal of content, deplatforming, legal liability, or criminal penalty. They argue the threshold is applied inconsistently and weaponized against disfavored speakers; they must navigate an ambiguous boundary.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, speakers_restricted_by_harm_threshold, payer,
    moderate, biographical, constrained, national).

% Courts and constitutional review bodies adjudicate which speech restrictions pass the harm threshold and are proportional. They set precedent that determines which categories of speech fall under the override, how harm must be demonstrated, and what weight speakers' interests receive in the balance.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, judicial_interpreters, agenda_setter,
    institutional, generational, analytical, national).

% Digital platforms enforce speech norms at scale, removing content that triggers harm-based restrictions. They bear compliance costs, face pressure from both directions (to restrict more and to restrict less), and their enforcement patterns shape which speech categories are effectively protected.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, platform_operators, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(speech_harm_boundary__harm_balancing_reading, platform_operators, payer).

% Advocates who believe speech should be protected nearly absolutely, regardless of harm, contest this reading. They argue the harm threshold is both too permissive (it allows too much restriction in principle) and too vague (it invites weaponization in practice). They are excluded from the harm-balancing consensus but present continual challenge.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, absolutist_advocates, excluded,
    moderate, generational, constrained, national).

% Advocates who believe personhood-denying speech should be categorically unprotected (speech that denies the human status of a group) contest the harm-balancing reading as insufficiently protective of dignity. They argue requiring demonstrated individualized harm misses the structural violation of group personhood.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, dignity_advocates, excluded,
    moderate, generational, constrained, national).

% Legislatures may codify the harm-balancing standard into statute or leave it to common law and constitutional interpretation. Their silence or action shapes the boundary between constitutional presumption and statutory override.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, legislative_bodies, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_harm_boundary__harm_balancing_reading, judicial_interpreters).
narrative_ontology:fixing_cost_class(speech_harm_boundary__harm_balancing_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable, generally-predictable framework for distinguishing protected speech from harmful speech by placing the burden on those who would restrict speech to demonstrate concrete harm. This solves the coordination problem of how a society maintains free expression while acknowledging that speech can injure, without collapsing into either absolutism (no restriction possible) or censoriousness (restriction at mere discomfort).
% TRANSFER_FUNCTION: Transfers restriction costs (the burden of not speaking, of censorship, of criminal or civil liability) from the general public to speakers whose utterances are demonstrated to cause severe harm. The harm-balancing reading asymmetrically allocates this burden: the default presumption protects speakers, but once harm is shown to be severe and proportional, the speaker bears the cost of stopping or facing sanction.
% ABSENT_VOICES: Absolutist free-speech advocates argue that even the harm threshold is illegitimate and should not exist; dignity-centered advocates argue the threshold is too weak and allows personhood-denying speech to persist. Both groups contest the harm-balancing reading but are excluded from the consensus that sets it as the framework. Historically silenced groups who would benefit from more restrictive speech norms (if such restrictions applied to dominant-group speech) are underrepresented in constitutional interpretation.
% DISAPPEARANCE_RATIONALE: If the harm-balancing reading evaporated overnight, either strict absolutism or categorical dignity-based restrictions would likely become the default framework. The entire infrastructure of hate-speech law, harassment-based restriction, and proportionality balancing would collapse, reorganizing speech norms around whichever successor framework emerged. Publishers, platforms, and judicial systems would have to rapidly establish a new boundary.
% FOUNDING_PROBLEM: The founding problem is the need to coherently distinguish between speech worth protecting and speech that causes concrete injury, without either absolutizing protection (ignoring real harm) or collapsing into paternalistic censorship (restricting speech because it is discomfiting or controversial). The harm-balancing reading was built to avoid both extremes.
% FOUNDING_PROBLEM_CORROBORATION: Judicial opinions, academic literature on freedom of expression, and international human-rights bodies (e.g., the European Court of Human Rights) attest that the founding problem is live: courts regularly must adjudicate between speech rights and harm claims. Absolutist advocates and dignity-centered advocates contest whether the problem is solved correctly by this reading, but both accept that a problem exists.
narrative_ontology:disappearance_verdict(speech_harm_boundary__harm_balancing_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_harm_boundary__harm_balancing_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_harm_boundary__harm_balancing_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(speech_harm_boundary__harm_balancing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_harm_boundary__harm_balancing_reading, 0.52, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_harm_boundary__harm_balancing_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_harm_boundary__harm_balancing_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_harm_boundary__harm_balancing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness score (0.52, rising to 0.61 then falling to 0.52) reflects the constraint's hybrid nature: it benefits general speakers via presumption, but extracts restriction costs from those whose speech crosses the harm threshold. The measurement trajectory models increasing extraction pressure in the middle interval (t=20–30, where enforcement infrastructure matured and harm categories broadened) followed by a slight regression (t=35–40, reflecting increased political contestation and pushback from absolutist and dignity advocates). Theater is moderate (0.28): some judicial reasoning publicly emphasizes 'careful balancing' while enforcement increasingly relies on simplified harm categories that bypass the balance. Suppression is moderate (0.48): the framework is publicly legitimated and not coercive in the sense of secret police, but active enforcement (content removal, platform moderation, legal liability) does suppress speech that practitioners argue should be protected. Accessibility collapse is moderate (0.62): speakers can understand the boundary in principle (harm must be demonstrated and severe), but the boundary is porous and context-dependent, so exit options are neither fully trapped nor fully free — speakers who want to critique a group walk a contested line.
 *
 * PERSPECTIVAL GAP:
 *   From the judicial/platform-operator seat, the harm-balancing reading is a coherent, proportional solution that protects most speech while addressing genuine injury. From the absolutist seat, it is a betrayal of free-speech principle that invites mission-creep: today's 'demonstrated harm' becomes tomorrow's 'possible harm' becomes next year's 'controversy.' From the dignity seat, it is insufficient: requiring individualized harm-showing perpetuates the structural exclusion of groups whose personhood is at stake. The engine computes these divergent types from the power and exit differential: powerful speakers (e.g., mainstream media) have better exit options and face lower effective restriction risk; powerless speakers (e.g., dissidents without platform) have constrained exit and face higher effective risk of the harm threshold being used against them. The same reading produces different constraints for different seats.
 *
 * DIRECTIONALITY LOGIC:
 *   The judicial interpreter (agenda setter) sits at low d: they set the rules and do not bear restriction costs. General speech practitioners and protected-speech beneficiaries sit at low-moderate d (beneficiaries): they gain from the presumption and bear few costs unless their speech causes demonstrable harm. Targeted harm-bearers and restricted speakers sit at high d (targets): they bear the restriction/injury costs. The harm-balancing reading shifts both beneficiaries and targets compared to the absolutist reading (where speakers would sit near d=0 and harm-bearers at d=1 with no override) and compared to the dignity reading (where speakers causing dignity violation sit near d=1 categorically). This reading's directionality is moderate because it genuinely coordinates speech protection while acknowledging asymmetric harm costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to balance speech protection and harm prevention) is not dead, but it is contested whether the harm-balancing framework solves it or perpetuates it. The constraint's persistence does NOT depend on the founding problem remaining unresolved — both absolutist and dignity advocates accept that a problem exists, they just dispute the solution. What the constraint does extract is the cost of negotiating the boundary: speakers must engage in constant calibration; platforms must apply ambiguous rules; harm-bearers must navigate a system that may or may not vindicate their injury. The measuring point (extractiveness rising then stabilizing) suggests the constraint has matured into an extraction mechanism: the coordination benefit (a stable framework for mediating speech claims) is real, but an increasing share of the enforcement machinery goes into defending the boundary against both absolutist and dignity-based challenges. This is tangled-rope structure, not snare, because the coordination is genuine and the beneficiaries (general speakers) are not trapped.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    demonstrated_harm_boundary_ambiguity,
    'What constitutes ''demonstrated harm'' sufficient to override speech protection? How severe must it be, how proximate must causation be, and who must demonstrate it?',
    'Systematic study of how courts and platforms apply the harm threshold across cases; tracking whether harm-demonstration standards converge or diverge; comparing outcomes for dominant vs. marginalized speakers applying the same standard.',
    'If the threshold is clear and consistently applied, the constraint operates as genuine coordination. If the threshold is vague and applied inconsistently, it operates as a covert restriction mechanism that privileges powerful speakers (who can afford legal defense and have sympathetic adjudicators) and harms powerless speakers (who cannot). A high divergence would suggest mandatrophy (founding problem unsolved) and would shift the computed type toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(demonstrated_harm_boundary_ambiguity, empirical, 'Whether ''demonstrated harm'' is operationally coherent or serves as a cover for inconsistent suppression.').

omega_variable(
    proportionality_balancing_weaponization,
    'Is the proportionality requirement (that restriction be proportional to the harm) genuinely applied, or does it function as mere rhetoric that permitting adjudicators can set aside?',
    'Audit of restriction decisions: measure the ratio of harm severity to restriction severity across cases; track whether proportionality objections are ever sustained; compare cases where the same harm level produces different restrictions.',
    'If proportionality is genuine, the constraint is truly tangled-rope: harm-bearers get their injury acknowledged and some speakers face real restriction. If proportionality is rhetorical, the constraint functions as pure restriction (snare) where harm-balancing language disguises the fact that restriction is applied inconsistently and excessively.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_balancing_weaponization, empirical, 'Whether proportionality balancing is a real constraint on restriction or performative cover.').

omega_variable(
    reading_coexistence_vs_foreclosure,
    'Can the harm-balancing reading and the dignity reading both be held within a single legal framework, or does the harm-balancing reading''s case-by-case approach logically foreclose the dignity reading''s categorical stance?',
    'Jurisprudential analysis: can a court apply both ''harm must be demonstrated and proportional'' AND ''personhood-denying speech is categorically unprotected'' in the same case? Or do they necessarily conflict?',
    'If they coexist, the reading_relations entry should be ''coexists_with''. If they foreclose, it should be ''forecloses''. The classifier affects how the engine models constraint family interaction: coexisting readings can both be live in a pluralistic system, but foreclosing readings cannot.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_coexistence_vs_foreclosure, conceptual, 'Whether this reading structurally forecloses the dignity reading or merely competes with it.').

omega_variable(
    platform_vs_state_authority_divergence,
    'Does the harm-balancing framework apply the same way to state speech restrictions (government censorship) and private-platform content moderation? Or do courts and platforms interpret the boundary differently, creating two separate constraint instances?',
    'Comparative analysis: track how harm-balancing is applied in constitutional cases (state action) vs. platform terms-of-service cases (private action); assess whether the threshold differs and whether the measurement metrics (extractiveness, suppression, theater) would differ if the same harm-balancing reading were applied in both contexts.',
    'If the two contexts produce genuinely different constraints, the proper decomposition is two stories linked by network.affects_constraints (per ε-invariance principle). If they apply the same reading and boundary, they are one story. The measurement series in this story assumes the judicial/constitutional context; platform-level instantiation would have different metrics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_vs_state_authority_divergence, conceptual, 'Whether harm-balancing produces one constraint or should decompose into state and platform variants.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_harm_boundary__harm_balancing_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_harm_boundary__harm_balancing_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(spee_tr_t5, speech_harm_boundary__harm_balancing_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement(spee_tr_t10, speech_harm_boundary__harm_balancing_reading, theater_ratio, 10, 0.21).
narrative_ontology:measurement(spee_tr_t15, speech_harm_boundary__harm_balancing_reading, theater_ratio, 15, 0.24).
narrative_ontology:measurement(spee_tr_t20, speech_harm_boundary__harm_balancing_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(spee_tr_t25, speech_harm_boundary__harm_balancing_reading, theater_ratio, 25, 0.29).
narrative_ontology:measurement(spee_tr_t30, speech_harm_boundary__harm_balancing_reading, theater_ratio, 30, 0.31).
narrative_ontology:measurement(spee_tr_t35, speech_harm_boundary__harm_balancing_reading, theater_ratio, 35, 0.29).
narrative_ontology:measurement(spee_tr_t40, speech_harm_boundary__harm_balancing_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(spee_be_t5, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(spee_be_t10, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 10, 0.46).
narrative_ontology:measurement(spee_be_t15, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(spee_be_t20, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 20, 0.53).
narrative_ontology:measurement(spee_be_t25, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 25, 0.56).
narrative_ontology:measurement(spee_be_t30, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 30, 0.59).
narrative_ontology:measurement(spee_be_t35, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 35, 0.61).
narrative_ontology:measurement(spee_be_t40, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 40, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(spee_su_t5, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 5, 0.39).
narrative_ontology:measurement(spee_su_t10, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(spee_su_t15, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 15, 0.45).
narrative_ontology:measurement(spee_su_t20, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement(spee_su_t25, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 25, 0.5).
narrative_ontology:measurement(spee_su_t30, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement(spee_su_t35, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 35, 0.5).
narrative_ontology:measurement(spee_su_t40, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 40, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_harm_boundary__harm_balancing_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(speech_harm_boundary__harm_balancing_reading, 0.12).
narrative_ontology:affects_constraint(speech_harm_boundary__harm_balancing_reading, speech_harm_boundary__absolutist_reading).
narrative_ontology:affects_constraint(speech_harm_boundary__harm_balancing_reading, speech_harm_boundary__dignity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the speech-protection kernel. The absolutist_reading constrains speech near-absolutely; the dignity_reading subordinates speech to categorical dignity protection; this reading balances speech protection against demonstrated proportional harm. All three are live positions in constitutional and political philosophy. Their ε values, beneficiary/victim structures, and computed types differ substantially because their reference frames (what they take the proper starting point to be) differ. They are linked as a constraint family via network.affects_constraints, not as one constraint with measurement-basis variants.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_harm_boundary__harm_balancing_reading, powerless, 0.68).
constraint_indexing:directionality_override(speech_harm_boundary__harm_balancing_reading, moderate, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
