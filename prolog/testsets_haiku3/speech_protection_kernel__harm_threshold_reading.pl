% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__harm_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_kernel__harm_threshold_reading, []).

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
 *   constraint_id: speech_protection_kernel__harm_threshold_reading
 *   human_readable: Speech Protection Conditional on Demonstrable Victim Harm
 *   domain: constitutional/political_philosophy
 *
 * SUMMARY:
 *   This constraint represents one reading of the speech protection kernel:
 *   the harm-threshold reading, under which speech protection is conditional
 *   on the speaker avoiding demonstrable harm to victims and target groups.
 *   Under this reading, the speech boundary is narrower than under absolutist
 *   or marketplace readings — harm becomes a legitimate ground for speech
 *   restriction. The constraint is CLAIMED as tangled_rope because it
 *   coordinates two genuine functions (victim protection + speech regulation)
 *   while extracting institutional authority from speakers. The metrics track
 *   how extractiveness and enforcement intensity have accumulated as harm
 *   concepts have expanded and adjudicatory bodies have developed broader
 *   harm-finding practices.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__harm_threshold_reading, 0.68).
domain_priors:suppression_score(speech_protection_kernel__harm_threshold_reading, 0.72).
domain_priors:theater_ratio(speech_protection_kernel__harm_threshold_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__harm_threshold_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__harm_threshold_reading, "Speech Protection Conditional on Demonstrable Victim Harm").
narrative_ontology:topic_domain(speech_protection_kernel__harm_threshold_reading, "constitutional/political_philosophy").

domain_priors:requires_active_enforcement(speech_protection_kernel__harm_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__harm_threshold_reading, 'f38e3432-1301-4a5b-8b39-779d90555370').
narrative_ontology:cs_kernel_codification('f38e3432-1301-4a5b-8b39-779d90555370', formalized).
narrative_ontology:cs_authority_grounding('f38e3432-1301-4a5b-8b39-779d90555370', extraction).
narrative_ontology:cs_interpretation_layer_present('f38e3432-1301-4a5b-8b39-779d90555370').
narrative_ontology:cs_reading_relation('f38e3432-1301-4a5b-8b39-779d90555370', speech_protection_kernel__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('f38e3432-1301-4a5b-8b39-779d90555370', speech_protection_kernel__marketplace_reading, influences).
narrative_ontology:cs_reading_relation('f38e3432-1301-4a5b-8b39-779d90555370', speech_protection_kernel__dignity_reading, coexists_with).
narrative_ontology:cs_reading_relation('f38e3432-1301-4a5b-8b39-779d90555370', speech_protection_kernel__democratic_participation_reading, influences).
narrative_ontology:cs_axiom('f38e3432-1301-4a5b-8b39-779d90555370', foundational, demonstrable_harm_justifies_restriction).
narrative_ontology:cs_axiom_status(demonstrable_harm_justifies_restriction, holdable).
narrative_ontology:cs_axiom_grounding('f38e3432-1301-4a5b-8b39-779d90555370', demonstrable_harm_justifies_restriction, empirically_contingent).
narrative_ontology:cs_axiom('f38e3432-1301-4a5b-8b39-779d90555370', foundational, victim_harm_overrides_speaker_autonomy).
narrative_ontology:cs_axiom_status(victim_harm_overrides_speaker_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('f38e3432-1301-4a5b-8b39-779d90555370', victim_harm_overrides_speaker_autonomy, deontological).
narrative_ontology:cs_reference_frame('f38e3432-1301-4a5b-8b39-779d90555370', speech_autonomy_balanced_with_harm_prevention).
narrative_ontology:cs_drift_state('f38e3432-1301-4a5b-8b39-779d90555370', contemporary_expanded_harm_recognition, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f38e3432-1301-4a5b-8b39-779d90555370', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__harm_threshold_reading, harm_victims_and_target_groups).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__harm_threshold_reading, state_enforcement_authority).
narrative_ontology:constraint_victim(speech_protection_kernel__harm_threshold_reading, speakers_and_publishers).
narrative_ontology:constraint_victim(speech_protection_kernel__harm_threshold_reading, marginal_speech_categories).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Face uncertain boundaries for protected expression: the harm threshold is applied ex post by adjudicators after speech occurs, creating chilling effects on marginal categories (satire, provocation, academic discussion of taboo topics, artistic depiction). Exit from the speech community itself is identity-fused — the speaker's role and public engagement depend on expression. Compliance requires self-censorship to avoid harm claims.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, speakers_and_publishers, payer,
    moderate, biographical, identity_locked, national).

% Gain standing to restrict speech when demonstrable harm to their dignity, safety, or group status is established. The reading centers their right to be free from targeted expression; speech is constrained at the boundary where it produces measurable harm. Their power is institutional (through law, regulation, litigation) rather than individual.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, harm_victims_and_target_groups, beneficiary,
    organized, generational, constrained, national).

% Adjudicates harm claims and enforces boundaries through law, courts, platform regulation, and policy. Sets the operative definition of 'demonstrable harm' (measurable psychological injury, material threat to safety, structural subordination of groups). Has discretion to expand or contract the harm threshold over time. Collects institutional authority to police the speech boundary.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, state_enforcement_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Categories of speech most vulnerable to harm characterization: satire that risks being read as endorsement, academic treatment of stigmatized groups, artistic provocation, heterodox political expression, speech by unpopular speakers. These categories lack organized defense and are most exposed to ex post harm adjudication. Their speakers are often already marginalized (dissidents, artists, unpopular minorities). They cannot easily exit because their speech IS their identity or livelihood.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, marginal_speech_categories, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_kernel__harm_threshold_reading, marginal_speech_categories, excluded).

% Courts, regulatory agencies, and platform moderation bodies that determine whether speech meets the harm threshold. They interpret what counts as demonstrable harm, apply the standard to marginal cases, and produce precedent that shapes future boundaries. Their judgments accumulate into a practice regime that may expand the harm concept over time (scope creep).
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, adjudicatory_bodies, agenda_setter,
    institutional, generational, arbitrage, national).

% Hold a principled position that speech protection should not be conditional on harm avoidance. They advocate for a different reading of the speech kernel entirely (the absolutist reading). Under the harm-threshold reading, they are partly excluded from the conversation — their objections to harm framing are not heard as legitimate input to harm adjudication, but as evidence of extremism or bad faith.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, absolutist_speakers, excluded,
    moderate, biographical, mobile, global).

% Represent the marketplace reading: the view that false and harmful speech is best countered by more speech, not by restriction. They generate analysis suggesting that ex post harm-based speech restriction produces epistemic harms (suppression of evidence, foreclosure of debate, institutionalization of false consensus). Their critique feeds into the contestation of the harm reading itself.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, truth_discovery_theorists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_kernel__harm_threshold_reading, state_enforcement_authority).
narrative_ontology:fixing_cost_class(speech_protection_kernel__harm_threshold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Protects group dignity and victim safety by establishing that speech producing demonstrable harm to target groups is not a protected form of expression. Solves the coordination problem: how to construct speech boundaries that honor both speaker autonomy AND the right of harmed groups not to be targeted.
% TRANSFER_FUNCTION: Moves the authority to restrict speech from the speaker (who decides what to say) to the victim/target group and state enforcement (who decide when speech counts as harmful). Moves institutional power over speech boundaries to adjudicatory bodies (courts, regulators, platforms).
% ABSENT_VOICES: Absolutist speakers and defenders of categorical speech protection are structurally excluded — their foundational claim (speech should not be restricted on harm grounds) is treated as a forfeited premise within the harm-threshold framework. Marginal speech practitioners are weakly represented; their interests surface only in litigation after restriction. Communities invested in marketplace-of-ideas ideology are absent from harm-threshold adjudication.
% DISAPPEARANCE_RATIONALE: If the harm-threshold constraint vanished overnight, speech boundaries would shift dramatically: categories currently restricted (incitement, targeted harassment, group-subordinating expression) would return to speaker-determined scope; institutional adjudication of harm would cease; institutional authority over speech would compress to narrow carve-outs. The balance between speaker autonomy and victim protection would reorganize entirely around a different constraint (likely absolutist or marketplace).
% FOUNDING_PROBLEM: Unfettered speech can cause measurable harm to vulnerable groups and victims: targeted harassment drives people from public sphere, slurs reinforce subordination, conspiracy theories incite violence, stigmatizing expression damages mental health and opportunity.
% FOUNDING_PROBLEM_CORROBORATION: Harm victims and target groups attest the founding problem is live and urgent. Academic research in psychology, sociology, and political science documents measurable harms (trauma, self-censorship, reduced political participation, health outcomes) from targeted speech. Absolutist defenders of speech protection and empirical researchers studying speech-restriction effects contest both the magnitude of harms and the causal attribution to speech (rather than to structural factors). The founding problem's reality is not denied across the board, but its normative weight is strongly disputed.
narrative_ontology:disappearance_verdict(speech_protection_kernel__harm_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__harm_threshold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__harm_threshold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(speech_protection_kernel__harm_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__harm_threshold_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__harm_threshold_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_kernel__harm_threshold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_kernel__harm_threshold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness begins moderate (0.55) when harm-threshold speech law first crystallizes as a doctrine but remains episodically applied. It rises to a plateau (0.68 by interval point 25) as adjudicatory practice develops, harm definitions expand to cover reputational, psychological, and structural harms, and institutional actors (courts, regulators, platforms) accumulate experience applying the standard. The plateau reflects that extractiveness stabilizes once the harm concept settles into predictable patterns — but measurement shows no further rise, suggesting the constraint has reached an equilibrium where harm is regularly found but the institutional machinery is not expanding further (absent a new category like 'epistemic harm' that would re-widen definitions). Theater rises more slowly (0.28 to 0.41): harm adjudication is substantively engaged at first, but by interval end theatrical elements appear (performing harm-sensitivity, demonstrative court proceedings, platform moderation theater that emphasizes victim protection over actual harm reduction). Suppression rises throughout (0.58 to 0.72): the enforcement machinery to police the speech boundary hardened over the interval — more adjudicatory capacity, more platform enforcement, more prosecutorial attention, more institutional commitment to the harm-threshold frame. All three metrics track the same shared time grid (integer steps 0–35), authored at every point because the constraint's operation has observable history across the interval.
 *
 * PERSPECTIVAL GAP:
 *   From the victim/target-group seat, this constraint solves a genuine coordination problem (how to be free from targeted harassment while speakers retain autonomy in non-harmful domains) and the extractiveness is justified as the cost of institutional adjudication of the harm boundary. From the speaker seat, especially marginal speakers, the constraint appears as enforced extraction: institutional authority extracted from speakers, applied ex post with uncertain boundaries, amplifying chilling effects on protected speech categories. From the adjudicatory seat, the constraint is neutral machinery applying the harm principle. From the marketplace-of-ideas seat, it is institutional suppression of speech that should be countered by counter-speech. These perspectives are NOT reconcilable within a single framework — the engine computes them per-seat from the structural data. The claim/metric independence is deliberate: the constraint is CLAIMED as tangled_rope (coordination + extraction) while the metrics show substantial extraction (0.68) and high enforcement (0.72), inviting scrutiny of whether the coordination function justifies the extraction, or whether the reading has drifted into institutional suppression.
 *
 * DIRECTIONALITY LOGIC:
 *   Speakers and publishers are the structural targets: they bear the cost of speech restriction, face uncertainty about harm adjudication, and have identity-locked exits (exit from speech = loss of role). The harm-threshold reading extracts institutional authority from them (they no longer unilaterally decide what they may say; adjudicators do). Harm victims and target groups are beneficiaries under this reading: they gain standing to restrict harmful speech and institutional backing for their claims. The state enforcement authority sits as agenda-setter: it gains institutional power to adjudicate harm, set definitions, police boundaries. Marginal speech categories face the highest suppression because they cannot easily organize defense and lack institutional allies — they are lowest-power speakers asking for protection of highest-contestable expression. The asymmetry is structural: beneficiary seats (organized groups, institutional authority) have high power and institutional arbitrage; payer seats (individual speakers, marginalized voices) have moderate to powerless status and identity-locked or trapped exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (harm from targeted speech) is contested in status, not merely in remedy — absolutists and marketplace theorists dispute whether demonstrable harm is the right criterion at all, or whether it over-counts epistemic harms from suppression. The constraint's persistence depends on this contestation remaining unresolved at the institutional level. If empirical research established that harm-threshold speech restriction produced greater social harms than the harms it prevented (epistemic damage, political polarization, loss of dissent), the constraint would face mandatrophy pressure — the founding problem would be seen as creating the very harm it claims to prevent. The classification as tangled_rope (not snare) depends on the coordination function being genuine: institutional adjudication of harm claims does coordinate between speaker autonomy and victim protection, even if the extraction is substantial. If the coordination eroded (e.g., if adjudicators became pure institutional authority-accumulators indifferent to victim protection), the type would drift toward snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harm_threshold_definability,
    'What counts as demonstrable harm sufficient to restrict speech? Is harm measurable and adjudicable, or does harm determination require normative judgment that cannot be separated from the outcome being adjudicated?',
    'Comparative analysis of harm findings across adjudicatory bodies: do they converge on stable criteria (objective harm measures) or diverge widely (subjective application of the harm frame)? Empirical study of speech-restriction predictability from harm-threshold doctrine.',
    'If harm is measurable and adjudication converges, the constraint retains legitimacy as a coordination mechanism. If harm determination is fundamentally contestable and adjudication diverges, the constraint drifts toward institutional authority extraction (snare-proximate). A finding of wide divergence would support mandatrophy analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_threshold_definability, empirical, 'Whether demonstrable harm is a determinate category or a cover for discretionary institutional expansion.').

omega_variable(
    suppression_mechanism_ambiguity,
    'How much of the suppression of marginal speech categories is structural (legal prohibition, institutional deplatforming) versus internalized (speakers'' own self-suppression in anticipation of harm claims)?',
    'Post-speech-protection measurement: if speech categories that are technically protected from legal restriction still disappear from public discourse, the suppression is internalized rather than structural. Comparative study of speaker behavior before and after explicit harm-threshold regime adoption.',
    'If suppression is predominantly structural, it is measurable and contestable through institutional reform. If predominantly internalized, the constraint''s effective extraction is higher than the institutional measures suggest — speakers carry the suppression with them even in settings where formal institutional restriction is absent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Whether the chilling effect on marginal speech is institutional or cognitive.').

omega_variable(
    harm_reading_foreclosure_of_absolutism,
    'Does the harm-threshold reading logically foreclose the absolutist reading within a single constitutional framework, or do they remain coexisting live positions?',
    'Jurisprudential analysis: can a court or constitutional authority endorse both a harm-threshold and an absolutist position without logical contradiction, or must it choose? Historical case law examining whether harm-threshold and absolutist holdings appear in the same legal system.',
    'If the readings foreclose each other, the constraint''s persistence is zero-sum: adopting harm-threshold necessarily rejects absolutism at the constitutional level. If coexisting, both readings can be live despite institutional pressure toward one or the other. The answer affects how to model the kernel''s contestation: foreclosure implies terminal legal conflict; coexistence implies ongoing institutional negotiation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(harm_reading_foreclosure_of_absolutism, conceptual, 'Whether harm-threshold and absolutist readings are logically compatible within a single framework.').

omega_variable(
    epistemic_harm_expansion,
    'Will the harm concept expand to include epistemic harms — harm to knowledge, to discourse quality, to collective reasoning — or remain focused on measurable individual/group harms (psychological injury, safety threats)?',
    'Monitoring adjudicatory and academic discourse: emergence of ''epistemic harm'' or ''informational harm'' as a recognized harm category in law, regulation, or platform policy. Natural experiments in jurisdictions that attempt to restrict speech on epistemic-harm grounds.',
    'Expansion to epistemic harm would dramatically widen the restriction boundary: false claims, misleading narratives, and suppression of context could be restricted as harms, even without measurable individual injury. This would shift the constraint from tangled_rope (coordination + extraction) toward snare (pure institutional extraction). Holding the boundary at individual/group harms keeps the coordination function visible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_harm_expansion, empirical, 'Direction of harm-concept expansion and whether epistemic harms will be recognized as restriction grounds.').

omega_variable(
    kernel_reading_contestation_live,
    'Is the contest among the five readings of the speech kernel (absolutist, marketplace, dignity, democratic, harm-threshold) live and open, or has one reading achieved institutional dominance such that others are foreclosed in practice?',
    'Mapping institutional commitments: which reading does each institutional actor (courts, legislatures, platforms, media organizations) officially endorse and apply? Detecting shifts in institutional consensus over time. Evidence from jurisdictions with sharply different speech regimes.',
    'If one reading achieves institutional dominance, the kernel''s contestation becomes mere rhetoric and the constraint becomes monolithic (foreclosed siblings are narrative cover, not live options). If multiple readings remain institutionally live in different jurisdictions or institutional sectors, the kernel''s contestation remains genuinely open. This affects how to model the sibling relationships in reading_relations (foreclosed vs. coexists_with).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation_live, empirical, 'Whether the kernel''s five readings remain genuinely contested or one has achieved institutional closure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__harm_threshold_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_kernel__harm_threshold_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(spee_tr_t5, speech_protection_kernel__harm_threshold_reading, theater_ratio, 5, 0.31).
narrative_ontology:measurement(spee_tr_t10, speech_protection_kernel__harm_threshold_reading, theater_ratio, 10, 0.34).
narrative_ontology:measurement(spee_tr_t15, speech_protection_kernel__harm_threshold_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement(spee_tr_t20, speech_protection_kernel__harm_threshold_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(spee_tr_t25, speech_protection_kernel__harm_threshold_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement(spee_tr_t30, speech_protection_kernel__harm_threshold_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement(spee_tr_t35, speech_protection_kernel__harm_threshold_reading, theater_ratio, 35, 0.41).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(spee_be_t5, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 5, 0.59).
narrative_ontology:measurement(spee_be_t10, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(spee_be_t15, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(spee_be_t20, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(spee_be_t25, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(spee_be_t30, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(spee_be_t35, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 35, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(spee_su_t5, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(spee_su_t10, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(spee_su_t15, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(spee_su_t20, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(spee_su_t25, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(spee_su_t30, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(spee_su_t35, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 35, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__harm_threshold_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(speech_protection_kernel__harm_threshold_reading, 0.12).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel__marketplace_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel__dignity_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel__democratic_participation_reading).

% DUAL FORMULATION NOTE:
% The speech_protection_kernel decomposes into five structurally distinct constraints, each representing a different reading's operative boundary on speech protection. The harm_threshold_reading (this constraint) treats harm as the operative restriction criterion. The absolutist_reading (sibling) treats speech as categorically protected regardless of harm. These readings share a kernel (state power over speech + speaker autonomy) but instantiate different ε values and different beneficiary/victim structures. The network links them as members of a constraint family; each story models one reading's constraint independently; the engine's per-reading classification will reveal how institutional adoption of one reading shapes the structural character of the others (influences relationships).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_protection_kernel__harm_threshold_reading, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
