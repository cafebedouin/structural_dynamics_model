% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_kernel__absolutist_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: speech_protection_kernel__absolutist_reading
 *   human_readable: Absolutist Speech Protection — Listener Harm Not Grounds for Restriction
 *   domain: constitutional_law/political_philosophy/communication_rights
 *
 * SUMMARY:
 *   The absolutist reading of speech protection holds that protection
 *   operates near-categorically: listener harm is not a valid ground for
 *   restricting speech. Only narrow, pre-defined categorical exclusions
 *   (incitement to imminent lawless action, true threats, obscenity as
 *   narrowly defined) may restrict speech. The constraint instantiates the
 *   widest possible protection boundary, maximizing speaker autonomy and
 *   rejecting victim harm claims as restriction grounds. This is one reading
 *   of the contested speech_protection_kernel; sibling readings
 *   (harm_threshold, marketplace, dignity, democratic_participation) impose
 *   different structural boundaries.
 *
 * KEY AGENTS:
 *   - speakers: Primary beneficiary (powerful/arbitrage) — gains maximal protection for expression
 *   - publishers: Primary beneficiary (institutional/arbitrage) — gains maximal protection for dissemination
 *   - press_institutions: Primary beneficiary (institutional/arbitrage) — gains institutional protection for newsgathering and publication
 *   - harmed_listeners: Excluded (powerless/trapped) — bears harm with no structural remedy under this reading
 *   - democratic_institutions: Observer (institutional/analytical) — monitors whether absolutist boundary destabilizes self-governance
 *   - analytical_observer: Observer (analytical/analytical) — sees full structure of kernel and all readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__absolutist_reading, 0.15).
domain_priors:suppression_score(speech_protection_kernel__absolutist_reading, 0.1).
domain_priors:theater_ratio(speech_protection_kernel__absolutist_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__absolutist_reading, mountain).
narrative_ontology:human_readable(speech_protection_kernel__absolutist_reading, "Absolutist Speech Protection — Listener Harm Not Grounds for Restriction").
narrative_ontology:topic_domain(speech_protection_kernel__absolutist_reading, "constitutional_law/political_philosophy/communication_rights").

domain_priors:emerges_naturally(speech_protection_kernel__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__absolutist_reading, 'c0098e7c-039d-4fb3-a6e2-5c3672a44005').
narrative_ontology:cs_kernel_codification('c0098e7c-039d-4fb3-a6e2-5c3672a44005', formalized).
narrative_ontology:cs_authority_grounding('c0098e7c-039d-4fb3-a6e2-5c3672a44005', lineage).
narrative_ontology:cs_interpretation_layer_present('c0098e7c-039d-4fb3-a6e2-5c3672a44005').
narrative_ontology:cs_reading_relation('c0098e7c-039d-4fb3-a6e2-5c3672a44005', speech_protection_kernel__harm_threshold_reading, forecloses).
narrative_ontology:cs_reading_relation('c0098e7c-039d-4fb3-a6e2-5c3672a44005', speech_protection_kernel__marketplace_reading, coexists_with).
narrative_ontology:cs_reading_relation('c0098e7c-039d-4fb3-a6e2-5c3672a44005', speech_protection_kernel__dignity_reading, forecloses).
narrative_ontology:cs_reading_relation('c0098e7c-039d-4fb3-a6e2-5c3672a44005', speech_protection_kernel__democratic_participation_reading, influences).
narrative_ontology:cs_axiom('c0098e7c-039d-4fb3-a6e2-5c3672a44005', foundational, listener_harm_never_restriction_ground).
narrative_ontology:cs_axiom_status(listener_harm_never_restriction_ground, holdable).
narrative_ontology:cs_axiom_grounding('c0098e7c-039d-4fb3-a6e2-5c3672a44005', listener_harm_never_restriction_ground, deontological).
narrative_ontology:cs_axiom('c0098e7c-039d-4fb3-a6e2-5c3672a44005', foundational, speaker_autonomy_lexically_prior).
narrative_ontology:cs_axiom_status(speaker_autonomy_lexically_prior, holdable).
narrative_ontology:cs_axiom_grounding('c0098e7c-039d-4fb3-a6e2-5c3672a44005', speaker_autonomy_lexically_prior, deontological).
narrative_ontology:cs_reference_frame('c0098e7c-039d-4fb3-a6e2-5c3672a44005', classical_liberal_speech_floor).
narrative_ontology:cs_drift_state('c0098e7c-039d-4fb3-a6e2-5c3672a44005', contemporary_digital_speech_ecosystem, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c0098e7c-039d-4fb3-a6e2-5c3672a44005', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__absolutist_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__absolutist_reading, speakers).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__absolutist_reading, publishers).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__absolutist_reading, press_institutions).
narrative_ontology:constraint_vindicates(speech_protection_kernel__absolutist_reading, speaker_autonomy_maximized).
narrative_ontology:constraint_vindicates(speech_protection_kernel__absolutist_reading, listener_harm_not_restriction_ground).
narrative_ontology:constraint_vindicates(speech_protection_kernel__absolutist_reading, categorical_protection_boundary).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Speakers gain maximal protection for expression: the constraint places the burden of restriction on the state to prove speech falls within narrow categorical exclusions. They can speak on any topic, in any forum, without anticipating harm-based liability. Exit is arbitrage-grade: they can move across jurisdictions, platforms, and media to find protection.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, speakers, beneficiary,
    powerful, biographical, arbitrage, universal).

% Publishers gain maximal protection for dissemination: they can publish without prior restraint and with near-immunity from harm-based liability for content. The constraint structures the entire publishing ecosystem around speaker/publisher autonomy. Exit is arbitrage-grade: they operate across jurisdictional boundaries and platform infrastructures.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, publishers, beneficiary,
    institutional, generational, arbitrage, universal).

% Press institutions gain structural protection for newsgathering, source protection, and publication — the constraint treats press freedom as a categorical component of speech protection. They benefit from the widest possible shield against state compulsion and harm-based liability. Exit is arbitrage-grade: institutional infrastructure spans jurisdictions.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, press_institutions, beneficiary,
    institutional, generational, arbitrage, universal).

% Harmed listeners bear the costs of absolutist protection: exposure to hate speech, harassment, disinformation, and dignitary harm with no structural remedy under this reading. The reading's premise rejects their harm as a cognizable ground for restriction. They are structurally excluded from the constraint's beneficiary/victim calculus — their injury is not recognized as a claim. Exit is trapped: they cannot avoid exposure in a public sphere structured by absolutist protection, and the reading provides no mechanism for their harm to register.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, harmed_listeners, excluded,
    powerless, biographical, trapped, universal).

% Democratic institutions (legislatures, courts, election bodies) monitor whether the absolutist boundary destabilizes self-governance — e.g., whether disinformation, hate speech, or coordinated manipulation undermine democratic deliberation. They do not collect from or pay into the constraint directly but bear systemic risk if the protection boundary proves incompatible with democratic stability. Their seat is analytical: they observe the constraint's systemic effects.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, democratic_institutions, observer,
    institutional, generational, analytical, national).

% The analytical observer sees the full kernel structure: all five readings, their structural deltas, and the classification each would compute. This seat does not collect from or pay into any single reading; it maps the constraint family. Exit is analytical: the observer can shift between readings as analytical frames.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_kernel__absolutist_reading, diffuse).
narrative_ontology:fixing_cost_class(speech_protection_kernel__absolutist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of establishing a stable speech floor in a liberal order: by categorically rejecting listener harm as a restriction ground, the constraint creates a clear, predictable boundary that speakers, publishers, and institutions can rely on without case-by-case negotiation.
% TRANSFER_FUNCTION: The constraint does not move resources in a transfer sense — it allocates immunities and liabilities. It transfers the burden of restriction entirely to the state (must prove narrow categorical exclusion) and transfers the cost of harm to listeners (who bear it without remedy). No monetary or status flow occurs between seats.
% ABSENT_VOICES: Harmed listeners — especially members of historically subordinated groups targeted by hate speech, harassment, and structural subordination — would object if present. They are structurally excluded by the reading's premise that listener harm is not a cognizable category. Their absence is not accidental; it is the reading's defining structural move.
% DISAPPEARANCE_RATIONALE: If absolutist protection vanished overnight, the speech floor would collapse into case-by-case harm balancing. States would restrict speech on harm grounds routinely; publishers would face liability for content; speakers would self-censor. The entire liberal speech ecosystem would reorganize around harm-threshold or dignity-based boundaries. The world rearranges because arrangements (publishing models, platform governance, protest rights, press freedom) depend on the categorical boundary.
% FOUNDING_PROBLEM: The arrangement was built to solve the problem of state censorship of dissent: in pre-liberal and authoritarian orders, the state restricted speech by claiming it harmed public order, morality, or state security. The absolutist boundary was constructed to make such censorship structurally difficult by rejecting harm as a restriction ground and limiting exclusions to narrow, pre-defined categories.
% FOUNDING_PROBLEM_CORROBORATION: Authoritarian regression in contemporary states (Hungary, Turkey, Russia, etc.) demonstrates the founding problem remains live: states still use harm-based justifications to restrict dissent. Corroboration from outside the beneficiary set: human rights organizations (Article 19, PEN International), democratic theorists (Schauer, Waldron — though they contest the absolutist remedy), and courts in transitional democracies all attest that state censorship of dissent via harm claims is a persistent threat. The beneficiaries (speakers, publishers) are not the sole attestors.
narrative_ontology:disappearance_verdict(speech_protection_kernel__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__absolutist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__absolutist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(speech_protection_kernel__absolutist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__absolutist_reading, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__absolutist_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, ExtMetricName, E),
    domain_priors:suppression_score(speech_protection_kernel__absolutist_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(speech_protection_kernel__absolutist_reading),
    narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(speech_protection_kernel__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Metrics reflect the absolutist reading's structural claim: near-zero extraction (0.15) because the constraint imposes almost no positive obligations or transfers; near-zero suppression (0.1) because alternatives (silence, counter-speech, exit) remain open; near-zero theater (0.05) because the protection boundary is genuinely enforced, not performative. High accessibility_collapse (0.92) because the categorical rule leaves almost no room for case-by-case harm balancing; low resistance (0.08) because the constraint is structurally stable within liberal orders that adopt it. The claimed_type mountain is structurally asserted — the reading treats the protection boundary as a natural limit of liberal constitutionalism, not a contingent policy choice.
 *
 * PERSPECTIVAL GAP:
 *   From the speaker/publisher seats (beneficiaries), the constraint appears as mountain — a natural law of liberal order. From the harmed_listener seat (excluded), the same constraint would compute as snare if their harm claims were recognized: they bear costs (psychic harm, dignity injury, structural subordination) with zero structural remedy and trapped exit. The engine computes this divergence from the structural data; the absolutist reading's exclusion of listener harm as a cognizable category is exactly what produces the mountain classification for beneficiaries and would produce snare for excluded parties if they were recognized as victims.
 *
 * DIRECTIONALITY LOGIC:
 *   Speakers, publishers, and press_institutions are declared beneficiaries — they collect the protection rent (maximal autonomy, minimal restriction) without administering the constraint. Harmed_listeners are NOT declared victims in this reading because the reading's structural premise rejects listener harm as a cognizable category. This is the defining move of the absolutist reading: by refusing to recognize listener harm as a valid claim, the reading structurally eliminates victims. Democratic_institutions and analytical_observer hold observer seats. Directionality for beneficiaries derives toward 0.0 (full beneficiary); for excluded parties, if they were recognized, it would derive toward 1.0 (full target).
 *
 * MANDATROPHY ANALYSIS:
 *   The absolutist reading's founding problem (preventing state censorship of dissent) remains live — authoritarian regression is a persistent threat. The constraint has not atrophied into piton because its coordination function (protecting the speech floor) remains necessary and actively invoked. Mandatrophy is not resolved; the mountain classification holds because the constraint's natural-law claim is internally coherent within the reading's framework.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of the speech_protection_kernel, and which reading does it instantiate?',
    'Structural: this story instantiates the absolutist_reading of the speech_protection_kernel. Sibling readings are harm_threshold_reading, marketplace_reading, dignity_reading, democratic_participation_reading — each a separate constraint with its own ε and beneficiary/victim structure.',
    'Confirms this is not a single constraint with variable measurement, but a distinct constraint within a kernel family. Prevents ε-averaging across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment that this story is the absolutist_reading of the speech_protection_kernel.').

omega_variable(
    sibling_reading_structural_deltas,
    'What would each sibling reading change structurally relative to this absolutist reading?',
    'Comparative constraint authoring: each sibling reading gets its own story file with distinct base_properties, beneficiaries, victims, and claimed_type.',
    'Documents the structural deltas: harm_threshold_reading adds victims (harmed listeners) and raises extractiveness; marketplace_reading adds coordination function (truth-discovery) and beneficiaries (audience); dignity_reading adds victims (subordinated groups) and suppression; democratic_participation_reading narrows protection to political speech and adds agenda_setter role for democratic institutions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_deltas, conceptual, 'Structural differences between absolutist_reading and its four sibling readings of the speech_protection_kernel.').

omega_variable(
    natural_law_vs_constitutional_construction,
    'Is the near-categorical protection a structural feature of liberal constitutionalism (natural law) or a constructed constraint that benefits identifiable agents?',
    'Historical and comparative analysis: if every stable liberal order converges on near-categorical protection regardless of origin, natural-law claim strengthens; if protection boundaries track power configurations of speakers/publishers, constructed claim strengthens.',
    'If natural law, mountain classification holds; if constructed with beneficiaries, false_summit_mountain signature triggers reclassification toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constitutional_construction, empirical, 'Whether absolutist speech protection is a natural limit or a constructed constraint benefiting speakers and publishers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__absolutist_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_kernel__absolutist_reading, theater_ratio, 0, 0.02).
narrative_ontology:measurement(spee_tr_t50, speech_protection_kernel__absolutist_reading, theater_ratio, 50, 0.03).
narrative_ontology:measurement(spee_tr_t100, speech_protection_kernel__absolutist_reading, theater_ratio, 100, 0.05).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_kernel__absolutist_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(spee_be_t50, speech_protection_kernel__absolutist_reading, base_extractiveness, 50, 0.12).
narrative_ontology:measurement(spee_be_t100, speech_protection_kernel__absolutist_reading, base_extractiveness, 100, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_kernel__absolutist_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(spee_su_t50, speech_protection_kernel__absolutist_reading, suppression_requirement, 50, 0.07).
narrative_ontology:measurement(spee_su_t100, speech_protection_kernel__absolutist_reading, suppression_requirement, 100, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__absolutist_reading, information_standard).
narrative_ontology:boltzmann_floor_override(speech_protection_kernel__absolutist_reading, 0.02).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, speech_protection_kernel__harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, speech_protection_kernel__marketplace_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, speech_protection_kernel__dignity_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, speech_protection_kernel__democratic_participation_reading).

% DUAL FORMULATION NOTE:
% The speech_protection_kernel decomposes into five constraint stories, one per reading. This absolutist_reading claims mountain with ε≈0.15. Sibling readings author higher extractiveness and distinct beneficiary/victim structures. All five are linked via affects_constraints. The kernel's colloquial label 'free speech' conflates structurally distinct claims with different ε values — this decomposition follows the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
