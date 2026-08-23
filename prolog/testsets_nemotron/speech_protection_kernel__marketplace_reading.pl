% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__marketplace_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_kernel__marketplace_reading, []).

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
 *   constraint_id: speech_protection_kernel__marketplace_reading
 *   human_readable: Marketplace-of-Ideas Speech Protection
 *   domain: constitutional_law/political_philosophy/communication_rights
 *
 * SUMMARY:
 *   The marketplace-of-ideas reading justifies speech protection as a
 *   collective truth-discovery mechanism: false and harmful speech is best
 *   countered by more speech, not by state restriction. Content-based
 *   restrictions are rejected as distorting the epistemic process. The
 *   reading presents itself as a coordination solution (rope) — a genuine
 *   collective-action problem (how to discover truth in pluralistic
 *   discourse) solved with minimal coercion. But the coordination function
 *   depends on empirical conditions (roughly equal speech access, functional
 *   correction dynamics, absence of structural amplification bias) that may
 *   not hold. Where they fail, the constraint may operate as extraction for
 *   targeted groups (snare/tangled_rope seats) while presenting as
 *   coordination for dominant speakers. This story authors the marketplace
 *   reading as a single ε-invariant constraint; sibling readings are separate
 *   constraint stories linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - public_sphere_participants: Primary beneficiary (organized/constrained) — benefits from open discourse as truth-seeking mechanism
 *   - truth_seeking_institutions: Primary beneficiary (institutional/analytical) — universities, journals, courts rely on open contestation
 *   - minority_viewpoint_holders: Beneficiary (powerless/constrained) — protection enables dissenting views to enter marketplace
 *   - targeted_groups_under_harmful_speech: Victim (powerless/identity_locked) — bears disproportionate harm; counter-speech capacity limited by structural position
 *   - vulnerable_audiences: Victim (powerless/constrained) — lacks epistemic resilience against optimized persuasion/harm
 *   - platform_architects: Agenda setter (institutional/arbitrage) — designs amplification architecture that shapes marketplace dynamics
 *   - state_regulators: Observer (institutional/analytical) — monitors for market failure but constrained by content-neutrality premise
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__marketplace_reading, 0.38).
domain_priors:suppression_score(speech_protection_kernel__marketplace_reading, 0.22).
domain_priors:theater_ratio(speech_protection_kernel__marketplace_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__marketplace_reading, rope).
narrative_ontology:human_readable(speech_protection_kernel__marketplace_reading, "Marketplace-of-Ideas Speech Protection").
narrative_ontology:topic_domain(speech_protection_kernel__marketplace_reading, "constitutional_law/political_philosophy/communication_rights").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__marketplace_reading, '9d7fe7aa-8e79-4c94-8fae-bc209341214b').
narrative_ontology:cs_kernel_codification('9d7fe7aa-8e79-4c94-8fae-bc209341214b', formalized).
narrative_ontology:cs_authority_grounding('9d7fe7aa-8e79-4c94-8fae-bc209341214b', lineage).
narrative_ontology:cs_interpretation_layer_present('9d7fe7aa-8e79-4c94-8fae-bc209341214b').
narrative_ontology:cs_reading_relation('9d7fe7aa-8e79-4c94-8fae-bc209341214b', speech_protection_kernel__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('9d7fe7aa-8e79-4c94-8fae-bc209341214b', speech_protection_kernel__harm_threshold_reading, coexists_with).
narrative_ontology:cs_reading_relation('9d7fe7aa-8e79-4c94-8fae-bc209341214b', speech_protection_kernel__dignity_reading, coexists_with).
narrative_ontology:cs_reading_relation('9d7fe7aa-8e79-4c94-8fae-bc209341214b', speech_protection_kernel__democratic_participation_reading, coexists_with).
narrative_ontology:cs_axiom('9d7fe7aa-8e79-4c94-8fae-bc209341214b', foundational, truth_discovery_requires_unrestricted_contestation).
narrative_ontology:cs_axiom_status(truth_discovery_requires_unrestricted_contestation, holdable).
narrative_ontology:cs_axiom_grounding('9d7fe7aa-8e79-4c94-8fae-bc209341214b', truth_discovery_requires_unrestricted_contestation, empirically_contingent).
narrative_ontology:cs_axiom('9d7fe7aa-8e79-4c94-8fae-bc209341214b', foundational, more_speech_remedy_superior_to_state_restriction).
narrative_ontology:cs_axiom_status(more_speech_remedy_superior_to_state_restriction, holdable).
narrative_ontology:cs_axiom_grounding('9d7fe7aa-8e79-4c94-8fae-bc209341214b', more_speech_remedy_superior_to_state_restriction, empirically_contingent).
narrative_ontology:cs_axiom('9d7fe7aa-8e79-4c94-8fae-bc209341214b', secondary, content_based_restrictions_distort_epistemic_process).
narrative_ontology:cs_axiom_status(content_based_restrictions_distort_epistemic_process, holdable).
narrative_ontology:cs_axiom_grounding('9d7fe7aa-8e79-4c94-8fae-bc209341214b', content_based_restrictions_distort_epistemic_process, deontological).
narrative_ontology:cs_reference_frame('9d7fe7aa-8e79-4c94-8fae-bc209341214b', classical_marketplace_of_ideas).
narrative_ontology:cs_drift_state('9d7fe7aa-8e79-4c94-8fae-bc209341214b', algorithmic_amplification_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9d7fe7aa-8e79-4c94-8fae-bc209341214b', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__marketplace_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__marketplace_reading, public_sphere_participants).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__marketplace_reading, truth_seeking_institutions).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__marketplace_reading, minority_viewpoint_holders).
narrative_ontology:constraint_victim(speech_protection_kernel__marketplace_reading, targeted_groups_under_harmful_speech).
narrative_ontology:constraint_victim(speech_protection_kernel__marketplace_reading, vulnerable_audiences).
narrative_ontology:constraint_vindicates(speech_protection_kernel__marketplace_reading, marketplace_of_ideas_thesis).
narrative_ontology:constraint_vindicates(speech_protection_kernel__marketplace_reading, more_speech_remedy).
narrative_ontology:constraint_vindicates(speech_protection_kernel__marketplace_reading, collective_epistemic_benefit).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Engage in public discourse expecting that false claims will be corrected through open contestation. Benefit from the epistemic commons the constraint maintains. Exit is constrained — leaving the public sphere means losing voice in collective truth-seeking.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, public_sphere_participants, beneficiary,
    organized, biographical, constrained, national).

% Universities, scientific journals, courts, and investigative journalism depend on the marketplace constraint to protect the open contestation their methods require. They are institutional beneficiaries whose epistemic practices are structured by the constraint.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, truth_seeking_institutions, beneficiary,
    institutional, generational, analytical, global).

% Hold views outside the current consensus. The marketplace constraint is their primary protection against majoritarian suppression — without it, their views are excluded before they can be tested. Their exit is constrained: they cannot 'leave' the need for protection.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, minority_viewpoint_holders, beneficiary,
    powerless, biographical, constrained, national).

% Groups disproportionately targeted by hate speech, harassment, and structural vilification. The marketplace reading denies them content-based remedies ('more speech' is the only remedy). Their identity makes exit from the target position impossible — they cannot stop being the group that harmful speech targets. They pay the cost of the constraint's coordination function for others.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, targeted_groups_under_harmful_speech, payer,
    powerless, biographical, identity_locked, national).

% Individuals lacking epistemic resilience (children, cognitively impaired, information-poor environments) who absorb optimized harmful speech without functional capacity for counter-speech. They are trapped in the target position with no exit and no effective remedy under the marketplace logic.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, vulnerable_audiences, payer,
    powerless, immediate, trapped, local).

% Design and control the algorithmic amplification, recommendation, and moderation infrastructure that shapes the actual marketplace of ideas. The constraint protects their architectural choices from content-based regulation. They have arbitrage-grade exit: they can modify architecture, shift jurisdictions, or restructure platforms.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, platform_architects, agenda_setter,
    institutional, generational, arbitrage, global).

% Monitor the speech ecosystem for market failure (monopoly, amplification bias, harm concentration). Constrained by the marketplace reading's content-neutrality premise from using the most direct remedial tools. Their analytical seat sees the full structure but their remedial power is constrained by the constraint they observe.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, state_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_kernel__marketplace_reading, diffuse).
narrative_ontology:fixing_cost_class(speech_protection_kernel__marketplace_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective epistemic problem: how can a pluralistic society discover truth without a state orthodoxy? The marketplace constraint coordinates distributed truth-seeking by protecting all speech from content-based restriction, trusting that open contestation converges on truth better than any censor could.
% TRANSFER_FUNCTION: Moves the burden of harmful speech from the speaker (who faces no content-based penalty) to the targeted audience (who absorbs the harm and must generate counter-speech). Moves epistemic benefit from the open contestation to all participants, but the benefit is diffuse while the harm burden is concentrated on structurally vulnerable groups.
% ABSENT_VOICES: Targeted groups who would argue for content-based remedies (hate speech bans, platform accountability) are structurally excluded by the marketplace reading's premise — their preferred remedy is defined as the distortion the constraint exists to prevent. Also absent: future generations who inherit the epistemic environment shaped by today's marketplace dynamics.
% DISAPPEARANCE_RATIONALE: If the marketplace constraint vanished overnight, content-based speech restrictions would proliferate (hate speech laws, platform liability regimes, state orthodoxy enforcement). The epistemic commons would fragment into regulated speech zones. Truth-seeking institutions would lose their protective framework. Targeted groups might gain direct remedies but at the cost of a general suppression infrastructure that could be turned against them.
% FOUNDING_PROBLEM: How to secure collective truth-discovery in a pluralistic society without empowering the state to designate orthodoxy? The marketplace reading was built to solve this by making speech protection structural rather than discretionary — the remedy for bad speech is more speech, not state silence.
% FOUNDING_PROBLEM_CORROBORATION: Marketplace proponents (civil liberties organizations, press freedom groups, classical liberal theorists) attest the problem is live and the mechanism functions. Harm-threshold and dignity reading proponents (critical race theorists, feminist legal scholars, platform accountability advocates) attest the problem has shifted: algorithmic amplification and structural asymmetry mean the mechanism no longer functions as theorized. No consensus outside the benefiting parties.
narrative_ontology:disappearance_verdict(speech_protection_kernel__marketplace_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__marketplace_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__marketplace_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(speech_protection_kernel__marketplace_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__marketplace_reading, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__marketplace_reading_tests).
:- end_tests(speech_protection_kernel__marketplace_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38) reflects the cost borne by targeted groups who absorb harm while the 'more speech' remedy fails to functionally protect them — the coordination mechanism extracts their dignity/safety for the collective epistemic benefit. Suppression (0.22) is low: the constraint itself is a protection against suppression; the measured suppression is the constraint's own restriction on content-based regulation. Theater ratio (0.18) captures the growing gap between the marketplace ideal and platform-mediated reality where amplification algorithms, not open contestation, determine speech reach. Accessibility collapse (0.25) is modest: alternative regulatory frameworks (European hate speech law, platform moderation) exist and operate. Resistance (0.55) is moderate: significant contestation from harm-threshold and dignity readings, plus platform governance struggles.
 *
 * PERSPECTIVAL GAP:
 *   From public_sphere_participants and truth_seeking_institutions seats: the constraint is a genuine rope — coordination with diffuse benefits, minimal coercion. From targeted_groups_under_harmful_speech: the constraint operates as a snare — they pay the cost of harmful speech while the 'more speech' remedy is structurally inaccessible (identity_locked exit, no amplification access). From minority_viewpoint_holders: the constraint is a vital rope — without it, their views are excluded entirely. The engine computes this seat divergence from the structural data (power, exit_options, spatial_scope, beneficiary/victim declarations).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: public_sphere_participants, truth_seeking_institutions, minority_viewpoint_holders — they gain from the open contestation structure. Victims: targeted_groups_under_harmful_speech, vulnerable_audiences — they bear disproportionate harm when the marketplace mechanism fails. The marketplace reading's premise (counter-speech corrects harm) assigns low directionality to beneficiaries (they benefit from the arrangement) and high directionality to victims (the arrangement extracts from them by denying remedy). Platform_architects sit near d=0.15 (beneficiary: they control the architecture the constraint protects). State_regulators sit near d=0.5 (analytical: they bear enforcement costs but gain legitimacy).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to discover truth in pluralistic discourse without state orthodoxy) remains live but contested. The marketplace reading claims the problem is still live and the arrangement still solves it. Harm-threshold and dignity readings claim the problem has shifted: the epistemic environment has changed (algorithmic amplification, epistemic closure, power asymmetry) such that the original mechanism no longer functions — the arrangement persists as extraction for some seats. The mandate has not atrophied to piton because the coordination function remains real for many seats; but seat divergence is widening.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'Does this constraint instantiate one reading of the contested speech_protection_kernel, and how does that constrain its ε and structural data?',
    'This reading commits to the marketplace-of-ideas justification (collective epistemic benefit, more-speech remedy). Other readings (absolutist, harm-threshold, dignity, democratic-participation) instantiate DIFFERENT constraints with different ε and different victim/beneficiary structures. Do not average across readings.',
    'Treating the kernel as one constraint with multiple readings folded in would violate ε-invariance (DP-001). Each reading must be its own constraint story with its own ε, linked via network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer frame: this is the marketplace_reading of speech_protection_kernel').

omega_variable(
    marketplace_mechanism_empirical_validity,
    'Does the more-speech remedy actually correct false/harmful speech in practice, or does the mechanism fail under conditions of asymmetry, amplification, or epistemic closure?',
    'Empirical study of correction dynamics in networked public spheres: measure retraction diffusion, belief updating after exposure to counter-speech, and conditions under which false claims persist despite countervailing speech.',
    'If the mechanism systematically fails for structurally identifiable classes of speakers or claims, the constraint''s coordination function is impaired and its extraction profile shifts — potentially from rope toward tangled_rope or snare for those seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marketplace_mechanism_empirical_validity, empirical, 'Whether the marketplace coordination mechanism operates as theorized').

omega_variable(
    harm_asymmetry_under_marketplace_logic,
    'When harmful speech disproportionately targets groups with less platform access or epistemic authority, does the marketplace reading''s beneficiary structure conceal a snare dynamic?',
    'Map speech harm incidence and counter-speech capacity by speaker/audience power position. If harm concentrates on low-power groups while counter-speech capacity concentrates on high-power groups, the ''more speech'' remedy extracts from the harmed without providing functional coordination for them.',
    'Would reclassify the constraint for targeted-group seats from rope/beneficiary toward snare/payer — seat divergence the engine computes from the structural data.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_asymmetry_under_marketplace_logic, empirical, 'Whether asymmetric harm and asymmetric remedy capacity create extraction the marketplace framing obscures').

omega_variable(
    content_neutrality_vs_structural_bias,
    'Does the marketplace reading''s rejection of content-based restrictions structurally entrench existing speech power distributions?',
    'Analyze whether facially neutral speech rules (time/place/manner, platform architecture, amplification algorithms) produce systematically biased outcomes that the ''no content-based restrictions'' premise prevents correcting.',
    'If neutrality operates as a ratchet for existing advantage, the constraint''s extraction is higher than the marketplace framing admits — especially for seats lacking amplification access.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(content_neutrality_vs_structural_bias, conceptual, 'Whether formal content-neutrality masks structural extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__marketplace_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(speech_protection_kernel__marketplace_reading_tr_t0, speech_protection_kernel__marketplace_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(speech_protection_kernel__marketplace_reading_tr_t25, speech_protection_kernel__marketplace_reading, theater_ratio, 25, 0.12).
narrative_ontology:measurement(speech_protection_kernel__marketplace_reading_tr_t50, speech_protection_kernel__marketplace_reading, theater_ratio, 50, 0.15).
narrative_ontology:measurement(speech_protection_kernel__marketplace_reading_tr_t75, speech_protection_kernel__marketplace_reading, theater_ratio, 75, 0.17).
narrative_ontology:measurement(speech_protection_kernel__marketplace_reading_tr_t100, speech_protection_kernel__marketplace_reading, theater_ratio, 100, 0.18).

% Extraction over time
narrative_ontology:measurement(speech_protection_kernel__marketplace_reading_be_t0, speech_protection_kernel__marketplace_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(speech_protection_kernel__marketplace_reading_be_t25, speech_protection_kernel__marketplace_reading, base_extractiveness, 25, 0.32).
narrative_ontology:measurement(speech_protection_kernel__marketplace_reading_be_t50, speech_protection_kernel__marketplace_reading, base_extractiveness, 50, 0.35).
narrative_ontology:measurement(speech_protection_kernel__marketplace_reading_be_t75, speech_protection_kernel__marketplace_reading, base_extractiveness, 75, 0.37).
narrative_ontology:measurement(speech_protection_kernel__marketplace_reading_be_t100, speech_protection_kernel__marketplace_reading, base_extractiveness, 100, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(speech_protection_kernel__marketplace_reading_su_t0, speech_protection_kernel__marketplace_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement(speech_protection_kernel__marketplace_reading_su_t25, speech_protection_kernel__marketplace_reading, suppression_requirement, 25, 0.15).
narrative_ontology:measurement(speech_protection_kernel__marketplace_reading_su_t50, speech_protection_kernel__marketplace_reading, suppression_requirement, 50, 0.18).
narrative_ontology:measurement(speech_protection_kernel__marketplace_reading_su_t75, speech_protection_kernel__marketplace_reading, suppression_requirement, 75, 0.2).
narrative_ontology:measurement(speech_protection_kernel__marketplace_reading_su_t100, speech_protection_kernel__marketplace_reading, suppression_requirement, 100, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__marketplace_reading, information_standard).
narrative_ontology:boltzmann_floor_override(speech_protection_kernel__marketplace_reading, 0.02).
narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, speech_protection_kernel__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, speech_protection_kernel__harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, speech_protection_kernel__dignity_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, speech_protection_kernel__democratic_participation_reading).

% DUAL FORMULATION NOTE:
% Speech protection kernel decomposes into five constraint stories (one per reading) with different ε, different beneficiary/victim structures, and different claimed types. The marketplace reading claims rope (coordination for truth-discovery); absolutist likely claims mountain; harm_threshold and dignity readings likely claim tangled_rope or snare for targeted seats; democratic_participation likely claims scaffold or tangled_rope. All linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_protection_kernel__marketplace_reading, powerless, 0.85).
constraint_indexing:directionality_override(speech_protection_kernel__marketplace_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
