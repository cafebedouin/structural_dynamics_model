% ============================================================================
% CONSTRAINT STORY: impression_management_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_impression_management_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: impression_management_reading
 *   human_readable: Audience-Tailored Persona Management Reading of Claim-Making
 *   domain: social_epistemology/signaling_theory/conflict_economics
 *
 * SUMMARY:
 *   This story instantiates the audience-tailored-persona-management reading
 *   of a broader kernel about what an unqualified, publicly-repeated factual
 *   claim structurally IS. Under this reading, the claim's apparent strength
 *   is not a fixed epistemic commitment but a variable output of an
 *   audience-sensing process: the speaker calibrates confidence and
 *   defensiveness to what the specific audience in the room will accept, and
 *   retreats when a domain expert changes the audience composition. The
 *   reading predicts a specific empirical signature — hedging correlated with
 *   audience expertise, not with new evidence. In the underlying case (a
 *   hotel manager's claim, tracked across a lay-facing and an expert-facing
 *   presentation), this signature was in fact observed: the claim softened
 *   only when an expert entered, not when new information arrived. That
 *   single data point is consistent with this reading but does not by itself
 *   establish it as the generally correct reading of all such claims — hence
 *   the reading remains a live, testable hypothesis rather than a settled
 *   fact, and its siblings (stance, register, drift, filter) remain open
 *   descriptions of other cases.
 *
 * KEY AGENTS:
 *   - speaker_making_the_claim: sets the claim's presented strength per audience (moderate/mobile) — collects persuasive advantage
 *   - sympathetic_lay_audiences: receive the confident version, bear miscalibration risk (moderate/constrained)
 *   - domain_expert_challengers: trigger retreat, but often can't see the confident version shown elsewhere (powerful/constrained)
 *   - downstream_claim_consumers: inherit the lay-facing confidence several steps removed (powerless/trapped)
 *   - analytical_observers: track the claim across contexts to test the audience-expertise correlation (analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(impression_management_reading, 0.44).
domain_priors:suppression_score(impression_management_reading, 0.28).
domain_priors:theater_ratio(impression_management_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(impression_management_reading, extractiveness, 0.44).
narrative_ontology:constraint_metric(impression_management_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(impression_management_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(impression_management_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(impression_management_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(impression_management_reading, tangled_rope).
narrative_ontology:human_readable(impression_management_reading, "Audience-Tailored Persona Management Reading of Claim-Making").
narrative_ontology:topic_domain(impression_management_reading, "social_epistemology/signaling_theory/conflict_economics").

domain_priors:requires_active_enforcement(impression_management_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(impression_management_reading, '53035c08-d23d-4412-a6a5-6ef1e640d6d5').
narrative_ontology:cs_kernel_codification('53035c08-d23d-4412-a6a5-6ef1e640d6d5', distributed).
narrative_ontology:cs_authority_grounding('53035c08-d23d-4412-a6a5-6ef1e640d6d5', distributed).
narrative_ontology:cs_reading_relation('53035c08-d23d-4412-a6a5-6ef1e640d6d5', unsettled_claim_ontology__stance_reading, coexists_with).
narrative_ontology:cs_reading_relation('53035c08-d23d-4412-a6a5-6ef1e640d6d5', unsettled_claim_ontology__register_reading, coexists_with).
narrative_ontology:cs_reading_relation('53035c08-d23d-4412-a6a5-6ef1e640d6d5', unsettled_claim_ontology__drift_reading, influences).
narrative_ontology:cs_reading_relation('53035c08-d23d-4412-a6a5-6ef1e640d6d5', unsettled_claim_ontology__filter_reading, influences).
narrative_ontology:cs_axiom('53035c08-d23d-4412-a6a5-6ef1e640d6d5', foundational, claim_strength_tracks_audience_composition_not_evidence).
narrative_ontology:cs_axiom_status(claim_strength_tracks_audience_composition_not_evidence, holdable).
narrative_ontology:cs_axiom_grounding('53035c08-d23d-4412-a6a5-6ef1e640d6d5', claim_strength_tracks_audience_composition_not_evidence, empirically_contingent).
narrative_ontology:cs_axiom('53035c08-d23d-4412-a6a5-6ef1e640d6d5', secondary, expert_challenge_is_the_diagnostic_trigger_for_retreat).
narrative_ontology:cs_axiom_status(expert_challenge_is_the_diagnostic_trigger_for_retreat, holdable).
narrative_ontology:cs_axiom_grounding('53035c08-d23d-4412-a6a5-6ef1e640d6d5', expert_challenge_is_the_diagnostic_trigger_for_retreat, empirically_contingent).
narrative_ontology:cs_reference_frame('53035c08-d23d-4412-a6a5-6ef1e640d6d5', single_stable_claim_across_all_audiences).
narrative_ontology:cs_drift_state('53035c08-d23d-4412-a6a5-6ef1e640d6d5', post_cross_audience_comparison, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('53035c08-d23d-4412-a6a5-6ef1e640d6d5', '').
narrative_ontology:cs_kernel_id(impression_management_reading, unsettled_claim_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(impression_management_reading, speaker_making_the_claim).
narrative_ontology:constraint_beneficiary(impression_management_reading, sympathetic_lay_audiences).
narrative_ontology:constraint_victim(impression_management_reading, domain_expert_challengers).
narrative_ontology:constraint_victim(impression_management_reading, downstream_claim_consumers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(impression_management_reading, sympathetic_lay_audiences).
narrative_ontology:constraint_vindicates(impression_management_reading, audience_relativity_of_epistemic_confidence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjusts the strength and defense of the claim to the audience present in the room. Presents confidently to lay or sympathetic audiences, retreats to hedged or qualified versions when a domain expert enters the conversation. Controls the presentation; bears reputational cost only if caught shifting register across audiences.
narrative_ontology:constraint_stakeholder(impression_management_reading, speaker_making_the_claim, agenda_setter,
    moderate, immediate, mobile, local).

% Receive the confident, unhedged version of the claim, which is easier to act on and socially satisfying to hear. They are also the ones who absorb the claim's overstated certainty if it turns out to be wrong, since they lack the expertise to detect the tailoring.
narrative_ontology:constraint_stakeholder(impression_management_reading, sympathetic_lay_audiences, beneficiary,
    moderate, immediate, constrained, local).
narrative_ontology:stakeholder_secondary_role(impression_management_reading, sympathetic_lay_audiences, payer).

% When present, trigger the speaker's retreat to a hedged version of the claim — but this retreat is often invisible to them if they only see the hedged version and never the confident version shown to lay audiences. They bear the cost of not knowing the claim was ever presented more strongly elsewhere, which limits their ability to correct the record for the audiences who received the stronger claim.
narrative_ontology:constraint_stakeholder(impression_management_reading, domain_expert_challengers, payer,
    powerful, biographical, constrained, local).

% Act on the claim as relayed through lay audiences, often without ever encountering the hedged expert-facing version. They inherit whatever certainty was manufactured for the original sympathetic audience, several steps removed from the original tailoring.
narrative_ontology:constraint_stakeholder(impression_management_reading, downstream_claim_consumers, payer,
    powerless, generational, trapped, regional).

% Track the claim across multiple audience contexts to detect whether its strength systematically varies with audience expertise — the empirical signature this reading predicts and that any single-instance test can only partially confirm or falsify.
narrative_ontology:constraint_stakeholder(impression_management_reading, analytical_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(impression_management_reading, speaker_making_the_claim).
narrative_ontology:fixing_cost_class(impression_management_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Audience-calibrated presentation solves a real communicative problem: matching technical precision and hedging to the audience's capacity to process qualification, so lay audiences aren't drowned in caveats and experts aren't insulted by oversimplification.
% TRANSFER_FUNCTION: Moves epistemic confidence away from where it would be warranted by the evidence and toward whatever level plays best with the audience physically present, transferring the cost of miscalibration to audiences who never see the alternate version.
% ABSENT_VOICES: Downstream consumers of the lay-audience version never hear the hedged expert-facing version and have no way to know a stronger claim was made elsewhere; they would object to the disparity if it were visible to them.
% DISAPPEARANCE_RATIONALE: If audience-tailoring vanished, some speakers would default to uniformly hedged claims (world barely changes for careful communicators) while others who relied on strategic confidence-shifting to persuade would lose a persuasion tool that shaped downstream belief and action (world rearranges for audiences who no longer receive an inflated-confidence version). The parties dispute which describes them.
% FOUNDING_PROBLEM: Genuine communicative adaptation problem: the same true information requires different framing, vocabulary, and hedging depending on the audience's background knowledge, to be both accurate and comprehensible.
% FOUNDING_PROBLEM_CORROBORATION: The speaker attests the tailoring is purely pedagogical register-matching. A documented case (the hotel-manager instance underlying this constraint family) shows retreat specifically triggered by the arrival of a domain expert rather than by audience comprehension level, which is the signature this reading predicts and which an outside observer — not the speaker — identified from comparing the two audience-facing versions.
narrative_ontology:disappearance_verdict(impression_management_reading, contested).
narrative_ontology:founding_problem_status(impression_management_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(impression_management_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-21',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(impression_management_reading, 'none', 1).
narrative_ontology:epsilon_provenance(impression_management_reading, 0.44, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(impression_management_reading_tests).
:- end_tests(impression_management_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.44): the speaker gains persuasive traction and reputational insulation by presenting different claim strengths to different audiences, at the expense of audiences who never see the alternate version and therefore cannot properly discount the claim's real evidentiary basis. Suppression is comparatively low (0.28) because nothing coercively prevents audiences from comparing notes — the mechanism relies on audience segregation, not force. Theater ratio is elevated and rising (0.40 to 0.62) because an increasing share of the speaker's presentational effort goes toward managing appearance-of-confidence rather than communicating actual uncertainty, particularly once the tailoring becomes habitual across many audience encounters.
 *
 * PERSPECTIVAL GAP:
 *   From the speaker's seat, this looks like ordinary register-matching — good communicative practice. From the domain-expert seat, the same behavior looks like selective concealment: the claim they see hedged was, to their knowledge, never confidently defended to them, but they cannot know it was defended elsewhere without cross-audience comparison. The engine should compute divergent seat-level types precisely because the coordination function (matching complexity to audience) and the extraction function (manufacturing appearance-consistency across incompatible presentations) are both genuinely present and asymmetrically distributed.
 *
 * DIRECTIONALITY LOGIC:
 *   The speaker sits near the beneficiary end: they set the tailoring, retain mobility across audience contexts, and are rarely held to a single consistent public claim. Sympathetic lay audiences are dual-positioned — beneficiaries of comprehensible, confident framing, but payers when that confidence overstates the evidence. Domain expert challengers and downstream consumers are targets: the former because their challenge is neutralized by version-switching rather than genuinely engaged, the latter because they receive the least-hedged version furthest from its point of production and origin scrutiny.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — that different audiences genuinely need different levels of technical hedging to correctly understand the same claim — is real and durable; it does not disappear. What can go stale is treating EVERY instance of audience-sensitive variation as illegitimate impression management, which would misclassify genuine pedagogical adaptation as extraction. Conversely, treating every instance as innocent register-matching risks laundering strategic retreat-under-expert-challenge as pedagogy. The tangled_rope classification holds both: a genuine coordination function (comprehensibility) riding alongside an extraction function (confidence laundering) that requires the audience segregation to persist — separating audiences (i.e., always presenting to mixed audiences, or archiving claims across audiences) would collapse the extraction component while leaving the coordination component intact.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_generality,
    'Does the audience-tailored persona management reading generalize beyond the hotel-manager instance that motivated it, or is it a single-case pattern being extrapolated into a general reading of unqualified claims?',
    'Track claim strength for a sample of speakers across matched audience-expertise variations (lay vs. domain-expert audiences, holding evidence constant); look for the specific signature of expertise-triggered retreat rather than evidence-triggered revision.',
    'If the signature generalizes, this reading correctly describes a structural feature of much public claim-making and the tangled_rope classification is well-founded broadly. If it is idiosyncratic to the single documented case, this reading should be scoped narrowly and the sibling readings (register, drift, stance, filter) may better describe the general phenomenon.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_generality, empirical, 'Whether the impression-management signature generalizes beyond its founding case.').

omega_variable(
    sibling_reading_disambiguation,
    'In any given instance of claim-strength variation across audiences, is the correct reading impression management (strategic, expertise-triggered retreat), register adaptation (pedagogically appropriate framing), stance shift (genuine belief update), drift (unintentional inconsistency), or filter (selective disclosure of caveats already held)?',
    'Compare the trigger condition for the shift: expertise of audience (impression management), comprehension level (register), new evidence (stance/drift), or pre-existing caveat suppression revealed under scrutiny (filter). The hotel-manager case shows an expertise-triggered trigger, which is this reading''s diagnostic signature.',
    'Misidentifying which reading applies to a given instance would misclassify pedagogical adaptation as extraction, or launder strategic confidence-shifting as legitimate register-matching.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_disambiguation, conceptual, 'The five sibling readings are structurally distinct claims about the same surface phenomenon; disambiguation is instance-specific.').

omega_variable(
    detectability_asymmetry,
    'How often does audience-tailoring escape detection because audiences are structurally segregated (lay audiences and expert audiences rarely compare notes on the same claim)?',
    'Audit rate of cross-audience claim comparison in comparable domains (public communication, expert testimony, marketing) to estimate what fraction of tailoring instances would surface if audiences were merged or claims were archived publicly.',
    'A high detection-escape rate would support treating the theater_ratio trend as understated rather than overstated, since much tailoring never becomes visible enough to measure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(detectability_asymmetry, empirical, 'Structural audience segregation likely undercounts the true extent of tailoring.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(impression_management_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impr_tr_t0, impression_management_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(impr_tr_t4, impression_management_reading, theater_ratio, 4, 0.46).
narrative_ontology:measurement(impr_tr_t8, impression_management_reading, theater_ratio, 8, 0.51).
narrative_ontology:measurement(impr_tr_t12, impression_management_reading, theater_ratio, 12, 0.55).
narrative_ontology:measurement(impr_tr_t16, impression_management_reading, theater_ratio, 16, 0.58).
narrative_ontology:measurement(impr_tr_t20, impression_management_reading, theater_ratio, 20, 0.6).
narrative_ontology:measurement(impr_tr_t24, impression_management_reading, theater_ratio, 24, 0.62).

% Extraction over time
narrative_ontology:measurement(impr_be_t0, impression_management_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(impr_be_t4, impression_management_reading, base_extractiveness, 4, 0.34).
narrative_ontology:measurement(impr_be_t8, impression_management_reading, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(impr_be_t12, impression_management_reading, base_extractiveness, 12, 0.4).
narrative_ontology:measurement(impr_be_t16, impression_management_reading, base_extractiveness, 16, 0.42).
narrative_ontology:measurement(impr_be_t20, impression_management_reading, base_extractiveness, 20, 0.43).
narrative_ontology:measurement(impr_be_t24, impression_management_reading, base_extractiveness, 24, 0.44).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(impression_management_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(impression_management_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(impression_management_reading, 0.08).
narrative_ontology:affects_constraint(impression_management_reading, stance_reading).
narrative_ontology:affects_constraint(impression_management_reading, register_reading).
narrative_ontology:affects_constraint(impression_management_reading, drift_reading).
narrative_ontology:affects_constraint(impression_management_reading, filter_reading).

% DUAL FORMULATION NOTE:
% This story is one of five siblings decomposing the natural-language concept 'what kind of claim was this' under the unsettled_claim_ontology kernel. Each sibling names a structurally distinct hypothesis about why claim strength varied across contexts: impression_management_reading (audience-tailored persona management, expertise-triggered retreat), stance_reading (genuine belief change), register_reading (legitimate comprehension-matched framing), drift_reading (unintentional inconsistency over time), filter_reading (selective disclosure of pre-existing caveats). Each carries its own epsilon and stakeholder structure; they are linked via affects_constraints rather than merged, per the ε-invariance principle — averaging or hedging across them would misrepresent five distinct empirical claims as one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(impression_management_reading, powerful, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
