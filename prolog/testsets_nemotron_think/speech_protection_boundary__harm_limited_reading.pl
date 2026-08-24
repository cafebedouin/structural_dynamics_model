% ============================================================================
% CONSTRAINT STORY: speech_protection_boundary__harm_limited_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_boundary__harm_limited_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: speech_protection_boundary__harm_limited_reading
 *   human_readable: Speech Protection Conditional on Absence of Significant Harm to Dignity, Equality, and Freedom from Harassment
 *   domain: constitutional_law/political_philosophy/speech_regulation
 *
 * SUMMARY:
 *   This constraint instantiates the harm-limited reading of the
 *   speech_protection_boundary kernel: speech loses constitutional protection
 *   when it causes significant harm to dignity, equality, or freedom from
 *   harassment. The protected set narrows categorically (hate speech,
 *   harassment, coded dog whistles are unprotected); the state becomes the
 *   gatekeeper defining and enforcing these boundaries. The coordination
 *   claim is protecting vulnerable groups' participatory equality; the
 *   extraction is state power to define harm expansively and suppress
 *   dissent. Over the interval, extractiveness rises as harm categories
 *   expand (from explicit incitement to coded dog whistles, from public to
 *   private speech, from state to platform enforcement), theater grows as
 *   procedural safeguards become performative, and suppression hardens as
 *   enforcement shifts from criminal prosecution to administrative and
 *   private ordering.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_boundary__harm_limited_reading, 0.62).
domain_priors:suppression_score(speech_protection_boundary__harm_limited_reading, 0.58).
domain_priors:theater_ratio(speech_protection_boundary__harm_limited_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_boundary__harm_limited_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_boundary__harm_limited_reading, "Speech Protection Conditional on Absence of Significant Harm to Dignity, Equality, and Freedom from Harassment").
narrative_ontology:topic_domain(speech_protection_boundary__harm_limited_reading, "constitutional_law/political_philosophy/speech_regulation").

domain_priors:requires_active_enforcement(speech_protection_boundary__harm_limited_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_boundary__harm_limited_reading, 'dc1bc00b-a66b-4328-be2c-3ee0f65c40cf').
narrative_ontology:cs_kernel_codification('dc1bc00b-a66b-4328-be2c-3ee0f65c40cf', formalized).
narrative_ontology:cs_authority_grounding('dc1bc00b-a66b-4328-be2c-3ee0f65c40cf', extraction).
narrative_ontology:cs_interpretation_layer_present('dc1bc00b-a66b-4328-be2c-3ee0f65c40cf').
narrative_ontology:cs_reading_relation('dc1bc00b-a66b-4328-be2c-3ee0f65c40cf', speech_protection_boundary__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('dc1bc00b-a66b-4328-be2c-3ee0f65c40cf', speech_protection_boundary__balancing_reading, influences).
narrative_ontology:cs_axiom('dc1bc00b-a66b-4328-be2c-3ee0f65c40cf', foundational, dignity_equality_trumps_speech).
narrative_ontology:cs_axiom_status(dignity_equality_trumps_speech, holdable).
narrative_ontology:cs_axiom_grounding('dc1bc00b-a66b-4328-be2c-3ee0f65c40cf', dignity_equality_trumps_speech, deontological).
narrative_ontology:cs_axiom('dc1bc00b-a66b-4328-be2c-3ee0f65c40cf', foundational, state_gatekeeping_legitimate_for_harm_prevention).
narrative_ontology:cs_axiom_status(state_gatekeeping_legitimate_for_harm_prevention, holdable).
narrative_ontology:cs_axiom_grounding('dc1bc00b-a66b-4328-be2c-3ee0f65c40cf', state_gatekeeping_legitimate_for_harm_prevention, conventional).
narrative_ontology:cs_reference_frame('dc1bc00b-a66b-4328-be2c-3ee0f65c40cf', harm_based_speech_boundary).
narrative_ontology:cs_drift_state('dc1bc00b-a66b-4328-be2c-3ee0f65c40cf', contemporary_digital_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('dc1bc00b-a66b-4328-be2c-3ee0f65c40cf', '').
narrative_ontology:cs_kernel_id(speech_protection_boundary__harm_limited_reading, speech_protection_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_boundary__harm_limited_reading, marginalized_groups_targeted_by_hate_speech).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__harm_limited_reading, general_public_dignity_interest).
narrative_ontology:constraint_victim(speech_protection_boundary__harm_limited_reading, speakers_restricted_by_harm_categories).
narrative_ontology:constraint_victim(speech_protection_boundary__harm_limited_reading, marginalized_groups_subject_to_state_gatekeeping_abuse).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(speech_protection_boundary__harm_limited_reading, marginalized_groups_targeted_by_hate_speech).
narrative_ontology:constraint_victim(speech_protection_boundary__harm_limited_reading, digital_platforms).
narrative_ontology:constraint_vindicates(speech_protection_boundary__harm_limited_reading, dignity_equality_as_constitutional_values).
narrative_ontology:constraint_vindicates(speech_protection_boundary__harm_limited_reading, harm_prevention_justifies_speech_restriction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts and enforces laws defining harm categories (hate speech, harassment, coded dog whistles) that remove speech from protection. Controls prosecution resources and administrative guidance. Gains regulatory authority over public discourse but faces constitutional challenges and political backlash when overreach is perceived.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, state_legislature_executive, agenda_setter,
    institutional, generational, arbitrage, national).

% Gain protection from identity-based attacks that silence participation and inflict dignitary harm. Also bear risk that state gatekeeping power is turned against their own advocacy (e.g., protest speech coded as harassment, counter-speech suppressed). Exit is constrained — leaving the jurisdiction is costly; internal dissent risks retaliation.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, marginalized_groups_targeted_by_hate_speech, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_boundary__harm_limited_reading, marginalized_groups_targeted_by_hate_speech, payer).

% Face criminal or civil liability for speech falling within legislated harm categories (hate speech, harassment, dog whistles). Includes both malicious actors and speakers engaged in legitimate but edgy discourse (satire, academic inquiry, political rhetoric). Exit options: self-censor, litigate (costly, uncertain), or speak from extraterritorial platforms (technically constrained).
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, speakers_restricted_by_harm_categories, payer,
    moderate, biographical, constrained, national).

% Benefits from a public sphere with less identity-based vitriol and harassment, enabling broader participation. Bears diffuse cost of chilled discourse and state monitoring. Mobile exit: can disengage from toxic platforms or migrate to curated communities, but cannot exit the legal regime.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, general_public_dignity_interest, beneficiary,
    organized, generational, mobile, national).

% Interpret harm categories, balance dignity/equality against speech, and review state enforcement for overreach. Their jurisprudence shapes the operational boundary. Neither collect nor pay directly but structure the constraint's application. Analytical exit: can dissent in opinions but cannot leave the adjudicative role.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, courts_adjudicators, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_boundary__harm_limited_reading, courts_adjudicators, agenda_setter).

% Argue that harm categories are overbroad, vague, and enable viewpoint discrimination. Would challenge every expansion of unprotected categories. Excluded from legislative drafting and enforcement discretion; operate through litigation, advocacy, and public campaigns. Mobile: can shift forums (courts, legislatures, international bodies) but cannot escape the domestic constraint.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, civil_liberties_advocates, excluded,
    organized, biographical, mobile, national).

% Bear compliance costs for state-mandated content removal (hate speech, harassment takedowns). Simultaneously set private speech rules that align with or exceed state standards, gaining legitimacy and avoiding regulation. Arbitrage exit: can restructure operations across jurisdictions, geofence content, or shift moderation burden to users.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, digital_platforms, payer,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(speech_protection_boundary__harm_limited_reading, digital_platforms, agenda_setter).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a public sphere where dignity and equality are preconditions for participation by establishing categorical exclusions for speech that inflicts significant harm on protected groups, replacing case-by-case adjudication with legislated boundaries.
% TRANSFER_FUNCTION: Moves regulatory authority over speech boundaries from speakers and platforms to the state; moves dignitary security to marginalized groups at the cost of speakers' expressive liberty and platforms' operational autonomy.
% ABSENT_VOICES: Speakers in jurisdictions without harm-based regimes (who would testify to chilling effects); future generations who inherit the precedent of state-defined harm categories; minority dissenters within marginalized groups whose speech the state may classify as harassment.
% DISAPPEARANCE_RATIONALE: If the harm-limited regime vanished overnight, legislated hate speech and harassment prohibitions would fall to case-by-case balancing or absolutist standards; marginalized groups would lose categorical protection; state gatekeeping authority would dissolve; platforms would revert to private ordering; the public sphere would reorganize around whatever default rule courts or legislatures adopt next.
% FOUNDING_PROBLEM: Post-war constitutional orders recognized that unregulated speech could entrench hierarchy and exclude groups from democratic participation; categorical harm-based restrictions were adopted to secure the preconditions of equal citizenship.
% FOUNDING_PROBLEM_CORROBORATION: International human rights bodies (UN HRC, ECtHR) and comparative constitutional courts (Canada, Germany, South Africa) attest the founding problem remains live — identity-based hate speech continues to silence participation. Civil liberties organizations and originalist scholars attest the problem is substantially solved or that the remedy has become the disease, citing mission creep into political dissent.
narrative_ontology:disappearance_verdict(speech_protection_boundary__harm_limited_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_boundary__harm_limited_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_boundary__harm_limited_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_protection_boundary__harm_limited_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_boundary__harm_limited_reading, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_boundary__harm_limited_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_boundary__harm_limited_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_boundary__harm_limited_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects the state's captured regulatory authority: it defines harm categories, controls enforcement, and gains legitimacy from the coordination function while expanding control over discourse. Suppression (0.58) is substantial — the constraint persists through active enforcement (criminal laws, platform mandates, administrative guidance) not voluntary compliance. Theater (0.31) captures the gap between stated dignitary protection and actual gatekeeping: procedural safeguards (narrow tailoring, clear definitions) exist but are routinely stretched. Accessibility collapse (0.42) is moderate — alternatives (absolutist, balancing) remain legally and intellectually available but are politically marginalized in jurisdictions adopting this reading. Resistance (0.54) is significant — courts, civil society, and international bodies contest expansions, but the constraint holds.
 *
 * PERSPECTIVAL GAP:
 *   From the state's seat, the constraint is genuine coordination solving a collective-action problem (protecting equality). From restricted speakers' seat, it is enforced extraction (state defines harm to suppress dissent). From marginalized groups' seat, it is both — protection that can invert into persecution. The engine computes this divergence from the structural data; the claimed tangled_rope reflects the author's judgment that both functions are real and inseparable.
 *
 * DIRECTIONALITY LOGIC:
 *   State legislature/executive: full beneficiary (d ≈ 0.1) — collects regulatory authority, sets agenda, arbitrage exit. Marginalized groups: dual beneficiary/payer (d ≈ 0.45) — gain dignitary security but pay abuse risk; constrained exit. Speakers restricted: full payer (d ≈ 0.85) — bear liability, chilled speech, constrained exit. General public: net beneficiary (d ≈ 0.35) — gain cleaner discourse, pay diffuse chill; mobile exit. Courts: analytical observer (d ≈ 0.5) — structure application, no direct stake. Civil liberties advocates: excluded (d ≈ 0.7) — would object, structurally locked out of gatekeeping. Platforms: dual payer/agenda_setter (d ≈ 0.4) — pay compliance, gain regulatory capture; arbitrage exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (securing participatory equality for historically excluded groups) remains live but contested. The constraint shows mandatrophy indicators: extractiveness rising as harm categories expand beyond the original justification (dog whistles, microaggressions, private speech), theater increasing as safeguards become performative, and the state's gatekeeping power creating new harms (suppression of minority dissent within protected groups). The mandate has not been resolved — the problem persists — but the arrangement has accumulated extraction beyond its coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harm_category_expansion_trajectory,
    'Will harm categories continue expanding (dog whistles → microaggressions → disfavored viewpoints) or stabilize at a principled boundary?',
    'Longitudinal coding of legislative and judicial harm definitions across jurisdictions; track whether expansion correlates with political cycles or follows internal logic.',
    'Unbounded expansion pushes classification toward snare (coordination story becomes cover); stabilization supports tangled_rope (genuine coordination with bounded extraction).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(harm_category_expansion_trajectory, empirical, 'Whether the constraint''s extraction trajectory is bounded or open-ended.').

omega_variable(
    state_gatekeeping_abuse_incidence,
    'How frequently does state harm-definition power target marginalized groups'' own advocacy versus majority-group hate speech?',
    'Comparative case law analysis: code enforcement actions by target group identity and speech type (hate vs. protest vs. dissent).',
    'High abuse incidence reclassifies marginalized groups from net beneficiaries to net payers, altering the beneficiary/victim structure and effective extraction distribution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_gatekeeping_abuse_incidence, empirical, 'Whether the gatekeeping power is disproportionately turned against its stated beneficiaries.').

omega_variable(
    coordination_extraction_separability,
    'Can the dignitary protection function be achieved without state categorical gatekeeping (e.g., through counterspeech norms, platform self-regulation, targeted harassment laws)?',
    'Natural experiment: jurisdictions with balancing or absolutist regimes — measure participatory equality outcomes for marginalized groups.',
    'If protection is achievable without categorical state gatekeeping, the extraction component is separable and the constraint leans snare; if not, the coordination function genuinely requires the extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether the coordination and extraction components are structurally inseparable.').

omega_variable(
    kernel_reading_foreclosure_structure,
    'Does the harm_limited_reading''s core premise (dignity/equality categorically trump speech) logically foreclose the absolutist_reading within a single legal framework, or do they coexist as competing interpretive traditions?',
    'Analyze constitutional texts and court decisions that adopt harm-limited regimes: do they explicitly reject absolutism as incompatible, or treat it as a disfavored but available interpretation?',
    'If foreclosure is structural, the kernel has a genuine forecloses edge; if coexistence, the edge is coexists_with — affects how the engine models reading competition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_structure, conceptual, 'Structural relationship between this reading and the absolutist sibling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_boundary__harm_limited_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t1945, speech_protection_boundary__harm_limited_reading, theater_ratio, 1945, 0.08).
narrative_ontology:measurement(spee_tr_t1965, speech_protection_boundary__harm_limited_reading, theater_ratio, 1965, 0.12).
narrative_ontology:measurement(spee_tr_t1985, speech_protection_boundary__harm_limited_reading, theater_ratio, 1985, 0.18).
narrative_ontology:measurement(spee_tr_t2000, speech_protection_boundary__harm_limited_reading, theater_ratio, 2000, 0.24).
narrative_ontology:measurement(spee_tr_t2010, speech_protection_boundary__harm_limited_reading, theater_ratio, 2010, 0.28).
narrative_ontology:measurement(spee_tr_t2025, speech_protection_boundary__harm_limited_reading, theater_ratio, 2025, 0.31).

% Extraction over time
narrative_ontology:measurement(spee_be_t1945, speech_protection_boundary__harm_limited_reading, base_extractiveness, 1945, 0.25).
narrative_ontology:measurement(spee_be_t1965, speech_protection_boundary__harm_limited_reading, base_extractiveness, 1965, 0.32).
narrative_ontology:measurement(spee_be_t1985, speech_protection_boundary__harm_limited_reading, base_extractiveness, 1985, 0.41).
narrative_ontology:measurement(spee_be_t2000, speech_protection_boundary__harm_limited_reading, base_extractiveness, 2000, 0.52).
narrative_ontology:measurement(spee_be_t2010, speech_protection_boundary__harm_limited_reading, base_extractiveness, 2010, 0.57).
narrative_ontology:measurement(spee_be_t2025, speech_protection_boundary__harm_limited_reading, base_extractiveness, 2025, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t1945, speech_protection_boundary__harm_limited_reading, suppression_requirement, 1945, 0.35).
narrative_ontology:measurement(spee_su_t1965, speech_protection_boundary__harm_limited_reading, suppression_requirement, 1965, 0.42).
narrative_ontology:measurement(spee_su_t1985, speech_protection_boundary__harm_limited_reading, suppression_requirement, 1985, 0.48).
narrative_ontology:measurement(spee_su_t2000, speech_protection_boundary__harm_limited_reading, suppression_requirement, 2000, 0.52).
narrative_ontology:measurement(spee_su_t2010, speech_protection_boundary__harm_limited_reading, suppression_requirement, 2010, 0.55).
narrative_ontology:measurement(spee_su_t2025, speech_protection_boundary__harm_limited_reading, suppression_requirement, 2025, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_boundary__harm_limited_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(speech_protection_boundary__harm_limited_reading, 0.08).
narrative_ontology:affects_constraint(speech_protection_boundary__harm_limited_reading, platform_content_moderation_regime).
narrative_ontology:affects_constraint(speech_protection_boundary__harm_limited_reading, academic_freedom_boundary).
narrative_ontology:affects_constraint(speech_protection_boundary__harm_limited_reading, protest_assembly_regulation).

% DUAL FORMULATION NOTE:
% Part of the speech_protection_boundary constraint family. Decomposed from the colloquial 'hate speech exception' label per ε-invariance: this reading has ε=0.62 (substantial extraction via state gatekeeping); absolutist_reading has ε≈0.05 (negligible extraction); balancing_reading has ε≈0.35 (moderate extraction via case-by-case adjudication). Each reading instantiates a different constraint with different stakeholder structures and temporal trajectories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_protection_boundary__harm_limited_reading, institutional, 0.1).
constraint_indexing:directionality_override(speech_protection_boundary__harm_limited_reading, organized, 0.45).
constraint_indexing:directionality_override(speech_protection_boundary__harm_limited_reading, moderate, 0.85).
constraint_indexing:directionality_override(speech_protection_boundary__harm_limited_reading, powerful, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
