% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__harm_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
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
 *   human_readable: Harm-Threshold Reading of Speech Protection (Victim Harm Overrides Speaker Autonomy)
 *   domain: constitutional_law/political_philosophy/communication_rights
 *
 * SUMMARY:
 *   This constraint captures the harm-threshold reading of the
 *   speech-protection kernel: protection is conditional, not categorical — it
 *   holds so long as demonstrable harm to identifiable victims is absent, and
 *   yields once that threshold is met. This is a distinct constraint from the
 *   absolutist, marketplace, dignity, and democratic-participation readings
 *   of the same kernel; each of those instantiates a different protection
 *   boundary with a different beneficiary/victim structure and a different
 *   epsilon. This story is about the harm-threshold reading alone, assessed
 *   by its own lights: the standing arrangement under contest is the current
 *   evidentiary-threshold doctrine as practiced in courts and platform
 *   policy, not any reading's preferred endpoint.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__harm_threshold_reading, 0.52).
domain_priors:suppression_score(speech_protection_kernel__harm_threshold_reading, 0.58).
domain_priors:theater_ratio(speech_protection_kernel__harm_threshold_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__harm_threshold_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__harm_threshold_reading, "Harm-Threshold Reading of Speech Protection (Victim Harm Overrides Speaker Autonomy)").
narrative_ontology:topic_domain(speech_protection_kernel__harm_threshold_reading, "constitutional_law/political_philosophy/communication_rights").

domain_priors:requires_active_enforcement(speech_protection_kernel__harm_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__harm_threshold_reading, 'c464d450-8a9d-4a0c-9b3c-10e9f52cbbf6').
narrative_ontology:cs_kernel_codification('c464d450-8a9d-4a0c-9b3c-10e9f52cbbf6', distributed).
narrative_ontology:cs_authority_grounding('c464d450-8a9d-4a0c-9b3c-10e9f52cbbf6', distributed).
narrative_ontology:cs_reading_relation('c464d450-8a9d-4a0c-9b3c-10e9f52cbbf6', speech_protection_kernel__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('c464d450-8a9d-4a0c-9b3c-10e9f52cbbf6', speech_protection_kernel__marketplace_reading, coexists_with).
narrative_ontology:cs_reading_relation('c464d450-8a9d-4a0c-9b3c-10e9f52cbbf6', speech_protection_kernel__dignity_reading, influences).
narrative_ontology:cs_reading_relation('c464d450-8a9d-4a0c-9b3c-10e9f52cbbf6', speech_protection_kernel__democratic_participation_reading, coexists_with).
narrative_ontology:cs_axiom('c464d450-8a9d-4a0c-9b3c-10e9f52cbbf6', foundational, demonstrable_victim_harm_overrides_speaker_autonomy).
narrative_ontology:cs_axiom_status(demonstrable_victim_harm_overrides_speaker_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('c464d450-8a9d-4a0c-9b3c-10e9f52cbbf6', demonstrable_victim_harm_overrides_speaker_autonomy, empirically_contingent).
narrative_ontology:cs_axiom('c464d450-8a9d-4a0c-9b3c-10e9f52cbbf6', secondary, harm_must_be_individually_evidenced_not_presumed).
narrative_ontology:cs_axiom_status(harm_must_be_individually_evidenced_not_presumed, holdable).
narrative_ontology:cs_axiom_grounding('c464d450-8a9d-4a0c-9b3c-10e9f52cbbf6', harm_must_be_individually_evidenced_not_presumed, conventional).
narrative_ontology:cs_created_at('c464d450-8a9d-4a0c-9b3c-10e9f52cbbf6', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__harm_threshold_reading, harm_claim_adjudicators).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__harm_threshold_reading, documented_harm_victims).
narrative_ontology:constraint_victim(speech_protection_kernel__harm_threshold_reading, boundary_case_speakers).
narrative_ontology:constraint_victim(speech_protection_kernel__harm_threshold_reading, unpopular_minority_speakers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__harm_threshold_reading, platform_and_media_gatekeepers).
narrative_ontology:constraint_vindicates(speech_protection_kernel__harm_threshold_reading, harm_principle_as_speech_limit).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Courts, tribunals, and administrative bodies that operationalize 'demonstrable harm' into rules, thresholds, and precedent. They decide what counts as sufficient evidence of harm, set the evidentiary bar, and thereby control the actual protection boundary. Their institutional authority and case volume grow as harm-threshold doctrine becomes the operative test.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, harm_claim_adjudicators, agenda_setter,
    institutional, generational, analytical, national).

% Individuals or groups who can point to concrete, provable injury from targeted speech (defamation, incitement, targeted harassment with documented harm). They gain a legal path to redress or suppression of the speech that harmed them, but only if they can meet the demonstrability standard — those who cannot document harm as cleanly get nothing under this reading even if injury is real.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, documented_harm_victims, beneficiary,
    moderate, biographical, constrained, national).

% Speakers whose expression sits near the harm threshold — satire, provocative commentary, contested factual claims, art that offends — face genuine uncertainty about whether their speech will be classified as harmful. They must self-censor or litigate to find out, since the threshold is applied case-by-case and the standard of 'demonstrable' shifts with adjudicator composition and social climate.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, boundary_case_speakers, payer,
    moderate, biographical, constrained, national).

% Speakers holding minority or dissident views are disproportionately vulnerable because well-resourced opponents can more easily manufacture or amplify harm claims against them (reputational harm, emotional distress, economic harm from boycotts triggered by the speech itself), while they lack resources to litigate the threshold question. The harm standard is nominally neutral but its application tracks who can afford to prove harm and who can afford to defend against the claim.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, unpopular_minority_speakers, payer,
    powerless, biographical, trapped, national).

% Social media platforms and publishers adopt harm-threshold logic into content moderation policy, using 'demonstrable harm to victims' as the operative standard for removal decisions. This gives them a defensible, legally-aligned rationale for moderation choices and shields them from liability, while letting them set their own internal harm bars largely free of external review.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, platform_and_media_gatekeepers, beneficiary,
    organized, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(speech_protection_kernel__harm_threshold_reading, platform_and_media_gatekeepers, agenda_setter).

% Civil liberties groups and free-expression advocates argue the harm threshold is manipulable and chills legitimate speech long before any harm is proven, but their objections are treated as a matter of degree (raise the bar) rather than a challenge to the framework itself — the harm-threshold logic has already won the framing and their preferred near-categorical protection is not seriously on the table in adjudication.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, speaker_autonomy_advocates, excluded,
    organized, generational, constrained, national).

% Study how the harm-threshold doctrine evolves across jurisdictions, compare it to sibling doctrines (absolutist, marketplace, dignity-based), and document evidentiary drift — whether 'demonstrable' harm requirements loosen or tighten over time and who benefits from each shift.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a workable adjudicative standard that lets courts and platforms distinguish speech that causes concrete injury from speech that merely offends or contests values, avoiding both unlimited speech license and unlimited censorship discretion.
% TRANSFER_FUNCTION: Moves protection away from speakers whose expression can be characterized as demonstrably harmful and toward claimants who can produce evidence of injury; simultaneously moves discretionary power toward the institutions that define and apply the 'demonstrable' evidentiary bar.
% ABSENT_VOICES: Speakers whose expression causes diffuse, dignitary, or structural harm not easily reduced to demonstrable individual injury (the dignity reading's target) are neither fully protected nor fully covered by this reading's threshold; absolutist free-expression advocates are present in the debate but structurally outvoted once harm-threshold logic is adopted as the governing test.
% DISAPPEARANCE_RATIONALE: If the harm-threshold standard vanished, courts and platforms would need an entirely different operative test (near-categorical protection, truth-discovery balancing, or dignity-based subordination analysis); harm claimants would lose their current primary avenue for redress, and platform moderation policy built on 'demonstrable harm' language would need wholesale reconstruction.
% FOUNDING_PROBLEM: Unrestricted speech protection left genuinely injured parties (defamed individuals, incitement victims, targets of documented harassment) without redress, while genuinely unlimited restriction on 'harm' grounds threatened to swallow protected expression; the threshold was built to give courts a limiting principle that ties restriction to provable injury rather than to offense or disagreement.
% FOUNDING_PROBLEM_CORROBORATION: Tort and defamation scholars outside the harm-claimant advocacy community attest the demonstrable-injury requirement still serves a live function of preventing unbounded 'I was harmed by disagreement' claims. Free-expression organizations and empirical studies of platform moderation outcomes (produced independently of both claimant groups and platforms) report the threshold has drifted toward a lower, more subjective bar in practice, increasingly capturing contested and dissident speech rather than only clearly injurious speech.
narrative_ontology:disappearance_verdict(speech_protection_kernel__harm_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__harm_threshold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__harm_threshold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(speech_protection_kernel__harm_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__harm_threshold_reading, 0.52, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction (0.52 by interval end) reflects the doctrine's drift: originally a limiting principle protecting speech from unlimited restriction, the demonstrable-harm bar has been progressively lowered in practice (rising base_extractiveness and suppression_requirement over the interval), capturing more boundary-case and dissident speech than the founding framework intended. Suppression (0.58) is meaningfully above extraction because the chilling effect operates even on speech never formally adjudicated — speakers self-censor rather than risk litigation over an uncertain threshold. Theater ratio (0.28) is moderate-low: the adjudicative function is largely real, but a growing share of enforcement now defends institutional harm-finding authority itself rather than adjudicating genuine injury.
 *
 * DIRECTIONALITY LOGIC:
 *   Documented harm victims and harm-claim adjudicators sit toward the beneficiary end: victims gain a redress path when they can meet the evidentiary bar, and adjudicators gain expanding discretionary authority as the doctrine's application scope grows. Boundary-case speakers and unpopular minority speakers sit toward the target end: their exposure to harm claims is asymmetric because well-resourced opponents can more easily manufacture or amplify 'demonstrable' harm, while the speakers themselves often lack resources to contest the threshold determination. Platform gatekeepers occupy a dual position — nominal enforcers of a legal-style standard, but the entities who actually benefit from having a liability-shielding rationale for moderation choices they would likely make anyway.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (redress for genuinely injured parties without swallowing protected expression) remains partially live — genuine incitement and defamation harms are real and current — which prevents wholesale mandatrophy declaration. But the corroboration split (claimant-side attestation of continued necessity vs. independent-scholarship attestation of evidentiary drift toward capturing contested/dissident speech) signals the doctrine's boundary has moved beyond its original mandate even as its core justification persists. This is a live-with-drift case, not a dead-mandate case: the tangled_rope classification (genuine coordination function plus asymmetric extraction from boundary and minority speakers) is the structurally accurate label precisely because full mandatrophy resolution would be premature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    demonstrability_standard_drift,
    'Has the evidentiary bar for ''demonstrable harm'' genuinely tightened, loosened, or stayed constant across the measured interval, and is any observed drift a function of doctrine or of who is bringing the claims?',
    'Longitudinal coding of adjudicated harm claims (courts and major platform appeals bodies) for evidentiary standard applied, cross-referenced against claimant and respondent characteristics over the interval.',
    'If the bar has genuinely loosened, this reading has drifted from a limiting principle toward a broad discretionary suppression tool, supporting reclassification toward snare; if constant, the tangled_rope classification with stable but real extraction is the accurate resting state.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demonstrability_standard_drift, empirical, 'Whether the demonstrable-harm evidentiary standard is drifting or stable.').

omega_variable(
    asymmetric_claim_capacity,
    'Is the disproportionate vulnerability of unpopular minority speakers to harm claims a structural feature of the threshold itself, or an artifact of resource asymmetry that any legal standard would reproduce?',
    'Comparative analysis against jurisdictions using sibling readings (e.g., dignity_reading or absolutist_reading) to see whether resource asymmetry in speech disputes produces similar disparate outcomes independent of which doctrinal test is used.',
    'If the disparity is doctrine-specific to harm-threshold logic, it strengthens the case that this reading structurally disadvantages powerless speakers; if it is a general feature of adversarial legal process, the harm-threshold reading is not distinctively at fault.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(asymmetric_claim_capacity, conceptual, 'Whether asymmetric vulnerability is intrinsic to this reading or a general litigation artifact.').

omega_variable(
    kernel_framing_alternative,
    'Could the harm-threshold reading and the dignity reading be read as the same underlying commitment (protection conditional on injury) differing only in whether injury must be individually demonstrable or may be structural/group-level — making them variants of one reading rather than genuinely distinct siblings?',
    'Doctrinal history review: whether courts treat demonstrable-individual-harm and structural-subordination-harm as the same legal test with different evidentiary burdens, or as categorically different tests invoking different rights frameworks.',
    'If they collapse into one reading, the network decomposition should merge or tightly couple these two constraint stories; if genuinely distinct (as currently authored), they remain separate constraints with an influences edge rather than identity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_alternative, conceptual, 'Whether harm_threshold_reading and dignity_reading are genuinely distinct readings or variants of one commitment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__harm_threshold_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_kernel__harm_threshold_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(spee_tr_t8, speech_protection_kernel__harm_threshold_reading, theater_ratio, 8, 0.17).
narrative_ontology:measurement(spee_tr_t16, speech_protection_kernel__harm_threshold_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement(spee_tr_t24, speech_protection_kernel__harm_threshold_reading, theater_ratio, 24, 0.23).
narrative_ontology:measurement(spee_tr_t32, speech_protection_kernel__harm_threshold_reading, theater_ratio, 32, 0.26).
narrative_ontology:measurement(spee_tr_t40, speech_protection_kernel__harm_threshold_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(spee_be_t8, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 8, 0.39).
narrative_ontology:measurement(spee_be_t16, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 16, 0.44).
narrative_ontology:measurement(spee_be_t24, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 24, 0.47).
narrative_ontology:measurement(spee_be_t32, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 32, 0.5).
narrative_ontology:measurement(spee_be_t40, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 40, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(spee_su_t8, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 8, 0.46).
narrative_ontology:measurement(spee_su_t16, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 16, 0.5).
narrative_ontology:measurement(spee_su_t24, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 24, 0.53).
narrative_ontology:measurement(spee_su_t32, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 32, 0.56).
narrative_ontology:measurement(spee_su_t40, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__harm_threshold_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel__marketplace_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel__dignity_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel__democratic_participation_reading).

% DUAL FORMULATION NOTE:
% This story is one of five sibling constraints decomposing the natural-language concept 'speech protection' under the speech_protection_kernel. Each reading (absolutist, marketplace, dignity, harm_threshold, democratic_participation) has its own epsilon, beneficiary/victim structure, and classification because each instantiates a structurally distinct legal test with different triggers for when protection yields. They are linked here rather than merged because measuring 'speech protection' by one reading's lights yields a materially different extraction value than measuring it by another's — the ε-invariance principle requires decomposition rather than a single averaged constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
