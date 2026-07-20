% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   constraint_id: speech_protection_kernel__absolutist_reading
 *   human_readable: Absolutist Reading of Speech Protection Kernel
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the absolutist reading of the
 *   speech_protection_kernel: a constitutional doctrine that treats speech
 *   protection as near-categorical and rejects listener harm as a ground for
 *   restriction. It is authored as a Tangled Rope because it carries a
 *   genuine coordination function (preventing government censorship)
 *   alongside asymmetric extraction (externalizing costs of harmful speech
 *   onto targets without legal remedy). The kernel is contested across five
 *   readings; this file isolates the absolutist reading with a single stable
 *   epsilon per the epsilon-invariance principle.
 *
 * KEY AGENTS:
 *   - judiciary: Primary agenda-setter (institutional/analytical) â administers and enforces the constitutional doctrine
 *   - commercial_media: Primary beneficiary (powerful/mobile) â collects legal immunity from liability
 *   - individual_speakers: Secondary beneficiary (moderate/mobile) â protected from speech restrictions
 *   - targets_of_protected_harm: Primary target (powerless/trapped) â bears extraction through foreclosed remedies
 *   - advocates_of_harm_prevention: Excluded voice (organized/constrained) â argues for listener-harm frameworks that are structurally barred
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__absolutist_reading, 0.58).
domain_priors:suppression_score(speech_protection_kernel__absolutist_reading, 0.6).
domain_priors:theater_ratio(speech_protection_kernel__absolutist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__absolutist_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__absolutist_reading, "Absolutist Reading of Speech Protection Kernel").
narrative_ontology:topic_domain(speech_protection_kernel__absolutist_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(speech_protection_kernel__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__absolutist_reading, 'd06f4c3f-6ae7-436e-9631-94f5dc17fc88').
narrative_ontology:cs_kernel_codification('d06f4c3f-6ae7-436e-9631-94f5dc17fc88', fixed_text).
narrative_ontology:cs_authority_grounding('d06f4c3f-6ae7-436e-9631-94f5dc17fc88', lineage).
narrative_ontology:cs_interpretation_layer_present('d06f4c3f-6ae7-436e-9631-94f5dc17fc88').
narrative_ontology:cs_reading_relation('d06f4c3f-6ae7-436e-9631-94f5dc17fc88', speech_protection_kernel__harm_threshold_reading, forecloses).
narrative_ontology:cs_reading_relation('d06f4c3f-6ae7-436e-9631-94f5dc17fc88', speech_protection_kernel__dignity_reading, forecloses).
narrative_ontology:cs_reading_relation('d06f4c3f-6ae7-436e-9631-94f5dc17fc88', speech_protection_kernel__marketplace_reading, coexists_with).
narrative_ontology:cs_reading_relation('d06f4c3f-6ae7-436e-9631-94f5dc17fc88', speech_protection_kernel__democratic_participation_reading, forecloses).
narrative_ontology:cs_axiom('d06f4c3f-6ae7-436e-9631-94f5dc17fc88', foundational, speaker_immunity_from_listener_harm_claims).
narrative_ontology:cs_axiom_status(speaker_immunity_from_listener_harm_claims, holdable).
narrative_ontology:cs_axiom_grounding('d06f4c3f-6ae7-436e-9631-94f5dc17fc88', speaker_immunity_from_listener_harm_claims, deontological).
narrative_ontology:cs_axiom('d06f4c3f-6ae7-436e-9631-94f5dc17fc88', foundational, state_speech_restriction_presumptively_void).
narrative_ontology:cs_axiom_status(state_speech_restriction_presumptively_void, holdable).
narrative_ontology:cs_axiom_grounding('d06f4c3f-6ae7-436e-9631-94f5dc17fc88', state_speech_restriction_presumptively_void, conventional).
narrative_ontology:cs_reference_frame('d06f4c3f-6ae7-436e-9631-94f5dc17fc88', categorical_speaker_immunity).
narrative_ontology:cs_drift_state('d06f4c3f-6ae7-436e-9631-94f5dc17fc88', contemporary_harm_awareness_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d06f4c3f-6ae7-436e-9631-94f5dc17fc88', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__absolutist_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__absolutist_reading, commercial_media).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__absolutist_reading, individual_speakers).
narrative_ontology:constraint_victim(speech_protection_kernel__absolutist_reading, targets_of_protected_harm).
narrative_ontology:constraint_vindicates(speech_protection_kernel__absolutist_reading, content_neutrality_doctrine).
narrative_ontology:constraint_vindicates(speech_protection_kernel__absolutist_reading, anti_censorship_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces the constitutional speech clause through judicial review, striking down laws that restrict speech based on listener harm or content preference. Maintains the doctrine through precedent, facial overbreadth doctrine, and strict scrutiny of content-based regulations.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, judiciary, agenda_setter,
    institutional, civilizational, analytical, national).

% Operate broadcast, print, and digital outlets protected by high constitutional barriers against defamation liability, prior restraint, and content regulation. Benefit from legal immunity that reduces the cost of publishing controversial or damaging material about private and public figures.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, commercial_media, beneficiary,
    powerful, biographical, mobile, national).

% Engage in expression protected from government restriction including offensive, hateful, or harassing speech that falls outside narrow categorical exceptions. Bear minimal risk of civil or criminal liability for listener harm so long as the speech does not meet exacting thresholds like incitement to imminent lawless action.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, individual_speakers, beneficiary,
    moderate, biographical, mobile, national).

% Bear the costs of harassment, defamation, intimidation, and hate speech that the absolutist doctrine protects from legal remedy. Lack effective exit because relocating, changing identity, or self-censoring are the only alternatives to enduring protected expression; courts reject their claims for redress.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, targets_of_protected_harm, payer,
    powerless, biographical, trapped, national).

% Argue for listener-harm-based speech restrictions including hate speech laws, expanded defamation standards, and dignity protections. Their frameworks are structurally excluded from constitutional doctrine; legislative victories they achieve are routinely invalidated on facial overbreadth or content-neutrality grounds.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, advocates_of_harm_prevention, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_kernel__absolutist_reading, diffuse).
narrative_ontology:fixing_cost_class(speech_protection_kernel__absolutist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents government censorship and chilling effects by creating a high constitutional barrier against content-based speech restrictions, enabling open discourse and dissent without fear of official retaliation.
% TRANSFER_FUNCTION: Transfers legal immunity from speech-related liability away from the state and from injured listeners toward speakers and publishers; moves the cost of harmful expression from the speaker to the target by foreclosing listener-harm-based remedies and regulatory alternatives.
% ABSENT_VOICES: Targets of harassment and defamation seeking civil or criminal redress; democratic majorities and legislatures attempting to regulate speech based on listener harm or dignity interests; comparative constitutionalists pointing to civil-law frameworks that balance speech with personality and equality rights.
% DISAPPEARANCE_RATIONALE: If the absolutist protection vanished overnight, legislatures would enact hate speech prohibitions, lower defamation barriers, recognize listener-harm torts, and experiment with dignity-based balancing tests; media and speakers would face new liability; the constitutional order would shift toward harm-threshold or dignity-based frameworks.
% FOUNDING_PROBLEM: Preventing government suppression of political dissent and ensuring speakers could criticize the state without official retaliation or prior restraint.
% FOUNDING_PROBLEM_CORROBORATION: Free-speech historians attest the anti-censorship problem was genuine at the founding. Critical race theorists and feminist legal scholars attest the doctrine has expanded far beyond anti-censorship to protect private and commercial harm; comparative constitutionalists from outside the US system corroborate that the founding anti-censorship problem does not require an absolutist solution.
narrative_ontology:disappearance_verdict(speech_protection_kernel__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__absolutist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__absolutist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_protection_kernel__absolutist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__absolutist_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__absolutist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_kernel__absolutist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_kernel__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects the substantial legal immunity speakers enjoy at the expense of targets' remedial options. Suppression (0.60) captures the active judicial suppression of alternative regulatory frameworks such as hate speech laws and broad defamation standards. Theater ratio (0.42) registers the gap between the doctrine's anti-censorship rhetoric and its contemporary application to commercial and harmful expression. Accessibility collapse (0.72) measures that once the absolutist framework is established, alternative harm-prevention frameworks become constitutionally inaccessible. Resistance (0.38) measures ongoing scholarly and victim-group opposition that has not shifted doctrine. The measurement series share one time grid to prevent misaligned substitution.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (commercial media, individual speakers) experience the constraint as necessary protection from government overreach and chilling liability. The payer seat (targets of protected harm) experiences the same structure as the categorical denial of legal recourse for dignitary and psychological injury. The agenda-setter seat (judiciary) experiences it as a principled interpretive commitment to textual lineage. The engine computes these divergences from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Commercial media and individual speakers are declared beneficiaries: the constraint subsidizes their expressive activity by foreclosing liability that would otherwise attach. Targets of protected harm are declared victims: the constraint extracts from them by denying remedies they would possess under alternative legal regimes such as civil-law dignity systems or harm-threshold frameworks. The judiciary is agenda-setter, not beneficiary â it does not collect rents but administers and enforces the constraint. Advocates of harm prevention are excluded: their preferred frameworks are structurally barred from constitutional consideration regardless of democratic support.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as Tangled Rope prevents mislabeling the constraint as pure coordination (Rope) â which would erase the victim seat â or as pure extraction (Snare) â which would deny the genuine anti-censorship coordination the doctrine provides against state retaliation. The founding problem (anti-censorship) is contested: it may be substantially solved for political dissent yet the arrangement persists and now protects private and commercial harm. The mandate has drifted, but the enforcement machinery has intensified, preventing mandatrophy resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    listener_harm_empirical_status,
    'Is listener harm (psychological, dignitary, and social) from protected speech empirically substantial enough to justify regulatory response, or is it ineradicable social friction?',
    'Meta-analysis of empirical studies on speech harms combined with comparative regulatory outcomes from jurisdictions that recognize listener-harm frameworks.',
    'If listener harm is substantial and remediable, the absolutist reading''s extraction is higher than its coordination value; if minimal, the coordination function dominates and the constraint leans toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(listener_harm_empirical_status, empirical, 'Whether protected speech produces measurable harms that justify regulatory intervention.').

omega_variable(
    content_neutrality_necessity,
    'Is content neutrality a structurally necessary feature of speech protection, or a contingent doctrinal choice that could be revised without collapsing the coordination function?',
    'Comparative constitutional analysis of non-content-neutral speech regimes and historical analysis of pre-neutrality First Amendment doctrine.',
    'If contingent, the absolutist reading is a constructed constraint with tunable extraction rather than a necessary coordination structure; if necessary, it resists reclassification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(content_neutrality_necessity, conceptual, 'Whether content neutrality is essential or contingent to speech protection.').

omega_variable(
    absolutist_expansion_drift,
    'Has the absolutist reading expanded beyond its founding anti-censorship function to protect commercial, harassing, and harmful expression that the original coordination problem did not encompass?',
    'Genealogical analysis of doctrine from founding-era sedition prosecutions to contemporary commercial speech and harassment jurisprudence.',
    'If the doctrine has expanded protectively without a corresponding live coordination need, the extraction component has grown relative to coordination, supporting tangled_rope or snare classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(absolutist_expansion_drift, empirical, 'Whether doctrinal expansion has outpaced the founding coordination problem.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__absolutist_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spr_abs_tr_t0, speech_protection_kernel__absolutist_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(spr_abs_tr_t12, speech_protection_kernel__absolutist_reading, theater_ratio, 12, 0.23).
narrative_ontology:measurement(spr_abs_tr_t24, speech_protection_kernel__absolutist_reading, theater_ratio, 24, 0.28).
narrative_ontology:measurement(spr_abs_tr_t36, speech_protection_kernel__absolutist_reading, theater_ratio, 36, 0.33).
narrative_ontology:measurement(spr_abs_tr_t48, speech_protection_kernel__absolutist_reading, theater_ratio, 48, 0.38).
narrative_ontology:measurement(spr_abs_tr_t60, speech_protection_kernel__absolutist_reading, theater_ratio, 60, 0.42).

% Extraction over time
narrative_ontology:measurement(spr_abs_be_t0, speech_protection_kernel__absolutist_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(spr_abs_be_t12, speech_protection_kernel__absolutist_reading, base_extractiveness, 12, 0.36).
narrative_ontology:measurement(spr_abs_be_t24, speech_protection_kernel__absolutist_reading, base_extractiveness, 24, 0.42).
narrative_ontology:measurement(spr_abs_be_t36, speech_protection_kernel__absolutist_reading, base_extractiveness, 36, 0.48).
narrative_ontology:measurement(spr_abs_be_t48, speech_protection_kernel__absolutist_reading, base_extractiveness, 48, 0.53).
narrative_ontology:measurement(spr_abs_be_t60, speech_protection_kernel__absolutist_reading, base_extractiveness, 60, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(spr_abs_su_t0, speech_protection_kernel__absolutist_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(spr_abs_su_t12, speech_protection_kernel__absolutist_reading, suppression_requirement, 12, 0.4).
narrative_ontology:measurement(spr_abs_su_t24, speech_protection_kernel__absolutist_reading, suppression_requirement, 24, 0.45).
narrative_ontology:measurement(spr_abs_su_t36, speech_protection_kernel__absolutist_reading, suppression_requirement, 36, 0.5).
narrative_ontology:measurement(spr_abs_su_t48, speech_protection_kernel__absolutist_reading, suppression_requirement, 48, 0.55).
narrative_ontology:measurement(spr_abs_su_t60, speech_protection_kernel__absolutist_reading, suppression_requirement, 60, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, speech_protection_kernel__harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, speech_protection_kernel__dignity_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, speech_protection_kernel__marketplace_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, speech_protection_kernel__democratic_participation_reading).

% DUAL FORMULATION NOTE:
% The speech_protection_kernel decomposes into five structurally distinct readings per the epsilon-invariance principle. The absolutist reading claims the widest protection boundary and is linked to its sibling readings as a constraint family; each reading has a distinct epsilon, beneficiary structure, and victim set.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
