% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: speech_protection_kernel__absolutist_reading
 *   human_readable: Absolutist Reading of the Speech Protection Kernel
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This story authors the absolutist reading of the speech protection
 *   kernel: speech protection operates near-categorically, and
 *   listener/target harm is not, by itself, grounds for restriction unless
 *   the speech falls into a narrow, historically fixed set of categorical
 *   exclusions (true threats, incitement to imminent lawless action,
 *   obscenity, fighting words). This is the doctrinal architecture most
 *   associated with mid-to-late twentieth-century American First Amendment
 *   jurisprudence. It is ONE of five readings of a contested kernel about
 *   what speech protection is for; the sibling readings (harm-threshold,
 *   marketplace, dignity, democratic-participation) are separate constraints
 *   with their own ε and are not described here except to route committer
 *   structure into omegas per Rule 2.
 *
 * KEY AGENTS:
 *   - high_reach_speakers: primary beneficiary (powerful/arbitrage) — exercises speech at scale without harm-liability exposure
 *   - media_and_platform_incumbents: institutional beneficiary (institutional/arbitrage) — externalizes distribution harm onto targets
 *   - extremist_and_hate_organizers: organized beneficiary (organized/mobile) — uses categorical line as permission structure
 *   - civil_liberties_litigators: agenda_setter (institutional/analytical) — articulates and defends the categorical boundary
 *   - targeted_minority_communities: primary target (powerless/trapped) — bears harm doctrine treats as legally irrelevant
 *   - harassment_and_doxxing_targets: primary target (powerless/trapped) — bears aggregated harm from individually-protected utterances
 *   - appellate_courts: analytical observer (institutional/analytical) — adjudicates and periodically revisits the exclusion list
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__absolutist_reading, 0.44).
domain_priors:suppression_score(speech_protection_kernel__absolutist_reading, 0.28).
domain_priors:theater_ratio(speech_protection_kernel__absolutist_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, extractiveness, 0.44).
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__absolutist_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__absolutist_reading, "Absolutist Reading of the Speech Protection Kernel").
narrative_ontology:topic_domain(speech_protection_kernel__absolutist_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(speech_protection_kernel__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__absolutist_reading, 'd4c0f069-3bdb-466e-a707-8657ba0afe9c').
narrative_ontology:cs_kernel_codification('d4c0f069-3bdb-466e-a707-8657ba0afe9c', formalized).
narrative_ontology:cs_authority_grounding('d4c0f069-3bdb-466e-a707-8657ba0afe9c', lineage).
narrative_ontology:cs_interpretation_layer_present('d4c0f069-3bdb-466e-a707-8657ba0afe9c').
narrative_ontology:cs_reading_relation('d4c0f069-3bdb-466e-a707-8657ba0afe9c', speech_protection_kernel__harm_threshold_reading, forecloses).
narrative_ontology:cs_reading_relation('d4c0f069-3bdb-466e-a707-8657ba0afe9c', speech_protection_kernel__marketplace_reading, coexists_with).
narrative_ontology:cs_reading_relation('d4c0f069-3bdb-466e-a707-8657ba0afe9c', speech_protection_kernel__dignity_reading, forecloses).
narrative_ontology:cs_reading_relation('d4c0f069-3bdb-466e-a707-8657ba0afe9c', speech_protection_kernel__democratic_participation_reading, influences).
narrative_ontology:cs_axiom('d4c0f069-3bdb-466e-a707-8657ba0afe9c', foundational, listener_harm_not_a_cognizable_restriction_ground).
narrative_ontology:cs_axiom_status(listener_harm_not_a_cognizable_restriction_ground, holdable).
narrative_ontology:cs_axiom_grounding('d4c0f069-3bdb-466e-a707-8657ba0afe9c', listener_harm_not_a_cognizable_restriction_ground, deontological).
narrative_ontology:cs_axiom('d4c0f069-3bdb-466e-a707-8657ba0afe9c', foundational, categorical_exclusions_exhaustively_fixed).
narrative_ontology:cs_axiom_status(categorical_exclusions_exhaustively_fixed, holdable).
narrative_ontology:cs_axiom_grounding('d4c0f069-3bdb-466e-a707-8657ba0afe9c', categorical_exclusions_exhaustively_fixed, conventional).
narrative_ontology:cs_reference_frame('d4c0f069-3bdb-466e-a707-8657ba0afe9c', categorical_exclusion_doctrine).
narrative_ontology:cs_drift_state('d4c0f069-3bdb-466e-a707-8657ba0afe9c', platform_scale_harassment_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d4c0f069-3bdb-466e-a707-8657ba0afe9c', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__absolutist_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__absolutist_reading, high_reach_speakers).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__absolutist_reading, media_and_platform_incumbents).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__absolutist_reading, extremist_and_hate_organizers).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__absolutist_reading, civil_liberties_litigators).
narrative_ontology:constraint_victim(speech_protection_kernel__absolutist_reading, targeted_minority_communities).
narrative_ontology:constraint_victim(speech_protection_kernel__absolutist_reading, harassment_and_doxxing_targets).
narrative_ontology:constraint_victim(speech_protection_kernel__absolutist_reading, low_status_counter_speakers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__absolutist_reading, low_status_counter_speakers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Public figures, pundits, and organizers with large platforms who can say nearly anything short of incitement or true threats without legal exposure. They benefit from a doctrine that treats their reach and the resulting harm to targets as legally irrelevant to whether the speech is protected.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, high_reach_speakers, beneficiary,
    powerful, biographical, arbitrage, national).

% Broadcasters, publishers, and platform companies rely on the near-categorical protection to avoid liability for hosting or airing harmful content, and to resist regulatory pressure to moderate. The doctrine externalizes the cost of harmful speech onto targets rather than onto the entities that profit from distributing it.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, media_and_platform_incumbents, beneficiary,
    institutional, generational, arbitrage, national).

% Groups organizing around racial, religious, or gender-based hostility use the doctrine's narrow categorical exclusions (true threats, incitement to imminent lawless action) as a permission structure, staying just inside the line while achieving intimidation effects the line does not count as harm.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, extremist_and_hate_organizers, beneficiary,
    organized, biographical, mobile, national).

% Legal organizations and doctrine-shaping judges who articulate and defend the categorical framework, litigating to keep the exclusion list narrow and to reject listener-harm balancing tests. They administer the boundary and could, in principle, argue for narrower protection, but their institutional identity is built around resisting exactly that move.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, civil_liberties_litigators, agenda_setter,
    institutional, generational, analytical, national).

% Racial, religious, gender, and sexual minorities absorb the cumulative psychological, reputational, and safety costs of speech the doctrine treats as categorically protected. They cannot bring a harm-based claim regardless of documented injury; their only recourse is counter-speech or migration away from hostile spaces, neither of which the law recognizes as a cost of the doctrine.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, targeted_minority_communities, payer,
    powerless, biographical, trapped, national).

% Individuals subjected to coordinated harassment campaigns that stay short of true-threat doctrine bear real safety and livelihood costs (job loss, relocation, chilling of their own speech) that the categorical framework does not weigh, because the framework treats the aggregate campaign as a series of individually protected utterances.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, harassment_and_doxxing_targets, payer,
    powerless, immediate, trapped, local).

% People who would answer harmful speech with more speech, per the doctrine's own marketplace logic, but lack the platform reach, legal resources, or safety margin to do so effectively. They technically hold the same formal right as high-reach speakers but cannot exercise it on comparable terms; they also benefit in principle from the same absolutist shield when their own speech is unpopular.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, low_status_counter_speakers, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_kernel__absolutist_reading, low_status_counter_speakers, beneficiary).

% Adjudicate the boundary of the categorical exclusions case by case, weighing precedent, and periodically revisit whether the exclusion list (true threats, incitement, fighting words, obscenity) should expand or contract, without formally adopting a harm-balancing standard.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, appellate_courts, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_kernel__absolutist_reading, diffuse).
narrative_ontology:fixing_cost_class(speech_protection_kernel__absolutist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a bright-line, predictable rule that prevents government officials, courts, and majorities from suppressing speech by invoking claimed harm, which is otherwise infinitely manipulable — the rule solves the real problem that harm-based speech restriction has historically been the primary tool of political and religious persecution.
% TRANSFER_FUNCTION: Moves the cost of adjudicating and absorbing harmful speech from the state (which would otherwise have to make harm determinations) and from powerful speakers (who would otherwise face liability) onto the targets of that speech, who bear the injury without a corresponding legal remedy.
% ABSENT_VOICES: Targeted minority communities and harassment victims raise harm claims routinely in public discourse and academic literature, but structurally they are not parties to the doctrinal architecture — courts hear speaker-side First Amendment claims and government-side regulatory interest claims, but there is no doctrinal seat for the listener/target's injury to be weighed as a countervailing interest.
% DISAPPEARANCE_RATIONALE: If the categorical rule vanished and harm-balancing became the operative standard overnight, litigators would immediately begin bringing harm-based restriction claims, platforms and speakers would face new liability exposure, and the exclusion list would become a sliding scale — the entire architecture of what counts as protected speech would be renegotiated case by case.
% FOUNDING_PROBLEM: Twentieth-century courts built the categorical, near-absolute standard to stop government suppression of dissent, labor organizing, and unpopular political speech that had previously been restricted under vague harm or 'public order' rationales (sedition laws, Red Scare prosecutions, civil rights-era speech suppression).
% FOUNDING_PROBLEM_CORROBORATION: Civil liberties litigators and constitutional historians attest the founding problem (state suppression of dissent) remains live, citing ongoing government efforts to restrict protest speech. Civil rights scholars, targeted-community advocacy organizations, and comparative law researchers outside the free-speech-absolutist bar attest that the doctrine has been substantially repurposed to shield concentrated private and organized harassment power that the mid-century framers did not anticipate, and that this repurposing is corroborated by empirical harassment-outcome studies rather than by the beneficiary groups themselves.
narrative_ontology:disappearance_verdict(speech_protection_kernel__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__absolutist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__absolutist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(speech_protection_kernel__absolutist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__absolutist_reading, 0.44, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__absolutist_reading_tests).
:- end_tests(speech_protection_kernel__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.44) is authored moderate-high rather than low: the doctrine has a genuine coordination function (preventing state suppression of dissent) but that function has increasingly become a shield for organized harassment and platform-scale harm that the founding cases did not contemplate, producing real asymmetric costs on identifiable target groups. Suppression is authored lower (0.28) because the doctrine does not coercively suppress speech generally — its suppressive force is narrow and categorical, aimed only at the excluded categories, not at the broader population. Theater ratio is low-moderate (0.22) and rising: a growing share of doctrinal maintenance work goes into defending the bright-line rule against harm-based challenges rather than into the rule's original anti-suppression function, which is itself a mild Goodhart signal. Accessibility collapse is moderate (0.40): the doctrine has not eliminated harm-based advocacy (scholars and advocates continue to press the case), but it has closed off harm-balancing as a judicially cognizable remedy. Resistance is fairly high (0.62), reflecting sustained doctrinal, legislative, and academic contestation from the harm-threshold and dignity camps.
 *
 * DIRECTIONALITY LOGIC:
 *   High-reach speakers, media/platform incumbents, and extremist organizers are beneficiaries: the doctrine's categorical shape converts their reach and impact into legally irrelevant facts, giving them d near the beneficiary end. Targeted minority communities and harassment targets are victims: they are structurally powerless, trapped (cannot exit the jurisdiction or the public sphere without abandoning participation), and bear costs the doctrine formally disregards — d near the full-target end. Low-status counter-speakers occupy an ambiguous middle: they hold the same formal right as high-reach speakers (nominal beneficiary) but cannot exercise it on comparable terms, and are frequently the ones absorbing the harm the doctrine licenses (payer) — hence the dual role. Civil liberties litigators are the agenda-setters: they administer and could in principle revise the boundary, but institutional identity (professional and ideological commitment to categorical rules as the only reliable anti-suppression mechanism) makes revision practically unthinkable from within that seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope (rather than snare or mountain) prevents two mislabelings. First, it prevents treating the doctrine as pure extraction (snare): the anti-suppression coordination function is real and historically load-bearing — courts built this rule specifically to stop governments from criminalizing dissent, and that problem has not disappeared. Second, it prevents treating the doctrine as natural law (mountain): the categorical boundary is a constructed doctrinal choice with identifiable winners (concentrated speakers, platforms, organized harassment actors) and identifiable losers (diffuse, powerless targets), which the false-summit signature would flag if declared with emerges_naturally. Tangled rope names the structure honestly: genuine coordination function plus asymmetric extraction, held in place by active judicial enforcement of the categorical line against harm-balancing challenges.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_absolutist,
    'Is the absolutist reading the correct interpretation of what the speech-protection kernel commits its adherents to, or is it one contestable reading among several live alternatives (harm_threshold, marketplace, dignity, democratic_participation)?',
    'No empirical resolution exists; this is a genealogical and normative dispute within constitutional theory. Track which reading commands majority doctrinal support across appellate and supreme court decisions over time, and whether legislative or constitutional amendment activity shifts the operative reading.',
    'If the harm_threshold or dignity readings gain doctrinal ascendance, the categorical exclusion list would expand to include harm-based restriction, and this story''s beneficiary/victim structure would substantially reverse — current beneficiaries (high-reach speakers, platforms) would become the class bearing new restriction, and current victims would gain a doctrinal remedy currently unavailable to them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_absolutist, conceptual, 'Which reading of the speech-protection kernel is doctrinally and normatively correct is unresolved and actively contested across the five declared readings.').

omega_variable(
    categorical_line_capture_ambiguity,
    'Is the narrow categorical exclusion list (true threats, incitement, obscenity, fighting words) a principled, stable boundary that correctly separates genuine anti-suppression coordination from harm licensing, or has it been substantively captured by organized actors who route harmful speech just inside the line?',
    'Empirical study of harassment campaign structure and outcomes: do coordinated campaigns that stay individually within the categorical line produce aggregate harms comparable to speech the line does exclude? Comparative analysis against jurisdictions using harm-threshold standards.',
    'If capture is substantial, the tangled_rope classification understates extraction — the coordination function would be more cover than substance, moving the constraint toward snare. If the line remains principled and capture is marginal, the coordination function dominates and the constraint sits closer to a genuine rope with incidental costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_line_capture_ambiguity, empirical, 'Whether the categorical exclusion boundary is a stable principled line or has been substantively gamed by organized speech-harm actors.').

omega_variable(
    listener_harm_exclusion_naturalness,
    'Is the exclusion of listener/target harm from the balancing calculus a considered normative commitment (autonomy of the speaker as foundational) or a historically contingent artifact of which litigants and interests were positioned to shape mid-century doctrine?',
    'Historical and comparative constitutional analysis: examine whether other liberal democracies with comparable commitments to free expression weigh listener harm, and whether the U.S. doctrine''s harm-exclusion tracks a principled theory or the litigation posture of the era''s dominant free-speech interest groups (ACLU-era civil liberties bar, media defendants).',
    'If contingent, the absolutist reading''s claim to categorical necessity weakens considerably, supporting reclassification pressure toward the harm_threshold or dignity readings gaining ground. If principled, the current boundary has stronger normative footing independent of who benefits from it today.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(listener_harm_exclusion_naturalness, conceptual, 'Whether excluding listener harm from the doctrine''s calculus reflects principle or historical contingency in who shaped the doctrine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__absolutist_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_kernel__absolutist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(spee_tr_t10, speech_protection_kernel__absolutist_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(spee_tr_t20, speech_protection_kernel__absolutist_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement(spee_tr_t30, speech_protection_kernel__absolutist_reading, theater_ratio, 30, 0.16).
narrative_ontology:measurement(spee_tr_t40, speech_protection_kernel__absolutist_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement(spee_tr_t50, speech_protection_kernel__absolutist_reading, theater_ratio, 50, 0.2).
narrative_ontology:measurement(spee_tr_t60, speech_protection_kernel__absolutist_reading, theater_ratio, 60, 0.22).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_kernel__absolutist_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(spee_be_t10, speech_protection_kernel__absolutist_reading, base_extractiveness, 10, 0.26).
narrative_ontology:measurement(spee_be_t20, speech_protection_kernel__absolutist_reading, base_extractiveness, 20, 0.31).
narrative_ontology:measurement(spee_be_t30, speech_protection_kernel__absolutist_reading, base_extractiveness, 30, 0.35).
narrative_ontology:measurement(spee_be_t40, speech_protection_kernel__absolutist_reading, base_extractiveness, 40, 0.39).
narrative_ontology:measurement(spee_be_t50, speech_protection_kernel__absolutist_reading, base_extractiveness, 50, 0.42).
narrative_ontology:measurement(spee_be_t60, speech_protection_kernel__absolutist_reading, base_extractiveness, 60, 0.44).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_kernel__absolutist_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(spee_su_t10, speech_protection_kernel__absolutist_reading, suppression_requirement, 10, 0.19).
narrative_ontology:measurement(spee_su_t20, speech_protection_kernel__absolutist_reading, suppression_requirement, 20, 0.21).
narrative_ontology:measurement(spee_su_t30, speech_protection_kernel__absolutist_reading, suppression_requirement, 30, 0.23).
narrative_ontology:measurement(spee_su_t40, speech_protection_kernel__absolutist_reading, suppression_requirement, 40, 0.25).
narrative_ontology:measurement(spee_su_t50, speech_protection_kernel__absolutist_reading, suppression_requirement, 50, 0.27).
narrative_ontology:measurement(spee_su_t60, speech_protection_kernel__absolutist_reading, suppression_requirement, 60, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__absolutist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, speech_protection_kernel__harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, speech_protection_kernel__marketplace_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, speech_protection_kernel__dignity_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, speech_protection_kernel__democratic_participation_reading).

% DUAL FORMULATION NOTE:
% This story is one of five constraint files decomposing the natural-language 'speech protection kernel' per the ε-invariance principle. Each reading (absolutist, harm_threshold, marketplace, dignity, democratic_participation) has its own ε, its own beneficiary/victim structure, and its own classification, because measuring 'speech protection' by the absolutist reading's lights versus the dignity reading's lights yields structurally different constraints, not the same constraint viewed from two angles. All five are linked via affects_constraints to preserve the family relationship; the harm_threshold and dignity readings are foreclosed by this reading's foundational axioms (a single doctrinal framework cannot simultaneously hold that listener harm is not cognizable AND that it is the conditioning ground for protection), while the marketplace reading coexists (different justificatory theory, compatible outcome in most cases) and the democratic_participation reading is influenced (this reading's categorical breadth creates downstream pressure on how political-speech-specific protections are justified, without logically foreclosing that narrower theory).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
