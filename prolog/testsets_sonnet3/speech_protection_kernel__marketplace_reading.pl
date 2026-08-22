% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__marketplace_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: speech_protection_kernel__marketplace_reading
 *   human_readable: Marketplace-of-Ideas Reading of Speech Protection
 *   domain: constitutional_law/political_philosophy/communication_rights
 *
 * SUMMARY:
 *   The marketplace-of-ideas rationale for speech protection holds that truth
 *   is best discovered through unrestricted contestation of claims, with
 *   falsehood countered by counter-speech rather than suppression. As a
 *   doctrinal commitment (traceable to Mill and Holmes's dissent in Abrams),
 *   it has genuine coordination value: it removes discretionary censorship
 *   power from the state and lets contested claims be adjudicated through
 *   public discourse rather than official fiat. But the model's underlying
 *   assumption — that speakers have roughly symmetric capacity to be heard —
 *   increasingly diverges from a communication environment structured by
 *   algorithmic amplification, platform curation, and resource asymmetry.
 *   Those best positioned to exploit the doctrine (well-resourced speakers,
 *   incumbent platforms, institutions whose authority the doctrine
 *   legitimizes) are also structurally shielded from the more skeptical
 *   harm-threshold or dignity-based sibling readings.
 *
 * KEY AGENTS:
 *   - well_resourced_speakers: primary beneficiary (powerful/arbitrage) — exploits asymmetric reach under doctrinal cover
 *   - incumbent_media_platforms: agenda_setter/beneficiary (institutional/arbitrage) — administers the practical marketplace infrastructure
 *   - targets_of_disinformation_campaigns: primary payer (powerless/trapped) — bears cost of asymmetric counter-speech capacity
 *   - marginalized_speakers_with_low_reach: payer (powerless/constrained) — formally protected, functionally unheard
 *   - courts_and_legislators: agenda_setter (institutional/analytical) — chooses among sibling readings case by case
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__marketplace_reading, 0.42).
domain_priors:suppression_score(speech_protection_kernel__marketplace_reading, 0.22).
domain_priors:theater_ratio(speech_protection_kernel__marketplace_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__marketplace_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__marketplace_reading, "Marketplace-of-Ideas Reading of Speech Protection").
narrative_ontology:topic_domain(speech_protection_kernel__marketplace_reading, "constitutional_law/political_philosophy/communication_rights").

domain_priors:requires_active_enforcement(speech_protection_kernel__marketplace_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__marketplace_reading, '79b41ce9-cd73-471f-b66a-d12f6d8dbc73').
narrative_ontology:cs_kernel_codification('79b41ce9-cd73-471f-b66a-d12f6d8dbc73', distributed).
narrative_ontology:cs_authority_grounding('79b41ce9-cd73-471f-b66a-d12f6d8dbc73', practice).
narrative_ontology:cs_interpretation_layer_present('79b41ce9-cd73-471f-b66a-d12f6d8dbc73').
narrative_ontology:cs_reading_relation('79b41ce9-cd73-471f-b66a-d12f6d8dbc73', speech_protection_kernel__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('79b41ce9-cd73-471f-b66a-d12f6d8dbc73', speech_protection_kernel__harm_threshold_reading, influences).
narrative_ontology:cs_reading_relation('79b41ce9-cd73-471f-b66a-d12f6d8dbc73', speech_protection_kernel__dignity_reading, influences).
narrative_ontology:cs_reading_relation('79b41ce9-cd73-471f-b66a-d12f6d8dbc73', speech_protection_kernel__democratic_participation_reading, coexists_with).
narrative_ontology:cs_axiom('79b41ce9-cd73-471f-b66a-d12f6d8dbc73', foundational, truth_emerges_from_unrestricted_contestation).
narrative_ontology:cs_axiom_status(truth_emerges_from_unrestricted_contestation, holdable).
narrative_ontology:cs_axiom_grounding('79b41ce9-cd73-471f-b66a-d12f6d8dbc73', truth_emerges_from_unrestricted_contestation, empirically_contingent).
narrative_ontology:cs_axiom('79b41ce9-cd73-471f-b66a-d12f6d8dbc73', foundational, counter_speech_is_a_sufficient_remedy_for_false_speech).
narrative_ontology:cs_axiom_status(counter_speech_is_a_sufficient_remedy_for_false_speech, holdable).
narrative_ontology:cs_axiom_grounding('79b41ce9-cd73-471f-b66a-d12f6d8dbc73', counter_speech_is_a_sufficient_remedy_for_false_speech, empirically_contingent).
narrative_ontology:cs_reference_frame('79b41ce9-cd73-471f-b66a-d12f6d8dbc73', holmesian_marketplace_rationale).
narrative_ontology:cs_drift_state('79b41ce9-cd73-471f-b66a-d12f6d8dbc73', platform_algorithmic_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('79b41ce9-cd73-471f-b66a-d12f6d8dbc73', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__marketplace_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__marketplace_reading, well_resourced_speakers).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__marketplace_reading, incumbent_media_platforms).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__marketplace_reading, academic_and_scientific_institutions).
narrative_ontology:constraint_victim(speech_protection_kernel__marketplace_reading, targets_of_disinformation_campaigns).
narrative_ontology:constraint_victim(speech_protection_kernel__marketplace_reading, marginalized_speakers_with_low_reach).
narrative_ontology:constraint_victim(speech_protection_kernel__marketplace_reading, misinformed_publics).
narrative_ontology:constraint_vindicates(speech_protection_kernel__marketplace_reading, epistemic_proceduralism).
narrative_ontology:constraint_vindicates(speech_protection_kernel__marketplace_reading, counter_speech_sufficiency_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Media organizations, political operations, and well-funded advocacy groups that can produce, amplify, and sustain speech at scale. Under the marketplace reading, they are free to flood a contested topic with volume, secure in the doctrine that any resulting distortion will be self-correcting through further speech. They rarely need the state's help and are the ones whose competitive advantage the doctrine effectively protects.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, well_resourced_speakers, beneficiary,
    powerful, biographical, arbitrage, national).

% Platforms and broadcasters that curate what counts as 'more speech' through algorithmic amplification or editorial choice. They administer the practical infrastructure through which the marketplace metaphor is supposed to operate, and their ranking and moderation choices determine whose counter-speech is even heard — a role the courts and legislators mostly decline to touch because the marketplace doctrine treats intervention as the greater evil.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, incumbent_media_platforms, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(speech_protection_kernel__marketplace_reading, incumbent_media_platforms, agenda_setter).

% Universities, journals, and expert bodies whose claim to authority rests on the idea that open contestation of claims, given enough time, converges on truth. The doctrine legitimizes their institutional role as arbiters of the process, even where actual public belief formation diverges sharply from the idealized model.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, academic_and_scientific_institutions, beneficiary,
    institutional, generational, mobile, national).

% Individuals and communities subject to sustained, well-funded false speech campaigns (defamation, coordinated harassment, targeted disinformation). Under the marketplace doctrine, the remedy offered is more speech — but they typically lack the reach, resources, or platform access to mount an effective rebuttal, so the promised corrective mechanism does not function symmetrically for them.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, targets_of_disinformation_campaigns, payer,
    powerless, immediate, trapped, national).

% Speakers without institutional platforms, algorithmic favor, or financial backing whose speech is nominally as protected as anyone's but functionally drowned out. The formal right to speak is preserved; the practical capacity to be heard in the 'marketplace' is not — leaving them bearing the doctrine's costs without its promised benefits.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, marginalized_speakers_with_low_reach, payer,
    powerless, biographical, constrained, national).

% The general public relying on the marketplace's self-correction to eventually surface accurate information. Where correction lags, never arrives, or arrives after decisions have been made on false premises (elections, public health choices, market decisions), this population bears the cost of the theory's failure to converge in real time.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, misinformed_publics, payer,
    powerless, generational, trapped, national).

% Advocates who argue that some speech operates as structural subordination or demonstrable harm rather than a contestable truth claim, and that treating it as raw material for the marketplace to process is itself the injury. Their framework is doctrinally excluded by marketplace reasoning, which treats content-based restriction as the greater harm regardless of documented downstream damage.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, harm_threshold_and_dignity_advocates, excluded,
    organized, biographical, constrained, national).

% Adjudicate speech disputes using marketplace reasoning as one live doctrinal framework among several, choosing when to defer to counter-speech as sufficient remedy and when to recognize harm or dignity claims as grounds for regulation. Their doctrinal choice among sibling readings determines which one governs a given case.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, courts_and_legislators, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_kernel__marketplace_reading, diffuse).
narrative_ontology:fixing_cost_class(speech_protection_kernel__marketplace_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a low-administrative-cost mechanism for resolving disputed truth claims without empowering any single authority (state, court, or platform) to adjudicate correctness directly — contested claims are left to public contestation rather than centralized suppression.
% TRANSFER_FUNCTION: Moves the burden of correcting falsehood and harm from the state (which is barred from acting on content) onto whoever is targeted or misled, while the benefit of unrestricted reach accrues to whoever already commands the largest audience or platform access.
% ABSENT_VOICES: Advocates for harm-threshold or dignity-based restriction are present in the broader legal debate but structurally excluded from THIS reading's own operative logic — the reading's premise is precisely that their harm claims should not, by themselves, justify restriction, so their objection is heard but doctrinally overridden within the reading's own terms.
% DISAPPEARANCE_RATIONALE: If the marketplace rationale disappeared as the governing doctrine, incumbent platforms and well-resourced speakers would lose a legal shield that currently limits liability and regulatory intervention for volume and reach-based speech advantages; academic institutions would need a different legitimating theory for open inquiry. Targets of disinformation and marginalized low-reach speakers might gain access to remedies currently foreclosed. Whether the world 'rearranges' or stays materially the same is itself contested between the beneficiary and payer seats.
% FOUNDING_PROBLEM: Historical censorship regimes (seditious libel prosecutions, licensing of the press, wartime suppression of dissent) demonstrated that granting government the power to suppress speech it deemed false or dangerous was reliably captured for political ends — the marketplace theory (Holmes/Mill-derived) was built to remove that discretionary power by trusting contestation over suppression.
% FOUNDING_PROBLEM_CORROBORATION: Free-speech scholars and civil liberties organizations outside the platform/media beneficiary set (e.g., historians of censorship regimes, comparative constitutional scholars) attest the original problem — state suppression of dissent — remains live in many jurisdictions and justifies strong protection. Empirical researchers studying platform-era disinformation dynamics (outside both the beneficiary and advocacy sets) report that the assumed self-correction mechanism frequently fails to converge within a relevant timeframe, particularly under algorithmic amplification, suggesting the doctrine's operating assumption no longer matches the communication environment it was designed for.
narrative_ontology:disappearance_verdict(speech_protection_kernel__marketplace_reading, contested).
narrative_ontology:founding_problem_status(speech_protection_kernel__marketplace_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__marketplace_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(speech_protection_kernel__marketplace_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__marketplace_reading, 0.42, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is moderate (0.42) rather than high because the doctrine does perform real coordination work — it forecloses a genuinely worse alternative (discretionary state censorship) — but the metric rises over the measured interval as platform-era communication dynamics (algorithmic amplification, coordinated disinformation at scale) widen the gap between the theory's assumed symmetric contestability and actual reach asymmetry. Suppression is kept low (0.22) because the marketplace reading, definitionally, resists coercive content restriction; what looks like 'suppression' under this reading is actually the amplification asymmetry that functions as a suppression-equivalent for underresourced speakers without formal censorship. Theater ratio rises to 0.4 because as the self-correction promise increasingly fails to converge in real time, invoking 'more speech is the remedy' functions increasingly as a rhetorical deflection of accountability for platforms that could otherwise intervene against demonstrable disinformation.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (platforms, well-resourced speakers, academic institutions), the doctrine reads as principled epistemic humility that protects everyone equally. From the payer seats (disinformation targets, marginalized low-reach speakers), the same formally neutral rule computes as an enforced asymmetry that leaves them without effective recourse precisely because the promised corrective mechanism (more speech) requires resources and reach they do not have.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (well-resourced speakers, platforms, academic institutions) sit near the low-d end: the doctrine subsidizes their existing reach advantage and shields them from liability or intervention. Payers (disinformation targets, marginalized low-reach speakers, misinformed publics) sit near the high-d end: they bear the asymmetric costs of a theoretically neutral rule that in practice privileges volume and platform access over accuracy or vulnerability. The doctrine's formal content-neutrality masks a real directional transfer from those without reach to those with it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — discretionary state censorship — remains partially live in many contexts, which is why this reading should not be flatly declared obsolete; but its status is genuinely contested because the communication environment that made symmetric contestability plausible (print-era pamphleteering, broadcast-era scarcity) has been replaced by an algorithmically curated environment where the self-correction mechanism the doctrine relies on frequently does not fire on a socially relevant timescale. Classifying this as tangled_rope rather than snare or mountain preserves the doctrine's genuine coordination function (removing state censorship discretion) while registering the asymmetric extraction that has grown alongside platform-era communication dynamics — collapsing it to either pure coordination or pure extraction would erase one half of the structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    convergence_timescale_validity,
    'Does the marketplace''s self-correction mechanism actually converge on accurate belief within a socially relevant timeframe, or does it fail to converge (or converge too slowly to prevent harm) under contemporary algorithmic amplification conditions?',
    'Longitudinal empirical studies tracking belief correction rates for documented disinformation campaigns pre- and post-algorithmic-curation communication environments, compared against decision-relevant deadlines (elections, public health windows).',
    'If convergence reliably fails within relevant timeframes, the coordination justification for this reading weakens substantially and the classification should drift toward snare; if convergence is empirically robust, the tangled_rope classification with moderate extraction is well-supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(convergence_timescale_validity, empirical, 'Whether the truth-discovery mechanism the reading relies on actually functions on relevant timescales.').

omega_variable(
    kernel_reading_selection_mechanism,
    'Is the marketplace_reading the correct lens for THIS kernel''s application in a given case, or is the same speech-protection kernel better read through harm_threshold_reading or dignity_reading depending on the speech''s structural function (truth claim vs. targeted subordination)?',
    'Case-level doctrinal analysis of when courts invoke marketplace reasoning versus harm-based or dignity-based reasoning; tracking whether the choice of reading correlates with the identity/power of the speaker versus the target.',
    'If reading-selection systematically favors marketplace reasoning when the beneficiary is powerful and harm-threshold reasoning when the target is powerful, the kernel-level system exhibits capture independent of any single reading''s internal logic — this is a question about the kernel, not resolvable within this single reading''s story.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_mechanism, conceptual, 'Whether reading-selection among the kernel''s sibling readings is itself systematically biased.').

omega_variable(
    asymmetric_reach_as_suppression_equivalent,
    'Should reach asymmetry that functions to silence underresourced speakers, absent any formal state censorship, be treated as suppression for classification purposes, or does the marketplace reading''s own framework correctly exclude it as a different phenomenon (private market outcome, not state action)?',
    'Comparative analysis of speaker outcomes under formally neutral marketplace rules versus outcomes under harm-threshold rules that account for reach asymmetry directly, holding underlying speech content constant.',
    'If reach asymmetry is functionally equivalent to suppression, the authored suppression score (0.22) understates the reading''s true coercive effect and should be revised upward; if it is a genuinely distinct phenomenon, the low suppression score is structurally correct and the extraction is better modeled entirely through the extractiveness metric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asymmetric_reach_as_suppression_equivalent, conceptual, 'Whether market-driven reach asymmetry should be counted as suppression or kept structurally distinct from it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__marketplace_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_kernel__marketplace_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(spee_tr_t8, speech_protection_kernel__marketplace_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement(spee_tr_t16, speech_protection_kernel__marketplace_reading, theater_ratio, 16, 0.31).
narrative_ontology:measurement(spee_tr_t24, speech_protection_kernel__marketplace_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement(spee_tr_t32, speech_protection_kernel__marketplace_reading, theater_ratio, 32, 0.38).
narrative_ontology:measurement(spee_tr_t40, speech_protection_kernel__marketplace_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_kernel__marketplace_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(spee_be_t8, speech_protection_kernel__marketplace_reading, base_extractiveness, 8, 0.31).
narrative_ontology:measurement(spee_be_t16, speech_protection_kernel__marketplace_reading, base_extractiveness, 16, 0.36).
narrative_ontology:measurement(spee_be_t24, speech_protection_kernel__marketplace_reading, base_extractiveness, 24, 0.4).
narrative_ontology:measurement(spee_be_t32, speech_protection_kernel__marketplace_reading, base_extractiveness, 32, 0.41).
narrative_ontology:measurement(spee_be_t40, speech_protection_kernel__marketplace_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_kernel__marketplace_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(spee_su_t8, speech_protection_kernel__marketplace_reading, suppression_requirement, 8, 0.16).
narrative_ontology:measurement(spee_su_t16, speech_protection_kernel__marketplace_reading, suppression_requirement, 16, 0.18).
narrative_ontology:measurement(spee_su_t24, speech_protection_kernel__marketplace_reading, suppression_requirement, 24, 0.19).
narrative_ontology:measurement(spee_su_t32, speech_protection_kernel__marketplace_reading, suppression_requirement, 32, 0.21).
narrative_ontology:measurement(spee_su_t40, speech_protection_kernel__marketplace_reading, suppression_requirement, 40, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__marketplace_reading, information_standard).
narrative_ontology:boltzmann_floor_override(speech_protection_kernel__marketplace_reading, 0.05).
narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, speech_protection_kernel__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, speech_protection_kernel__harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, speech_protection_kernel__dignity_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, speech_protection_kernel__democratic_participation_reading).

% DUAL FORMULATION NOTE:
% This story is one of five sibling constraints decomposing the natural-language label 'speech protection doctrine' per the ε-invariance principle. Each sibling reading (absolutist, harm_threshold, dignity, democratic_participation, marketplace) instantiates a structurally distinct constraint with its own ε, beneficiary/victim structure, and classification, sharing the same underlying kernel (the contested legitimating rationale for speech protection). This file authors only the marketplace_reading; the committer structure (which reading, what siblings would change, where disagreement is located) is routed to omega variables rather than folded into this constraint's own metrics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
