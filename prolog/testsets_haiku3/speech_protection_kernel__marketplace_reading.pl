% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__marketplace_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Speech Protection via Epistemic Marketplace (Marketplace Reading)
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The marketplace reading of speech protection justifies constitutional
 *   speech protection on epistemic grounds: truth emerges from open
 *   competition among ideas, and state suppression of false or harmful speech
 *   distorts the truth-discovery process. Under this reading, the remedy for
 *   false speech is counter-speech — more evidence, better arguments,
 *   institutional refutation — not prior restraint. The reading coheres with
 *   institutional truth-seeking (adversarial litigation, peer review,
 *   scientific contestation) and rejects content-based regulation as
 *   incompatible with collective reasoning. The constraint is ONE READING of
 *   the speech-protection kernel; alternative readings (absolutist,
 *   harm-threshold, dignity, democratic-participation) offer different
 *   justifications and different scope boundaries. The marketplace reading's
 *   key structural feature is the DENIAL OF STATE AUTHORITY to suppress
 *   speech on grounds of falsity or harm, paired with the ASSIGNMENT OF
 *   REMEDY TO COUNTER-SPEECH AND DISTRIBUTED COMPETITION. This produces
 *   asymmetric extraction: targets of false speech bear the burden of
 *   correction; epistemically isolated populations lack the resources to
 *   mount effective counter-speech; institutional epistemic communities and
 *   courts benefit from the norm without running most of the correction work.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__marketplace_reading, 0.58).
domain_priors:suppression_score(speech_protection_kernel__marketplace_reading, 0.42).
domain_priors:theater_ratio(speech_protection_kernel__marketplace_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__marketplace_reading, rope).
narrative_ontology:human_readable(speech_protection_kernel__marketplace_reading, "Speech Protection via Epistemic Marketplace (Marketplace Reading)").
narrative_ontology:topic_domain(speech_protection_kernel__marketplace_reading, "constitutional_law/political_philosophy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__marketplace_reading, '067467d5-915e-4aae-9abc-c630b26c4d40').
narrative_ontology:cs_kernel_codification('067467d5-915e-4aae-9abc-c630b26c4d40', formalized).
narrative_ontology:cs_authority_grounding('067467d5-915e-4aae-9abc-c630b26c4d40', lineage).
narrative_ontology:cs_interpretation_layer_present('067467d5-915e-4aae-9abc-c630b26c4d40').
narrative_ontology:cs_reading_relation('067467d5-915e-4aae-9abc-c630b26c4d40', speech_protection_kernel__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('067467d5-915e-4aae-9abc-c630b26c4d40', speech_protection_kernel__harm_threshold_reading, coexists_with).
narrative_ontology:cs_reading_relation('067467d5-915e-4aae-9abc-c630b26c4d40', speech_protection_kernel__dignity_reading, coexists_with).
narrative_ontology:cs_reading_relation('067467d5-915e-4aae-9abc-c630b26c4d40', speech_protection_kernel__democratic_participation_reading, coexists_with).
narrative_ontology:cs_axiom('067467d5-915e-4aae-9abc-c630b26c4d40', foundational, truth_discovery_requires_open_contestation).
narrative_ontology:cs_axiom_status(truth_discovery_requires_open_contestation, holdable).
narrative_ontology:cs_axiom_grounding('067467d5-915e-4aae-9abc-c630b26c4d40', truth_discovery_requires_open_contestation, empirically_contingent).
narrative_ontology:cs_axiom('067467d5-915e-4aae-9abc-c630b26c4d40', foundational, state_suppression_distorts_epistemic_process).
narrative_ontology:cs_axiom_status(state_suppression_distorts_epistemic_process, holdable).
narrative_ontology:cs_axiom_grounding('067467d5-915e-4aae-9abc-c630b26c4d40', state_suppression_distorts_epistemic_process, empirically_contingent).
narrative_ontology:cs_reference_frame('067467d5-915e-4aae-9abc-c630b26c4d40', open_epistemic_contestation_framework).
narrative_ontology:cs_drift_state('067467d5-915e-4aae-9abc-c630b26c4d40', contemporary_information_fragmentation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('067467d5-915e-4aae-9abc-c630b26c4d40', '2026-06-12T14:23:17Z').
narrative_ontology:cs_kernel_id(speech_protection_kernel__marketplace_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__marketplace_reading, epistemic_communities).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__marketplace_reading, institutional_truth_seekers).
narrative_ontology:constraint_victim(speech_protection_kernel__marketplace_reading, targets_of_false_speech).
narrative_ontology:constraint_victim(speech_protection_kernel__marketplace_reading, epistemically_isolated_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Scientists, journalists, academics, and organized truth-seeking communities benefit from the norm that speech cannot be restricted on grounds of falsity or offense alone. They defend against speech bans by producing counter-evidence, alternative narratives, and methodological critique. Their operating premise is that error is best corrected through exposure to better information, not through silencing. If speech were conditional on state-approved truth, these communities would lose the epistemic autonomy to contest received wisdom.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, epistemic_communities, beneficiary,
    organized, generational, mobile, global).

% Courts, universities, scientific bodies, and other institutions charged with adjudicating truth or settling disputes operate under the premise that open debate produces better outcomes than prior restraint. They administer speech norms and enforce the principle that counter-speech is the remedy. Their structural interest is in maintaining epistemic authority without prior censorship — they decide outcomes through adversarial process, not through preventing certain voices from speaking.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, institutional_truth_seekers, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_kernel__marketplace_reading, institutional_truth_seekers, agenda_setter).

% Individuals and groups harmed by false statements or lies face the constraint that truth-discovery is structured as their problem to solve: they must mount counter-argument, produce evidence, and out-compete the false narrative in the epistemic marketplace. They cannot call for state silencing of the falsehood; the remedy is more speech from them. The burden is on targets to correct the record, not on the state to prevent the harm. Where targets lack institutional resources or epistemic standing, the practical remedy is unavailable despite its theoretical existence.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, targets_of_false_speech, payer,
    moderate, biographical, constrained, national).

% Communities with limited access to counter-narrative infrastructure, educational resources, or trusted institutional voices face the constraint most acutely: false speech circulates within their information ecosystem while the counter-speech that would correct it never reaches them, or reaches them only from sources they have learned not to trust. The marketplace remedy assumes information parity and institutional access that does not exist. Escape requires geographic or social mobility that most members lack.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, epistemically_isolated_populations, payer,
    powerless, biographical, trapped, regional).

% Government institutions that might otherwise regulate false or harmful speech to protect public health or social cohesion are structurally prevented from doing so by the reading's core logic: they are treated as incompetent judges of truth and as having incentive to suppress dissent. The reading forecloses state censorship even where harm is demonstrable. Regulatory authorities are excluded from the conversation about whether the remedy is working because the marketplace reading denies them legitimacy as truth-adjudicators.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, state_regulatory_authority, excluded,
    institutional, generational, trapped, national).

% Courts and legal advocates maintain and interpret the constitutional framework governing speech. They witness the constraint in operation and apply doctrinal tests (strict scrutiny for content-based restrictions, rational-basis review for content-neutral rules). They can produce evidence about whether the marketplace remedy is effective, whether false speech causes measurable harm despite counter-speech, and whether the distribution of epistemic resources matches the theory.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, speech_litigators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_kernel__marketplace_reading, institutional_truth_seekers).
narrative_ontology:fixing_cost_class(speech_protection_kernel__marketplace_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a norm that truth emerges through open contestation rather than through prior state review. Solves the coordination problem of how to adjudicate contested claims without vesting final authority in a single gatekeeper. Produces epistemic benefit through diversity of perspective and continuous error-correction.
% TRANSFER_FUNCTION: Transfers the burden of correcting false speech from state gatekeepers to the speech-target and the epistemic communities that contest the falsehood. Moves the risk of error from censorship (silencing true dissent) to un-corrected circulation (false claims reaching audiences that do not encounter counter-argument). The constraint moves responsibility for truth-seeking from institutional guardians to distributed competition.
% ABSENT_VOICES: Targets of false speech lacking institutional standing or epistemic resources to mount effective counter-argument are structurally silent in the marketplace reading's logic — the reading assumes they can and will contest falsehood, but does not center the cases where counter-speech is unavailable or unheard. Public health authorities whose evidence is outcompeted by disinformation, and communities isolated from epistemic institutions, would contest the premise that more-speech is adequate remedy.
% DISAPPEARANCE_RATIONALE: If the constitutional protection of false speech were removed and replaced with state power to suppress harmful falsehoods, the epistemic landscape would reorganize around official truth-claims. Institutions would shift from adversarial truth-seeking (where error is corrected through counter-evidence) to hierarchical truth-gatekeeping (where forbidden claims are removed rather than refuted). The academy, science, courts, and journalism would all function under altered incentives — more protection against prosecution, but less mandate to contest and disprove falsehood. The marketplace reading would cease to structure the epistemic commons.
% FOUNDING_PROBLEM: Government and religious authorities historically suppressed inconvenient truths by criminalizing heresy, sedition, and blasphemy. The founding problem the marketplace reading addresses is: how to prevent censorship regimes from using 'false' or 'harmful' as a pretext to silence dissent and innovation. The answer: structure speech protection so categorically that state power to suppress speech is minimal, and let truth emerge from open competition rather than official verdict.
% FOUNDING_PROBLEM_CORROBORATION: Historical atrocities of censorship regimes — the suppression of heliocentrism, germ theory, and political dissent — are documented by historians and cited by advocates of strong speech protection (Popper, Mill, Brandenburg doctrine). However, contemporary public health authorities and harm-prevention researchers attest that the problem has shifted: false speech now circulates faster and reaches more people than counter-speech, and the founding assumption (that truth out-competes falsehood in open discourse) does not hold when information ecosystems are fragmented and audiences epistemically isolated. The claim that silence equals censorship is corroborated by advocates of strong protection; the claim that the remedy is insufficient is corroborated by epidemiologists, social scientists studying misinformation spread, and targets of harassment who operate under the marketplace reading's constraints.
narrative_ontology:disappearance_verdict(speech_protection_kernel__marketplace_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__marketplace_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__marketplace_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(speech_protection_kernel__marketplace_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__marketplace_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__marketplace_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_kernel__marketplace_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_kernel__marketplace_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness reaches 0.58 (moderate-to-high) because the constraint systematically transfers burden from powerful epistemic institutions to vulnerable targets of false speech, especially epistemically isolated populations. At interval start (t=0), extractiveness is lower (0.38) — the marketplace reading operated in a context where information was scarcer and institutional gatekeeping more visible; the remedy was plausible because counter-speech could reach relevant audiences and institutional truth-seekers had resources to contest falsehood. Over 50 years, extractiveness rises (asymptoting at 0.58) as information ecosystems fragment and false speech circulates faster than counter-speech in isolated communities; the marketplace remedy becomes less effective while the constraint's formal denial of suppression authority remains stable. Suppression requirement rises from 0.15 to 0.42, tracking the growing effort needed to maintain speech-protection norms against evidence that the remedy is failing — courts and institutions invest more in defending the principle (strict scrutiny, anti-censorship doctrine, rhetorical emphasis on counter-speech) precisely because the empirical basis is eroding. Theater ratio rises from 0.12 to 0.31, reflecting the growing gap between the stated remedy (counter-speech will correct falsehood) and actual operation (false claims persist despite counter-evidence in isolated populations). The shared time grid ensures every metric is authored at every examined point; no metric is interpolated.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats experience this constraint through incompatible frames. From the institutional truth-seeker seat: the constraint is a genuine coordination mechanism that produces epistemic benefit and prevents censorship regimes. From the epistemically-isolated-population seat: the constraint is a rule that says 'your harms are not valid grounds for suppression, and you are responsible for correcting the record despite lacking institutional infrastructure.' The divergence arises because the marketplace reading assumes two things: (1) counter-speech is an available remedy, and (2) truth eventually wins in competition. Both assumptions hold for institutional seats with resources and standing; both fail for powerless, isolated populations. The engine's per-seat classification system will surface this divergence. The authored claim (rope) reflects the reading's own self-understanding; the authored metrics reflect the actual extraction profile when the reading encounters populations for which the remedy is unavailable.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional epistemic communities (courts, universities, scientific bodies) sit near the beneficiary end of directionality (d ≈ 0.15–0.25): they benefit from norms that prevent state censorship of their work and structure disputeresolution through adversarial contestation; they bear low exit cost and have mobile alternatives. Targets of false speech, especially epistemically isolated populations, sit near the target end (d ≈ 0.75–0.90): they bear the cost (burden of correction, circulation of falsehoods) and have trapped or identity-locked exit (they cannot relocate to information-rich environments). The state is excluded: it is structurally denied authority to suppress speech despite having the capacity to do so. This asymmetry should produce divergent type-classifications across seats: from the institutional seat, the constraint looks like coordination (we all benefit from open contestation); from the payer seat, especially the isolated population seat, it looks like enforced extraction (we bear the burden while you profit from epistemic immunity). The engine computes this per-seat divergence; the claim/metric independence rule means the story's CLAIMED type (rope) can diverge from what the engine COMPUTES (which may range from rope to tangled-rope depending on the seat's measurements).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing censorship regimes from suppressing inconvenient truth) is LIVE in some jurisdictions and DEAD or IRRELEVANT in others. In democracies with weak state-suppression capacity (US, Western Europe post-WWII), the founding problem remains a standing concern and the marketplace remedy is operationally plausible. In jurisdictions with authoritarian speech control (China, Russia, Turkey), the founding problem is urgent but the marketplace reading is foreclosed by the state apparatus — there is no open marketplace. In the contemporary US context, the founding problem has SHIFTED: the threat is not state censorship but the circulation of demonstrable falsehoods in fragmented information ecosystems where the remedy (counter-speech from institutional sources) does not reach isolated audiences. The constraint persists (courts continue to apply strict scrutiny, the norm holds) but its functional relationship to the founding problem has degraded — it still prevents state censorship, but the problem preventing the marketplace remedy from working is NOT state gatekeeping, it is information architecture and epistemic isolation. This mandatrophy (mismatch between constraint and founding problem) manifests as rising theater ratio: institutional actors invest more in defending the principle of counter-speech precisely as evidence accumulates that the principle does not prevent harm in fragmented ecosystems. The constraint is not broken, but the founding problem has changed shape.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    marketplace_remedy_availability,
    'Does the marketplace remedy (counter-speech correcting false claims) actually function in fragmented information ecosystems where target populations are epistemically isolated?',
    'Empirical: track whether false claims circulating in isolated communities are substantially corrected by counter-speech that originates outside those communities. Compare belief trajectories pre- and post-counter-argument in populations with and without institutional epistemic access.',
    'If remedy is substantially unavailable (false claims persist despite counter-evidence in isolated populations), the constraint functions as pure extraction for those populations — transferring burden without providing relief. The reading''s foundational premise that truth-discovery is served by untrammeled speech breaks down. This would support reclassification from rope (genuine coordination) to tangled_rope or snare (asymmetric extraction masked by coordination narrative).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marketplace_remedy_availability, empirical, 'Whether counter-speech actually corrects false speech in practice, especially for isolated populations.').

omega_variable(
    institutional_gatekeeping_vs_market_failure,
    'Is the primary problem preventing the marketplace remedy the ABSENCE of state censorship oversight (the founding problem), or the PRESENCE of information architecture that fragments audiences and prevents counter-speech from circulating?',
    'Counterfactual: if state censorship authority were granted but exercised neutrally (no political targeting), would adding suppression capacity improve truth-discovery compared to relying on counter-speech in fragmented markets? What does the evidence say about centralized fact-checking vs. distributed counter-speech?',
    'If the primary problem is now information-market failure (not state suppression), the reading''s justification — that truth emerges from open competition — no longer applies. The constraint would be justified by a different problem (preventing political censorship), not by epistemic benefit. This would shift the reading''s axioms and potentially trigger foreclosure pressure from harm-threshold or dignity readings that offer state-mediated alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_gatekeeping_vs_market_failure, conceptual, 'Whether the reading''s foundational problem (state censorship) or a different problem (information fragmentation) is the actual barrier to truth-discovery.').

omega_variable(
    suppression_internalization_trajectory,
    'Is the measured suppression (0.42) structural (the rule itself prevents targets from seeking remedy) or internalized (targets have internalized that seeking state remedy is illegitimate)?',
    'Behavioral: if the constitutional rule were changed and state suppression authority were granted, would targets of false speech immediately shift to demanding censorship, or has the norm sufficiently internalized that they continue seeking counter-speech even with suppression authority available? Post-exit trajectory in jurisdictions that permit suppression.',
    'If suppression is fully internalized, the constraint''s effective suppression is higher than 0.42 — targets carry the constraint with them even after the legal rule changes. The reading has produced cultural/epistemic lock-in where targets believe counter-speech is the only legitimate remedy even when it is unavailable. This would support reclassification toward snare (extraction dependent on internalized suppression) rather than rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization_trajectory, empirical, 'Mechanism of suppression: structural rule vs. internalized norm.').

omega_variable(
    kernel_reading_contestation,
    'Among the five readings of the speech-protection kernel (absolutist, marketplace, harm-threshold, dignity, democratic-participation), which reading''s foundational premise is most undermined by contemporary evidence about false speech circulation and epistemic isolation?',
    'Doctrinal and empirical: track how courts apply each reading''s test (strict scrutiny for marketplace, harm-balancing for harm-threshold, subordination-analysis for dignity, political-speech enhancement for democratic-participation). Document failures: where does the reading''s logic break down under contemporary conditions?',
    'The marketplace reading''s premise (truth emerges from open contestation) is most stressed by evidence of persistent false beliefs in isolated populations despite counter-speech availability. A foreclosure signal would appear if evidence decisively showed that another reading''s logic (e.g., harm-threshold with institutional fact-checking) produces better epistemic outcomes. This would not mean the marketplace reading is false, but that the kernel''s resolution requires a different reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, empirical, 'Which reading of the speech-protection kernel has the most degraded empirical foundation?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__marketplace_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_kernel__marketplace_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(spee_tr_t10, speech_protection_kernel__marketplace_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(spee_tr_t20, speech_protection_kernel__marketplace_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement(spee_tr_t30, speech_protection_kernel__marketplace_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(spee_tr_t40, speech_protection_kernel__marketplace_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement(spee_tr_t50, speech_protection_kernel__marketplace_reading, theater_ratio, 50, 0.31).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_kernel__marketplace_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(spee_be_t10, speech_protection_kernel__marketplace_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(spee_be_t20, speech_protection_kernel__marketplace_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(spee_be_t30, speech_protection_kernel__marketplace_reading, base_extractiveness, 30, 0.56).
narrative_ontology:measurement(spee_be_t40, speech_protection_kernel__marketplace_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(spee_be_t50, speech_protection_kernel__marketplace_reading, base_extractiveness, 50, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_kernel__marketplace_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(spee_su_t10, speech_protection_kernel__marketplace_reading, suppression_requirement, 10, 0.22).
narrative_ontology:measurement(spee_su_t20, speech_protection_kernel__marketplace_reading, suppression_requirement, 20, 0.3).
narrative_ontology:measurement(spee_su_t30, speech_protection_kernel__marketplace_reading, suppression_requirement, 30, 0.37).
narrative_ontology:measurement(spee_su_t40, speech_protection_kernel__marketplace_reading, suppression_requirement, 40, 0.4).
narrative_ontology:measurement(spee_su_t50, speech_protection_kernel__marketplace_reading, suppression_requirement, 50, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__marketplace_reading, information_standard).
narrative_ontology:boltzmann_floor_override(speech_protection_kernel__marketplace_reading, 0.05).
narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, speech_protection_kernel__harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, speech_protection_kernel__dignity_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, speech_protection_kernel__democratic_participation_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, speech_protection_kernel__absolutist_reading).

% DUAL FORMULATION NOTE:
% The speech-protection kernel has five distinct constraint stories, one per reading. Each story instantiates a different constraint (different ε, different beneficiary/victim set, different cs_structure axioms) while reading the same foundational kernel (the constitutional commitment to speech protection). The marketplace reading justifies protection by epistemic benefit; the absolutist reading justifies it by individual autonomy; the harm-threshold reading conditions protection on absence of harm; the dignity reading conditions it on not functioning as structural subordination; the democratic-participation reading prioritizes political speech. These readings coexist in contemporary jurisprudence; none is fully foreclosed. The marketplace reading influences the others by making truth-discovery a standing consideration. Network edges track which reading's logic constrains or pressures the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
