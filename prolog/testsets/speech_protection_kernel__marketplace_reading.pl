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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: speech_protection_kernel__marketplace_reading
 *   human_readable: Speech Protection as Truth-Discovery Coordination (Marketplace Reading)
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The marketplace reading justifies speech protection as a mechanism for
 *   truth-discovery: false and harmful claims should be countered by more
 *   speech, evidence, and argument rather than suppressed by state authority
 *   or platform gatekeeping. This reading assumes listener rationality,
 *   institutional science's self-correction, and the superiority of
 *   distributed correction over centralized prevention. It has dominated
 *   First Amendment doctrine but faces mounting pressure from harm-threshold,
 *   dignity, and democratic-participation readings that argue the marketplace
 *   assumption breaks down when false claims spread faster than correction,
 *   exploit information asymmetries, or subordinate target groups. The
 *   constraint story treats the marketplace reading as ONE interpretation of
 *   the contested speech-protection kernel, distinct from but structurally
 *   related to its sibling readings.
 *
 * KEY AGENTS:
 *   - truth_seeking_public: Coordinate beneficiary; gains access to uncensored information; depends on the marketplace assumption that rationality and counter-speech will settle truth.
 *   - targets_of_false_damaging_speech: Payer; must endure false claims and mount counter-speech rather than seek prior removal; their recourse is limited to narrow exceptions (defamation, incitement).
 *   - institutional_science: Beneficiary; protects freedom to publish, test hypotheses, and correct errors through evidence; relies on open peer debate and error correction rather than preemptive suppression.
 *   - state_licensing_authorities: Excluded; the reading rejects their gatekeeping function as distorting truth-discovery; they are barred from pre-publication content review except in narrow post-hoc harm categories.
 *   - epistemic_commons_users: Powerless beneficiary/payer; depend on the marketplace for information access but lack resources to evaluate competing claims; structurally locked into the marketplace assumption by their identity as speech participants.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__marketplace_reading, 0.58).
domain_priors:suppression_score(speech_protection_kernel__marketplace_reading, 0.62).
domain_priors:theater_ratio(speech_protection_kernel__marketplace_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__marketplace_reading, rope).
narrative_ontology:human_readable(speech_protection_kernel__marketplace_reading, "Speech Protection as Truth-Discovery Coordination (Marketplace Reading)").
narrative_ontology:topic_domain(speech_protection_kernel__marketplace_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(speech_protection_kernel__marketplace_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__marketplace_reading, '2295e31e-a84e-433c-ade3-47078825ef9e').
narrative_ontology:cs_kernel_codification('2295e31e-a84e-433c-ade3-47078825ef9e', formalized).
narrative_ontology:cs_authority_grounding('2295e31e-a84e-433c-ade3-47078825ef9e', lineage).
narrative_ontology:cs_interpretation_layer_present('2295e31e-a84e-433c-ade3-47078825ef9e').
narrative_ontology:cs_reading_relation('2295e31e-a84e-433c-ade3-47078825ef9e', speech_protection_kernel__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('2295e31e-a84e-433c-ade3-47078825ef9e', speech_protection_kernel__harm_threshold_reading, forecloses).
narrative_ontology:cs_reading_relation('2295e31e-a84e-433c-ade3-47078825ef9e', speech_protection_kernel__dignity_reading, influences).
narrative_ontology:cs_reading_relation('2295e31e-a84e-433c-ade3-47078825ef9e', speech_protection_kernel__democratic_participation_reading, coexists_with).
narrative_ontology:cs_axiom('2295e31e-a84e-433c-ade3-47078825ef9e', foundational, listener_rationality_sufficiency).
narrative_ontology:cs_axiom_status(listener_rationality_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('2295e31e-a84e-433c-ade3-47078825ef9e', listener_rationality_sufficiency, empirically_contingent).
narrative_ontology:cs_axiom('2295e31e-a84e-433c-ade3-47078825ef9e', foundational, decentralized_correction_superiority).
narrative_ontology:cs_axiom_status(decentralized_correction_superiority, holdable).
narrative_ontology:cs_axiom_grounding('2295e31e-a84e-433c-ade3-47078825ef9e', decentralized_correction_superiority, empirically_contingent).
narrative_ontology:cs_reference_frame('2295e31e-a84e-433c-ade3-47078825ef9e', prior_restraint_rejection).
narrative_ontology:cs_drift_state('2295e31e-a84e-433c-ade3-47078825ef9e', digital_communication_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2295e31e-a84e-433c-ade3-47078825ef9e', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__marketplace_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__marketplace_reading, truth_seeking_public).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__marketplace_reading, institutional_science).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__marketplace_reading, competing_truth_claimants).
narrative_ontology:constraint_victim(speech_protection_kernel__marketplace_reading, targets_of_false_damaging_speech).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__marketplace_reading, epistemic_commons_users).
narrative_ontology:constraint_victim(speech_protection_kernel__marketplace_reading, epistemic_commons_users).
narrative_ontology:constraint_vindicates(speech_protection_kernel__marketplace_reading, epistemic_market_self_correction).
narrative_ontology:constraint_vindicates(speech_protection_kernel__marketplace_reading, listener_rationality_assumption).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gains access to a wide range of claims and counter-claims without prior state curation; expected to evaluate, test, and settle truth through discourse. Benefits from the coordination function (no speech licensing authority blocks access to information) but bears the cost of exposure to false and harmful claims until correction emerges.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, truth_seeking_public, beneficiary,
    organized, generational, mobile, national).

% Suffer reputational, professional, or psychological harm from false or damaging claims about them. The marketplace reading denies them proactive removal of damaging speech, instead requiring them to mount counter-speech to correct the record. Their recourse is limited to speech-in-response or eventual legal remedy under narrow categories (defamation, incitement) that the marketplace reading treats as exceptions.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, targets_of_false_damaging_speech, payer,
    moderate, biographical, constrained, national).

% Benefits from freedom to publish findings, conduct open peer debate, and correct errors through additional evidence and counter-argument. The constraint protects the institutional conditions for hypothesis testing and error correction. Relies on the assumption that false or misleading claims in the scientific marketplace will be defeated by evidence and superior argument rather than regulatory suppression.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, institutional_science, beneficiary,
    institutional, generational, mobile, global).

% Can advance claims, hypotheses, and frameworks without pre-clearance; expected to defend them through argument and evidence. Benefit from the constraint's protection against content-based filtering, but depend on the marketplace assumption that superior claims will eventually prevail. Face the cost of reputational contest and the burden of continuous demonstration.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, competing_truth_claimants, beneficiary,
    moderate, biographical, mobile, global).

% Are structurally excluded from adjudicating which claims may be published. The marketplace reading rejects speech licensing, prior restraint, and content-based filtering by state authorities as distorting the truth-discovery process itself. They retain authority only in narrow post-hoc categories (incitement, defamation) rather than prospective gatekeeping. Their exclusion is built into the constraint's definition.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, state_licensing_authorities, excluded,
    institutional, generational, trapped, national).

% Can disseminate false claims without prior censorship but face correction through counter-speech and, if sufficiently coordinated and damaging, potential legal liability under narrow harm-based exceptions. The marketplace reading does NOT exclude them by design; rather, it tolerates their speech as the cost of protecting the truth-discovery process, betting that counter-speech and evidence will defeat them.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, disinformation_operators, excluded,
    powerful, biographical, trapped, global).

% Adjudicate post-hoc harm claims (defamation, incitement, false advertising) but are barred from pre-publication content review. They monitor whether the marketplace assumption holds — whether truth-discovery and correction occur fast enough to prevent irreparable harm. Their decisions shape the boundaries of the exceptions that carve into the otherwise-absolute protection.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, civil_courts, observer,
    institutional, generational, analytical, national).

% Depend on the marketplace to sort true from false claims but lack the institutional resources, education, or time to evaluate competing claims independently. They are structural beneficiaries of the coordination (access to unsuppressed speech) but vulnerable to being flooded with false information that exploits their information asymmetry. Their identity as epistemic participants is locked into the marketplace assumption — they cannot opt out of having to evaluate claims.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, epistemic_commons_users, beneficiary,
    powerless, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_kernel__marketplace_reading, epistemic_commons_users, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_kernel__marketplace_reading, institutional_science).
narrative_ontology:fixing_cost_class(speech_protection_kernel__marketplace_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of allocating scarce truth-discovery resources: rather than empowering a central licensing authority to pre-judge which claims are publishable, distributes the burden of evaluation and correction across all speech participants. Assumes that truth emerges from open competition of claims, that listeners are rational enough to prefer true over false claims when evidence is available, and that institutional science and counter-speech will outrun deception over time.
% TRANSFER_FUNCTION: Transfers the cost of evaluating speech from a gatekeeper (state authority, institutional monopoly) to the distributed population of listeners and competing speakers. Also transfers the burden of harm-remediation from prevention (restricting false speech in advance) to correction (mounting counter-speech, accumulating evidence, and mounting legal action after the fact).
% ABSENT_VOICES: State licensing authorities are excluded from the conversation about what counts as publishable; they would argue that certain false claims (medical misinformation, incitement to violence, election interference) do measurable harm fast enough that counter-speech cannot correct them in time. Targets of false speech campaigns would argue that the marketplace assumption breaks down when false claims overwhelm correction capacity and exploit information asymmetries. Epistemic commons users (those without institutional resources to evaluate competing claims) are not absent but under-resourced in the marketplace frame.
% DISAPPEARANCE_RATIONALE: If speech protection under the marketplace reading disappeared overnight, the institutional landscape would reorganize: state licensing authorities would erect pre-publication gates; institutional science would face content review; competing truth-claimants would require approval before publication. The epistemic commons would shift from one in which false claims circulate and are corrected to one in which a gatekeeper decides what reaches the public. The assumption that distributed correction is faster and less distorting than centralized prevention would cease to structure legal doctrine.
% FOUNDING_PROBLEM: Prior restraint by state authorities and religious/political licensing created an epistemic commons where truth could not emerge because competing claims were suppressed before reaching the public. The founding problem is the recognition that gatekeeping by authorities corrupts the truth-discovery process itself — a state censor will always suppress claims that threaten the ruling regime, regardless of their truth value.
% FOUNDING_PROBLEM_CORROBORATION: The marketplace reading's proponents (First Amendment scholars, institutional science, press organizations) attest the founding problem remains live — any centralized authority over publication will bias what counts as truth. Critics from outside this reading (harm-threshold advocates, dignity theorists, democratic-participation scholars) attest the founding problem has been partially solved by technology and institutional maturation, and that the costs of the marketplace assumption (unchecked disinformation, harassment of targets) now exceed the benefits. No external consensus exists; the contest is between established readings of the kernel.
narrative_ontology:disappearance_verdict(speech_protection_kernel__marketplace_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__marketplace_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__marketplace_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(speech_protection_kernel__marketplace_reading, 'none', 1).

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
 *   Extractiveness is moderate-high (0.58) because the constraint imposes costs on targets of false speech and on epistemic commons users who bear the risk of information floods. The measurement trajectory shows rising extractiveness over time: as digital platforms amplify false claims and coordination of disinformation improves, the gap between false-claim spread and correction-speech accumulation widens, revealing the constraint's extractive edge. Suppression is higher (0.62) than extractiveness because the constraint requires active enforcement against alternative readings — legal doctrine must suppress harm-threshold reasoning, dignity-based restrictions, and licensing authorities' gatekeeping impulses to maintain the marketplace frame. Theater ratio is moderate (0.41) because the constraint legitimizes itself through an epistemic narrative (truth emerges from open competition) while performing an institutional function (protecting specific truth-claimants and power structures that benefit from unsuppressed speech). The three metrics sit on one shared time grid; all are authored at every examined time point. The trajectory plateaus after t=25, reflecting the assumption that the marketplace reading has stabilized in doctrine even as its costs accumulate.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (truth-seeking public, institutional science) experience the constraint as genuine coordination — the conditions necessary for truth-discovery. The payer seats (targets, epistemic commons users) experience it as enforced asymmetry: they are forbidden from seeking removal of damaging claims and must instead shoulder the burden of continuous correction. State authorities experience themselves as excluded from a function they believe they perform better (gatekeeping harmful content). The engine computes these perspectival gaps from the power atoms, exit options, and beneficiary/victim declarations — a moderate-power payer with constrained exit (targets of false speech) will compute a higher d than an organized beneficiary with mobile exit (truth-seeking public). The divergence is structural, not a measurement error.
 *
 * DIRECTIONALITY LOGIC:
 *   The truth-seeking public and institutional science are directional beneficiaries: they gain the coordination benefit (unsuppressed speech, distributed truth-discovery) without bearing extraction. Targets of false speech are directional payers: they bear the cost of exposure to uncorrected claims and have constrained exit (they cannot avoid being discussed or defamed). Epistemic commons users are dual-positioned: they benefit from access to unsuppressed information but pay the cost of information asymmetry; their identity as speakers/listeners is locked into the marketplace frame (identity_locked exit). State authorities are not payers or beneficiaries; they are excluded from the coordination function itself — their exclusion is built into the constraint's structural definition. This directionality derives from the beneficiary/victim declarations and the exit-options spectrum, not from authored directionality scores.
 *
 * MANDATROPHY ANALYSIS:
 *   The marketplace reading avoids mandatrophy detection through two mechanisms: (1) its founding problem (prior restraint by state authorities distorts truth-discovery) remains contested, so it has not yet crossed into deadness; (2) institutional science, press organizations, and First Amendment scholars continue to defend the marketplace assumption as live and necessary. However, the omega variables documenting marketplace-assumption breakdown and correction-lag effects capture the risk zone. A future corpus measurement showing suppression_requirement converging to 1.0 while theater_ratio stays flat (enforcement hardening while the epistemic narrative grows perfunctory) would signal mandatrophy development. The story is not yet mandatrophic, but the rising extractiveness trajectory (0.35 → 0.58 over the interval) marks a warning trajectory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    marketplace_assumption_breakdown,
    'Does the marketplace assumption hold — that truth emerges from open competition of claims and that counter-speech and evidence will defeat false claims faster than false claims spread?',
    'Empirical measurement of spread-to-correction lag in digital communication; modeling of disinformation cascades; historical analysis of cases where false claims persisted despite abundant counter-evidence (QAnon, flat earth, vaccine hesitancy); institutional audit of whether institutional science''s correction mechanisms keep pace with false claims in high-volume domains.',
    'If the assumption breaks (correction lag exceeds spread speed consistently), the reading''s justification for rejecting content-based restrictions collapses, and harm-threshold and dignity readings become structurally more defensible. If the assumption holds, the marketplace frame is reinforced. The empirical status of this axiom (listener_rationality_sufficiency + decentralized_correction_superiority) gates whether the reading remains live or becomes functionally obsolete.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marketplace_assumption_breakdown, empirical, 'Core epistemic premise of the marketplace reading: whether distributed truth-discovery outperforms centralized prevention in practice.').

omega_variable(
    listener_rationality_distribution,
    'What proportion of the public has access to the information, time, and cognitive resources necessary to evaluate competing truth-claims as the marketplace reading assumes?',
    'Population survey of epistemic literacy; measurement of information access and evaluation capacity; demographic breakdown of who benefits from and who is harmed by the marketplace assumption.',
    'If listener rationality is rare or unequally distributed, the reading''s justification shifts: it becomes a protection for credentialed speakers and institutional science at the expense of epistemic commons users (those without resources to evaluate claims). The reading would then be revealed as imposing extractive costs on the information-poor. If rationality is broadly distributed, the reading''s coordination narrative holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(listener_rationality_distribution, empirical, 'Whether the marketplace reading''s assumption about listener rationality and evaluation capacity is descriptively accurate.').

omega_variable(
    harm_threshold_boundary,
    'Where should the boundary between marketplace protection and harm-based restriction be drawn? Is demonstrable harm to identifiable targets grounds for preventive speech restriction, or only post-hoc remedy?',
    'Comparative law analysis of jurisdictions that have moved toward harm-threshold restrictions (Australia, Canada, UK) and measured outcomes; case study of specific categories (incitement, defamation, harassment, medical misinformation) where courts have grappled with the boundary; empirical measurement of harm accumulation rate versus correction rate in high-damage domains.',
    'If the boundary moves toward harm-based restrictions, the marketplace reading forecloses and the harm_threshold reading becomes dominant. If the boundary holds at post-hoc remedy only, the marketplace reading maintains dominance. This omega documents the highest-stakes structural contest between this reading and its harm_threshold sibling.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(harm_threshold_boundary, empirical, 'The empirically contested boundary between marketplace protection and harm-prevention exception.').

omega_variable(
    reading_kernel_vs_sibling_readings,
    'Which reading of the speech_protection_kernel is structurally true — marketplace, absolutist, harm_threshold, dignity, or democratic_participation?',
    'This omega cannot be resolved empirically because it is fundamentally a reading contest. The resolution mechanism is institutional drift: over time, one reading becomes dominant in case law, legislation, and doctrine. Currently (2026), the marketplace reading remains institutionally dominant in US doctrine but faces sustained pressure from harm_threshold (EU, UK, Australia) and dignity readings (feminist jurisprudence) and democratic_participation readings (newer scholarship). The reading will be settled not by empirical discovery but by which faction''s institutional power increases or decreases.',
    'If a sibling reading becomes dominant, this constraint''s type classification would be superseded by the new reading''s constraint story. The entire analytical frame (beneficiaries, victims, extraction mechanism) would shift. This is the deepest uncertainty: which kernel reading becomes hegemonic doctrine.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_vs_sibling_readings, conceptual, 'The irreducible contest between structurally incompatible readings of the speech-protection kernel.').

omega_variable(
    suppression_internalization,
    'Is the suppression measured in this reading (0.62) structural (external barriers to content-based restriction, legal doctrine that blocks authorities from gatekeeping) or internalized (speakers and platforms self-censor to avoid reputational/legal risk, even when legal doctrine permits speech)?',
    'Comparative analysis of self-censorship before and after formal speech-protection doctrine; interviews with speakers and platform operators about their decision-making; measurement of speech patterns in jurisdictions with and without formal protections.',
    'If suppression is internalized, the constraint''s effective suppression may be higher than the structural measure suggests — speakers carry the suppression with them even in formally protected spaces. This would raise the constraint''s extractiveness: targets of false speech face suppression that occurs through distributed self-censorship rather than central authority, making the extraction harder to remedy. If suppression is structural, the count of 0.62 accurately reflects the force required to maintain the reading against alternative readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Whether suppression of alternative readings is structural or internalized (or both).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__marketplace_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_kernel__marketplace_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(spee_tr_t0, observed).
narrative_ontology:measurement(spee_tr_t5, speech_protection_kernel__marketplace_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement_basis(spee_tr_t5, observed).
narrative_ontology:measurement(spee_tr_t10, speech_protection_kernel__marketplace_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement_basis(spee_tr_t10, observed).
narrative_ontology:measurement(spee_tr_t15, speech_protection_kernel__marketplace_reading, theater_ratio, 15, 0.33).
narrative_ontology:measurement_basis(spee_tr_t15, observed).
narrative_ontology:measurement(spee_tr_t20, speech_protection_kernel__marketplace_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement_basis(spee_tr_t20, observed).
narrative_ontology:measurement(spee_tr_t25, speech_protection_kernel__marketplace_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement_basis(spee_tr_t25, projected).
narrative_ontology:measurement(spee_tr_t30, speech_protection_kernel__marketplace_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(spee_tr_t30, projected).
narrative_ontology:measurement(spee_tr_t40, speech_protection_kernel__marketplace_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(spee_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_kernel__marketplace_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(spee_be_t0, observed).
narrative_ontology:measurement(spee_be_t5, speech_protection_kernel__marketplace_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement_basis(spee_be_t5, observed).
narrative_ontology:measurement(spee_be_t10, speech_protection_kernel__marketplace_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement_basis(spee_be_t10, observed).
narrative_ontology:measurement(spee_be_t15, speech_protection_kernel__marketplace_reading, base_extractiveness, 15, 0.52).
narrative_ontology:measurement_basis(spee_be_t15, observed).
narrative_ontology:measurement(spee_be_t20, speech_protection_kernel__marketplace_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement_basis(spee_be_t20, observed).
narrative_ontology:measurement(spee_be_t25, speech_protection_kernel__marketplace_reading, base_extractiveness, 25, 0.57).
narrative_ontology:measurement_basis(spee_be_t25, projected).
narrative_ontology:measurement(spee_be_t30, speech_protection_kernel__marketplace_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement_basis(spee_be_t30, projected).
narrative_ontology:measurement(spee_be_t40, speech_protection_kernel__marketplace_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement_basis(spee_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_kernel__marketplace_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(spee_su_t0, observed).
narrative_ontology:measurement(spee_su_t5, speech_protection_kernel__marketplace_reading, suppression_requirement, 5, 0.45).
narrative_ontology:measurement_basis(spee_su_t5, observed).
narrative_ontology:measurement(spee_su_t10, speech_protection_kernel__marketplace_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement_basis(spee_su_t10, observed).
narrative_ontology:measurement(spee_su_t15, speech_protection_kernel__marketplace_reading, suppression_requirement, 15, 0.57).
narrative_ontology:measurement_basis(spee_su_t15, observed).
narrative_ontology:measurement(spee_su_t20, speech_protection_kernel__marketplace_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement_basis(spee_su_t20, observed).
narrative_ontology:measurement(spee_su_t25, speech_protection_kernel__marketplace_reading, suppression_requirement, 25, 0.61).
narrative_ontology:measurement_basis(spee_su_t25, projected).
narrative_ontology:measurement(spee_su_t30, speech_protection_kernel__marketplace_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement_basis(spee_su_t30, projected).
narrative_ontology:measurement(spee_su_t40, speech_protection_kernel__marketplace_reading, suppression_requirement, 40, 0.62).
narrative_ontology:measurement_basis(spee_su_t40, projected).


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
% The marketplace_reading is one of five structurally distinct readings of the contested speech_protection_kernel. All five constraint stories share the kernel (speech protection as a constitutional commitment) but differ in their ε values, beneficiary/victim structures, and classification. This reading justifies protection by collective epistemic benefit (truth-discovery); the absolutist reading justifies it by individual autonomy; the harm_threshold reading conditions it on demonstrable harm; the dignity reading conditions it on not functioning as subordination; the democratic_participation reading centers political speech for self-governance. The readings influence each other: the marketplace reading creates institutional pressure on harm_threshold and dignity readings by centering epistemic function, but the harm_threshold reading forecloses the marketplace reading's core premise if empirical breakdowns of the marketplace assumption accumulate. The five stories form a constraint family linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_protection_kernel__marketplace_reading, powerless, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
