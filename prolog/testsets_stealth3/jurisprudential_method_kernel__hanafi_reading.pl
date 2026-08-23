% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__hanafi_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jurisprudential_method_kernel__hanafi_reading, []).

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
 *   constraint_id: jurisprudential_method_kernel__hanafi_reading
 *   human_readable: Hanafi Analogical-Derivation Regime (Qiyas/Istihsan Gate)
 *   domain: religious-legal/institutional-history
 *
 * SUMMARY:
 *   This file instantiates the hanafi_reading of the
 *   jurisprudential_method_kernel: binding law derives from Qur'an and Hadith
 *   filtered through disciplined analogical reasoning (qiyas) and juristic
 *   preference (istihsan), with reason a sanctioned instrument for extending
 *   divine intent to cases the texts do not address. Structurally it is a
 *   hybrid: a genuine coordination achievement (finite revelation meets
 *   endless novelty, and someone must bridge the gap or the law freezes)
 *   fused with an asymmetric transfer (a class of trained reasoners issues
 *   rulings on human inference that arrive wearing divine authority, and
 *   compliance is collected accordingly). The claim/metric independence rule
 *   is honored deliberately: claimed_type records the hybrid structure I
 *   believe is true; the metrics record how the regime actually operates,
 *   including its entrenchment and its drift toward imitation. The sibling
 *   readings (maliki, shafii, hanbali) are separate constraints with their
 *   own structures, linked through the network, not folded in here.
 *
 * KEY AGENTS:
 *   - - rationalist_trained_jurists: Agenda-setter and principal beneficiary (institutional / identity_locked) — performs the analogical derivations, staffs the courts and colleges, collects the interpretive authority
 *   - - imperial_state_administrators: Secondary beneficiary (institutional / mobile) — adopts the school for administrative flexibility and offloads the divine-warrant burden onto the jurists
 *   - - lay_believers_subject_to_fiqh: Primary target (powerless / constrained) — bears compliance duties for novel-case rulings they cannot trace to text
 *   - - textualist_hadith_scholars: Displaced rival supplier of legal authority (organized / identity_locked) — pays in lost jurisdiction and marginalization
 *   - - zahiri_literalists: Excluded rival (organized / trapped) — the anti-analogy position without an institutional seat
 *   - - historians_of_islamic_law: Analytical observer (analytical / analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__hanafi_reading, 0.7).
domain_priors:suppression_score(jurisprudential_method_kernel__hanafi_reading, 0.55).
domain_priors:theater_ratio(jurisprudential_method_kernel__hanafi_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__hanafi_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__hanafi_reading, "Hanafi Analogical-Derivation Regime (Qiyas/Istihsan Gate)").
narrative_ontology:topic_domain(jurisprudential_method_kernel__hanafi_reading, "religious-legal/institutional-history").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__hanafi_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__hanafi_reading, '55c5896a-e7d8-4670-b4a5-0155af06b2bc').
narrative_ontology:cs_kernel_codification('55c5896a-e7d8-4670-b4a5-0155af06b2bc', fixed_text).
narrative_ontology:cs_authority_grounding('55c5896a-e7d8-4670-b4a5-0155af06b2bc', lineage).
narrative_ontology:cs_interpretation_layer_present('55c5896a-e7d8-4670-b4a5-0155af06b2bc').
narrative_ontology:cs_reading_relation('55c5896a-e7d8-4670-b4a5-0155af06b2bc', jurisprudential_method_kernel__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('55c5896a-e7d8-4670-b4a5-0155af06b2bc', jurisprudential_method_kernel__shafii_reading, influences).
narrative_ontology:cs_reading_relation('55c5896a-e7d8-4670-b4a5-0155af06b2bc', jurisprudential_method_kernel__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('55c5896a-e7d8-4670-b4a5-0155af06b2bc', foundational, reason_legitimately_extends_divine_intent).
narrative_ontology:cs_axiom_status(reason_legitimately_extends_divine_intent, holdable).
narrative_ontology:cs_axiom_grounding('55c5896a-e7d8-4670-b4a5-0155af06b2bc', reason_legitimately_extends_divine_intent, theological).
narrative_ontology:cs_axiom('55c5896a-e7d8-4670-b4a5-0155af06b2bc', foundational, juristic_preference_may_override_strict_analogy_for_equity).
narrative_ontology:cs_axiom_status(juristic_preference_may_override_strict_analogy_for_equity, holdable).
narrative_ontology:cs_axiom_grounding('55c5896a-e7d8-4670-b4a5-0155af06b2bc', juristic_preference_may_override_strict_analogy_for_equity, instrumental).
narrative_ontology:cs_reference_frame('55c5896a-e7d8-4670-b4a5-0155af06b2bc', revelation_supplemented_by_disciplined_reason).
narrative_ontology:cs_drift_state('55c5896a-e7d8-4670-b4a5-0155af06b2bc', contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('55c5896a-e7d8-4670-b4a5-0155af06b2bc', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanafi_reading, rationalist_trained_jurists).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanafi_reading, imperial_state_administrators).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanafi_reading, lay_believers_subject_to_fiqh).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanafi_reading, textualist_hadith_scholars).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__hanafi_reading, illah_ratio_legis_discoverability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Complete a multi-year curriculum in the roots of law, master the canons of analogy and juristic preference, and staff the courts, teaching colleges, and advisory offices of the states that adopt this school. Their standing, income, appointments, and endowed posts all presuppose that disciplined reasoning is a licensed route from the revealed texts to binding rulings. Leaving the school means abandoning their entire formative identity and professional world; within it, they decide how every new question gets answered.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, rationalist_trained_jurists, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__hanafi_reading, rationalist_trained_jurists, beneficiary).

% Adopt this school as official law for the courts because its reasoned flexibility lets judges resolve the novel commercial and administrative questions of a growing empire without waiting for new revelation or universal agreement. They obtain a workable, adaptable legal order while the jurist class carries the burden of claiming divine warrant for its outputs. Switching to a rival school is administratively feasible and has happened more than once.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, imperial_state_administrators, beneficiary,
    institutional, generational, mobile, continental).

% Pray, trade, marry, inherit, and litigate under rulings whose content, in novel matters, comes from juristic analogy and preference rather than from any verse or report they could check themselves. Compliance is demanded as obedience to God, so declining a ruling reads as impiety rather than disagreement with a lawyer. Most cannot audit the derivation chain; their practical recourse is consulting a jurist of a different school where one is available.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, lay_believers_subject_to_fiqh, payer,
    powerless, biographical, constrained, continental).

% Compete to supply legal answers directly from the transmitted reports and companion precedent, treating elaborate analogical machinery as an unauthorized human addition to revelation. Wherever this school holds the judgeships and the teaching chairs, their route to authority narrows: fewer posts, smaller circles, and public accusations that their rivals legislate by whim. Their counter-position is that reports suffice, including weaker ones, before a question is left to human invention.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, textualist_hadith_scholars, payer,
    organized, generational, identity_locked, continental).

% Reject analogy altogether and hold that only explicit texts and unanimous consensus bind. Patronage for this position withdrew early; its institutions shrank to isolated circles and eventually to memory, leaving its holders without a seat in the settled arrangement and little platform from which to object to it.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, zahiri_literalists, excluded,
    organized, generational, trapped, regional).

% Reconstruct how the school formed, spread, and hardened from charter documents, legal opinions, and court records; they take no side and hold no stake in whether the method binds anyone.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, historians_of_islamic_law, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jurisprudential_method_kernel__hanafi_reading, rationalist_trained_jurists).
narrative_ontology:fixing_cost_class(jurisprudential_method_kernel__hanafi_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the novel-case problem for a community committed to divine law: the canonical texts are finite while commercial forms, procedures, and injuries are endless, so a repeatable procedure (analogy anchored in disclosed causes of the rulings, corrected by equitable preference) extends known norms to unknown cases and keeps the law uniform across regions and generations.
% TRANSFER_FUNCTION: Moves adjudicative authority and compliance from lay believers and from the bare texts to the trained jurist class: every novel-case ruling relocates decision power over conduct, and the status of speaking for God, from subjects and scripture to professional reasoners; judicial fees, appointments, and endowed wealth flow toward jurist institutions alongside.
% ABSENT_VOICES: Anti-analogy literalists rejected the method wholesale and lost their patronage, so they stand outside the settlement with no institutional seat; lay subjects have no procedural voice in methodological debates, surfacing objection only through occasional cross-school consultation or popular practice; women governed by family-law determinations had no formal place in the councils that fixed the method.
% DISAPPEARANCE_RATIONALE: If the method vanished overnight, courts would be unable to rule on any matter the texts do not expressly address: either the law freezes into an enormous unresolved caseload, or a rival derivation regime (report-first textualism, Medinan living practice, or a strict tiered hierarchy) takes over the courts, the teaching posts, and the authority to answer new questions. Whoever supplies the replacement inherits the appointments and the income.
% FOUNDING_PROBLEM: After the conquests, Muslim communities met cases with no explicit scriptural ruling — new contracts, new administrative questions, new injuries — and the companions' ad hoc individual judgments produced inconsistencies; a stable, teachable procedure for extending revealed norms was needed.
% FOUNDING_PROBLEM_CORROBORATION: The problem is attested from outside the benefiting set: the textualist rivals concede it exists (their dispute is over the permissible remedy, and their own fallback to companion opinion and weaker reports is itself an extension device); the tiered-hierarchy school's founding treatise frames the same gap as its organizing problem; and modern purposive-method reformers attest that novel cases still outstrip explicit text. No party denies the founding problem — they fight over who may solve it and how.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__hanafi_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__hanafi_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__hanafi_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jurisprudential_method_kernel__hanafi_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jurisprudential_method_kernel__hanafi_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisprudential_method_kernel__hanafi_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jurisprudential_method_kernel__hanafi_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jurisprudential_method_kernel__hanafi_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.70) because the regime's distinctive yield sits in novel cases: where no verse or report speaks, the binding ruling is a product of trained human reasoning, yet it is issued and obeyed as divine law. The gap between the method's own epistemic honesty (its manuals concede these inferences are probable and revisable) and the unconditional compliance demanded of subjects is the load-bearing extraction. Suppression (0.55) is real but bounded: enforcement runs through state-appointed judgeships and gated teaching careers rather than broad persecution, and rival schools remain reachable in many places. Theater rises across the interval (0.40 at end) as originating reasoning gave way to imitation of authoritative summaries — the outward forms of derivation continued long after most jurists stopped originating. Accessibility_collapse is moderate (0.50): once reason is admitted as an extender of the texts, purely textual answers cease to exist for novel questions, but the neighboring schools persist as live alternative regimes. Resistance (0.62) was sustained for centuries by the report-scholar movements, and the outright anti-analogy school survives as a standing reproach. The three temporal series share one grid (points 0, 10, 20, 30, 40, 50) so every metric is authored at every examined time point; suppression_requirement is tracked because this story specifically traces enforcement-capacity growth (progressive state adoption of the school), not merely shifting rents. The trajectories are monotonic entrenchment, not cyclical: no intermittent-reinforcement mechanism is claimed.
 *
 * PERSPECTIVAL GAP:
 *   From the jurist seat the same structure is the faithful completion of revelation: reason doing what the texts invite it to do, with the school's own manuals insisting every inference is fallible and revisable. From the lay seat it is an unappealable pipeline in which human preference arrives wearing divine authority, and refusal looks like impiety. From the textualist seat it is an illegitimate rival that captured the courts. These are computed divergences from one structural dataset, not reconciled views; the authored claim asserts the hybrid structure and leaves each seat's classification to the engine.
 *
 * DIRECTIONALITY LOGIC:
 *   The two declared beneficiary groups anchor the low end of directionality. The jurists collect interpretive authority, appointments, and fees; their identity-lock deepens their investment in the arrangement rather than exposing them to it. The administrators collect an adaptable legal order and retain cheap exit (schools have been swapped by fiat), placing them nearest the beneficiary pole. The two declared victim groups anchor the high end. Lay believers bear compliance duties they cannot audit, with exit limited to occasional cross-school consultation; the textualist scholars bear displacement of their authority claim and are locked in by their own textual identity, placing them near the full-target end among organized agents. The vindicated proposition — that the disclosed causes of divine rulings are discoverable by disciplined inquiry — collects no rents and is listed separately from the beneficiaries, as it should be.
 *
 * MANDATROPHY ANALYSIS:
 *   Reading the regime as pure coordination misses the transfer: a class of trained reasoners converts its own preferences into obligations owed to God, and that conversion is where the gain accrues. Reading it as pure extraction misses the solved problem: finite texts meet endless novel cases, and without a bridge the law freezes. The hybrid verdict keeps both facts in view and prevents the two classic mislabels. On the genealogy question, the founding problem (novelty outstripping explicit text) is still live and corroborated by rival seats, so no mandatrophy resolution is declared. But the theater series marks a partial atrophy of the original function: as imitation replaced origination, the regime drifted toward performing a derivation fewer and fewer practitioners actually conducted. If imitation fully displaced origination while enforcement persisted, the structure would slide toward inertial performance — the temporal record exists to catch that turn if it completes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hanafi_reading_kernel_position,
    'This story is one reading (hanafi_reading) of the jurisprudential_method_kernel among four declared readings; what would adopting a sibling reading change structurally?',
    'Compile and compare the sibling files: maliki_reading reallocates derivational authority to the bearers of Medinan living practice, shafii_reading to report-transmission specialists, hanbali_reading to textual literalists — each changes the beneficiary set, the victim set, and the epsilon profile rather than tuning a parameter of this one.',
    'No sibling reading''s classification carries over to this file; if a sibling became the operative regime, this story''s beneficiaries, extraction profile, and type would describe a different constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hanafi_reading_kernel_position, conceptual, 'Committer structure: this constraint is the hanafi_reading of a four-reading kernel; siblings are separate files.').

omega_variable(
    divine_warrant_attribution,
    'When a novel-case ruling binds as divine law, is the compliance it commands actually tracking discerned divine intent, or juristic preference carrying borrowed warrant?',
    'Historical comparison of compliance behavior where recognized juristic disagreement was open and public: if subjects followed school affiliation and the local judgeship rather than any felt textual anchor, the preference component dominated; where rulings converged across schools on parallel reasoning, discernment carried more weight.',
    'If borrowed warrant dominates, the measured extractiveness understates the arrangement, because what moves is authority itself rather than a discovery about the texts; if discernment dominates, part of the compliance load is the genuine price of the coordination and epsilon should be read lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_warrant_attribution, empirical, 'Attribution ambiguity between discerned divine intent and preference dressed in divine authority.').

omega_variable(
    istihsan_interest_alignment,
    'How much of the recorded variation in juristic preference tracks principled hardship-avoidance versus the documented interests of patrons and ruling households in the school''s heartland?',
    'Systematic coding of preference-driven departures from strict analogy in classical legal opinions, checked against the contemporaneous fiscal and dynastic interests of the patrons who appointed and paid the jurists.',
    'A high interest-aligned share pushes the arrangement toward pure extraction with the equity language as cover; a principled-equity majority supports the hybrid verdict and keeps the coordination function load-bearing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(istihsan_interest_alignment, empirical, 'Whether juristic preference operated as equity or as patron service.').

omega_variable(
    taqlid_contingency,
    'Was the drift from originating reasoning to imitation of authoritative summaries inherent to the method itself, or a contingent effect of patronage collapse, credentialing, and career incentives?',
    'Compare regions and periods where independent reasoning retained patronage, licensing, and career paths against those where summary-and-commentary took over; if origination revived wherever incentives returned, the drift was contingent.',
    'If inherent, the regime trends toward inertial performance and the theater series should keep climbing toward piton territory; if contingent, rope-like operation is recoverable and the climb is reversible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(taqlid_contingency, empirical, 'Endogeneity of the atrophy of originating reasoning.').

omega_variable(
    cross_school_exit_reality,
    'Does consulting another school''s jurist constitute material exit that caps the burden on subjects, or nominal choice inside a shared frame in which every school issues similarly untraceable novel-case rulings?',
    'Track outcome distributions for petitioners who switched schools on contested questions: did the rulings materially differ, and did the switched-to answer hold in practice?',
    'If exit is nominal, suppression and extractiveness understate the subject-seat burden, since the apparent alternative reproduces the same structure; if exit is real, inter-school rivalry operates as a genuine check and supports the hybrid verdict.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cross_school_exit_reality, empirical, 'Whether school pluralism constitutes real exit for subjects or decoration on a shared frame.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__hanafi_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(juri_tr_t0, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(juri_tr_t0, observed).
narrative_ontology:measurement(juri_tr_t10, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement_basis(juri_tr_t10, observed).
narrative_ontology:measurement(juri_tr_t20, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 20, 0.23).
narrative_ontology:measurement_basis(juri_tr_t20, observed).
narrative_ontology:measurement(juri_tr_t30, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 30, 0.29).
narrative_ontology:measurement_basis(juri_tr_t30, observed).
narrative_ontology:measurement(juri_tr_t40, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 40, 0.35).
narrative_ontology:measurement_basis(juri_tr_t40, observed).
narrative_ontology:measurement(juri_tr_t50, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 50, 0.4).
narrative_ontology:measurement_basis(juri_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(juri_be_t0, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(juri_be_t0, observed).
narrative_ontology:measurement(juri_be_t10, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(juri_be_t10, observed).
narrative_ontology:measurement(juri_be_t20, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement_basis(juri_be_t20, observed).
narrative_ontology:measurement(juri_be_t30, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement_basis(juri_be_t30, observed).
narrative_ontology:measurement(juri_be_t40, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 40, 0.67).
narrative_ontology:measurement_basis(juri_be_t40, observed).
narrative_ontology:measurement(juri_be_t50, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 50, 0.7).
narrative_ontology:measurement_basis(juri_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(juri_su_t0, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(juri_su_t0, observed).
narrative_ontology:measurement(juri_su_t10, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 10, 0.36).
narrative_ontology:measurement_basis(juri_su_t10, observed).
narrative_ontology:measurement(juri_su_t20, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 20, 0.41).
narrative_ontology:measurement_basis(juri_su_t20, observed).
narrative_ontology:measurement(juri_su_t30, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 30, 0.46).
narrative_ontology:measurement_basis(juri_su_t30, observed).
narrative_ontology:measurement(juri_su_t40, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 40, 0.51).
narrative_ontology:measurement_basis(juri_su_t40, observed).
narrative_ontology:measurement(juri_su_t50, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 50, 0.55).
narrative_ontology:measurement_basis(juri_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__hanafi_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanafi_reading, maliki_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanafi_reading, shafii_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanafi_reading, hanbali_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Islamic legal methodology' covers four structurally distinct derivation regimes, decomposed here per the epsilon-invariance principle: this hanafi reading (reason-filtered extension), plus the maliki reading (living Medinan practice as a source), the shafii reading (strict four-tier hierarchy arbitrated by report transmission), and the hanbali reading (literal text and companion opinion, analogical tools rejected as innovation). Each has its own epsilon, beneficiary structure, and failure modes; they are joined as one constraint family through affects_constraints edges rather than forced into a single story with a measurement dial. Documented historical influence runs from this reading toward the shafii reading — the tiered hierarchy was built partly as a discipline upon Hanafi latitude — while the maliki reading developed as a parallel accommodation of non-textual sources.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
