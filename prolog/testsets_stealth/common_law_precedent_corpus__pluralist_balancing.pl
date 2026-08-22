% ============================================================================
% CONSTRAINT STORY: common_law_precedent_corpus__pluralist_balancing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_law_precedent_corpus__pluralist_balancing, []).

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
 *   constraint_id: common_law_precedent_corpus__pluralist_balancing
 *   human_readable: Common-Law Precedent Corpus — Pluralist Balancing Reading
 *   domain: legal/jurisprudential
 *
 * SUMMARY:
 *   In common-law systems the operative rule is not that precedent binds
 *   uniformly but that its force is calibrated: constitutional holdings weigh
 *   differently from commercial ones, recent from aged, majority from
 *   plurality, and the calibration happens case-by-case under announced
 *   factors (workability, reliance, doctrinal coherence, changed
 *   understanding). The standing arrangement under contest — the precedent
 *   corpus as actually administered through this contextual weighing — is
 *   what this story measures, assessed by this reading's own lights; the
 *   reading's endorsed ideal is not the referent. This file instantiates ONLY
 *   the pluralist_balancing reading of the common_law_precedent_corpus
 *   kernel; the strict and evolutionary readings are separate constraints in
 *   separate files, linked through the network edges. The claim/metric
 *   independence rule applies in full: the claimed type states what this
 *   reading believes is structurally true of the arrangement it endorses, the
 *   metrics state what is descriptively true of its operation, and any
 *   computed divergence is the datum. KEY AGENTS (by structural
 *   relationship): - apex_court: Agenda setter
 *   (institutional/identity_locked) — administers the calibration, collects
 *   interpretive supremacy - intermediate_appellate_courts: Dual-positioned
 *   beneficiary-payer (institutional/constrained) — local discretion
 *   purchased with reversal exposure - trial_judges: Payer
 *   (institutional/constrained) — decides under unsettled weights, bears
 *   reversal - sophisticated_repeat_litigants: Beneficiary
 *   (powerful/constrained) — hedges and purchases doctrinal movement -
 *   one_shot_litigants: Primary target (powerless/trapped) — absorbs
 *   unpredictability in full - reliance_interest_holders: Target
 *   (moderate/trapped) — completed plans bear recalibration losses -
 *   deterred_claimants: Excluded voice (powerless/trapped) — priced out
 *   before filing - legal_academy: Analytical observer (organized/analytical)
 *   — codes outcomes, tests factor sincerity
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_law_precedent_corpus__pluralist_balancing, 0.58).
domain_priors:suppression_score(common_law_precedent_corpus__pluralist_balancing, 0.62).
domain_priors:theater_ratio(common_law_precedent_corpus__pluralist_balancing, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, extractiveness, 0.58).
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, resistance, 0.57).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_law_precedent_corpus__pluralist_balancing, tangled_rope).
narrative_ontology:human_readable(common_law_precedent_corpus__pluralist_balancing, "Common-Law Precedent Corpus — Pluralist Balancing Reading").
narrative_ontology:topic_domain(common_law_precedent_corpus__pluralist_balancing, "legal/jurisprudential").

domain_priors:requires_active_enforcement(common_law_precedent_corpus__pluralist_balancing).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_law_precedent_corpus__pluralist_balancing, '7de80b99-af65-4960-a2e2-f90ff4358f9c').
narrative_ontology:cs_kernel_codification('7de80b99-af65-4960-a2e2-f90ff4358f9c', formalized).
narrative_ontology:cs_authority_grounding('7de80b99-af65-4960-a2e2-f90ff4358f9c', lineage).
narrative_ontology:cs_interpretation_layer_present('7de80b99-af65-4960-a2e2-f90ff4358f9c').
narrative_ontology:cs_reading_relation('7de80b99-af65-4960-a2e2-f90ff4358f9c', common_law_precedent_corpus__strict_stare_decisis, coexists_with).
narrative_ontology:cs_reading_relation('7de80b99-af65-4960-a2e2-f90ff4358f9c', common_law_precedent_corpus__evolutionary_framework, coexists_with).
narrative_ontology:cs_axiom('7de80b99-af65-4960-a2e2-f90ff4358f9c', foundational, precedent_weight_is_domain_relative).
narrative_ontology:cs_axiom_status(precedent_weight_is_domain_relative, holdable).
narrative_ontology:cs_axiom_grounding('7de80b99-af65-4960-a2e2-f90ff4358f9c', precedent_weight_is_domain_relative, instrumental).
narrative_ontology:cs_axiom('7de80b99-af65-4960-a2e2-f90ff4358f9c', foundational, case_by_case_calibration_over_fixed_rules).
narrative_ontology:cs_axiom_status(case_by_case_calibration_over_fixed_rules, holdable).
narrative_ontology:cs_axiom_grounding('7de80b99-af65-4960-a2e2-f90ff4358f9c', case_by_case_calibration_over_fixed_rules, conventional).
narrative_ontology:cs_reference_frame('7de80b99-af65-4960-a2e2-f90ff4358f9c', precedent_as_contextually_weighted_authority).
narrative_ontology:cs_drift_state('7de80b99-af65-4960-a2e2-f90ff4358f9c', contemporary_factor_balancing_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7de80b99-af65-4960-a2e2-f90ff4358f9c', '').
narrative_ontology:cs_kernel_id(common_law_precedent_corpus__pluralist_balancing, common_law_precedent_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__pluralist_balancing, apex_court).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__pluralist_balancing, intermediate_appellate_courts).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__pluralist_balancing, sophisticated_repeat_litigants).
narrative_ontology:constraint_victim(common_law_precedent_corpus__pluralist_balancing, one_shot_litigants).
narrative_ontology:constraint_victim(common_law_precedent_corpus__pluralist_balancing, trial_judges).
narrative_ontology:constraint_victim(common_law_precedent_corpus__pluralist_balancing, reliance_interest_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(common_law_precedent_corpus__pluralist_balancing, intermediate_appellate_courts).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__pluralist_balancing, contextual_stare_decisis_factors).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__pluralist_balancing, common_law_incrementalism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decides which of its own prior decisions continue to govern, announces the factors that guide that judgment, and grants or denies review to shape the docket. Collects final interpretive authority over the entire body of decided cases; its legitimacy rests on the very method it administers, so stepping outside the method would undercut the source of its own standing. It cannot delegate the calibration elsewhere without dissolving its role.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, apex_court, agenda_setter,
    institutional, generational, identity_locked, national).

% Sit between the apex court and the trial benches: they weigh prior decisions panel by panel, gaining room to shape doctrine within their circuits, while knowing the apex court may reweigh the same authorities and reverse them. Their published opinions become part of the corpus everyone below must reckon with. Leaving the hierarchy is not an option short of resignation.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, intermediate_appellate_courts, beneficiary,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(common_law_precedent_corpus__pluralist_balancing, intermediate_appellate_courts, payer).

% Decide cases under authorities whose weight may not be settled until an appeal resolves how firmly the higher courts will honor them. Bear reversal and remand when their weighing diverges from the appellate balance. Their discretion is whatever remains after the layers above finish calibrating.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, trial_judges, payer,
    institutional, biographical, constrained, local).

% Corporations, insurers, industry groups, and government agencies that appear constantly across decades. They can shop among fora, settle, structure transactions around unsettled areas, and fund the appeals that move doctrine. Unpredictability is a cost they can hedge and sometimes weaponize; doctrinal movement is an asset they can invest in.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, sophisticated_repeat_litigants, beneficiary,
    powerful, generational, constrained, global).

% Individuals, families, and small businesses usually in their first significant case. They cannot price outcomes reliably when the weight of governing decisions shifts domain by domain, they absorb adverse surprises in full, and they have no later case in which to recoup. Their practical alternative to accepting the system's verdict is giving up the claim.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, one_shot_litigants, payer,
    powerless, immediate, trapped, local).

% People and firms who arranged contracts, investments, property holdings, and long-term plans around settled rules. When a court recalibrates how firmly those rules bind, the rearrangement lands on completed plans that cannot be unwound. They live inside the system's effects but outside its proceedings.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, reliance_interest_holders, payer,
    moderate, biographical, trapped, national).

% People with potentially meritorious grievances who never file because outcome unpredictability makes the gamble unaffordable. They appear in no docket and are represented by no party, yet the doctrine developed in the cases they avoided governs the disputes they dropped.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, deterred_claimants, excluded,
    powerless, immediate, trapped, local).

% Scholars, restaters, and empirical researchers who code outcomes, test whether the announced factors predict results, and propose reconstructions of doctrine. They hold no adjudicative seat; their influence runs through citations, clerk pipelines, and the occasional adoption of their frameworks by judges.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, legal_academy, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(common_law_precedent_corpus__pluralist_balancing, apex_court).
narrative_ontology:fixing_cost_class(common_law_precedent_corpus__pluralist_balancing, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A dispersed set of courts deciding enormous numbers of disputes needs shared answers: the corpus of prior decisions lets a judge in one district resolve a contract question the way a judge in another will, lets lawyers advise clients before filing, and lets parties order their affairs against known rules. The balancing reading coordinates by keeping the corpus authoritative while letting each generation adjust weights where circumstances have changed.
% TRANSFER_FUNCTION: Moves decisional authority upward and inward: predictability held by private planners and lower courts is converted into discretionary authority exercised by appellate and apex benches, and the costs of doctrinal movement flow from the parties least able to hedge them (one-shot litigants, reliance holders) toward those best positioned to exploit movement (repeat players and the appellate bar).
% ABSENT_VOICES: Deterred claimants — priced out before filing — and non-party stakeholders whose affairs are governed by doctrine developed in litigation they could not join. Both would object that balances struck between litigated extremes silently allocate burdens onto the unrepresented middle; they are absent because access itself is what the unpredictability taxes.
% DISAPPEARANCE_RATIONALE: If the corpus and its calibration vanished overnight, every dispute becomes first impression: pending dockets stall, contracts drafted against case law lose their interpretive baseline, and courts would need either a comprehensive code or open-ended discretion to decide anything. The entire architecture of legal advice, settlement pricing, and long-term planning built on citable authority would rebuild itself around whatever replaced it.
% FOUNDING_PROBLEM: Decentralized adjudication without a comprehensive code: English and early American courts needed yesterday's answers to hold across itinerant benches and generations so that like cases would come out alike, without waiting for a legislature to supply rules.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and comparative-law scholars corroborate the founding problem from outside the bench: the recorded rationale of the reporting system, the persistence of precedent-like institutions in mixed jurisdictions, and bar-association studies of settlement markets all attest that coordinating decentralized adjudication remains a live need. No corroborating source depends on the balancing reading specifically — the need predates and outlives any particular calibration method.
narrative_ontology:disappearance_verdict(common_law_precedent_corpus__pluralist_balancing, world_rearranges).
narrative_ontology:founding_problem_status(common_law_precedent_corpus__pluralist_balancing, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_law_precedent_corpus__pluralist_balancing, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(common_law_precedent_corpus__pluralist_balancing, 'none', 1).
narrative_ontology:epsilon_provenance(common_law_precedent_corpus__pluralist_balancing, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_law_precedent_corpus__pluralist_balancing_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_law_precedent_corpus__pluralist_balancing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_law_precedent_corpus__pluralist_balancing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness sits mid-range (0.58) because the arrangement couples a genuine coordination good — citable, cross-jurisdictional authority that no participant could cheaply replace — with a real transfer: predictability is converted into appellate discretion, and the conversion costs land unevenly. Suppression (0.62) is structural, not internalized: jurisdiction is compulsory, deviation draws reversal, and there is no exit from the legal order that governs a dispute; suppression is authored raw and unscaled — the engine scales only extractiveness, by directionality and scope. Theater (0.42) reflects the documented gap between announced factors and outcome drivers: the factors are real instruments, but a growing share of their invocation dresses results reached on other grounds; the series stays below 0.5 throughout, marking function still dominant over performance. Accessibility collapse is low-moderate (0.38): codified civil-law systems demonstrate a working alternative, and recurring codification movements keep that alternative visible even where domestically unavailable. Resistance (0.57) is sustained: dissents, overrule advocacy, critical scholarship, and legislative overrides meet every major recalibration. All three tracked series share one grid (T=0..60, mapped to approximately 1965–2025); trajectories are monotonic rather than cyclical, so no intermittent-reinforcement mechanism is alleged. Coalition dynamics partially cap the resource asymmetry: class aggregation and test-case strategies convert some one-shot litigants into repeat-player-like coalitions, which is why payer-seat extraction plateaus late in the series rather than climbing linearly.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the apex bench the arrangement is stewardship: it built the method, profits in authority, and cannot imagine exit without self-dissolution. From the intermediate bench it is a squeeze: enough discretion to matter, not enough to be safe. From the trial bench it is a reversal lottery priced into every ruling. From one-shot litigants it is an unpredictability tax levied exactly where hedging is impossible; from repeat players the same unpredictability is a purchasable option. Same nominal system, materially different constraints per seat — the engine computes this divergence from power, exit, and role, not from the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (apex_court, intermediate_appellate_courts, sophisticated_repeat_litigants) derive low d — the arrangement subsidizes them. Declared victims (one_shot_litigants, trial_judges, reliance_interest_holders) derive high d. Exit modulates within roles: the apex court's identity_locked status pins it at the beneficiary extreme, since its position is constituted by the method itself and cannot be cashed out; repeat players' constrained-but-strategic mobility keeps them subsidized without locking them in; trial judges and reliance holders, unable to unwind exposure, sit near the target end. No directionality overrides are authored: the derivation chain already separates the three institutional benches by role and exit, and an override keyed to the shared institutional power atom could not distinguish apex from trial bench without misdescribing one of them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — coordinating decentralized adjudication without a code — is live, so this is not a mandate outliving its function; mandatrophy_resolved stays unset. The classification discipline cuts both ways. Reading the corpus as pure coordination would erase the measured transfer from trapped litigants to the bench and to resourced repeat players; reading it as pure extraction would erase the coordination good that explains why abolition finds no constituency and why every reform proposal rebuilds rather than removes the corpus. The tangled_rope claim keeps both halves on the table, and the slowly rising theater series marks where the balance is drifting: toward performance of principled calibration over its practice, without yet reaching the cost-asymmetry signature of an inertial remainder.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'Is the standing arrangement better classified under a sibling reading of the common_law_precedent_corpus kernel — strict_stare_decisis or evolutionary_framework — and does this file''s ε travel across readings?',
    'Doctrine-profiling of apex-court opinions: frequency of extraordinary-justification language versus express normative-evolution reasoning, compared against the sibling files'' classifications of the same corpus.',
    'Under strict_stare_decisis the victim set shifts toward adaptation-seekers and ε rises; under evolutionary_framework suppression falls as reinterpretation is licensed. This file''s classification holds only for the pluralist reading; ε is reading-indexed over a fixed referent, not topic-indexed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Which reading of the precedent-corpus kernel this classification instantiates.').

omega_variable(
    factor_balancing_sincerity,
    'Are the articulated stare-decisis factors (workability, reliance, doctrinal coherence, changed understanding) causally operative in outcomes, or post-hoc rationalization of result-driven votes?',
    'Outcome-coding studies correlating factor invocation with outcome valence independent of the factors; internal-court evidence (drafts, conference notes) where available; natural experiments where the factors cut against the majority''s preferred result.',
    'If post-hoc, theater_ratio is understated and the arrangement drifts toward performative maintenance; if operative, the coordination component is stronger than the current metrics suggest and the rope half of the hybrid thickens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(factor_balancing_sincerity, empirical, 'Sincerity of the announced balancing factors.').

omega_variable(
    domain_variance_dispersion,
    'How much does precedent weight actually vary across domains in application, and is the variation principled (tracked to reliance and workability) or unstructured?',
    'Cross-domain citation-and-follow studies: measure overrule, limitation, and distinction frequencies in constitutional, commercial, property, and criminal corpora over the interval.',
    'Low realized variance collapses this reading toward strict_stare_decisis; high unstructured variance converts unpredictability into the dominant cost channel and pushes payer seats toward a snare-flavored computed classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_variance_dispersion, empirical, 'Realized dispersion of precedent weight across domains.').

omega_variable(
    repeat_player_advantage,
    'To what degree do calibration outcomes systematically track litigant resource asymmetry rather than doctrinal merit?',
    'Panel-level datasets matching litigant type to doctrinal-shift wins across decades, controlling for merit indicators.',
    'High capture would mark the transfer as targeted rather than diffuse hierarchy overhead, sharpening payer-seat extraction; low capture supports the hybrid coordination reading and the current mid-range ε.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(repeat_player_advantage, empirical, 'Resource-asymmetry capture of balancing outcomes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_law_precedent_corpus__pluralist_balancing, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(comm_tr_t0, observed).
narrative_ontology:measurement(comm_tr_t10, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 10, 0.28).
narrative_ontology:measurement_basis(comm_tr_t10, observed).
narrative_ontology:measurement(comm_tr_t20, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 20, 0.32).
narrative_ontology:measurement_basis(comm_tr_t20, observed).
narrative_ontology:measurement(comm_tr_t30, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 30, 0.36).
narrative_ontology:measurement_basis(comm_tr_t30, observed).
narrative_ontology:measurement(comm_tr_t40, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 40, 0.39).
narrative_ontology:measurement_basis(comm_tr_t40, observed).
narrative_ontology:measurement(comm_tr_t50, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 50, 0.41).
narrative_ontology:measurement_basis(comm_tr_t50, observed).
narrative_ontology:measurement(comm_tr_t60, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 60, 0.42).
narrative_ontology:measurement_basis(comm_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(comm_be_t0, observed).
narrative_ontology:measurement(comm_be_t10, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 10, 0.46).
narrative_ontology:measurement_basis(comm_be_t10, observed).
narrative_ontology:measurement(comm_be_t20, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 20, 0.5).
narrative_ontology:measurement_basis(comm_be_t20, observed).
narrative_ontology:measurement(comm_be_t30, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 30, 0.53).
narrative_ontology:measurement_basis(comm_be_t30, observed).
narrative_ontology:measurement(comm_be_t40, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 40, 0.55).
narrative_ontology:measurement_basis(comm_be_t40, observed).
narrative_ontology:measurement(comm_be_t50, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 50, 0.56).
narrative_ontology:measurement_basis(comm_be_t50, observed).
narrative_ontology:measurement(comm_be_t60, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 60, 0.58).
narrative_ontology:measurement_basis(comm_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(comm_su_t0, observed).
narrative_ontology:measurement(comm_su_t10, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 10, 0.53).
narrative_ontology:measurement_basis(comm_su_t10, observed).
narrative_ontology:measurement(comm_su_t20, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 20, 0.56).
narrative_ontology:measurement_basis(comm_su_t20, observed).
narrative_ontology:measurement(comm_su_t30, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 30, 0.59).
narrative_ontology:measurement_basis(comm_su_t30, observed).
narrative_ontology:measurement(comm_su_t40, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 40, 0.61).
narrative_ontology:measurement_basis(comm_su_t40, observed).
narrative_ontology:measurement(comm_su_t50, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 50, 0.62).
narrative_ontology:measurement_basis(comm_su_t50, observed).
narrative_ontology:measurement(comm_su_t60, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 60, 0.62).
narrative_ontology:measurement_basis(comm_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_law_precedent_corpus__pluralist_balancing, enforcement_mechanism).
narrative_ontology:affects_constraint(common_law_precedent_corpus__pluralist_balancing, strict_stare_decisis).
narrative_ontology:affects_constraint(common_law_precedent_corpus__pluralist_balancing, evolutionary_framework).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the common_law_precedent_corpus kernel: the colloquial label 'stare decisis' covers three structurally distinct arrangements. strict_stare_decisis (backward-binding, extraordinary-justification threshold) carries high suppression and low realized variance; evolutionary_framework (reinterpretation licensed by normative change) carries low suppression and high adaptation; pluralist_balancing (this file) sits between with context-dependent variance and multi-tier extraction. The upstream reading in doctrinal citation chains is strict_stare_decisis — pluralist opinions invoke the bindingness tradition as their baseline before qualifying it — hence the edge direction. Each member authors its own ε over the same standing corpus; the ε differences index the readings, not measurement noise.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
