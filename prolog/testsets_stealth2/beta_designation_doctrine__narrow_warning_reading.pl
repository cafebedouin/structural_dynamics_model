% ============================================================================
% CONSTRAINT STORY: beta_designation_doctrine__narrow_warning_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_beta_designation_doctrine__narrow_warning_reading, []).

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
 *   constraint_id: beta_designation_doctrine__narrow_warning_reading
 *   human_readable: Beta Designation Doctrine — Narrow Warning Reading (Time-Bounded Testing Disclosure)
 *   domain: technology_law/software_liability/consumer_protection
 *
 * SUMMARY:
 *   The narrow warning reading governs beta designations in software
 *   liability law: a beta label discloses a time-bounded testing phase during
 *   which defect risk is allocated to informed users, baseline product
 *   liability survives the label, and the shield expires when genuine testing
 *   ends. This file instantiates ONE reading of the contested
 *   beta_designation_doctrine kernel. The sibling readings are separate
 *   constraints with their own epsilon values and are linked via
 *   network.affects_constraints: the expansive shield reading (indefinite
 *   comprehensive waiver) authors substantially higher extraction over
 *   under-informed users with no expiry; the severity carve-out reading
 *   (categorical unavailability for life-safety, financial, and other
 *   critical systems) authors near-zero extraction inside its excluded
 *   domains and narrows this reading's scope without altering its internal
 *   structure. The decomposition follows the epsilon-invariance principle:
 *   measuring the beta label as a permanent waiver, a bounded disclosure, or
 *   a domain-limited exclusion yields different epsilon values, different
 *   victim sets, and different classifications, so they are different
 *   constraints sharing one colloquial label. KEY AGENTS (by structural
 *   relationship): - established_software_vendors: Agenda-setting beneficiary
 *   (institutional/arbitrage) — runs beta programs, drafts disclosure terms,
 *   collects moderated liability during the window - startup_developers:
 *   Dependent beneficiary (moderate/constrained) — needs public testing to
 *   reach market; thin reserves behind the same baseline liability -
 *   early_adopter_users: Dual-positioned beneficiary/payer
 *   (moderate/constrained) — trades disclosed defect risk for early access;
 *   switching costs bind after adoption - beta_phase_harm_claimants: Primary
 *   target (powerless/trapped) — bears uncompensated residual harm inside the
 *   window; recovery, not exit, is the only question -
 *   nonconsenting_third_parties: Excluded seat (powerless/trapped) — harmed
 *   by beta failures without ever accepting beta terms -
 *   trial_and_appellate_courts: Enforcing agenda-setter
 *   (institutional/analytical) — supplies the time-boundedness test that
 *   gives the reading operative content - consumer_protection_regulators:
 *   Observer (institutional/analytical) — polices deceptive labeling at the
 *   margin
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beta_designation_doctrine__narrow_warning_reading, 0.3).
domain_priors:suppression_score(beta_designation_doctrine__narrow_warning_reading, 0.3).
domain_priors:theater_ratio(beta_designation_doctrine__narrow_warning_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beta_designation_doctrine__narrow_warning_reading, scaffold).
narrative_ontology:human_readable(beta_designation_doctrine__narrow_warning_reading, "Beta Designation Doctrine — Narrow Warning Reading (Time-Bounded Testing Disclosure)").
narrative_ontology:topic_domain(beta_designation_doctrine__narrow_warning_reading, "technology_law/software_liability/consumer_protection").

domain_priors:requires_active_enforcement(beta_designation_doctrine__narrow_warning_reading).
narrative_ontology:has_sunset_clause(beta_designation_doctrine__narrow_warning_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(beta_designation_doctrine__narrow_warning_reading, '90114e03-027d-4e91-9a9a-66bc3ff74dc0').
narrative_ontology:cs_kernel_codification('90114e03-027d-4e91-9a9a-66bc3ff74dc0', distributed).
narrative_ontology:cs_authority_grounding('90114e03-027d-4e91-9a9a-66bc3ff74dc0', practice).
narrative_ontology:cs_interpretation_layer_present('90114e03-027d-4e91-9a9a-66bc3ff74dc0').
narrative_ontology:cs_reading_relation('90114e03-027d-4e91-9a9a-66bc3ff74dc0', beta_designation_doctrine__expansive_shield_reading, forecloses).
narrative_ontology:cs_reading_relation('90114e03-027d-4e91-9a9a-66bc3ff74dc0', beta_designation_doctrine__severity_carve_out_reading, coexists_with).
narrative_ontology:cs_axiom('90114e03-027d-4e91-9a9a-66bc3ff74dc0', foundational, liability_shield_coterminous_with_testing_need).
narrative_ontology:cs_axiom_status(liability_shield_coterminous_with_testing_need, holdable).
narrative_ontology:cs_axiom_grounding('90114e03-027d-4e91-9a9a-66bc3ff74dc0', liability_shield_coterminous_with_testing_need, conventional).
narrative_ontology:cs_axiom('90114e03-027d-4e91-9a9a-66bc3ff74dc0', foundational, baseline_product_liability_not_waivable_by_label).
narrative_ontology:cs_axiom_status(baseline_product_liability_not_waivable_by_label, holdable).
narrative_ontology:cs_axiom_grounding('90114e03-027d-4e91-9a9a-66bc3ff74dc0', baseline_product_liability_not_waivable_by_label, deontological).
narrative_ontology:cs_axiom('90114e03-027d-4e91-9a9a-66bc3ff74dc0', secondary, designation_requires_genuine_testing_conduct).
narrative_ontology:cs_axiom_status(designation_requires_genuine_testing_conduct, holdable).
narrative_ontology:cs_axiom_grounding('90114e03-027d-4e91-9a9a-66bc3ff74dc0', designation_requires_genuine_testing_conduct, instrumental).
narrative_ontology:cs_reference_frame('90114e03-027d-4e91-9a9a-66bc3ff74dc0', time_bounded_genuine_testing_disclosure).
narrative_ontology:cs_drift_state('90114e03-027d-4e91-9a9a-66bc3ff74dc0', contemporary_continuous_deployment_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('90114e03-027d-4e91-9a9a-66bc3ff74dc0', '').
narrative_ontology:cs_kernel_id(beta_designation_doctrine__narrow_warning_reading, beta_designation_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__narrow_warning_reading, established_software_vendors).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__narrow_warning_reading, startup_developers).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__narrow_warning_reading, early_adopter_users).
narrative_ontology:constraint_victim(beta_designation_doctrine__narrow_warning_reading, beta_phase_harm_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(beta_designation_doctrine__narrow_warning_reading, early_adopter_users).
narrative_ontology:constraint_vindicates(beta_designation_doctrine__narrow_warning_reading, informed_user_risk_allocation_doctrine).
narrative_ontology:constraint_vindicates(beta_designation_doctrine__narrow_warning_reading, real_world_scale_testing_value).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Runs large-scale public beta and preview programs for flagship products; drafts the disclosure language and beta terms; decides when a testing phase begins and ends. Collects moderated liability exposure during the window plus the market-signaling value of maintaining a labeled pre-release tier. Can shift products between labeled tiers, rebrand previews, or retire labels when scrutiny rises.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, established_software_vendors, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(beta_designation_doctrine__narrow_warning_reading, established_software_vendors, beneficiary).

% Depends on public beta programs for affordable real-world testing and early revenue; cannot self-fund equivalent quality assurance. Bears the same post-window liability baseline as incumbents but with far thinner reserves to absorb it. Exiting the regime means delaying launch past the company's runway or shipping silently under full exposure — both potentially fatal at this scale.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, startup_developers, beneficiary,
    moderate, biographical, constrained, global).

% Receives early access, free or discounted use, and influence over the development roadmap in exchange for accepting disclosed defect risk during the testing window. Once workflows and data live inside the beta product, waiting for stable release or moving to a competitor carries real switching costs, so the disclosed risk is accepted once and lived with thereafter.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, early_adopter_users, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(beta_designation_doctrine__narrow_warning_reading, early_adopter_users, payer).

% Users who suffer data loss, service outage, or financial error caused by defects during the testing window. The harm has already occurred when the dispute arises, so exit is unavailable — the only question is the size of recovery, which the disclosure regime limits to what informed consent and preserved baseline liability leave intact.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, beta_phase_harm_claimants, payer,
    powerless, immediate, trapped, national).

% People who never accepted beta terms but are affected when beta software fails: recipients of corrupted messages, occupants of premises running preview building-management software, counterparties to transactions processed by pre-release payment code. They are party to no disclosure agreement and had no seat anywhere the beta terms were drafted.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, nonconsenting_third_parties, excluded,
    powerless, immediate, trapped, global).

% Decide whether a given beta period was a genuine testing phase — examining bug-fix cadence, feedback incorporation, and credible release intent — and withhold the shield where the label covered ordinary commercial deployment. Their time-boundedness jurisprudence is what gives this reading its operative content; without it the designation's duration limit is unenforceable language.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, trial_and_appellate_courts, agenda_setter,
    institutional, generational, analytical, national).

% Monitor beta and preview labeling for deceptive practice, particularly perpetual-beta marketing of mature products, and can bring enforcement actions that police the boundary between disclosure and misrepresentation. They rarely set the doctrine's core terms but shape the labeling environment the doctrine operates in.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, consumer_protection_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(beta_designation_doctrine__narrow_warning_reading, established_software_vendors).
narrative_ontology:fixing_cost_class(beta_designation_doctrine__narrow_warning_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the scale-testing problem: software fails in ways internal quality assurance cannot surface, so vendors need real-world usage before general release, and users need honest notice that a product is unfinished. The designation coordinates expectations — what the user may expect, what the vendor owes, when the special treatment ends — so pre-release distribution can occur without either silent shipping or blanket immunity.
% TRANSFER_FUNCTION: During the testing window, moves defect-risk-bearing and remediation cost from vendor to informed users, priced as early access, discounts, or roadmap influence; at the window's close, moves the full liability baseline back to the vendor along with the product's stable-release obligations.
% ABSENT_VOICES: Non-consenting third parties harmed by beta failures were never in the room where beta terms were drafted — they sign nothing yet absorb outage, corruption, and financial-error spillovers. Future users of prematurely stabilized products are similarly absent: pressure to exit the shielded window can export latent defects past the boundary where this reading's protections attach.
% DISAPPEARANCE_RATIONALE: Without the bounded-disclosure regime, vendors reorganize around one of two poles: delaying every release until internal testing substitutes for real-world exposure (slower cycles, higher prices, concentrated quality-assurance rents), or shipping silently unfinished software under full liability (a chilling effect pushing testing into unlabeled employee and partner channels). Early-access markets reprice, courts lose the doctrinal category they currently use to separate testing from deployment, and the perpetual-beta dispute resurfaces in whatever vocabulary replaces the label.
% FOUNDING_PROBLEM: Early mass-market software shipped under blanket as-is disclaimers: users had no recourse for defective products and vendors had no structured, honest way to test at scale before charging full price. The beta designation emerged to create a legible category of disclosed incompleteness — a window in which everyone knows the product is unfinished.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: appellate opinions continue to litigate where a testing phase ends and deny shields to products in years-long commercial deployment; consumer-protection agencies prosecute deceptive beta and preview labeling; consumer-law scholarship documents recurring perpetual-beta drift. None of these sources depends on vendor beta programs for its standing.
narrative_ontology:disappearance_verdict(beta_designation_doctrine__narrow_warning_reading, world_rearranges).
narrative_ontology:founding_problem_status(beta_designation_doctrine__narrow_warning_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(beta_designation_doctrine__narrow_warning_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(beta_designation_doctrine__narrow_warning_reading, 'none', 1).
narrative_ontology:epsilon_provenance(beta_designation_doctrine__narrow_warning_reading, 0.3, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(beta_designation_doctrine__narrow_warning_reading_tests).
:- end_tests(beta_designation_doctrine__narrow_warning_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.30: the referent is the standing beta-disclosure arrangement as this reading governs it — users are informed, the window is bounded, baseline liability survives — so extraction sits near the coordination-cost floor but above it because residual uncompensated harm concentrates on a powerless seat inside every window. Suppression 0.30 measures the coercive force needed to HOLD the reading's boundary against perpetual-beta drift, not barriers facing users, who can simply decline beta participation; suppression is authored as a raw structural property and is not scaled by power or scope, while extractiveness is the engine-scaled quantity. Theater 0.38: a substantial minority of beta and preview labels in the wild are marketing tiers rather than testing phases; the reading's function is precisely to convert that theater back into bounded disclosure, and the ratio tracks the share of labels resisting conversion. Accessibility collapse 0.25: alternatives survive everywhere — wait for stable release, use competitors, defer adoption — so understanding the regime eliminates no option. Resistance 0.40: sustained litigation over where testing ends, vendor opposition to bright-line duration caps, consumer advocacy for tighter bounds; individually powerless harm claimants occasionally aggregate into class actions, which is part of what the resistance figure registers. All three tracked metrics share one six-point grid spanning the mass-market beta era (roughly three decades compressed to T0–T30). Extraction peaks mid-interval during the perpetual-beta expansion and partially corrects as time-boundedness jurisprudence hardened; theater rises monotonically — the correction suppressed extraction, not labeling practice — and the rising suppression_requirement series records the growing enforcement effort the boundary now demands.
 *
 * PERSPECTIVAL GAP:
 *   The vendor seats compute a low-extraction coordination regime they built, priced, and rely on for release cadence; the harm-claimant seat computes an uncompensated loss with no exit; the court seat experiences the same structure as boundary-policing workload — deciding, case by case, whether a label described testing or deployment. Same market, same doctrine, divergent computed types. The divergence is structural, not attitudinal: it tracks each seat's directionality and exit position, which the engine derives from the declared beneficiary/victim data and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries derive low d: established vendors (agenda-setting beneficiary, arbitrage-grade exit) sit nearest the subsidy end of the scale; startup developers sit lower but still beneficiary-side because the regime is their only affordable path to real-world testing; early adopter users sit near symmetric — genuine early-access benefit against disclosed residual risk they partially bear. Beta-phase harm claimants derive high d as trapped payers whose costs are the regime's direct product. Non-consenting third parties would derive high d if seated; their exclusion from the term-drafting conversation is exactly why the authored victim list stays narrow, and the gap between their exposure and their absence from the beneficiary/victim declarations is carried by the third_party_recovery_gap omega. No directionality overrides are used: the beneficiary/victim declarations plus exit options reproduce these relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold claim is load-bearing. Reading the regime as a permanent rope licenses the expansive shield — coordination language laundering an indefinite waiver — while reading it as a snare erases the genuine coordination function of disclosed, large-scale testing. The sunset clause (shield expiry at the end of genuine testing) is the feature that keeps the mandate tied to its function: the arrangement's justification is the transition from unfinished to finished, never a steady state. The founding problem remains live — every platform cycle recreates the need to test at scale with honest risk allocation — so no mandatrophy resolution is declared. The live drift risk runs toward scope creep: mandate persistence inviting the designation to harden into the expansive reading, which the reading's foundational time-bound axiom exists to block.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location_of_disagreement,
    'Which reading of the beta_designation_doctrine kernel governs — and is the live disagreement located on the duration axis (this reading versus the expansive shield reading) or the domain axis (versus the severity carve-out reading)?',
    'Appellate consolidation or legislative codification adopting one reading; systematic tracking of citation patterns in beta-shield litigation across jurisdictions.',
    'Adopting the expansive reading removes the time bound and raises epsilon sharply (indefinite waiver over partially informed users); adopting the severity carve-out shrinks this reading''s scope to non-critical systems without changing its internal structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location_of_disagreement, conceptual, 'This story instantiates the narrow warning reading of the beta designation kernel; each sibling reading is a separate constraint with its own epsilon and classification.').

omega_variable(
    genuine_testing_verifiability,
    'What observable criteria reliably distinguish a genuine testing phase (active bug-fix cadence, feedback incorporation, credible release intent) from a perpetual-beta relabeling of ordinary commercial deployment?',
    'Discovery into issue-tracker telemetry, changelog cadence, and internal release-planning documents in beta-shield litigation.',
    'If genuineness is effectively unverifiable, the time bound degenerates into performance and the reading''s operation converges toward the expansive shield in practice despite its doctrinal form.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_testing_verifiability, empirical, 'Verifiability of the genuine-testing-phase condition that anchors the time bound.').

omega_variable(
    residual_risk_legitimacy_threshold,
    'How much residual uncompensated defect risk can informed consent legitimately shift to beta users during the window before informed becomes victimized?',
    'Comparative analysis of harm rates and recovery outcomes inside beta windows versus stable-release baselines, combined with revealed-preference studies of beta participation terms.',
    'If consent cannot legitimize the residual risk at current disclosure depth, effective extraction exceeds the authored value and the payer-side seats classify more severely than the structural data alone suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_risk_legitimacy_threshold, preference, 'Normative threshold at which disclosed risk-shifting becomes extraction.').

omega_variable(
    third_party_recovery_gap,
    'Does the preserved baseline product liability extend fully to non-consenting third parties harmed by beta-phase failures, or do doctrinal gaps (economic-loss limits, privity requirements) leave them bearing uncompensated harm?',
    'Survey of third-party claim outcomes in beta-defect litigation across jurisdictions.',
    'Recovery gaps would place real costs on a seat that never consented, raising effective extraction above the authored value and strengthening the case for the severity carve-out reading on domain grounds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(third_party_recovery_gap, empirical, 'Whether preserved baseline liability actually reaches non-consenting third parties.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beta_designation_doctrine__narrow_warning_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beta_narrow_reading_tr_t0, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement_basis(beta_narrow_reading_tr_t0, observed).
narrative_ontology:measurement(beta_narrow_reading_tr_t6, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 6, 0.19).
narrative_ontology:measurement_basis(beta_narrow_reading_tr_t6, observed).
narrative_ontology:measurement(beta_narrow_reading_tr_t12, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 12, 0.25).
narrative_ontology:measurement_basis(beta_narrow_reading_tr_t12, observed).
narrative_ontology:measurement(beta_narrow_reading_tr_t18, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 18, 0.31).
narrative_ontology:measurement_basis(beta_narrow_reading_tr_t18, observed).
narrative_ontology:measurement(beta_narrow_reading_tr_t24, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement_basis(beta_narrow_reading_tr_t24, observed).
narrative_ontology:measurement(beta_narrow_reading_tr_t30, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement_basis(beta_narrow_reading_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(beta_narrow_reading_be_t0, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 0, 0.24).
narrative_ontology:measurement_basis(beta_narrow_reading_be_t0, observed).
narrative_ontology:measurement(beta_narrow_reading_be_t6, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 6, 0.27).
narrative_ontology:measurement_basis(beta_narrow_reading_be_t6, observed).
narrative_ontology:measurement(beta_narrow_reading_be_t12, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 12, 0.33).
narrative_ontology:measurement_basis(beta_narrow_reading_be_t12, observed).
narrative_ontology:measurement(beta_narrow_reading_be_t18, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 18, 0.36).
narrative_ontology:measurement_basis(beta_narrow_reading_be_t18, observed).
narrative_ontology:measurement(beta_narrow_reading_be_t24, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 24, 0.33).
narrative_ontology:measurement_basis(beta_narrow_reading_be_t24, observed).
narrative_ontology:measurement(beta_narrow_reading_be_t30, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 30, 0.3).
narrative_ontology:measurement_basis(beta_narrow_reading_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(beta_narrow_reading_su_t0, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 0, 0.16).
narrative_ontology:measurement_basis(beta_narrow_reading_su_t0, observed).
narrative_ontology:measurement(beta_narrow_reading_su_t6, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 6, 0.2).
narrative_ontology:measurement_basis(beta_narrow_reading_su_t6, observed).
narrative_ontology:measurement(beta_narrow_reading_su_t12, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 12, 0.24).
narrative_ontology:measurement_basis(beta_narrow_reading_su_t12, observed).
narrative_ontology:measurement(beta_narrow_reading_su_t18, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 18, 0.27).
narrative_ontology:measurement_basis(beta_narrow_reading_su_t18, observed).
narrative_ontology:measurement(beta_narrow_reading_su_t24, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 24, 0.29).
narrative_ontology:measurement_basis(beta_narrow_reading_su_t24, observed).
narrative_ontology:measurement(beta_narrow_reading_su_t30, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 30, 0.3).
narrative_ontology:measurement_basis(beta_narrow_reading_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(beta_designation_doctrine__narrow_warning_reading, information_standard).
narrative_ontology:affects_constraint(beta_designation_doctrine__narrow_warning_reading, beta_designation_doctrine__expansive_shield_reading).
narrative_ontology:affects_constraint(beta_designation_doctrine__narrow_warning_reading, beta_designation_doctrine__severity_carve_out_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'beta designation doctrine' decomposes into three structurally distinct constraints per the epsilon-invariance principle. This story is the narrow warning reading (bounded testing disclosure, preserved baseline liability, low extraction, scaffold with a constitutive sunset). The expansive shield reading (indefinite comprehensive waiver) is the upstream rent-seeking form — vendors cite the genuine coordination value of testing disclosure as evidence for the unlimited waiver, so this reading structurally influences and constrains it. The severity carve-out reading (categorical exclusion for critical systems) is orthogonal: it restricts the kernel's domain rather than its duration, and coexists with this reading in most frameworks. Each member carries its own epsilon, victim set, and classification; none averages over the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
