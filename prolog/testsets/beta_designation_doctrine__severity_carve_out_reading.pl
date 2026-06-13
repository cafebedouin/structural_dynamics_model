% ============================================================================
% CONSTRAINT STORY: beta_designation_doctrine__severity_carve_out_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_beta_designation_doctrine__severity_carve_out_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: beta_designation_doctrine__severity_carve_out_reading
 *   human_readable: Beta Designation Categorical Exclusion for Critical Systems (Severity Carve-Out Reading)
 *   domain: technology_law/product_liability/consumer_protection
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested beta
 *   designation kernel: the severity carve-out reading. The kernel is the
 *   question 'what does beta designation mean and what scope does it cover?'
 *   Three structurally distinct readings persist: (1)
 *   expansive_shield_reading: beta is a comprehensive, indefinite liability
 *   waiver in all contexts; (2) narrow_warning_reading: beta is time-bounded
 *   experimental disclosure, base product liability preserved; (3)
 *   severity_carve_out_reading (this constraint): beta is categorically
 *   unavailable for life-safety, financial, and critical-infrastructure
 *   domains regardless of testing status. Each reading produces a different ε
 *   and different stakeholder structure. This constraint models the severity
 *   carve-out: a domain-specific physical constraint that harm severity
 *   overrides contractual allocation. The constraint is authored as MOUNTAIN
 *   because this reading rests on the irreducible principle that causation of
 *   severe harm cannot be contracted away—it emerges as a natural limit from
 *   the physics and ethics of harm prevention, not as a negotiated
 *   coordination mechanism. However, the constraint carries declared
 *   beneficiaries because the 'natural law' claim is itself contested (omega
 *   variable captures this). The low extractiveness (0.31) reflects that the
 *   constraint redistributes liability exposure rather than extracting value
 *   per se; beneficiaries gain protection, vendors pay via liability
 *   internalization, but no seat monopolizes the constraint's operation for
 *   concentrated gain.
 *
 * KEY AGENTS:
 *   - software_vendor: Institutional agenda_setter. Can no longer defer liability via beta designation in critical domains; must accept full product liability or wait for production-ready status.
 *   - life_safety_system_users: Powerless beneficiaries. Gain categorical protection from experimental software in medical, aviation, autonomous control systems.
 *   - financial_system_users: Moderate-power beneficiaries. Constrained exit; depend on constraint to prevent banking and trading infrastructure from being released as beta.
 *   - critical_infrastructure_operators: Organized beneficiaries. Gain assurance that power grids, water systems, telecommunications depend on production-grade software.
 *   - regulatory_authority: Institutional agenda_setter. Enforces the constraint via certification, pre-market review, and liability adjudication.
 *   - vendor_liability_industry: Organized payer. Absorbs increased liability claims; also gains clarity and pricing opportunity.
 *   - expansive_shield_advocates: Organized excluded. Would argue for broad beta protection but are locked out by the carve-out rule.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beta_designation_doctrine__severity_carve_out_reading, 0.31).
domain_priors:suppression_score(beta_designation_doctrine__severity_carve_out_reading, 0.18).
domain_priors:theater_ratio(beta_designation_doctrine__severity_carve_out_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, accessibility_collapse, 0.89).
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beta_designation_doctrine__severity_carve_out_reading, mountain).
narrative_ontology:human_readable(beta_designation_doctrine__severity_carve_out_reading, "Beta Designation Categorical Exclusion for Critical Systems (Severity Carve-Out Reading)").
narrative_ontology:topic_domain(beta_designation_doctrine__severity_carve_out_reading, "technology_law/product_liability/consumer_protection").

domain_priors:emerges_naturally(beta_designation_doctrine__severity_carve_out_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(beta_designation_doctrine__severity_carve_out_reading, 'efdf5ace-b9f4-460b-864c-bbc749c82129').
narrative_ontology:cs_kernel_codification('efdf5ace-b9f4-460b-864c-bbc749c82129', fixed_text).
narrative_ontology:cs_authority_grounding('efdf5ace-b9f4-460b-864c-bbc749c82129', extraction).
narrative_ontology:cs_interpretation_layer_present('efdf5ace-b9f4-460b-864c-bbc749c82129').
narrative_ontology:cs_reading_relation('efdf5ace-b9f4-460b-864c-bbc749c82129', beta_designation_doctrine__expansive_shield_reading, forecloses).
narrative_ontology:cs_reading_relation('efdf5ace-b9f4-460b-864c-bbc749c82129', beta_designation_doctrine__narrow_warning_reading, influences).
narrative_ontology:cs_axiom('efdf5ace-b9f4-460b-864c-bbc749c82129', foundational, harm_severity_overrides_contract).
narrative_ontology:cs_axiom_status(harm_severity_overrides_contract, holdable).
narrative_ontology:cs_axiom_grounding('efdf5ace-b9f4-460b-864c-bbc749c82129', harm_severity_overrides_contract, deontological).
narrative_ontology:cs_axiom('efdf5ace-b9f4-460b-864c-bbc749c82129', foundational, critical_domain_beta_categorically_unavailable).
narrative_ontology:cs_axiom_status(critical_domain_beta_categorically_unavailable, holdable).
narrative_ontology:cs_axiom_grounding('efdf5ace-b9f4-460b-864c-bbc749c82129', critical_domain_beta_categorically_unavailable, conventional).
narrative_ontology:cs_reference_frame('efdf5ace-b9f4-460b-864c-bbc749c82129', strict_liability_doctrine).
narrative_ontology:cs_drift_state('efdf5ace-b9f4-460b-864c-bbc749c82129', contemporary_regulatory_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('efdf5ace-b9f4-460b-864c-bbc749c82129', '').
narrative_ontology:cs_kernel_id(beta_designation_doctrine__severity_carve_out_reading, beta_designation_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__severity_carve_out_reading, life_safety_system_users).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__severity_carve_out_reading, financial_system_users).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__severity_carve_out_reading, critical_infrastructure_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(beta_designation_doctrine__severity_carve_out_reading, vendor_liability_industry).
narrative_ontology:constraint_vindicates(beta_designation_doctrine__severity_carve_out_reading, harm_severity_overrides_contractual_allocation).
narrative_ontology:constraint_vindicates(beta_designation_doctrine__severity_carve_out_reading, domain_specific_product_liability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develops and distributes software products. May wish to release experimental versions under beta designation to gather field data and defer liability exposure. Under this reading, the vendor cannot use beta designation for life-safety or financial critical systems; it must either wait for production-ready status or explicitly accept full product liability for any release into those domains.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, software_vendor, agenda_setter,
    institutional, biographical, mobile, global).

% Users of software that controls or influences life-safety outcomes (medical devices, autonomous vehicle control, aviation systems, industrial safety interlocks). They depend on the constraint to prevent vendors from treating life-critical functionality as experimental. Their exit option is effectively absent—they cannot simply refuse a medical device or aircraft system.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, life_safety_system_users, beneficiary,
    powerless, biographical, trapped, global).

% Users of financial infrastructure (banking platforms, payment systems, trading infrastructure, clearing systems). They rely on the constraint to prevent vendors from treating transaction processing, account management, or settlement systems as beta-stage experimental. Exit is limited to switching providers, which carries high switching costs and operational friction.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, financial_system_users, beneficiary,
    moderate, biographical, constrained, global).

% Operators of power grids, water systems, telecommunications networks, transportation systems. They depend on the constraint to prevent the software they rely on from being classified as beta-stage in production deployment. Their exit is constrained by monopolistic or heavily consolidated vendor bases and the operational necessity of their infrastructure.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, critical_infrastructure_operators, beneficiary,
    organized, generational, constrained, global).

% Government agencies and quasi-regulatory bodies (FDA, FAA, financial regulators, safety standard bodies) that enforce product liability, safety certification, and pre-market approval requirements. They enforce the constraint by prohibiting beta designation in high-stakes domains and by holding vendors liable for harms even if beta status was claimed.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, regulatory_authority, agenda_setter,
    institutional, generational, analytical, national).

% Users of non-critical software (productivity tools, games, consumer utilities) where beta designation remains permissible. They can opt to use or avoid beta releases based on personal risk tolerance; their stakes are lower and exit is available. They are not directly governed by this constraint but observe it apply to others.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, general_purpose_software_users, observer,
    powerless, biographical, mobile, global).

% Insurance and legal industries built around software vendor liability. The constraint forces vendors to internalize risk by accepting full liability rather than deferring it via beta designation. This increases liability claims, litigation, and insurance demand in critical domains, but also clarifies exposure and pricing.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, vendor_liability_industry, payer,
    organized, biographical, constrained, global).

% Vendors and industry groups advocating for broad beta-shield protections (unlimited duration, all domains). They would argue that severity-based carve-outs reduce innovation incentives and impose undue liability risk. They are excluded from the rule-making process in jurisdictions that adopt the severity carve-out reading.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, expansive_shield_advocates, excluded,
    organized, biographical, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes domain-specific product liability doctrine: separates domains where experimental beta designation is permissible (low-stakes consumer software) from domains where it is structurally unavailable (life-safety, financial, critical infrastructure). Coordinates on the principle that harm severity overrides contractual liability allocation mechanisms.
% TRANSFER_FUNCTION: Transfers liability exposure from software vendors in critical domains to the vendors themselves (they cannot defer via beta designation) and to product liability insurers and litigation systems. In non-critical domains, vendors retain the ability to allocate experimental-release risk to users via beta designation.
% ABSENT_VOICES: Vendors advocating for expansive beta-shield protections are structurally excluded—they argue for the sibling reading but are locked out of jurisdictions enforcing the severity carve-out. End-users of non-critical software whose interests might differ from those of critical-system users are not explicitly represented in regulatory deliberations that produce this constraint.
% DISAPPEARANCE_RATIONALE: If the constraint disappeared, vendors could release life-safety and financial software under indefinite beta designation, deferring or eliminating liability for failures. The world rearranges: medical device regulation would shift from pre-market approval to post-market liability discovery; financial institutions would face uninsurable tail risk; critical infrastructure would operate with experimental software and no contractual recourse. Regulatory regimes would have to be rebuilt around explicit approval and inspection rather than liability allocation.
% FOUNDING_PROBLEM: Early software products released beta versions with broad liability disclaimers that were later enforced to eliminate vendor accountability for failures in life-safety and financial domains. Users of medical devices and banking systems discovered their recourse was nil—the vendor had disclaimed liability via beta status, and the user bore the loss. Regulators observed that contractual liability allocation (beta waiver) was being used to circumvent physical safety requirements and harm prevention duties.
% FOUNDING_PROBLEM_CORROBORATION: Regulatory bodies (FDA, FAA, SEC, financial regulators across multiple jurisdictions) attest the founding problem is live and cite case histories of harm in critical domains traceable to vendors' use of beta designation to avoid liability. Medical and financial incident reports document the pattern. Vendor industry groups attest they did use beta designation in high-stakes domains but dispute that it was abusive. Independent analysis (product liability scholars, engineering ethics literature, regulatory impact assessments) corroborates the problem from outside the benefiting parties.
narrative_ontology:disappearance_verdict(beta_designation_doctrine__severity_carve_out_reading, world_rearranges).
narrative_ontology:founding_problem_status(beta_designation_doctrine__severity_carve_out_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(beta_designation_doctrine__severity_carve_out_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(beta_designation_doctrine__severity_carve_out_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(beta_designation_doctrine__severity_carve_out_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, ExtMetricName, E),
    domain_priors:suppression_score(beta_designation_doctrine__severity_carve_out_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(beta_designation_doctrine__severity_carve_out_reading),
    narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(beta_designation_doctrine__severity_carve_out_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is LOW (0.31) because the constraint primarily redistributes liability exposure rather than extracting rent. The beneficiaries (life-safety users, financial-system users) gain protection, but that protection is achieved by forcing vendors to internalize risk, not by a third party capturing value. Suppression is LOW (0.18) because the constraint operates through legal doctrine and regulatory enforcement, not through coercion or behavioral control—vendors understand the rule and comply or litigate, but they are not suppressed into belief. Theater ratio is LOW (0.12) because the constraint's enforcement is substantive: regulators actually deny beta certification in critical domains, vendors actually face liability if they try, and the constraint's function is real. Accessibility collapse is HIGH (0.89) because once the constraint is understood—that beta designation is unavailable in critical domains—alternatives (trying to claim beta status anyway, lobbying for exceptions) effectively collapse; the rule is absolute for the class of systems. Resistance is MODERATE (0.42) because vendors mount real resistance (litigation, regulatory advocacy, industry pressure) but the constraint persists because the underlying principle (severity overrides contract) has deep grounding in tort law and safety ethics. The measurement series shows stability across the interval: the constraint has reached equilibrium enforcement; no drift in extractiveness or suppression is expected because the rule itself does not change. This is characteristic of a mountain constraint—natural limits do not erode.
 *
 * PERSPECTIVAL GAP:
 *   From the vendor's seat, the constraint feels like an imposed constraint that removes a valuable tool (beta designation) in exchange for increased liability exposure. From the life-safety user's seat, the same constraint is a protective natural law—it simply reflects the fact that you cannot sell someone a medical device that might kill them and disclaim responsibility via beta status. The engine computes both perspectives from the structural data: the vendor's directionality (d toward target, high extraction via internalized liability) diverges from the user's directionality (d toward beneficiary, protection without cost). This divergence is the measurement the corpus takes. The claim (mountain) and the metrics (low extraction, high collapse) are intentionally independent; a mountain with extracted costs is exactly a false summit, and the FSM gate catches this if beneficiaries are present (which they are, triggering FSM evaluation).
 *
 * DIRECTIONALITY LOGIC:
 *   Vendors: institutional power but mobile exit options (can shift to non-critical domains, can litigate, can exit markets). They are targets under the constraint (internalized liability). Directionality near 0.7 (substantial target). Life-safety users: powerless, trapped exit. They are beneficiaries (protected). Directionality near 0.1 (clear beneficiary). Financial-system users: moderate power, constrained exit. Beneficiaries but with some negotiating position. Directionality near 0.2. Regulatory authority: institutional, analytical exit (they enforce, they do not suffer the constraint). Directionality near 0.5 (neutral administrator). The beneficiaries' directionality is derived from the fact that the constraint protects them without imposing cost; the vendors' directionality is derived from the fact that it imposes liability cost without reciprocal benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is LIVE (harm from experimental software in critical domains continues; the constraint persists because the problem persists). The world-rearranges verdict is CORRECT (removing the constraint would restructure liability, regulation, and user recourse). The constraint is classified as MOUNTAIN (natural limit: severity overrides contract), not SNARE (which would require victims and extraction). However, the constraint carries beneficiaries—users protected from experimental software. This triggers FALSE SUMMIT evaluation: is this a genuine natural law or a constructed allocation disguised as natural law? The omega variable addresses this directly: the 'natural law' reading (harm causation cannot be contracted away) is itself contested by the expansive-shield reading (which denies this principle in favor of broad contractual freedom). The constraint is mountain-shaped IF we accept that severity is an irreducible physical property that overrides contract; it is tangled_rope-shaped IF we accept the vendor's framing that contractual allocation (beta disclaimer) is a valid coordination mechanism even in high-stakes domains. The mandatrophy resolution: the founding problem is live, the constraint is structurally justified by the principle it rests on (severity override), and the constraint's persistence is explained by the ongoing problem and the legal/ethical principle, not by theater or inertia. This constraint is NOT a zombie—it is actively maintained because the underlying principle remains contested and the harms remain real.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_allocation,
    'Is the categorical unavailability of beta designation in critical domains a natural law (harm causation cannot be contracted away) or a constructed policy allocation (chosen to favor user protection over vendor contractual freedom)?',
    'Examine whether the principle (severity overrides contract) is grounded in irreducible physical properties of harm causation, or whether alternative contractual regimes (expansive shield) could persist without logical contradiction. Test against comparative legal systems and historical vendor practices.',
    'If natural law: the constraint is genuinely mountain-shaped (emerges naturally from the physics and ethics of harm prevention). If constructed: the constraint is tangled_rope-shaped (coordinates on the principle but requires active enforcement and beneficiaries to maintain). Reclassification affects whether the constraint is expected to persist indefinitely or erode under vendor pressure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_allocation, conceptual, 'Whether severity-override is a natural limit or a constructed policy choice.').

omega_variable(
    domain_boundary_ambiguity,
    'What constitutes a ''critical system'' for purposes of the beta unavailability rule? Where are the boundaries between critical (beta prohibited) and non-critical (beta permissible)?',
    'Regulatory precedent and case law establish the domain boundary: life-safety systems (medical devices, aviation control, autonomous vehicle safety-critical functions) are always critical; financial systems (payment, settlement, account management) are generally critical; consumer entertainment software is non-critical. Boundary cases (productivity software with access to health data, consumer financial apps) remain contested in regulatory adjudication.',
    'If the boundary is narrow (only explicit life-or-death functions), the constraint affects fewer vendors and has lower effective scope. If the boundary is broad (including health-adjacent, finance-adjacent consumer software), the constraint affects substantially more vendors and has higher scope. Scope affects effective extraction calculation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_boundary_ambiguity, empirical, 'Regulatory definition of critical systems for beta carve-out purposes.').

omega_variable(
    reading_foreclosure_status,
    'Does the severity carve-out reading logically foreclose the expansive-shield reading, or do they coexist as live but opposed positions?',
    'The two readings have incompatible core premises: severity carve-out asserts harm severity overrides contract; expansive shield asserts contractual freedom is universal. In a single legal framework, only one can be canonical law. But the readings can coexist across different jurisdictions, different industry sectors, and different types of software—some jurisdictions adopt the carve-out, others enforce broad beta shields. Check whether any single party can consistently hold both readings or whether they are genuinely mutually exclusive.',
    'If foreclosure: this reading makes the expansive-shield reading impossible in any framework that adopts it. If coexistence: they are live competing positions in the global regulatory landscape, and the constraint must expect ongoing challenge from the sibling reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_status, conceptual, 'Whether this reading logically rules out the expansive-shield sibling or both coexist as live alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beta_designation_doctrine__severity_carve_out_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beta_tr_t0, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(beta_tr_t0, observed).
narrative_ontology:measurement(beta_tr_t5, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 5, 0.11).
narrative_ontology:measurement_basis(beta_tr_t5, observed).
narrative_ontology:measurement(beta_tr_t10, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 10, 0.11).
narrative_ontology:measurement_basis(beta_tr_t10, observed).
narrative_ontology:measurement(beta_tr_t20, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement_basis(beta_tr_t20, observed).
narrative_ontology:measurement(beta_tr_t30, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 30, 0.12).
narrative_ontology:measurement_basis(beta_tr_t30, observed).
narrative_ontology:measurement(beta_tr_t40, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 40, 0.12).
narrative_ontology:measurement_basis(beta_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(beta_be_t0, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(beta_be_t0, observed).
narrative_ontology:measurement(beta_be_t5, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 5, 0.29).
narrative_ontology:measurement_basis(beta_be_t5, observed).
narrative_ontology:measurement(beta_be_t10, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 10, 0.3).
narrative_ontology:measurement_basis(beta_be_t10, observed).
narrative_ontology:measurement(beta_be_t20, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 20, 0.31).
narrative_ontology:measurement_basis(beta_be_t20, observed).
narrative_ontology:measurement(beta_be_t30, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 30, 0.31).
narrative_ontology:measurement_basis(beta_be_t30, observed).
narrative_ontology:measurement(beta_be_t40, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 40, 0.31).
narrative_ontology:measurement_basis(beta_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(beta_su_t0, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 0, 0.16).
narrative_ontology:measurement_basis(beta_su_t0, observed).
narrative_ontology:measurement(beta_su_t5, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 5, 0.17).
narrative_ontology:measurement_basis(beta_su_t5, observed).
narrative_ontology:measurement(beta_su_t10, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 10, 0.17).
narrative_ontology:measurement_basis(beta_su_t10, observed).
narrative_ontology:measurement(beta_su_t20, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 20, 0.18).
narrative_ontology:measurement_basis(beta_su_t20, observed).
narrative_ontology:measurement(beta_su_t30, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 30, 0.18).
narrative_ontology:measurement_basis(beta_su_t30, observed).
narrative_ontology:measurement(beta_su_t40, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 40, 0.18).
narrative_ontology:measurement_basis(beta_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(beta_designation_doctrine__severity_carve_out_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(beta_designation_doctrine__severity_carve_out_reading, 0.12).
narrative_ontology:affects_constraint(beta_designation_doctrine__severity_carve_out_reading, beta_designation_doctrine__expansive_shield_reading).
narrative_ontology:affects_constraint(beta_designation_doctrine__severity_carve_out_reading, beta_designation_doctrine__narrow_warning_reading).

% DUAL FORMULATION NOTE:
% The beta designation kernel decomposes into three structurally distinct constraints corresponding to three readings of what beta designation means and what domains it governs. Each reading has different ε, different beneficiary/victim sets, and different classification. The severity carve-out reading (this constraint) treats harm severity as the overriding principle; the expansive-shield reading treats contractual freedom as primary; the narrow-warning reading treats time-boundedness as primary. All three are linked via network.affects_constraints to reflect that they are readings of the same contested kernel. Each story carries an omega variable documenting the committer structure and the relationship between readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
