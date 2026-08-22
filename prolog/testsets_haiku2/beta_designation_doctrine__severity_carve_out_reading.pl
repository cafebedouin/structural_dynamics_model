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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: beta_designation_doctrine__severity_carve_out_reading
 *   human_readable: Beta Designation Severity Carve-Out: Unavailability in Critical Systems
 *   domain: technology_law/product_liability/consumer_protection
 *
 * SUMMARY:
 *   The beta designation doctrine governs when software vendors can deploy
 *   experimental or tested-but-not-production-ready code with associated
 *   liability limitations. This story instantiates ONE reading of a contested
 *   kernel: the severity-carve-out reading asserts that beta designation is
 *   categorically unavailable in life-safety, financial, and other critical
 *   systems, regardless of the vendor's testing status or disclosure. The
 *   sibling readings (expansive-shield and narrow-warning) dispute whether
 *   beta provides indefinite liability waiver vs. time-bounded testing
 *   disclosure, and whether criticality domains are exempt. This reading
 *   unites harm severity with non-contractable duty: beta cannot shield
 *   vendors from liability in domains where failure causes death, financial
 *   ruin, or systemic collapse. The constraint is authored as a mountain
 *   (natural law of liability grounded in harm severity) but declares
 *   beneficiaries and carries omegas to flag the natural-law vs.
 *   constructed-doctrine ambiguity per the FSM protocol.
 *
 * KEY AGENTS:
 *   - regulatory_authorities: FDA, FAA, banking regulators; set and enforce the rule that beta is unavailable in their domains
 *   - software_vendors: powerful institutional actors; bear the cost of the carve-out by being unable to limit liability even under beta designation in critical domains
 *   - critical_infrastructure_operators: hospitals, power grids, financial networks; benefit from the rule because it forces vendors to meet higher vetting standards before deployment
 *   - end_users_critical_domains: powerless, trapped; benefit because their harm cannot be pre-waived
 *   - consumer_safety_advocates: organized, analytical; advance the normative doctrine that harm severity overrides contractual form
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beta_designation_doctrine__severity_carve_out_reading, 0.31).
domain_priors:suppression_score(beta_designation_doctrine__severity_carve_out_reading, 0.18).
domain_priors:theater_ratio(beta_designation_doctrine__severity_carve_out_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beta_designation_doctrine__severity_carve_out_reading, mountain).
narrative_ontology:human_readable(beta_designation_doctrine__severity_carve_out_reading, "Beta Designation Severity Carve-Out: Unavailability in Critical Systems").
narrative_ontology:topic_domain(beta_designation_doctrine__severity_carve_out_reading, "technology_law/product_liability/consumer_protection").

domain_priors:emerges_naturally(beta_designation_doctrine__severity_carve_out_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(beta_designation_doctrine__severity_carve_out_reading, 'a233e4f6-2512-49f2-94c3-b2dffc01fab9').
narrative_ontology:cs_kernel_codification('a233e4f6-2512-49f2-94c3-b2dffc01fab9', fixed_text).
narrative_ontology:cs_authority_grounding('a233e4f6-2512-49f2-94c3-b2dffc01fab9', lineage).
narrative_ontology:cs_interpretation_layer_present('a233e4f6-2512-49f2-94c3-b2dffc01fab9').
narrative_ontology:cs_reading_relation('a233e4f6-2512-49f2-94c3-b2dffc01fab9', beta_designation_doctrine__expansive_shield_reading, forecloses).
narrative_ontology:cs_reading_relation('a233e4f6-2512-49f2-94c3-b2dffc01fab9', beta_designation_doctrine__narrow_warning_reading, influences).
narrative_ontology:cs_axiom('a233e4f6-2512-49f2-94c3-b2dffc01fab9', foundational, harm_severity_overrides_contract).
narrative_ontology:cs_axiom_status(harm_severity_overrides_contract, holdable).
narrative_ontology:cs_axiom_grounding('a233e4f6-2512-49f2-94c3-b2dffc01fab9', harm_severity_overrides_contract, deontological).
narrative_ontology:cs_axiom('a233e4f6-2512-49f2-94c3-b2dffc01fab9', foundational, critical_domain_categoricity).
narrative_ontology:cs_axiom_status(critical_domain_categoricity, holdable).
narrative_ontology:cs_axiom_grounding('a233e4f6-2512-49f2-94c3-b2dffc01fab9', critical_domain_categoricity, empirically_contingent).
narrative_ontology:cs_reference_frame('a233e4f6-2512-49f2-94c3-b2dffc01fab9', harm_severity_non_contractable_duty).
narrative_ontology:cs_drift_state('a233e4f6-2512-49f2-94c3-b2dffc01fab9', contemporary_2026, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('a233e4f6-2512-49f2-94c3-b2dffc01fab9', '').
narrative_ontology:cs_kernel_id(beta_designation_doctrine__severity_carve_out_reading, beta_designation_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__severity_carve_out_reading, consumer_safety_advocates).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__severity_carve_out_reading, regulatory_authorities).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__severity_carve_out_reading, critical_infrastructure_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__severity_carve_out_reading, critical_infrastructure_operators).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__severity_carve_out_reading, end_users_critical_domains).
narrative_ontology:constraint_victim(beta_designation_doctrine__severity_carve_out_reading, software_vendors).
narrative_ontology:constraint_victim(beta_designation_doctrine__severity_carve_out_reading, critical_infrastructure_operators).
narrative_ontology:constraint_vindicates(beta_designation_doctrine__severity_carve_out_reading, harm_severity_principle).
narrative_ontology:constraint_vindicates(beta_designation_doctrine__severity_carve_out_reading, structural_duty_doctrine).
narrative_ontology:constraint_vindicates(beta_designation_doctrine__severity_carve_out_reading, non_contractable_safety).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Argue that contractual liability waivers cannot override physical safety imperatives. Advance a normative doctrine that certain domain-specific harms (death, financial ruin, critical infrastructure failure) are categorically non-waivable. Their position is that beta status is a disclosure mechanism for genuine testing phases, not a universal shield against liability for known defects in critical systems. They benefit from a ruling that restricts beta availability in high-stakes domains because it re-anchors liability allocation to harm severity rather than contractual form.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, consumer_safety_advocates, beneficiary,
    organized, generational, arbitrage, national).

% Enforce safety standards (FDA for medical devices, FAA for flight systems, SEC/OCC for financial services, NTSB for infrastructure). Each domain has discovered that beta designation can be abused to avoid accountability for foreseeable harm. They set the doctrine that beta is unavailable in their regulated domains regardless of the vendor's testing claim. Their authority derives from the standing legal principle that no contract can waive duty of care where systemic harm to third parties is at stake.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, regulatory_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Seek the ability to deploy experimental or incremental features under beta designation, with associated liability limitation, even in critical domains. They argue that the line between testing and production is economically arbitrary and that rapid iteration is the competitive necessity in software markets. The carve-out constrains their ability to limit liability in domains like medical software, aviation systems, and financial platforms. They remain subject to product liability even if they disclose the beta status.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, software_vendors, payer,
    institutional, biographical, constrained, global).

% Operate hospitals, power grids, financial networks, and transportation systems. They depend on software and cannot accept indefinite liability waivers for components that could fail catastrophically. They benefit from the rule that vendors cannot deploy untested or partially-tested code under beta designation in their critical paths. They also bear the cost of vetting software more carefully and of driving remediation when defects are discovered, because the liability cannot be waived.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, critical_infrastructure_operators, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(beta_designation_doctrine__severity_carve_out_reading, critical_infrastructure_operators, payer).

% Have no choice in the software that runs their hospital systems, their financial accounts, their aviation systems, or their power supply. They cannot opt out of beta-deployed code even if they know it carries experimental risk. The carve-out prevents vendors from using beta designation to contractually escape liability for harm to them. They benefit because harm cannot be pre-waived; instead, the constraint forces vendors to meet a higher bar before deployment in critical domains.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, end_users_critical_domains, beneficiary,
    powerless, immediate, trapped, universal).

% Operate in non-critical domains (productivity software, entertainment, social platforms) where beta designation remains available. They are not directly constrained by the carve-out because it does not apply to their market. However, they are excluded from a voice in the kernel contest about what counts as critical and whether the carve-out should expand to their domains. They would argue for narrower definitions of criticality and longer beta windows.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, competing_vendors_non_critical, excluded,
    institutional, biographical, mobile, global).

% Provide product liability and professional liability coverage for vendors. They assess risk based on contractual carve-outs and liability waivers. The constraint forces them to price risk higher for vendors serving critical domains because beta designation is unavailable as a risk-allocation mechanism. They observe the doctrine but do not set it; their pricing adjusts to reflect the constraint.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, liability_insurers, observer,
    institutional, biographical, analytical, national).

% Represent a jurisprudential tradition holding that contractual freedom is paramount and that parties should be able to allocate any risk they agree to, even in critical domains. They argue the carve-out violates freedom of contract and creates an unjustifiable categorical exception. They are excluded from direct enforcement of the doctrine but contest it through litigation and scholarship.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, legal_doctrine_traditionalists, excluded,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(beta_designation_doctrine__severity_carve_out_reading, regulatory_authorities).
narrative_ontology:fixing_cost_class(beta_designation_doctrine__severity_carve_out_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Separates domains by risk severity and coordinates liability allocation so that harm severity takes precedence over contractual form. Software vendors can still use beta designation in non-critical domains; the carve-out is domain-specific. This enables rapid iteration in low-stakes markets while forcing due diligence in high-stakes ones.
% TRANSFER_FUNCTION: Moves liability (and the cost of managing it) from vendors to regulatory authorities and critical infrastructure operators in high-stakes domains. Vendors cannot contractually transfer the risk of harm; instead, they must manage the risk through more rigorous testing and vetting before deployment. The doctrine transfers authority to define criticality from vendors to domain regulators.
% ABSENT_VOICES: Vendors in non-critical domains have limited incentive to contest the carve-out because it does not constrain them. However, upstream vendors (component manufacturers, platform providers) that sell into both critical and non-critical domains are partially constrained and excluded from the voice of smaller vendors whose only market is non-critical domains; their interests pull in different directions and the carve-out aggregates them all under one rule. The expansive_shield_reading represents the excluded vendor voice advocating for unlimited beta availability everywhere.
% DISAPPEARANCE_RATIONALE: If the carve-out vanished, vendors would deploy experimental software under beta designation in hospitals, financial systems, and power grids with full liability waivers. Critical infrastructure operators would face catastrophic risk they could not contractually escape. Regulatory authorities would lose the doctrinal anchor they use to enforce minimum safety standards. The deployment practices, insurance models, and regulatory enforcement would all reorganize around a contractual-freedom baseline.
% FOUNDING_PROBLEM: Beta designation was originally a disclosure mechanism for genuine testing phases, signaling incremental deployment and inviting user feedback. Over time, vendors in high-stakes domains (medical software, aviation systems, financial platforms) began using beta status indefinitely as a blanket liability waiver, creating a moral hazard where vendors could avoid accountability for foreseeable harm by simply labeling code as experimental. The doctrine emerged to restore beta to its original function: a time-bounded testing disclosure, not a universal liability shield.
% FOUNDING_PROBLEM_CORROBORATION: Regulatory authorities (FDA, FAA, SEC, OCC) attest the founding problem is actively present: vendors routinely attempt to deploy substantially untested code in critical systems under beta designation with liability waivers. Consumer safety advocates cite case law and regulatory enforcement where courts and agencies have rejected beta status as a defense in critical domains. Independent analysis of failure reports in medical and aviation software shows patterns of vendors claiming beta status for code that caused foreseeable harm. No corroborating source from the vendor community attests the problem remains live; vendors argue instead that the problem is overstated and that contractual freedom should permit beta deployment everywhere.
narrative_ontology:disappearance_verdict(beta_designation_doctrine__severity_carve_out_reading, world_rearranges).
narrative_ontology:founding_problem_status(beta_designation_doctrine__severity_carve_out_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(beta_designation_doctrine__severity_carve_out_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(beta_designation_doctrine__severity_carve_out_reading, 'none', 1).
narrative_ontology:epsilon_provenance(beta_designation_doctrine__severity_carve_out_reading, 0.31, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(beta_designation_doctrine__severity_carve_out_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
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
 *   Extractiveness is low (0.31 at 2026) because the constraint does not extract value from any stakeholder; instead, it reallocates liability and duty. Suppression is very low (0.18) because the constraint is enforced by regulatory authority and tort law, not by active coercive machinery — vendors simply cannot use beta as a valid defense in critical domains. Theater ratio is very low (0.08) because the constraint's function is direct and transparent: it separates domain criticality from contractual form. Accessibility collapse is very high (0.92) because once a domain is designated critical, the availability of beta as a liability shield completely collapses — there is no workaround, no contractual path around it. Resistance is moderate (0.58) because vendors mount real resistance through litigation and by pushing the boundary of what counts as critical, but the core constraint remains in place across major regulatory regimes. The measurement series traces the constraint's evolution: extractiveness rose from near-zero (1995, when beta was a pure disclosure) to 0.31 as vendors attempted to use it as a liability shield and regulators hardened the doctrine in response. Theater ratio fell sharply from 0.62 (1995, when beta appeared purely consensual) to 0.08 (2026, when its function as a domain-separator is clear). Suppression requirement rose as enforcement machinery (FDA approvals, FAA certifications, banking regulations) built up to prevent vendors from deploying critical software under beta.
 *
 * PERSPECTIVAL GAP:
 *   The vendor seat and the regulatory seat should compute very differently. From the vendor's perspective (institutional power, biographical horizon, constrained exit), the carve-out appears as an unfair restriction on contractual freedom and rapid iteration — they experience high directionality toward target (high d, high extraction). From the critical infrastructure operator's perspective (institutional power, generational horizon, constrained exit), the same rule appears as essential risk management — they experience low directionality (low d, subsidy). From the powerless end-user's perspective (trapped, immediate horizon, universal scope), the rule appears as protection against pre-waived harm — also subsidy. The engine should compute these divergences from the declared power, exit, and scope atoms; no override is needed.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are: regulatory authorities (who gain enforcement authority), critical infrastructure operators (who gain vendor accountability), and end-users in critical domains (who gain protection from pre-waived harm). Victims are: software vendors (who lose a liability-limitation mechanism and bear higher vetting costs). The constraint does not extract value in the traditional sense; instead, it reallocates duty and liability. Vendors cannot contractually transfer risk; they must manage it through design and testing. This is not a zero-sum transfer but a structural reallocation that imposes costs on vendors in critical domains while protecting downstream parties.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (beta designation misused indefinitely as liability shield in critical domains) is live and actively contested. The carve-out persists not because the problem is solved but because regulators and courts continue enforcing it against vendor attempts to expand beta availability. Theater ratio decline (0.62 → 0.08) indicates the constraint's function has clarified over time: it is no longer performative disclosure but enforced domain-separation. The constraint does not risk mandatrophy because harm severity remains a persistent structural reality — vendors cannot escape liability in life-safety domains regardless of contractual form. However, the boundary between critical and non-critical domains is contested (omega: domain_criticality_boundary) and the doctrine is vulnerable to conceptual revision if the legal tradition shifts on whether harm severity is non-contractable or merely normatively chosen (omega: harm_severity_vs_contractual_freedom).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domain_criticality_boundary,
    'What metric or test determines whether a domain qualifies as critical for the purposes of the carve-out, and does that boundary hold across jurisdictions and over time?',
    'Regulatory harmonization and comparative legal analysis: comparing how FDA, FAA, banking regulators, and infrastructure operators define criticality reveals whether the boundary is stable and coherent or contested and shifting. Future litigation over adjacent domains (AI systems, autonomous vehicles, medical IoT) will probe whether the carve-out applies or does not.',
    'If the boundary is contested, the constraint''s scope is ambiguous and vendors can argue beta should be available in newly-critical domains. If the boundary is stable and widely enforced, the constraint applies predictably and vendors cannot use marginal criticality arguments to escape it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_criticality_boundary, empirical, 'Whether domain criticality is coherently defined and durably enforced.').

omega_variable(
    harm_severity_vs_contractual_freedom,
    'Can harm severity genuinely override contractual freedom, or does the carve-out represent a value-preference masquerading as a natural constraint?',
    'This is a jurisprudential question. The severability of the carve-out depends on whether one believes harm severity is a constraint on contract (physical reality: certain harms are non-waivable because they affect third parties) or a normative policy choice (legal tradition: we choose to override contractual freedom for critical domains). The reading presented here assumes the former; the expansive_shield_reading assumes the latter.',
    'If harm severity is indeed a non-contractable constraint, the carve-out is a mountain and persists regardless of vendor preference. If it is a normative choice, the carve-out is a scaffold or temporary policy, subject to revision if the normative consensus shifts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(harm_severity_vs_contractual_freedom, conceptual, 'Whether the carve-out is structurally necessary (harm-based) or normatively chosen (value-based).').

omega_variable(
    natural_law_vs_constructed_doctrine,
    'Is the carve-out a discovered law of liability (harm severity constrains contracts naturally) or a constructed legal doctrine (regulators and courts built this interpretation)?',
    'Historical analysis: legal traditions and jurisdictions differ in whether they recognize a non-contractable safety duty. Some traditions ground duty in contract only; others ground it in tort and strict liability. No single universal answer exists across legal systems. The constraint is constructed by legal doctrine, not discovered in nature.',
    'If constructed, the carve-out is vulnerable to reversal or narrowing by legal evolution or political change. If natural, it is durable because it reflects an irreducible feature of harm causation. The authoring here assumes it is a (culturally anchored) natural constraint within a particular legal tradition, triggering the mountain classification and FSM evaluation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_doctrine, conceptual, 'Whether the carve-out is a discovered constraint or a constructed legal doctrine.').

omega_variable(
    beta_designation_kernel_ambiguity,
    'Is beta designation itself a liability mechanism or a disclosure mechanism, and does that distinction determine whether the carve-out applies?',
    'Textual and historical analysis of how beta status originated and how it is used in practice. If it began as pure disclosure (announcing testing phase) and vendors later appropriated it as a liability shield, the question is whether the original function is the true referent or whether the appropriated function now defines it. This is the core dispute between the three readings of the kernel.',
    'If beta is fundamentally a disclosure mechanism, the carve-out is correct: beta is unavailable in critical domains because disclosure alone cannot replace liability. If beta is fundamentally a liability shield, the carve-out is an overreach: vendors should be able to use it anywhere with proper disclosure. This omega sits at the center of the kernel contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beta_designation_kernel_ambiguity, conceptual, 'Whether beta designation is intrinsically a disclosure or a liability mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beta_designation_doctrine__severity_carve_out_reading, 1995, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beta_tr_t1995, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 1995, 0.62).
narrative_ontology:measurement_basis(beta_tr_t1995, projected).
narrative_ontology:measurement(beta_tr_t2005, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 2005, 0.45).
narrative_ontology:measurement_basis(beta_tr_t2005, observed).
narrative_ontology:measurement(beta_tr_t2012, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 2012, 0.22).
narrative_ontology:measurement_basis(beta_tr_t2012, observed).
narrative_ontology:measurement(beta_tr_t2018, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 2018, 0.12).
narrative_ontology:measurement_basis(beta_tr_t2018, observed).
narrative_ontology:measurement(beta_tr_t2023, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 2023, 0.09).
narrative_ontology:measurement_basis(beta_tr_t2023, observed).
narrative_ontology:measurement(beta_tr_t2026, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 2026, 0.08).
narrative_ontology:measurement_basis(beta_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(beta_be_t1995, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 1995, 0.08).
narrative_ontology:measurement_basis(beta_be_t1995, projected).
narrative_ontology:measurement(beta_be_t2005, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 2005, 0.18).
narrative_ontology:measurement_basis(beta_be_t2005, observed).
narrative_ontology:measurement(beta_be_t2012, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 2012, 0.26).
narrative_ontology:measurement_basis(beta_be_t2012, observed).
narrative_ontology:measurement(beta_be_t2018, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 2018, 0.29).
narrative_ontology:measurement_basis(beta_be_t2018, observed).
narrative_ontology:measurement(beta_be_t2023, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 2023, 0.3).
narrative_ontology:measurement_basis(beta_be_t2023, observed).
narrative_ontology:measurement(beta_be_t2026, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 2026, 0.31).
narrative_ontology:measurement_basis(beta_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(beta_su_t1995, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 1995, 0.05).
narrative_ontology:measurement_basis(beta_su_t1995, projected).
narrative_ontology:measurement(beta_su_t2005, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 2005, 0.08).
narrative_ontology:measurement_basis(beta_su_t2005, observed).
narrative_ontology:measurement(beta_su_t2012, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 2012, 0.13).
narrative_ontology:measurement_basis(beta_su_t2012, observed).
narrative_ontology:measurement(beta_su_t2018, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 2018, 0.17).
narrative_ontology:measurement_basis(beta_su_t2018, observed).
narrative_ontology:measurement(beta_su_t2023, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 2023, 0.18).
narrative_ontology:measurement_basis(beta_su_t2023, observed).
narrative_ontology:measurement(beta_su_t2026, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 2026, 0.18).
narrative_ontology:measurement_basis(beta_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(beta_designation_doctrine__severity_carve_out_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(beta_designation_doctrine__severity_carve_out_reading, 0.12).
narrative_ontology:affects_constraint(beta_designation_doctrine__severity_carve_out_reading, beta_designation_doctrine__expansive_shield_reading).
narrative_ontology:affects_constraint(beta_designation_doctrine__severity_carve_out_reading, beta_designation_doctrine__narrow_warning_reading).

% DUAL FORMULATION NOTE:
% The beta-designation-doctrine kernel decomposes into three structurally distinct constraint stories, one per reading. All three share the same kernel (beta's role in liability allocation) but diverge sharply on whether beta constitutes indefinite liability waiver, time-bounded testing disclosure, or categorical unavailability in critical domains. The severity_carve_out_reading presented here grounds itself in non-contractable harm duty and creates structural pressure on the other two readings by reframing the question as domain-dependent rather than universally contractual. Network edges link all three readings; comparative analysis across the constraint family reveals how the same kernel generates opposite classification outcomes depending on which axiom (contractual freedom vs. harm severity) is foundational.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
