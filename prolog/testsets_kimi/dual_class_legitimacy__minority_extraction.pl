% ============================================================================
% CONSTRAINT STORY: dual_class_legitimacy__minority_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dual_class_legitimacy__minority_extraction, []).

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
 *   constraint_id: dual_class_legitimacy__minority_extraction
 *   human_readable: Dual-Class Controlled Company Governance Extraction
 *   domain: corporate_governance/securities_law/organizational_economics
 *
 * SUMMARY:
 *   Dual-class stock structures concentrate voting control in
 *   founder-insiders through super-voting shares while distributing economic
 *   risk to public minority shareholders. This constraint story instantiates
 *   the minority_extraction reading of the dual_class_legitimacy kernel: the
 *   claim that minority shareholders are entitled to governance proportional
 *   to capital and risk borne, and that the current structure extracts
 *   governance value from Class A holders through controlled-company
 *   exemptions that strip mandatory protections. The story is authored from a
 *   seat that acknowledges the founder_stewardship coordination story but
 *   treats it as increasingly overshadowed by asymmetric extraction.
 *
 * KEY AGENTS:
 *   - founder_controllers: Primary beneficiary/agenda_setter (institutional/arbitrage) â captures control premium and entrenchment value
 *   - public_minority_shareholders: Primary target (moderate/constrained) â bears economic risk without proportional voice
 *   - index_fund_managers: Secondary target (institutional/constrained) â trapped by tracking mandates, unable to divest or vote effectively
 *   - activist_investors: Excluded voice (powerful/constrained) â structurally blocked from governance participation by super-voting
 *   - securities_regulators: Analytical observer (institutional/analytical) â permits structure under disclosure framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dual_class_legitimacy__minority_extraction, 0.78).
domain_priors:suppression_score(dual_class_legitimacy__minority_extraction, 0.65).
domain_priors:theater_ratio(dual_class_legitimacy__minority_extraction, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, extractiveness, 0.78).
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dual_class_legitimacy__minority_extraction, tangled_rope).
narrative_ontology:human_readable(dual_class_legitimacy__minority_extraction, "Dual-Class Controlled Company Governance Extraction").
narrative_ontology:topic_domain(dual_class_legitimacy__minority_extraction, "corporate_governance/securities_law/organizational_economics").

domain_priors:requires_active_enforcement(dual_class_legitimacy__minority_extraction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dual_class_legitimacy__minority_extraction, '78cb9a41-90d0-4345-a6c7-820747cb4926').
narrative_ontology:cs_kernel_codification('78cb9a41-90d0-4345-a6c7-820747cb4926', formalized).
narrative_ontology:cs_authority_grounding('78cb9a41-90d0-4345-a6c7-820747cb4926', lineage).
narrative_ontology:cs_interpretation_layer_present('78cb9a41-90d0-4345-a6c7-820747cb4926').
narrative_ontology:cs_reading_relation('78cb9a41-90d0-4345-a6c7-820747cb4926', dual_class_legitimacy__founder_stewardship, coexists_with).
narrative_ontology:cs_reading_relation('78cb9a41-90d0-4345-a6c7-820747cb4926', dual_class_legitimacy__disclosure_consent, influences).
narrative_ontology:cs_axiom('78cb9a41-90d0-4345-a6c7-820747cb4926', foundational, governance_proportional_to_risk).
narrative_ontology:cs_axiom_status(governance_proportional_to_risk, holdable).
narrative_ontology:cs_axiom_grounding('78cb9a41-90d0-4345-a6c7-820747cb4926', governance_proportional_to_risk, deontological).
narrative_ontology:cs_axiom('78cb9a41-90d0-4345-a6c7-820747cb4926', foundational, dual_class_entrenchment_extractive).
narrative_ontology:cs_axiom_status(dual_class_entrenchment_extractive, holdable).
narrative_ontology:cs_axiom_grounding('78cb9a41-90d0-4345-a6c7-820747cb4926', dual_class_entrenchment_extractive, empirically_contingent).
narrative_ontology:cs_reference_frame('78cb9a41-90d0-4345-a6c7-820747cb4926', proportional_governance_default).
narrative_ontology:cs_drift_state('78cb9a41-90d0-4345-a6c7-820747cb4926', contemporary_dual_class_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('78cb9a41-90d0-4345-a6c7-820747cb4926', '').
narrative_ontology:cs_kernel_id(dual_class_legitimacy__minority_extraction, dual_class_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__minority_extraction, founder_controllers).
narrative_ontology:constraint_victim(dual_class_legitimacy__minority_extraction, public_minority_shareholders).
narrative_ontology:constraint_victim(dual_class_legitimacy__minority_extraction, index_fund_managers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold super-voting shares that confer disproportionate board control relative to their economic stake. They draft and maintain corporate charter provisions that entrench dual-class structures, benefit from controlled-company exemptions that waive independent-committee and say-on-pay requirements, and capture private benefits of control including related-party transaction approval and merger blocking.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, founder_controllers, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(dual_class_legitimacy__minority_extraction, founder_controllers, beneficiary).

% Hold Class A shares bearing full economic risk and residual claim, but with voting power that is negligible relative to capital contributed. They cannot nominate directors, cannot force charter amendments, and depend on founder-controlled boards for oversight. Exit is limited to selling shares into a market that prices in a governance discount.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, public_minority_shareholders, payer,
    moderate, biographical, constrained, global).

% Manage passive funds mandated to hold dual-class mega-caps at market weight to minimize tracking error. They bear fiduciary duties to beneficiaries but lack sufficient voting power to enforce governance standards. They file shareholder proposals that are routinely defeated by super-voting insiders, and cannot divest without violating benchmark mandates.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, index_fund_managers, payer,
    institutional, biographical, constrained, global).

% Would acquire stakes and wage proxy contests to unwind dual-class structures or force independent oversight, but are structurally blocked by super-voting provisions that make proxy victory mathematically impossible. They are vocal in public policy debates but excluded from the controlled company's actual governance conversation.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, activist_investors, excluded,
    powerful, biographical, constrained, global).

% SEC and stock exchange regulators that permit dual-class listings under disclosure-based frameworks. They evaluate whether investors are adequately informed of governance risks, but do not mandate proportional voting rights. They observe the extraction structure without intervening to alter charter terms.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, securities_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dual_class_legitimacy__minority_extraction, founder_controllers).
narrative_ontology:fixing_cost_class(dual_class_legitimacy__minority_extraction, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrates control in founding insiders to insulate management from short-term market pressures and enable long-horizon strategic execution without activist interference.
% TRANSFER_FUNCTION: Moves governance rights, control premiums, and waiver of mandatory protective provisions from public minority shareholders and index funds to founder-insiders, while economic risk remains broadly distributed.
% ABSENT_VOICES: Activist investors and governance advocates who would demand one-share-one-vote standards are structurally excluded from controlled-company governance; prospective public shareholders who avoid dual-class listings on principle are not in the room when charter terms are set at IPO.
% DISAPPEARANCE_RATIONALE: If dual-class charter provisions and controlled-company exemptions disappeared overnight, founder control would collapse to economic stake, proxy contests would become viable, independent committees and say-on-pay would become mandatory rather than waivable, and the market for corporate control would reprice governance premiums across the exchange-listed universe.
% FOUNDING_PROBLEM: Founders feared short-term market pressures and activist interference would derail long-term mission-driven company building after IPO, and sought to preserve visionary leadership against dispersed shareholder myopia.
% FOUNDING_PROBLEM_CORROBORATION: Founders and venture capitalists attest the problem remains live, citing the need for founder control to preserve corporate vision. Academic governance researchers, the Council of Institutional Investors, and proxy advisory firms attest that the problem is substantially solved by other mechanisms and the arrangement persists primarily for entrenchment; legislative testimony and independent empirical studies from outside the beneficiary set support the shifted-function reading.
narrative_ontology:disappearance_verdict(dual_class_legitimacy__minority_extraction, world_rearranges).
narrative_ontology:founding_problem_status(dual_class_legitimacy__minority_extraction, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dual_class_legitimacy__minority_extraction, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dual_class_legitimacy__minority_extraction, 'none', 1).
narrative_ontology:epsilon_provenance(dual_class_legitimacy__minority_extraction, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dual_class_legitimacy__minority_extraction_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dual_class_legitimacy__minority_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dual_class_legitimacy__minority_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because governance rights carry substantial value and are systematically transferred from risk-bearing public capital to non-risk-bearing insider control. Suppression is moderate-high (0.65): the constraint persists through charter provisions, controlled-company exemptions, and exchange listing standards rather than participant preference. Theater ratio is moderate (0.45): the 'long-term stewardship' narrative retains some genuine coordination content but is increasingly performative as empirical evidence of value destruction accumulates. Accessibility collapse is moderate (0.60): alternatives (single-class investment) exist in the broad market but collapse once capital is committed to a dual-class vehicle. Resistance is moderate (0.45): persistent but structurally defeated advocacy from proxy advisors, academics, and activists. Measurements share a single time grid (0â25) to prevent temporal misalignment artifacts.
 *
 * PERSPECTIVAL GAP:
 *   The founder_controllers seat experiences the constraint as legitimate institutional architecture protecting mission-driven enterprise; the engine will compute this as low directionality, low effective extraction, possibly rope-like. The public_minority_shareholders and index_fund_managers seats experience the same structure as extraction of governance rights; the engine will compute high directionality and high effective extraction, approaching snare. The divergence between these computed seats is the measurement the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Founder_controllers are beneficiaries (low d): they collect control premiums, entrenchment rents, and related-party discretion while bearing limited economic risk. Public_minority_shareholders and index_fund_managers are victims (high d): they pay through governance discount, waived protective provisions, and blocked proxy contests. Securities_regulators sit near symmetric: they permit the structure under a disclosure paradigm but do not collect its rents. Activist_investors are excluded rather than coordinated; their exclusion is the enforcement object.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling by requiring both beneficiaries and victims for tangled_rope: the founder_stewardship reading would identify only beneficiaries (rope-like coordination), while a pure minority-extraction reading would identify only victims (snare-like extraction). The tangled_rope classification captures that the same legal structure genuinely coordinates long-term capital commitment while simultaneously extracting governance value. If the coordination function atrophied completely (e.g., founder control becomes purely dynastic with no operational role), the constraint would degrade toward piton; if the extraction were removed (mandatory sunset of super-voting), it would collapse toward single-class equity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the dual-class structure a legitimate coordination mechanism for long-term value creation, or an extractive arrangement that transfers governance value from public shareholders to insiders?',
    'Long-term event study comparing dual-class and single-class matched firms, combined with founder-control sunset adoption rates and related-party transaction frequency.',
    'If empirically indeterminate or value-destructive, the minority_extraction reading strengthens; if founder control demonstrably produces superior long-term returns without excess extraction, the founder_stewardship reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'Contest between coordination and extraction readings of the same kernel').

omega_variable(
    controlled_company_exemption_naturalness,
    'Are controlled-company exemptions a natural outgrowth of corporate contractarianism, or a constructed carve-out that enables extraction?',
    'Historical analysis of exemption adoption and lobbying records; comparative corporate law analysis across jurisdictions.',
    'If constructed via targeted lobbying, the constraint''s naturality collapses and its extraction profile rises; if organic to corporate law evolution, the legitimacy of the waiver framework persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(controlled_company_exemption_naturalness, conceptual, 'Whether the legal exemption is natural or constructed').

omega_variable(
    index_fund_trapped_exit,
    'Does index fund tracking mandate constitute structural trapping that amplifies extraction, or is it a voluntary investment constraint?',
    'Analysis of fund prospectuses and benchmark inclusion rules; measurement of governance discount pass-through to beneficiaries.',
    'If trapped, effective extraction for the index fund seat is higher than for mobile retail investors; if voluntary, the seat is less structurally victimized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(index_fund_trapped_exit, empirical, 'Whether index fund investment is structurally trapped').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dual_class_legitimacy__minority_extraction, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dual_class_minority_ex_tr_t0, dual_class_legitimacy__minority_extraction, theater_ratio, 0, 0.2).
narrative_ontology:measurement(dual_class_minority_ex_tr_t5, dual_class_legitimacy__minority_extraction, theater_ratio, 5, 0.28).
narrative_ontology:measurement(dual_class_minority_ex_tr_t10, dual_class_legitimacy__minority_extraction, theater_ratio, 10, 0.35).
narrative_ontology:measurement(dual_class_minority_ex_tr_t15, dual_class_legitimacy__minority_extraction, theater_ratio, 15, 0.4).
narrative_ontology:measurement(dual_class_minority_ex_tr_t20, dual_class_legitimacy__minority_extraction, theater_ratio, 20, 0.43).
narrative_ontology:measurement(dual_class_minority_ex_tr_t25, dual_class_legitimacy__minority_extraction, theater_ratio, 25, 0.45).

% Extraction over time
narrative_ontology:measurement(dual_class_minority_ex_be_t0, dual_class_legitimacy__minority_extraction, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(dual_class_minority_ex_be_t5, dual_class_legitimacy__minority_extraction, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(dual_class_minority_ex_be_t10, dual_class_legitimacy__minority_extraction, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(dual_class_minority_ex_be_t15, dual_class_legitimacy__minority_extraction, base_extractiveness, 15, 0.68).
narrative_ontology:measurement(dual_class_minority_ex_be_t20, dual_class_legitimacy__minority_extraction, base_extractiveness, 20, 0.73).
narrative_ontology:measurement(dual_class_minority_ex_be_t25, dual_class_legitimacy__minority_extraction, base_extractiveness, 25, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(dual_class_minority_ex_su_t0, dual_class_legitimacy__minority_extraction, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(dual_class_minority_ex_su_t5, dual_class_legitimacy__minority_extraction, suppression_requirement, 5, 0.54).
narrative_ontology:measurement(dual_class_minority_ex_su_t10, dual_class_legitimacy__minority_extraction, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(dual_class_minority_ex_su_t15, dual_class_legitimacy__minority_extraction, suppression_requirement, 15, 0.61).
narrative_ontology:measurement(dual_class_minority_ex_su_t20, dual_class_legitimacy__minority_extraction, suppression_requirement, 20, 0.63).
narrative_ontology:measurement(dual_class_minority_ex_su_t25, dual_class_legitimacy__minority_extraction, suppression_requirement, 25, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dual_class_legitimacy__minority_extraction, resource_allocation).
narrative_ontology:affects_constraint(dual_class_legitimacy__minority_extraction, dual_class_legitimacy__founder_stewardship).
narrative_ontology:affects_constraint(dual_class_legitimacy__minority_extraction, dual_class_legitimacy__disclosure_consent).

% DUAL FORMULATION NOTE:
% This constraint is the minority_extraction reading of the dual_class_legitimacy kernel. It decomposes from the colloquial label 'dual-class stock structure' by isolating the claim that the structure extracts governance value from public shareholders. Sibling readings instantiate founder_stewardship and disclosure_consent claims from the same kernel. Each reading has distinct beneficiaries, victims, and epsilon values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
