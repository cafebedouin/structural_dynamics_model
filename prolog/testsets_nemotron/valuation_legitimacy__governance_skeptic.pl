% ============================================================================
% CONSTRAINT STORY: valuation_legitimacy__governance_skeptic
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-25
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_valuation_legitimacy__governance_skeptic, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: valuation_legitimacy__governance_skeptic
 *   human_readable: Tesla Governance: Dual-Class Control with 10:1 Voting Ratio
 *   domain: corporate_finance/technology_governance
 *
 * SUMMARY:
 *   Tesla's dual-class share structure (10:1 voting ratio) gives Elon Musk
 *   82.4% voting control with 42% equity ownership. The structure is defended
 *   as enabling long-term vision (FSD, Optimus, energy transition) insulated
 *   from quarterly capital market pressure. This reading argues the
 *   coordination function has atrophied: Tesla is now profitable,
 *   cash-generating, and S&P 500-included, yet governance protections for
 *   minority shareholders have weakened (no independent comp/nominating
 *   committees, corporate opportunity renouncement for Musk, board dominated
 *   by Musk allies). The $1.75T peak valuation prices in Musk's private
 *   benefits of control — the constraint extracts from Class A shareholders
 *   while coordinating for Musk's multi-company empire. The constraint is a
 *   tangled rope: genuine coordination (long-term R&D insulation) coexists
 *   with asymmetric extraction (control premium, conflicted allocation,
 *   accountability vacuum).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(valuation_legitimacy__governance_skeptic, 0.78).
domain_priors:suppression_score(valuation_legitimacy__governance_skeptic, 0.72).
domain_priors:theater_ratio(valuation_legitimacy__governance_skeptic, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(valuation_legitimacy__governance_skeptic, extractiveness, 0.78).
narrative_ontology:constraint_metric(valuation_legitimacy__governance_skeptic, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(valuation_legitimacy__governance_skeptic, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(valuation_legitimacy__governance_skeptic, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(valuation_legitimacy__governance_skeptic, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(valuation_legitimacy__governance_skeptic, tangled_rope).
narrative_ontology:human_readable(valuation_legitimacy__governance_skeptic, "Tesla Governance: Dual-Class Control with 10:1 Voting Ratio").
narrative_ontology:topic_domain(valuation_legitimacy__governance_skeptic, "corporate_finance/technology_governance").

domain_priors:requires_active_enforcement(valuation_legitimacy__governance_skeptic).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(valuation_legitimacy__governance_skeptic, 'f532928c-6e39-4e04-8db7-8676e4c366b5').
narrative_ontology:cs_kernel_codification('f532928c-6e39-4e04-8db7-8676e4c366b5', formalized).
narrative_ontology:cs_authority_grounding('f532928c-6e39-4e04-8db7-8676e4c366b5', extraction).
narrative_ontology:cs_interpretation_layer_present('f532928c-6e39-4e04-8db7-8676e4c366b5').
narrative_ontology:cs_reading_relation('f532928c-6e39-4e04-8db7-8676e4c366b5', valuation_legitimacy__dcf_fundamentalist, coexists_with).
narrative_ontology:cs_reading_relation('f532928c-6e39-4e04-8db7-8676e4c366b5', valuation_legitimacy__real_options_technologist, influences).
narrative_ontology:cs_reading_relation('f532928c-6e39-4e04-8db7-8676e4c366b5', valuation_legitimacy__musk_cult_believer, forecloses).
narrative_ontology:cs_axiom('f532928c-6e39-4e04-8db7-8676e4c366b5', foundational, minority_shareholder_protection_is_necessary_for_valuation_legitimacy).
narrative_ontology:cs_axiom_status(minority_shareholder_protection_is_necessary_for_valuation_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('f532928c-6e39-4e04-8db7-8676e4c366b5', minority_shareholder_protection_is_necessary_for_valuation_legitimacy, deontological).
narrative_ontology:cs_axiom('f532928c-6e39-4e04-8db7-8676e4c366b5', foundational, control_premium_without_accountability_is_extraction).
narrative_ontology:cs_axiom_status(control_premium_without_accountability_is_extraction, holdable).
narrative_ontology:cs_axiom_grounding('f532928c-6e39-4e04-8db7-8676e4c366b5', control_premium_without_accountability_is_extraction, empirically_contingent).
narrative_ontology:cs_reference_frame('f532928c-6e39-4e04-8db7-8676e4c366b5', founder_control_for_mission_protection).
narrative_ontology:cs_drift_state('f532928c-6e39-4e04-8db7-8676e4c366b5', post_s_and_p_500_inclusion, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f532928c-6e39-4e04-8db7-8676e4c366b5', '').
narrative_ontology:cs_kernel_id(valuation_legitimacy__governance_skeptic, valuation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(valuation_legitimacy__governance_skeptic, elon_musk).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__governance_skeptic, early_class_b_holders).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__governance_skeptic, tesla_board_musk_aligned).
narrative_ontology:constraint_victim(valuation_legitimacy__governance_skeptic, class_a_public_shareholders).
narrative_ontology:constraint_victim(valuation_legitimacy__governance_skeptic, index_fund_investors).
narrative_ontology:constraint_victim(valuation_legitimacy__governance_skeptic, retail_shareholders).
narrative_ontology:constraint_vindicates(valuation_legitimacy__governance_skeptic, dual_class_enables_long_term_vision).
narrative_ontology:constraint_vindicates(valuation_legitimacy__governance_skeptic, founder_control_creates_value).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls 82.4% of voting power with 42% equity through Class B shares (10:1 vote ratio). Serves as CEO of Tesla and SpaceX, CEO/CTO of X, and leads Neuralink, The Boring Company, and xAI. Time allocation across 5+ companies creates structural conflicts — e.g., Terafab benefits Tesla/SpaceX but Musk decides resource allocation. Tesla charter renounces corporate opportunities for Musk. Collects private benefits of control priced into $1.75T valuation.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, elon_musk, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(valuation_legitimacy__governance_skeptic, elon_musk, beneficiary).

% Early investors and insiders holding Class B shares with 10:1 voting rights. Benefit from Musk's control premium and alignment with founder vision. Can exit via secondary markets with minimal friction. Their voting power dilutes only if they sell Class B shares.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, early_class_b_holders, beneficiary,
    powerful, biographical, mobile, global).

% Board lacks independent compensation/nominating committees (controlled company exemptions). Directors have personal/financial ties to Musk (Kimbal Musk, Ira Ehrenpreis, James Murdoch, etc.). Benefit from board fees and insider access while rubber-stamping Musk's agenda. Exit requires resigning from a prestigious, lucrative position.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, tesla_board_musk_aligned, beneficiary,
    institutional, biographical, constrained, global).

% Hold Class A shares with 1 vote each — collectively 17.6% voting power despite 58% equity. No say in director elections, executive compensation, or major transactions. Valuation prices in Musk's private benefits of control, not their shareholder value. Exit means selling at a price that already discounts governance deficiency.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, class_a_public_shareholders, payer,
    powerless, biographical, constrained, global).

% BlackRock, Vanguard, State Street hold ~20% of Tesla but cannot vote against Musk's agenda due to dual-class structure. Excluded from governance dialogue despite fiduciary duty to beneficiaries. Constrained exit: index mandates require holding; active engagement is structurally futile.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, index_fund_investors, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(valuation_legitimacy__governance_skeptic, index_fund_investors, excluded).

% Individual holders of Class A shares. No governance rights, high volatility exposure. Many hold due to belief in Musk narrative rather than financial analysis. Mobile exit but often behaviorally locked by identity alignment with Musk vision.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, retail_shareholders, payer,
    powerless, immediate, mobile, global).

% Regulatory and judicial oversight bodies. SEC reviews disclosures; Delaware courts adjudicate fiduciary duty claims (e.g., 2022 Musk compensation package voided). Can impose structural remedies but move slowly and face political constraints.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, sec_and_delaware_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Dual-class structure coordinates long-term capital allocation under founder vision, insulating R&D from quarterly pressure. Enables bets on FSD, Optimus, energy storage, and vertical integration that public markets would otherwise penalize.
% TRANSFER_FUNCTION: Moves governance control and private benefits of control from Class A shareholders (58% equity, 17.6% votes) to Musk and Class B holders (42% equity, 82.4% votes). Transfers risk of Musk's divided attention and conflicted resource allocation to public shareholders. Transfers valuation premium (control premium) to insiders.
% ABSENT_VOICES: Future shareholders who will inherit the governance structure without having consented to it. Potential directors with genuine independence who are never nominated. Creditors and counterparties who rely on board independence for contractual protections. Voices excluded by controlled-company exemptions from exchange listing standards.
% DISAPPEARANCE_RATIONALE: If dual-class structure and controlled-company exemptions vanished overnight, Tesla would need independent board committees, Musk's compensation would face shareholder votes, corporate opportunity renouncement would be challengeable, and resource allocation across Musk's empire would require arm's-length terms. The $1.75T valuation would reprice to reflect governance risk discount.
% FOUNDING_PROBLEM: Early Tesla faced existential funding risk and pressure to prioritize short-term profitability over capital-intensive R&D (Model 3 production hell, 2017-2018). Founder control was justified as protection against short-termist capital markets that would kill the mission.
% FOUNDING_PROBLEM_CORROBORATION: Musk and board attest founding problem is live (FSD, Optimus, energy transition require insulation). Institutional investors (CalPERS, Norges Bank) and governance experts (Harvard Law School Forum, ISS) attest founding problem is substantially solved — Tesla is profitable, cash-rich, S&P 500 member — and structure now serves extraction. Delaware Chancery Court voided 2018 compensation plan as excessive, corroborating shifted function.
narrative_ontology:disappearance_verdict(valuation_legitimacy__governance_skeptic, world_rearranges).
narrative_ontology:founding_problem_status(valuation_legitimacy__governance_skeptic, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(valuation_legitimacy__governance_skeptic, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(valuation_legitimacy__governance_skeptic, 'none', 1).
narrative_ontology:epsilon_provenance(valuation_legitimacy__governance_skeptic, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(valuation_legitimacy__governance_skeptic_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(valuation_legitimacy__governance_skeptic, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(valuation_legitimacy__governance_skeptic_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.78) because the control premium is large and growing — the wedge between voting power and equity ownership has widened as Musk's empire expanded. Suppression (0.72) reflects active maintenance: controlled-company exemptions, charter provisions renouncing corporate opportunities, board capture, and exclusion of shareholder proposals. Theater ratio (0.45) rising: governance theater (annual meetings, proxy statements) masks the reality that no mechanism can challenge Musk's agenda. Accessibility collapse (0.58) moderate: alternatives exist (sell shares, litigation, regulatory pressure) but are costly or ineffective. Resistance (0.62) significant: shareholder lawsuits, proxy fights, SEC actions, and Delaware court rulings show active pushback.
 *
 * PERSPECTIVAL GAP:
 *   From Musk's seat, the constraint is genuine coordination enabling civilization-scale projects. From Class A shareholders' seat, it is enforced extraction with no accountability. The engine computes this divergence: Musk's d is low (beneficiary), public shareholders' d is high (target). The gap is structural — the same dual-class mechanism produces opposite experiences because exit options and power are asymmetric by design.
 *
 * DIRECTIONALITY LOGIC:
 *   Musk is structural beneficiary (d ~ 0.1): collects control premium, sets agenda, faces no accountability. Early Class B holders and aligned board are secondary beneficiaries (d ~ 0.2-0.3). Class A public shareholders, index funds, and retail are targets (d ~ 0.8-0.9): bear extraction, constrained exit, no voice. Index funds are organized but structurally excluded — their power is neutralized by the dual-class architecture. SEC/courts are analytical observers (d = 0.5): they see the structure but their remedial power is slow and uncertain.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protection from short-termism during existential risk) was live in 2010-2018. By 2020, Tesla achieved profitability, scale, and index inclusion — the founding problem is substantially solved. Yet the constraint intensified (extraction rose from 0.55 to 0.78, theater from 0.28 to 0.45). This is mandatrophy: the mandate (founder control for mission protection) has outlived its function, but the structure persists and deepens extraction. The controlled-company exemptions and corporate opportunity renouncement are not sunset — they are entrenched.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_persistence,
    'Is the founding problem (insulation from short-termism for capital-intensive R&D) still live, or has it been resolved by Tesla''s achieved scale and profitability?',
    'Compare current R&D intensity and capital allocation horizon against 2010-2018 baseline. If Tesla''s current projects (FSD, Optimus, 4680, Megapack) require similar insulation, the founding problem persists. If capital allocation now resembles a mature industrial, the problem is resolved.',
    'If founding problem is dead, the dual-class structure is pure mandatrophy — extraction without coordination justification. If live, the tangled rope classification holds: coordination function coexists with extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_persistence, empirical, 'Whether the original coordination justification for dual-class structure remains operative.').

omega_variable(
    control_premium_quantification,
    'What portion of Tesla''s valuation premium over automotive peers is attributable to Musk''s private benefits of control versus genuine optionality?',
    'Event study of governance-related announcements (e.g., 2022 compensation void, 2023 board changes, Musk time allocation disclosures). Compare to controlled-company peers (Meta, Google, Snap) and single-class auto peers (Toyota, Ford).',
    'A large control premium component supports extraction reading. A small component supports coordination reading (premium is for optionality, not control).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(control_premium_quantification, empirical, 'Decomposition of valuation premium into governance vs. technology components.').

omega_variable(
    cross_company_allocation_mechanism,
    'How are resources (engineering talent, capital, IP, procurement) allocated across Musk''s 5+ companies, and who decides?',
    'Discovery in shareholder litigation (e.g., Tornetta v. Musk), SEC examination of related-party transactions, or voluntary disclosure of inter-company service agreements.',
    'If allocation is decided by Musk unilaterally with no arm''s-length terms, extraction is confirmed. If independent committees or market pricing govern, coordination function is stronger.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cross_company_allocation_mechanism, empirical, 'Governance of resource allocation across Musk''s corporate empire.').

omega_variable(
    index_fund_voice_futility,
    'Do index funds'' structural inability to influence Tesla governance represent a systemic market failure, or is passive acceptance the rational equilibrium?',
    'Analyze index fund voting records at Tesla vs. single-class peers. Survey stewardship teams on engagement strategies. Model counterfactual: if index funds could vote, would outcomes change?',
    'If index funds are rationally passive, suppression is lower (exit is chosen). If they are structurally silenced, suppression is higher (voice is suppressed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(index_fund_voice_futility, conceptual, 'Whether organized shareholders'' exclusion is active suppression or rational non-participation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(valuation_legitimacy__governance_skeptic, 2010, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(valu_tr_t2010, valuation_legitimacy__governance_skeptic, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(valu_tr_t2013, valuation_legitimacy__governance_skeptic, theater_ratio, 2013, 0.22).
narrative_ontology:measurement(valu_tr_t2017, valuation_legitimacy__governance_skeptic, theater_ratio, 2017, 0.28).
narrative_ontology:measurement(valu_tr_t2020, valuation_legitimacy__governance_skeptic, theater_ratio, 2020, 0.35).
narrative_ontology:measurement(valu_tr_t2022, valuation_legitimacy__governance_skeptic, theater_ratio, 2022, 0.4).
narrative_ontology:measurement(valu_tr_t2025, valuation_legitimacy__governance_skeptic, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(valu_be_t2010, valuation_legitimacy__governance_skeptic, base_extractiveness, 2010, 0.35).
narrative_ontology:measurement(valu_be_t2013, valuation_legitimacy__governance_skeptic, base_extractiveness, 2013, 0.42).
narrative_ontology:measurement(valu_be_t2017, valuation_legitimacy__governance_skeptic, base_extractiveness, 2017, 0.55).
narrative_ontology:measurement(valu_be_t2020, valuation_legitimacy__governance_skeptic, base_extractiveness, 2020, 0.68).
narrative_ontology:measurement(valu_be_t2022, valuation_legitimacy__governance_skeptic, base_extractiveness, 2022, 0.73).
narrative_ontology:measurement(valu_be_t2025, valuation_legitimacy__governance_skeptic, base_extractiveness, 2025, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(valu_su_t2010, valuation_legitimacy__governance_skeptic, suppression_requirement, 2010, 0.4).
narrative_ontology:measurement(valu_su_t2013, valuation_legitimacy__governance_skeptic, suppression_requirement, 2013, 0.45).
narrative_ontology:measurement(valu_su_t2017, valuation_legitimacy__governance_skeptic, suppression_requirement, 2017, 0.55).
narrative_ontology:measurement(valu_su_t2020, valuation_legitimacy__governance_skeptic, suppression_requirement, 2020, 0.62).
narrative_ontology:measurement(valu_su_t2022, valuation_legitimacy__governance_skeptic, suppression_requirement, 2022, 0.68).
narrative_ontology:measurement(valu_su_t2025, valuation_legitimacy__governance_skeptic, suppression_requirement, 2025, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(valuation_legitimacy__governance_skeptic, resource_allocation).
narrative_ontology:affects_constraint(valuation_legitimacy__governance_skeptic, valuation_legitimacy__dcf_fundamentalist).
narrative_ontology:affects_constraint(valuation_legitimacy__governance_skeptic, valuation_legitimacy__real_options_technologist).
narrative_ontology:affects_constraint(valuation_legitimacy__governance_skeptic, valuation_legitimacy__musk_cult_believer).
narrative_ontology:affects_constraint(valuation_legitimacy__governance_skeptic, tesla_compensation_package_2018).
narrative_ontology:affects_constraint(valuation_legitimacy__governance_skeptic, musk_multi_company_resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint family (valuation_legitimacy kernel) decomposes the single label 'Tesla valuation' into four structurally distinct readings with divergent ε values. governance_skeptic (this story) has high ε (0.78) because governance architecture is the extraction mechanism. dcf_fundamentalist has lower ε (valuation gap from cash flow skepticism). real_options_technologist has moderate ε (optionality is real but hard to price). musk_cult_believer has near-zero ε (valuation is justified by founder genius). The ε-invariance principle requires separate stories — a single ε cannot capture the contested referent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(valuation_legitimacy__governance_skeptic, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
