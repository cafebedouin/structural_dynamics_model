% ============================================================================
% CONSTRAINT STORY: structural_adjustment_conditionalities__debtor_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_structural_adjustment_conditionalities__debtor_extraction_reading, []).

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
 *   constraint_id: structural_adjustment_conditionalities__debtor_extraction_reading
 *   human_readable: Structural Adjustment Conditionalities as Neo-Colonial Extraction Regime
 *   domain: international_political_economy/development_finance/institutional_economics
 *
 * SUMMARY:
 *   Structural adjustment conditionalities — the policy prescriptions
 *   attached to IMF/World Bank lending — are presented as technical
 *   coordination mechanisms to restore fiscal balance and market confidence.
 *   This reading (debtor_extraction_reading) treats them as a snare: a
 *   coercive extraction regime where the coordination story is cover for
 *   systematically transferring resources from debtor populations to
 *   transnational creditors. The constraint's persistence depends on active
 *   enforcement (conditionality compliance reviews, tranche release gates,
 *   debt sustainability analyses that assume adjustment works) and on
 *   suppressing alternatives (capital controls, industrial policy, unilateral
 *   restructuring, monetary sovereignty). The high theater ratio reflects the
 *   growing gap between the 'poverty reduction' rhetoric of PRSPs and the
 *   unchanged macroeconomic conditionalities underneath.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_adjustment_conditionalities__debtor_extraction_reading, 0.88).
domain_priors:suppression_score(structural_adjustment_conditionalities__debtor_extraction_reading, 0.92).
domain_priors:theater_ratio(structural_adjustment_conditionalities__debtor_extraction_reading, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 0.75).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_adjustment_conditionalities__debtor_extraction_reading, snare).
narrative_ontology:human_readable(structural_adjustment_conditionalities__debtor_extraction_reading, "Structural Adjustment Conditionalities as Neo-Colonial Extraction Regime").
narrative_ontology:topic_domain(structural_adjustment_conditionalities__debtor_extraction_reading, "international_political_economy/development_finance/institutional_economics").

domain_priors:requires_active_enforcement(structural_adjustment_conditionalities__debtor_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(structural_adjustment_conditionalities__debtor_extraction_reading, 'eeb63559-f829-49f0-9eac-ba022db63025').
narrative_ontology:cs_kernel_codification('eeb63559-f829-49f0-9eac-ba022db63025', formalized).
narrative_ontology:cs_authority_grounding('eeb63559-f829-49f0-9eac-ba022db63025', extraction).
narrative_ontology:cs_interpretation_layer_present('eeb63559-f829-49f0-9eac-ba022db63025').
narrative_ontology:cs_reading_relation('eeb63559-f829-49f0-9eac-ba022db63025', structural_adjustment_conditionalities__creditor_coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('eeb63559-f829-49f0-9eac-ba022db63025', structural_adjustment_conditionalities__hybrid_selectivity_reading, influences).
narrative_ontology:cs_axiom('eeb63559-f829-49f0-9eac-ba022db63025', foundational, conditionalities_are_extractive_by_design).
narrative_ontology:cs_axiom_status(conditionalities_are_extractive_by_design, holdable).
narrative_ontology:cs_axiom_grounding('eeb63559-f829-49f0-9eac-ba022db63025', conditionalities_are_extractive_by_design, empirically_contingent).
narrative_ontology:cs_axiom('eeb63559-f829-49f0-9eac-ba022db63025', foundational, creditor_seniority_is_neo_colonial_appropriation).
narrative_ontology:cs_axiom_status(creditor_seniority_is_neo_colonial_appropriation, holdable).
narrative_ontology:cs_axiom_grounding('eeb63559-f829-49f0-9eac-ba022db63025', creditor_seniority_is_neo_colonial_appropriation, deontological).
narrative_ontology:cs_reference_frame('eeb63559-f829-49f0-9eac-ba022db63025', breton_woods_conditional_loan_architecture).
narrative_ontology:cs_drift_state('eeb63559-f829-49f0-9eac-ba022db63025', contemporary_financialized_conditionality_regime, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('eeb63559-f829-49f0-9eac-ba022db63025', '').
narrative_ontology:cs_kernel_id(structural_adjustment_conditionalities__debtor_extraction_reading, structural_adjustment_conditionalities).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__debtor_extraction_reading, transnational_financial_capital).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__debtor_extraction_reading, creditor_banks).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__debtor_extraction_reading, multilateral_institution_staff).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__debtor_extraction_reading, export_oriented_corporations).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, debtor_state_populations).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, public_sector_workers).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, informal_economy_participants).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, rural_subsistence_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, debtor_governments).
narrative_ontology:constraint_vindicates(structural_adjustment_conditionalities__debtor_extraction_reading, creditor_seniority_doctrine).
narrative_ontology:constraint_vindicates(structural_adjustment_conditionalities__debtor_extraction_reading, market_fundamentalism_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and enforce conditionalities through loan agreements; rotate between multilateral institutions, central banks, and private finance; career advancement depends on 'successful' program implementation measured by fiscal targets not human outcomes
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, imf_world_bank_technocrats, agenda_setter,
    institutional, generational, arbitrage, global).

% Receive guaranteed repayment through conditionalities that prioritize debt service over domestic spending; use multilateral institutions as collection enforcement; can exit individual exposures but structurally benefit from the regime
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, creditor_banks, beneficiary,
    powerful, biographical, mobile, global).

% Gains privatized state assets, liberalized capital accounts, and deregulated markets through conditionalities; extracts value through arbitrage across newly opened economies; the regime creates the conditions for its own accumulation
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, transnational_financial_capital, beneficiary,
    powerful, biographical, arbitrage, global).

% Bear the full cost of austerity: health/education cuts, subsidy removal, wage freezes, public sector layoffs; no meaningful exit from the nation-state container; resistance met with repression or further conditionalities
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, debtor_state_populations, payer,
    powerless, biographical, trapped, national).

% Face mass layoffs, wage compression, pension cuts; some exit through migration (brain drain) but most are trapped by credential non-portability and family obligations; unions dismantled by labor market 'flexibility' conditionalities
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, public_sector_workers, payer,
    moderate, biographical, constrained, national).

% Absorb VAT increases, fuel subsidy removal, and currency devaluation without any social protection; no political voice in conditionalities negotiation; survival strategies criminalized by 'formalization' conditionalities
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, informal_economy_participants, payer,
    powerless, immediate, trapped, local).

% Lose land access through privatization conditionalities; seed/fertilizer subsidy removal destroys viability; identity fused to land makes exit unthinkable; resistance framed as 'anti-development'
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, rural_subsistence_communities, payer,
    powerless, generational, identity_locked, local).

% Formally sign conditionalities but under duress of balance-of-payments crisis; elite factions benefit from privatization kickbacks while bearing political cost of unrest; trapped between creditor demands and domestic legitimacy
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, debtor_governments, payer,
    organized, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(structural_adjustment_conditionalities__debtor_extraction_reading, debtor_governments, agenda_setter).

% Document extraction patterns, model counterfactuals, testify at tribunals; excluded from negotiation rooms; their evidence treated as 'political' not 'technical'
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, development_economists_critics, observer,
    analytical, generational, analytical, global).

% Organize resistance (Jubilee 2000, anti-austerity protests, climate justice); would redesign the architecture if admitted; systematically excluded from conditionalities design by 'technical competence' gatekeeping
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, global_south_movements, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Claims to coordinate creditor confidence and debtor fiscal sustainability through rule-based conditionality; in practice coordinates extraction by synchronizing creditor demands across multilaterals, bilaterals, and private creditors
% TRANSFER_FUNCTION: Moves fiscal space, public assets, policy autonomy, and social reproduction capacity from debtor populations to creditor balance sheets and transnational capital; debt service flows exceed new lending (net negative transfer) for decades
% ABSENT_VOICES: Debtor populations, rural communities, informal workers, and global South movements are structurally excluded from conditionalities design; they would demand debt cancellation, capital controls, and developmental state capacity but are kept out by 'technocratic' framing
% DISAPPEARANCE_RATIONALE: If conditionalities vanished overnight, debtor states would reclaim policy space for industrial policy, social protection, and capital management; creditor enforcement architecture would collapse; global finance would reorganize around sovereign autonomy not creditor seniority
% FOUNDING_PROBLEM: Post-1970s petrodollar recycling created unsustainable sovereign debt; conditionalities were built to ensure repayment without addressing the structural asymmetry of the lending itself
% FOUNDING_PROBLEM_CORROBORATION: UNCTAD, South Centre, and independent economists (e.g., Stiglitz, Ndikumana, Boyce) document that the original liquidity crisis became a solvency crisis manufactured by the conditionalities themselves; creditor institutions' own IEO evaluations admit programs failed their stated growth targets; no corroborating source outside the beneficiary set asserts the founding problem remains live
narrative_ontology:disappearance_verdict(structural_adjustment_conditionalities__debtor_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(structural_adjustment_conditionalities__debtor_extraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(structural_adjustment_conditionalities__debtor_extraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(structural_adjustment_conditionalities__debtor_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(structural_adjustment_conditionalities__debtor_extraction_reading, 0.88, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(structural_adjustment_conditionalities__debtor_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(structural_adjustment_conditionalities__debtor_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(structural_adjustment_conditionalities__debtor_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.88) is extremely high because net financial transfers have been negative for decades — debt service exceeds new lending — while conditionalities pry open economies for asset stripping. Suppression (0.92) is near-maximum because the regime survives only by blocking exits: capital controls prohibited, alternative development models delegitimized, debtor governments that resist face financial strangulation. Theater ratio (0.75) rose steadily as 'social dimensions' were added cosmetically while core extraction conditionalities (fiscal targets, privatization, liberalization) remained invariant. Accessibility collapse (0.85) reflects that once a state enters the conditionality regime, alternatives (default, capital controls, delinking) are treated as unthinkable by the epistemic community. Resistance (0.65) is substantial but fragmented — episodic uprisings (IMF riots, anti-austerity movements) lack coordinated alternative architecture.
 *
 * PERSPECTIVAL GAP:
 *   From the creditor_coordination_reading seat, the constraint appears as necessary discipline — the 'medicine' tastes bad but cures the disease. From the debtor_extraction_reading seat, the medicine IS the disease — the constraint creates the pathologies it claims to treat. The engine computes this divergence from the power/exit/beneficiary-victim structure: the same constraint is rope for the agenda_setter/beneficiary seats and snare for the payer seats. The hybrid_selectivity_reading occupies an unstable middle: it sees the extraction but treats it as 'misapplication' rather than structure.
 *
 * DIRECTIONALITY LOGIC:
 *   IMF/World Bank technocrats are agenda_setters with arbitrage-grade exit (revolving door to private finance) — they design the constraint but don't bear its costs. Creditor banks and transnational capital are beneficiaries with mobile/arbitrage exit — they collect the extraction and can reallocate instantly. Debtor populations are payers with trapped/identity_locked exit — they bear the full costs with no meaningful escape. Debtor governments are dual-positioned: formally agenda_setters (signing letters of intent) but substantively payers under duress, with constrained exit (political survival depends on compliance). The directionality derivation from these structural positions produces the extreme effective extraction on payer seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (1970s petrodollar recycling crisis) is dead — the original liquidity crisis was resolved by the 1990s, but the conditionalities architecture persisted and expanded. The mandate atrophied into pure extraction: the coordination function (restoring market access) was achieved for creditors but not for debtors, yet the constraint intensified rather than sunset. This is the classic mandatrophy pattern — the constraint's persistence is now explained only by the benefits it delivers to the agenda_setters and beneficiaries, not by any remaining coordination function. The 'enhanced structural adjustment facility' and PRSP rebranding were theatrical maintenance of an exhausted mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_boundary,
    'Is any genuine coordination function performed by conditionalities that could not be achieved by non-extractive alternatives (e.g., unconditional finance, debtor-owned development banks)?',
    'Counterfactual modeling: compare outcomes under conditional vs. unconditional finance for similarly situated debtors; historical cases where conditionality was waived (post-disaster, geopolitical exceptions)',
    'If a separable coordination core exists, the constraint is a tangled_rope not a snare; if all coordination is extractive, snare classification holds',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether the constraint''s coordination and extraction components are structurally separable or fused').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (financial sanctions, legal barriers, institutional exclusion) or internalized (technocratic consensus, epistemic capture of debtor elites, ''there is no alternative'' internalization)?',
    'Post-exit suppression trajectory: track debtor states that exited conditionality (default, paid off, political rupture) — does suppression persist through internalized policy frameworks?',
    'If substantially internalized, effective suppression is higher than structural measure suggests — the constraint reproduces itself through captured epistemic communities even after formal enforcement lapses',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the conditionality regime').

omega_variable(
    kernel_reading_framing,
    'Does the debtor_extraction_reading foreclose the creditor_coordination_reading within any single analytical framework, or do they coexist as competing readings of the same kernel?',
    'Test whether a single framework can simultaneously hold: (a) conditionalities are structurally extractive for debtors AND (b) conditionalities provide genuine coordination value for creditors. If yes, they coexist; if the extraction IS the coordination (creditor coordination THROUGH debtor extraction), then this reading forecloses the sibling''s core premise',
    'If forecloses, the kernel has a structural contradiction — no framework can hold both readings; if coexists_with, the kernel sustains pluralism; if influences, this reading''s evidence base structurally undermines the sibling''s legitimacy conditions',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Structural relationship between this reading and the creditor_coordination_reading sibling').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_adjustment_conditionalities__debtor_extraction_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stru_tr_t1980, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 1980, 0.45).
narrative_ontology:measurement(stru_tr_t1985, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 1985, 0.52).
narrative_ontology:measurement(stru_tr_t1990, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 1990, 0.58).
narrative_ontology:measurement(stru_tr_t1995, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 1995, 0.63).
narrative_ontology:measurement(stru_tr_t2000, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 2000, 0.68).
narrative_ontology:measurement(stru_tr_t2005, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 2005, 0.71).
narrative_ontology:measurement(stru_tr_t2010, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 2010, 0.72).
narrative_ontology:measurement(stru_tr_t2015, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 2015, 0.73).
narrative_ontology:measurement(stru_tr_t2020, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 2020, 0.74).
narrative_ontology:measurement(stru_tr_t2024, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 2024, 0.75).

% Extraction over time
narrative_ontology:measurement(stru_be_t1980, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 1980, 0.65).
narrative_ontology:measurement(stru_be_t1985, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 1985, 0.72).
narrative_ontology:measurement(stru_be_t1990, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 1990, 0.78).
narrative_ontology:measurement(stru_be_t1995, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 1995, 0.81).
narrative_ontology:measurement(stru_be_t2000, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 2000, 0.83).
narrative_ontology:measurement(stru_be_t2005, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 2005, 0.84).
narrative_ontology:measurement(stru_be_t2010, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 2010, 0.85).
narrative_ontology:measurement(stru_be_t2015, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 2015, 0.86).
narrative_ontology:measurement(stru_be_t2020, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 2020, 0.87).
narrative_ontology:measurement(stru_be_t2024, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 2024, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(stru_su_t1980, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 1980, 0.75).
narrative_ontology:measurement(stru_su_t1985, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 1985, 0.8).
narrative_ontology:measurement(stru_su_t1990, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 1990, 0.83).
narrative_ontology:measurement(stru_su_t1995, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 1995, 0.86).
narrative_ontology:measurement(stru_su_t2000, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 2000, 0.88).
narrative_ontology:measurement(stru_su_t2005, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 2005, 0.89).
narrative_ontology:measurement(stru_su_t2010, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 2010, 0.9).
narrative_ontology:measurement(stru_su_t2015, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 2015, 0.91).
narrative_ontology:measurement(stru_su_t2020, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 2020, 0.91).
narrative_ontology:measurement(stru_su_t2024, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 2024, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_adjustment_conditionalities__debtor_extraction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(structural_adjustment_conditionalities__debtor_extraction_reading, 0.15).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__debtor_extraction_reading, debt_sustainability_analysis_framework).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__debtor_extraction_reading, credit_rating_agency_sovereign_methodology).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__debtor_extraction_reading, capital_account_liberalization_regime).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__debtor_extraction_reading, investor_state_dispute_settlement).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__debtor_extraction_reading, multilateral_debt_restructuring_architecture).

% DUAL FORMULATION NOTE:
% Part of the structural_adjustment_conditionalities constraint family with creditor_coordination_reading and hybrid_selectivity_reading. This reading (debtor_extraction) has ε=0.88, snare classification, victims=debtor populations. The creditor_coordination_reading has ε≈0.25, rope classification, beneficiaries=creditor confidence. The hybrid_selectivity_reading has ε≈0.55, tangled_rope classification. They share the same nominal conditionalities but different structural referents — per ε-invariance, they are distinct constraints linked by network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(structural_adjustment_conditionalities__debtor_extraction_reading, organized, 0.75).
constraint_indexing:directionality_override(structural_adjustment_conditionalities__debtor_extraction_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
