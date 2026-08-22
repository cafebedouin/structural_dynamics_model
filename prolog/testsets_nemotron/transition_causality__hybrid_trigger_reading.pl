% ============================================================================
% CONSTRAINT STORY: transition_causality__hybrid_trigger_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_transition_causality__hybrid_trigger_reading, []).

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
 *   constraint_id: transition_causality__hybrid_trigger_reading
 *   human_readable: Bretton Woods Collapse — Hybrid Trigger Reading
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   The Bretton Woods system (1944-1973) instantiated a structural
 *   contradiction: the Triffin Dilemma meant the reserve currency issuer (US)
 *   had to run deficits to supply world liquidity, but those deficits
 *   undermined the gold convertibility that anchored confidence. This reading
 *   holds that the contradiction accumulated slowly (1944-1965) but required
 *   contingent triggers — the Vietnam War fiscal expansion (1965-1968) and
 *   French gold redemption runs (1965-1968) — to actualize collapse in
 *   1971-1973. Without these triggers, a managed transition or reformed
 *   gold-exchange standard might have persisted longer. The constraint is the
 *   hybrid causal structure itself: slow-burning structural necessity plus
 *   contingent trigger events.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transition_causality__hybrid_trigger_reading, 0.68).
domain_priors:suppression_score(transition_causality__hybrid_trigger_reading, 0.55).
domain_priors:theater_ratio(transition_causality__hybrid_trigger_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transition_causality__hybrid_trigger_reading, tangled_rope).
narrative_ontology:human_readable(transition_causality__hybrid_trigger_reading, "Bretton Woods Collapse — Hybrid Trigger Reading").
narrative_ontology:topic_domain(transition_causality__hybrid_trigger_reading, "monetary_economics/political_economy/international_finance").

domain_priors:requires_active_enforcement(transition_causality__hybrid_trigger_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(transition_causality__hybrid_trigger_reading, 'ff75cb40-d6d9-44ab-a910-dd0a9f88f3bc').
narrative_ontology:cs_kernel_codification('ff75cb40-d6d9-44ab-a910-dd0a9f88f3bc', formalized).
narrative_ontology:cs_authority_grounding('ff75cb40-d6d9-44ab-a910-dd0a9f88f3bc', lineage).
narrative_ontology:cs_interpretation_layer_present('ff75cb40-d6d9-44ab-a910-dd0a9f88f3bc').
narrative_ontology:cs_reading_relation('ff75cb40-d6d9-44ab-a910-dd0a9f88f3bc', transition_causality__contingent_choice_reading, influences).
narrative_ontology:cs_reading_relation('ff75cb40-d6d9-44ab-a910-dd0a9f88f3bc', transition_causality__overdetermined_collapse_reading, coexists_with).
narrative_ontology:cs_axiom('ff75cb40-d6d9-44ab-a910-dd0a9f88f3bc', foundational, structural_necessity_requires_contingent_trigger).
narrative_ontology:cs_axiom_status(structural_necessity_requires_contingent_trigger, holdable).
narrative_ontology:cs_axiom_grounding('ff75cb40-d6d9-44ab-a910-dd0a9f88f3bc', structural_necessity_requires_contingent_trigger, empirically_contingent).
narrative_ontology:cs_axiom('ff75cb40-d6d9-44ab-a910-dd0a9f88f3bc', foundational, triffin_dilemma_is_slow_burning_not_immediate).
narrative_ontology:cs_axiom_status(triffin_dilemma_is_slow_burning_not_immediate, holdable).
narrative_ontology:cs_axiom_grounding('ff75cb40-d6d9-44ab-a910-dd0a9f88f3bc', triffin_dilemma_is_slow_burning_not_immediate, empirically_contingent).
narrative_ontology:cs_reference_frame('ff75cb40-d6d9-44ab-a910-dd0a9f88f3bc', bretton_woods_adjustable_peg_system).
narrative_ontology:cs_drift_state('ff75cb40-d6d9-44ab-a910-dd0a9f88f3bc', post_nixon_shock, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('ff75cb40-d6d9-44ab-a910-dd0a9f88f3bc', '').
narrative_ontology:cs_kernel_id(transition_causality__hybrid_trigger_reading, transition_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transition_causality__hybrid_trigger_reading, us_treasury).
narrative_ontology:constraint_beneficiary(transition_causality__hybrid_trigger_reading, international_monetary_fund).
narrative_ontology:constraint_beneficiary(transition_causality__hybrid_trigger_reading, global_banks).
narrative_ontology:constraint_victim(transition_causality__hybrid_trigger_reading, foreign_central_banks).
narrative_ontology:constraint_victim(transition_causality__hybrid_trigger_reading, gold_standard_advocates).
narrative_ontology:constraint_victim(transition_causality__hybrid_trigger_reading, emerging_market_economies).
narrative_ontology:constraint_vindicates(transition_causality__hybrid_trigger_reading, triffin_dilemma_structural_necessity).
narrative_ontology:constraint_vindicates(transition_causality__hybrid_trigger_reading, contingent_trigger_mechanism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets dollar convertibility policy and controls the gold window. Benefits from seigniorage and the dollar's reserve currency status, which allows the US to run persistent deficits without immediate balance-of-payments discipline. Can delay adjustment by leveraging the system's structural dependence on dollar liquidity.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, us_treasury, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(transition_causality__hybrid_trigger_reading, us_treasury, beneficiary).

% Acts as system stabilizer and rule-setter for par values and adjustment. Gains institutional relevance and operational scope from managing the Bretton Woods adjustable-peg system. Its surveillance and lending functions expand precisely because the system generates recurrent crises.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, international_monetary_fund, beneficiary,
    institutional, generational, arbitrage, global).

% Intermediate the Eurodollar market that grows up around the system's dollar shortage. Profit from maturity transformation and regulatory arbitrage in offshore dollar deposits. Their business model depends on the system's structural contradiction — dollar liquidity demand exceeding US gold backing.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, global_banks, beneficiary,
    organized, biographical, mobile, global).

% Hold dollar reserves as system anchor but face accelerating loss of gold convertibility value. The Triffin Dilemma forces them to accumulate dollars to support world trade, while US deficits erode the gold backing. France's gold redemption runs (1965-1968) exemplify the constrained exit: they can redeem but only at the cost of system destabilization.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, foreign_central_banks, payer,
    institutional, biographical, constrained, national).

% Argue for strict gold convertibility and discipline but are marginalized within the institutional framework. Their policy alternative is structurally excluded because it would require US surplus contraction — politically impossible for the agenda setter. They bear the cost of watching the nominal anchor dissolve without institutional voice.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, gold_standard_advocates, excluded,
    moderate, generational, trapped, global).

% Depend on dollar-denominated trade finance and IMF conditionality. Bear the adjustment costs of system instability — inflation importation, capital flight, and conditional lending — without influence over the reserve currency issuer's policies. Their exit is blocked by dollar dependence and lack of alternatives.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, emerging_market_economies, payer,
    powerless, biographical, trapped, regional).

% Analyze the causal structure of the collapse from archival and quantitative evidence. Their reading shapes the kernel's contested interpretations but they do not collect rents or bear adjustment costs from the constraint itself.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, academic_monetary_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a stable nominal anchor (dollar-gold convertibility at $35/oz) and multilateral adjustment mechanism (IMF par values) enabling post-war trade reconstruction and global liquidity provision without a single country bearing full adjustment burden.
% TRANSFER_FUNCTION: Transferred seigniorage and adjustment costs from the US (reserve issuer) to foreign central banks (reserve holders) via persistent US deficits financed by dollar accumulation abroad. The Vietnam War fiscal shock accelerated this transfer by expanding deficits without gold backing. French gold runs (1965-1968) forced explicit recognition of the transfer.
% ABSENT_VOICES: Gold standard advocates and emerging market economies were structurally excluded from the decision to close the gold window (August 1971). The former would have demanded convertibility restoration; the latter would have opposed floating rates that imported inflation. Both were absent from the Camp David negotiations.
% DISAPPEARANCE_RATIONALE: If the hybrid trigger constraint (Triffin accumulation + contingent triggers) disappeared, the post-war monetary order would not have collapsed in 1971-1973. A different trigger timing or policy response could have produced a managed transition to floating rates or a reformed gold-exchange standard. The world monetary system rearranged precisely because structural contradictions met contingent triggers.
% FOUNDING_PROBLEM: Post-war reconstruction required a stable international monetary system that could provide global liquidity without repeating the deflationary bias of the interwar gold standard or the competitive devaluations of the 1930s.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (post-war reconstruction and stable trade finance) was substantially solved by 1960 — European recovery complete, trade expanding, current accounts roughly balanced. The system persisted a decade past its founding purpose. Corroborated by Triffin's 1960 congressional testimony, IMF annual reports 1960-1970, and Eichengreen's 'Globalizing Capital' — all outside the US Treasury/IMF beneficiary set.
narrative_ontology:disappearance_verdict(transition_causality__hybrid_trigger_reading, world_rearranges).
narrative_ontology:founding_problem_status(transition_causality__hybrid_trigger_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(transition_causality__hybrid_trigger_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(transition_causality__hybrid_trigger_reading, 'none', 1).
narrative_ontology:epsilon_provenance(transition_causality__hybrid_trigger_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(transition_causality__hybrid_trigger_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(transition_causality__hybrid_trigger_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(transition_causality__hybrid_trigger_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.15 to 0.72 as US deficits widen and gold coverage falls from 55% to 22%. The Vietnam War shock (1965+) and French runs (1965-1968) are the contingent triggers that convert structural pressure into acute crisis. Suppression requirement peaks at 0.60 (1968) when the London Gold Pool collapses and the two-tier market is imposed — active enforcement of the gold price against market pressure. Theater ratio remains low (0.22) because the system's coordination function (trade finance, IMF adjustment) was genuine throughout; the extraction is not performative but structural. The claimed type (tangled_rope) reflects genuine coordination (liquidity provision) combined with asymmetric extraction (seigniorage transfer to US).
 *
 * PERSPECTIVAL GAP:
 *   From the US Treasury seat, the arrangement is coordination (providing world liquidity) with manageable costs. From foreign central bank seats, it is extraction (forced accumulation of depreciating claims). From emerging market seats, it is extraction with no exit. The engine computes this divergence from power/exit/role declarations — the hybrid trigger reading does not adjudicate it but declares the structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   US Treasury is the primary beneficiary (seigniorage, deficit financing without adjustment) but also agenda setter (controls gold window, sets policy). IMF and global banks benefit institutionally and commercially from system management and Eurodollar intermediation. Foreign central banks are primary payers — they accumulate depreciating dollar reserves with constrained exit (redeeming gold destabilizes the system they depend on). Gold standard advocates are excluded — their policy alternative is structurally blocked. Emerging markets are trapped payers bearing adjustment costs without voice. The analytical observer sees the full causal structure but bears no material cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-war reconstruction) was dead by 1960 but the arrangement persisted 13 more years, accumulating extraction. The mandate atrophied into rent-seeking — the system continued because beneficiaries (US, IMF, global banks) captured its operation. The hybrid trigger reading identifies the contingent events that finally forced collapse, preventing indefinite mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    trigger_necessity_vs_sufficiency,
    'Were the Vietnam War fiscal shock and French gold runs necessary triggers, or would other triggers have produced collapse within a similar window?',
    'Counterfactual simulation: model the system with Vietnam War spending removed but Triffin accumulation continuing; assess whether gold runs or other political events (e.g., UK devaluation 1967, German mark revaluation 1969) would have triggered collapse by 1975.',
    'If triggers were sufficient but not necessary, the hybrid reading converges toward overdetermined_collapse_reading. If triggers were necessary and specific, the hybrid reading''s causal claim is structurally distinct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trigger_necessity_vs_sufficiency, empirical, 'Whether the identified contingent triggers were necessary for the timing of collapse or merely the historically realized path among many sufficient triggers.').

omega_variable(
    triffin_as_extraction_mechanism,
    'Is the Triffin Dilemma itself an extraction mechanism (structural seigniorage transfer) or a genuine coordination cost of reserve currency provision?',
    'Compare US net international investment position and seigniorage gains against the liquidity services provided to the rest of world, 1950-1970. If net transfer exceeds service value, Triffin is extractive; if service value exceeds transfer, it is coordination cost.',
    'If Triffin is extractive, the base extractiveness trajectory starts earlier and the constraint is more snare-like. If coordination cost, the early low extractiveness is genuine and the constraint is more tangled_rope-like.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(triffin_as_extraction_mechanism, conceptual, 'Whether the structural contradiction itself constitutes extraction or is the price of coordination.').

omega_variable(
    kernel_reading_relations,
    'What is the structural relationship between the hybrid_trigger_reading and its sibling readings (contingent_choice_reading, overdetermined_collapse_reading)?',
    'Analyze whether the core premises logically foreclose, coexist with, or influence each other within a single analytical framework. Hybrid trigger: structural necessity + contingent triggers. Contingent choice: policy agency could have avoided. Overdetermined: multiple contradictions make collapse inevitable.',
    'Determines cs_structure.reading_relations classification. If hybrid forecloses contingent choice (structural necessity contradicts policy agency), relation is ''forecloses''. If they coexist as competing historiographical positions, relation is ''coexists_with''. If hybrid''s structural pressure influences contingent choice''s policy space, relation is ''influences''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_relations, conceptual, 'Structural relationship between this kernel reading and its siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transition_causality__hybrid_trigger_reading, 1944, 1973).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tran_tr_t1944, transition_causality__hybrid_trigger_reading, theater_ratio, 1944, 0.05).
narrative_ontology:measurement(tran_tr_t1950, transition_causality__hybrid_trigger_reading, theater_ratio, 1950, 0.08).
narrative_ontology:measurement(tran_tr_t1958, transition_causality__hybrid_trigger_reading, theater_ratio, 1958, 0.12).
narrative_ontology:measurement(tran_tr_t1960, transition_causality__hybrid_trigger_reading, theater_ratio, 1960, 0.15).
narrative_ontology:measurement(tran_tr_t1965, transition_causality__hybrid_trigger_reading, theater_ratio, 1965, 0.18).
narrative_ontology:measurement(tran_tr_t1968, transition_causality__hybrid_trigger_reading, theater_ratio, 1968, 0.2).
narrative_ontology:measurement(tran_tr_t1971, transition_causality__hybrid_trigger_reading, theater_ratio, 1971, 0.22).
narrative_ontology:measurement(tran_tr_t1973, transition_causality__hybrid_trigger_reading, theater_ratio, 1973, 0.22).

% Extraction over time
narrative_ontology:measurement(tran_be_t1944, transition_causality__hybrid_trigger_reading, base_extractiveness, 1944, 0.15).
narrative_ontology:measurement(tran_be_t1950, transition_causality__hybrid_trigger_reading, base_extractiveness, 1950, 0.22).
narrative_ontology:measurement(tran_be_t1958, transition_causality__hybrid_trigger_reading, base_extractiveness, 1958, 0.35).
narrative_ontology:measurement(tran_be_t1960, transition_causality__hybrid_trigger_reading, base_extractiveness, 1960, 0.42).
narrative_ontology:measurement(tran_be_t1965, transition_causality__hybrid_trigger_reading, base_extractiveness, 1965, 0.58).
narrative_ontology:measurement(tran_be_t1968, transition_causality__hybrid_trigger_reading, base_extractiveness, 1968, 0.67).
narrative_ontology:measurement(tran_be_t1971, transition_causality__hybrid_trigger_reading, base_extractiveness, 1971, 0.72).
narrative_ontology:measurement(tran_be_t1973, transition_causality__hybrid_trigger_reading, base_extractiveness, 1973, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(tran_su_t1944, transition_causality__hybrid_trigger_reading, suppression_requirement, 1944, 0.2).
narrative_ontology:measurement(tran_su_t1950, transition_causality__hybrid_trigger_reading, suppression_requirement, 1950, 0.25).
narrative_ontology:measurement(tran_su_t1958, transition_causality__hybrid_trigger_reading, suppression_requirement, 1958, 0.35).
narrative_ontology:measurement(tran_su_t1960, transition_causality__hybrid_trigger_reading, suppression_requirement, 1960, 0.42).
narrative_ontology:measurement(tran_su_t1965, transition_causality__hybrid_trigger_reading, suppression_requirement, 1965, 0.52).
narrative_ontology:measurement(tran_su_t1968, transition_causality__hybrid_trigger_reading, suppression_requirement, 1968, 0.6).
narrative_ontology:measurement(tran_su_t1971, transition_causality__hybrid_trigger_reading, suppression_requirement, 1971, 0.58).
narrative_ontology:measurement(tran_su_t1973, transition_causality__hybrid_trigger_reading, suppression_requirement, 1973, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transition_causality__hybrid_trigger_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(transition_causality__hybrid_trigger_reading, 0.15).
narrative_ontology:affects_constraint(transition_causality__hybrid_trigger_reading, triffin_dilemma_structural_necessity).
narrative_ontology:affects_constraint(transition_causality__hybrid_trigger_reading, eurodollar_market_emergence).
narrative_ontology:affects_constraint(transition_causality__hybrid_trigger_reading, imf_surveillance_expansion).
narrative_ontology:affects_constraint(transition_causality__hybrid_trigger_reading, nixon_shock_1971).

% DUAL FORMULATION NOTE:
% Part of the transition_causality kernel family with contingent_choice_reading and overdetermined_collapse_reading. The three readings differ in their causal attribution: hybrid assigns causal weight to both structural accumulation and contingent triggers; contingent_choice assigns primary weight to policy agency; overdetermined assigns primary weight to structural overdetermination. All three share the same referent (Bretton Woods collapse 1971-1973) but instantiate different constraints with different ε values and stakeholder structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(transition_causality__hybrid_trigger_reading, institutional, 0.15).
constraint_indexing:directionality_override(transition_causality__hybrid_trigger_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
