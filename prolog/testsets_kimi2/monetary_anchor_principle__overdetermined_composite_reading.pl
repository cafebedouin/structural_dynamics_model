% ============================================================================
% CONSTRAINT STORY: monetary_anchor_principle__overdetermined_composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monetary_anchor_principle__overdetermined_composite_reading, []).

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
 *   constraint_id: monetary_anchor_principle__overdetermined_composite_reading
 *   human_readable: Overdetermined Composite Collapse of Bretton Woods Gold Anchor
 *   domain: monetary economics / political economy / international finance
 *
 * SUMMARY:
 *   This constraint story models the collapse of the Bretton Woods gold
 *   anchor as an overdetermined composite of structural pressuresâTriffin
 *   dilemma, Vietnam War deficits, Keynesian policy consensus, and
 *   technological capital mobilityâthat made the gold standard's collapse
 *   inevitable by the late 1960s. The standing arrangement under contest is
 *   the entangled policy space in which the US fiscal state gained capacity
 *   to run persistent deficits while foreign central banks and domestic
 *   inflation-bearing households absorbed the costs. The constraint is a
 *   tangled rope: it genuinely coordinated global liquidity and fiscal demand
 *   management, yet asymmetrically extracted purchasing power from trapped
 *   reserve holders and fixed-income households through the removal of the
 *   inflation constraint.
 *
 * KEY AGENTS:
 *   - US fiscal authority (agenda_setter/beneficiary): Gained fiscal space and global liquidity control; low directionality (beneficiary).
 *   - Foreign central banks (payer): Accumulated devaluing dollars; high directionality (target) with constrained exit.
 *   - Inflation-bearing public (payer): Domestic households paying implicit inflation tax; high directionality with constrained exit.
 *   - Hard-money constituency (excluded): Advocates for gold discipline sidelined by Keynesian consensus.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monetary_anchor_principle__overdetermined_composite_reading, 0.82).
domain_priors:suppression_score(monetary_anchor_principle__overdetermined_composite_reading, 0.72).
domain_priors:theater_ratio(monetary_anchor_principle__overdetermined_composite_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_anchor_principle__overdetermined_composite_reading, tangled_rope).
narrative_ontology:human_readable(monetary_anchor_principle__overdetermined_composite_reading, "Overdetermined Composite Collapse of Bretton Woods Gold Anchor").
narrative_ontology:topic_domain(monetary_anchor_principle__overdetermined_composite_reading, "monetary economics / political economy / international finance").

domain_priors:requires_active_enforcement(monetary_anchor_principle__overdetermined_composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monetary_anchor_principle__overdetermined_composite_reading, '84951b42-302d-4421-b6d2-25654787db11').
narrative_ontology:cs_kernel_codification('84951b42-302d-4421-b6d2-25654787db11', formalized).
narrative_ontology:cs_authority_grounding('84951b42-302d-4421-b6d2-25654787db11', lineage).
narrative_ontology:cs_interpretation_layer_present('84951b42-302d-4421-b6d2-25654787db11').
narrative_ontology:cs_reading_relation('84951b42-302d-4421-b6d2-25654787db11', monetary_anchor_principle__punctuated_swap_reading, coexists_with).
narrative_ontology:cs_reading_relation('84951b42-302d-4421-b6d2-25654787db11', monetary_anchor_principle__triffin_inevitability_reading, influences).
narrative_ontology:cs_axiom('84951b42-302d-4421-b6d2-25654787db11', foundational, monetary_anchor_validity_contingent_on_structural_compatibility).
narrative_ontology:cs_axiom_status(monetary_anchor_validity_contingent_on_structural_compatibility, holdable).
narrative_ontology:cs_axiom_grounding('84951b42-302d-4421-b6d2-25654787db11', monetary_anchor_validity_contingent_on_structural_compatibility, empirically_contingent).
narrative_ontology:cs_axiom('84951b42-302d-4421-b6d2-25654787db11', secondary, composite_causation_precludes_discrete_attribution).
narrative_ontology:cs_axiom_status(composite_causation_precludes_discrete_attribution, holdable).
narrative_ontology:cs_axiom_grounding('84951b42-302d-4421-b6d2-25654787db11', composite_causation_precludes_discrete_attribution, conventional).
narrative_ontology:cs_reference_frame('84951b42-302d-4421-b6d2-25654787db11', bretton_woods_gold_anchor).
narrative_ontology:cs_drift_state('84951b42-302d-4421-b6d2-25654787db11', late_1960s_collapse, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('84951b42-302d-4421-b6d2-25654787db11', '').
narrative_ontology:cs_kernel_id(monetary_anchor_principle__overdetermined_composite_reading, monetary_anchor_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__overdetermined_composite_reading, us_fiscal_authority).
narrative_ontology:constraint_victim(monetary_anchor_principle__overdetermined_composite_reading, foreign_central_banks).
narrative_ontology:constraint_victim(monetary_anchor_principle__overdetermined_composite_reading, inflation_bearing_public).
narrative_ontology:constraint_vindicates(monetary_anchor_principle__overdetermined_composite_reading, keynesian_macro_policy_consensus).
narrative_ontology:constraint_vindicates(monetary_anchor_principle__overdetermined_composite_reading, triffin_dilemma_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates the monetary-fiscal complex of the reserve-currency issuer; gained expanded fiscal space to fund Vietnam War spending and Great Society programs without gold-convertibility discipline; sets global liquidity and fiscal policy agendas while controlling the Bretton Woods enforcement machinery.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, us_fiscal_authority, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(monetary_anchor_principle__overdetermined_composite_reading, us_fiscal_authority, beneficiary).

% Accumulated dollar reserves under Bretton Woods; bore concentrated balance-sheet losses as dollar-gold convertibility eroded through the 1960s; trapped in dollar liquidity dependency that made coordinated exit via gold conversion individually costly and systemically destabilizing.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, foreign_central_banks, payer,
    institutional, generational, constrained, global).

% Domestic households and wage earners whose purchasing power eroded as the inflation constraint was removed to finance persistent fiscal deficits; subject to an implicit inflation tax with no legislative appropriation and limited capacity to denominate savings in gold or foreign currency.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, inflation_bearing_public, payer,
    moderate, biographical, constrained, national).

% Advocates for gold-backed monetary discipline and balanced fiscal policy; structurally sidelined by the Keynesian policy consensus and institutional momentum toward flexible fiscal capacity by the late 1960s; their policy preferences had no viable legislative or administrative path during the interval.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, hard_money_constituency, excluded,
    moderate, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(monetary_anchor_principle__overdetermined_composite_reading, us_fiscal_authority).
narrative_ontology:fixing_cost_class(monetary_anchor_principle__overdetermined_composite_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solved the global liquidity shortage inherent in the Triffin dilemma by allowing the reserve-currency issuer to run persistent deficits, while simultaneously enabling countercyclical fiscal policy and Vietnam War spending without immediate gold-constraint discipline.
% TRANSFER_FUNCTION: Transferred purchasing power from fixed-income households and foreign dollar-reserve holders to the US fiscal state through inflation and devaluation, in exchange for continued global liquidity provision and aggregate demand management.
% ABSENT_VOICES: Hard-money advocates and gold-standard defenders were structurally excluded from the Keynesian policy consensus by the late 1960s; foreign central banks bearing reserve losses were consulted in IMF forums but not empowered to veto the de facto transition.
% DISAPPEARANCE_RATIONALE: If the overdetermined composite vanishedâmeaning gold convertibility had been forcibly maintained despite Triffin pressures, Vietnam deficits, and capital mobilityâthe global liquidity system would have seized, US fiscal policy would have faced immediate retrenchment, and the Bretton Woods architecture would have collapsed into competing currency blocs rather than evolving into the fiat dollar standard.
% FOUNDING_PROBLEM: Postwar global liquidity shortage under gold-standard scarcity; need for a dominant reserve currency to finance reconstruction and trade; the Triffin dilemma inherent in that solution.
% FOUNDING_PROBLEM_CORROBORATION: The IMF and US Treasury attest the founding problem (liquidity shortage) was solved by the dollar standard. Hard-money advocates and later macro-historians attest the founding problem mutated into an extraction mechanism by the late 1960s. Corroboration from foreign central banksâFrench repudiation of dollar privilege and de Gaulle's gold campaignâsupports the shifted-function reading from outside the US beneficiary set.
narrative_ontology:disappearance_verdict(monetary_anchor_principle__overdetermined_composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(monetary_anchor_principle__overdetermined_composite_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monetary_anchor_principle__overdetermined_composite_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(monetary_anchor_principle__overdetermined_composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monetary_anchor_principle__overdetermined_composite_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monetary_anchor_principle__overdetermined_composite_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(monetary_anchor_principle__overdetermined_composite_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(monetary_anchor_principle__overdetermined_composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is high (0.82â0.85 by interval end) because the arrangement systematically transferred purchasing power from domestic households and foreign reserve holders to the fiscal state through inflation and devaluation. Suppression is substantial (0.72) because the constraint persisted through active enforcement: gold pool interventions, capital controls, legal-tender laws, and institutional exclusion of hard-money alternatives. Theater ratio rises to 0.6 by 1970 because the Bretton Woods forms were performatively maintainedâtwo-tier gold markets, rhetorical commitments to convertibilityâwhile the substance had shifted to de facto fiat. Accessibility collapse (0.68) reflects that once the overdetermined composite was entrenched, returning to gold discipline required reversing all causal streams simultaneously. Resistance (0.55) captures hard-money opposition and foreign central bank dissent (e.g., France) that was ultimately overcome.
 *
 * PERSPECTIVAL GAP:
 *   The US fiscal authority seat experiences the constraint as necessary coordination solving liquidity scarcity and enabling macro-stabilization; the foreign central bank and inflation-bearing public seats experience the same arrangement as structural extraction of their purchasing power. The engine computes this divergence from beneficiary/payer declarations and exit asymmetryâno tuning is required.
 *
 * DIRECTIONALITY LOGIC:
 *   The US fiscal authority is the declared beneficiary (collects seigniorage and fiscal space, controls enforcement, enjoys arbitrage-grade exit from the old rules) and therefore derives low directionality. Foreign central banks and the inflation-bearing public are declared victims (bear devaluation and inflation tax, with constrained or trapped exit) and therefore derive high directionality. The hard-money constituency is excluded rather than targeted, sitting outside the active transfer.
 *
 * MANDATROPHY ANALYSIS:
 *   The R5 genealogy interview prevents mislabeling this as pure extraction by documenting a live founding problem (postwar liquidity shortage) and contested status. The coordination function (global liquidity, countercyclical capacity) is genuine, which blocks snare classification. However, the transfer function (purchasing power from reserve holders and wage earners to the fiscal state) is asymmetric and actively enforced, which blocks rope classification. Tangled rope is the structurally accurate certification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_structural_delta,
    'This constraint instantiates the overdetermined_composite_reading of kernel monetary_anchor_principle; sibling readings (punctuated_swap_reading, triffin_inevitability_reading) assign different causal structures to the same transition. What classification changes if the causal structure is punctuated agency rather than structural overdetermination?',
    'Comparative analysis across the constraint family linking epsilon, directionality, and coordination/extraction balance to causal attribution.',
    'A punctuated-swap reading would raise agenda-setter discretion and lower structural inevitability, shifting classification toward snare or scaffold; a Triffin-only reading would narrow beneficiaries and victims to reserve-currency holders, reducing composite entanglement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Committer omega: sibling reading structural deltas for monetary anchor kernel.').

omega_variable(
    triffin_sufficiency_within_composite,
    'Is the Triffin dilemma alone sufficient to explain gold standard collapse, or only necessary within the overdetermined composite of Vietnam deficits, Keynesian consensus, and capital mobility?',
    'Counterfactual isolation of Triffin effects from concurrent fiscal and ideological pressures in the 1960s using balance-of-payments and reserve-coverage data.',
    'If Triffin is sufficient, this reading overstates composite complexity and the constraint approaches triffin_inevitability_reading; if necessary but insufficient, the tangled_rope classification with multiple causal streams holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(triffin_sufficiency_within_composite, empirical, 'Whether Triffin dilemma is sufficient or merely necessary within the composite.').

omega_variable(
    keynesian_consensus_internalization,
    'Was the abandonment of monetary discipline accepted because of internalized Keynesian policy consensus, or enforced through structural exclusion of hard-money alternatives?',
    'Examine policy-debate records, congressional testimony, and central-bank minutes for evidence of genuine ideological conversion versus elite closure.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggestsâthe target carries the suppression with them after exit; if purely structural, standard extraction model holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(keynesian_consensus_internalization, conceptual, 'Structural versus internalized suppression mechanism in the monetary regime transition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_anchor_principle__overdetermined_composite_reading, 1960, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ma_overdet_tr_t1960, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1960, 0.2).
narrative_ontology:measurement(ma_overdet_tr_t1963, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1963, 0.25).
narrative_ontology:measurement(ma_overdet_tr_t1966, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1966, 0.35).
narrative_ontology:measurement(ma_overdet_tr_t1968, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1968, 0.5).
narrative_ontology:measurement(ma_overdet_tr_t1970, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1970, 0.6).
narrative_ontology:measurement(ma_overdet_tr_t1971, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1971, 0.55).

% Extraction over time
narrative_ontology:measurement(ma_overdet_be_t1960, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1960, 0.45).
narrative_ontology:measurement(ma_overdet_be_t1963, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1963, 0.52).
narrative_ontology:measurement(ma_overdet_be_t1966, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1966, 0.62).
narrative_ontology:measurement(ma_overdet_be_t1968, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1968, 0.75).
narrative_ontology:measurement(ma_overdet_be_t1970, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1970, 0.82).
narrative_ontology:measurement(ma_overdet_be_t1971, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1971, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(ma_overdet_su_t1960, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1960, 0.5).
narrative_ontology:measurement(ma_overdet_su_t1963, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1963, 0.55).
narrative_ontology:measurement(ma_overdet_su_t1966, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1966, 0.65).
narrative_ontology:measurement(ma_overdet_su_t1968, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1968, 0.75).
narrative_ontology:measurement(ma_overdet_su_t1970, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1970, 0.8).
narrative_ontology:measurement(ma_overdet_su_t1971, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1971, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monetary_anchor_principle__overdetermined_composite_reading, resource_allocation).
narrative_ontology:affects_constraint(monetary_anchor_principle__overdetermined_composite_reading, punctuated_swap_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__overdetermined_composite_reading, triffin_inevitability_reading).

% DUAL FORMULATION NOTE:
% The monetary_anchor_principle kernel decomposes into three structurally distinct constraints: overdetermined_composite_reading (multicausal structural collapse), punctuated_swap_reading (discrete institutional choice), and triffin_inevitability_reading (monocausal structural logic). Each has distinct epsilon, beneficiary/victim structure, and causal attribution. Linked by network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
