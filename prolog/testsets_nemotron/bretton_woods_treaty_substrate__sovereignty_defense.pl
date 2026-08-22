% ============================================================================
% CONSTRAINT STORY: bretton_woods_treaty_substrate__sovereignty_defense
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bretton_woods_treaty_substrate__sovereignty_defense, []).

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
 *   constraint_id: bretton_woods_treaty_substrate__sovereignty_defense
 *   human_readable: Bretton Woods Treaty Substrate — Sovereignty Defense Reading
 *   domain: international_political_economy/monetary_history/institutional_design
 *
 * SUMMARY:
 *   The Bretton Woods treaty substrate (Articles I–VIII, the IMF Articles of
 *   Agreement) is a single legal text that instantiates multiple structurally
 *   distinct constraints depending on which articles are read as binding and
 *   which beneficiaries are recognized. This story instantiates the
 *   SOVEREIGNTY_DEFENSE reading: the treaty creates constraints on external
 *   monetary discipline (fixed par values, IMF surveillance, gold
 *   convertibility) that function to preserve national monetary sovereignty —
 *   but the sovereignty preserved is asymmetrically the United States', while
 *   non-reserve-currency states bear the discipline. The gold anchor,
 *   presented as a stabilizer, operates as a snare: it disciplines the
 *   periphery while the center enjoys exorbitant privilege. The coordination
 *   function (stable anchor, multilateral clearing) is real but hybridized
 *   with extraction — a tangled rope.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bretton_woods_treaty_substrate__sovereignty_defense, 0.68).
domain_priors:suppression_score(bretton_woods_treaty_substrate__sovereignty_defense, 0.72).
domain_priors:theater_ratio(bretton_woods_treaty_substrate__sovereignty_defense, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, extractiveness, 0.68).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bretton_woods_treaty_substrate__sovereignty_defense, tangled_rope).
narrative_ontology:human_readable(bretton_woods_treaty_substrate__sovereignty_defense, "Bretton Woods Treaty Substrate — Sovereignty Defense Reading").
narrative_ontology:topic_domain(bretton_woods_treaty_substrate__sovereignty_defense, "international_political_economy/monetary_history/institutional_design").

domain_priors:requires_active_enforcement(bretton_woods_treaty_substrate__sovereignty_defense).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bretton_woods_treaty_substrate__sovereignty_defense, '02684705-318f-4e12-b195-266aa6d6e3cb').
narrative_ontology:cs_kernel_codification('02684705-318f-4e12-b195-266aa6d6e3cb', formalized).
narrative_ontology:cs_authority_grounding('02684705-318f-4e12-b195-266aa6d6e3cb', extraction).
narrative_ontology:cs_interpretation_layer_present('02684705-318f-4e12-b195-266aa6d6e3cb').
narrative_ontology:cs_reading_relation('02684705-318f-4e12-b195-266aa6d6e3cb', bretton_woods_treaty_substrate__keynesian_embedded_liberalism, coexists_with).
narrative_ontology:cs_reading_relation('02684705-318f-4e12-b195-266aa6d6e3cb', bretton_woods_treaty_substrate__neoliberal_convertibility, influences).
narrative_ontology:cs_axiom('02684705-318f-4e12-b195-266aa6d6e3cb', foundational, national_monetary_sovereignty_requires_asymmetric_anchor).
narrative_ontology:cs_axiom_status(national_monetary_sovereignty_requires_asymmetric_anchor, holdable).
narrative_ontology:cs_axiom_grounding('02684705-318f-4e12-b195-266aa6d6e3cb', national_monetary_sovereignty_requires_asymmetric_anchor, instrumental).
narrative_ontology:cs_axiom('02684705-318f-4e12-b195-266aa6d6e3cb', secondary, exorbitant_privilege_is_system_stabilizer_not_extraction).
narrative_ontology:cs_axiom_status(exorbitant_privilege_is_system_stabilizer_not_extraction, holdable).
narrative_ontology:cs_axiom_grounding('02684705-318f-4e12-b195-266aa6d6e3cb', exorbitant_privilege_is_system_stabilizer_not_extraction, conventional).
narrative_ontology:cs_reference_frame('02684705-318f-4e12-b195-266aa6d6e3cb', bretton_woods_par_value_system_1944).
narrative_ontology:cs_drift_state('02684705-318f-4e12-b195-266aa6d6e3cb', nixon_shock_1971, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('02684705-318f-4e12-b195-266aa6d6e3cb', '').
narrative_ontology:cs_kernel_id(bretton_woods_treaty_substrate__sovereignty_defense, bretton_woods_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__sovereignty_defense, united_states_treasury_federal_reserve).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__sovereignty_defense, multinational_corporations_dollar_zone).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__sovereignty_defense, international_monetary_fund_staff).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__sovereignty_defense, non_reserve_currency_central_banks).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__sovereignty_defense, developing_country_governments).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__sovereignty_defense, labor_movements_fixed_exchange_regimes).
narrative_ontology:constraint_vindicates(bretton_woods_treaty_substrate__sovereignty_defense, national_monetary_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(bretton_woods_treaty_substrate__sovereignty_defense, exorbitant_privilege_thesis).
narrative_ontology:constraint_vindicates(bretton_woods_treaty_substrate__sovereignty_defense, asymmetric_adjustment_burden_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the world's primary reserve currency and sets the terms of the gold-dollar peg. Collects seigniorage and exorbitant privilege — the ability to finance deficits externally without immediate balance-of-payments constraint. Administers the IMF quota system and exercises de facto veto over major decisions. Exit is effectively costless: the dollar's role is self-reinforcing through network effects.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, united_states_treasury_federal_reserve, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__sovereignty_defense, united_states_treasury_federal_reserve, beneficiary).

% Operate in a stable dollar-denominated environment with predictable exchange rates and deep dollar funding markets. Benefit from U.S. monetary policy transmission without bearing adjustment costs. Can arbitrage across jurisdictions; exit options are extensive through global capital mobility.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, multinational_corporations_dollar_zone, beneficiary,
    organized, biographical, mobile, global).

% Administer the Bretton Woods surveillance and lending apparatus. Institutional mission and professional identity are fused to the system's continuity — the Fund's legitimacy depends on the regime it manages. Exit would mean abandoning the career capital and epistemic framework built around the par-value system.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, international_monetary_fund_staff, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__sovereignty_defense, international_monetary_fund_staff, beneficiary).

% Must defend fixed parities against the dollar by accumulating reserves and contracting domestic credit when the U.S. runs deficits. Bear the asymmetric adjustment burden: their currencies cannot serve as international reserves, so they absorb the discipline. Exit means floating (politically costly, seen as surrender) or imposing capital controls (invites IMF censure).
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, non_reserve_currency_central_banks, payer,
    moderate, biographical, constrained, national).

% Face the hardest version of the adjustment burden: primary commodity exports priced in dollars, dollar-denominated debt, and no swap lines. IMF conditionality ties crisis lending to austerity and structural adjustment designed to protect the par-value system. No meaningful voice in governance; quota shares are negligible. Exit is blocked by debt dependence and political fragility.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, developing_country_governments, payer,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__sovereignty_defense, developing_country_governments, excluded).

% Domestic wage-setting is disciplined by the external anchor: unemployment is used to defend the parity when reserves drain. The constraint operates through the Phillips curve trade-off the regime makes unavoidable. Exit would require breaking the fixed-exchange commitment — politically blocked by the same elites who benefit from dollar integration.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, labor_movements_fixed_exchange_regimes, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__sovereignty_defense, labor_movements_fixed_exchange_regimes, excluded).

% Read the same treaty as protecting domestic policy space via capital controls. See the sovereignty defense reading as capturing only the U.S.-centric half of the bargain — the 'embedded liberalism' compromise that gave others capital controls in exchange for the dollar anchor. Their reading coexists with this one but emphasizes different articles and different beneficiaries.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, keynesian_embedded_liberalism_advocates, observer,
    analytical, civilizational, analytical, global).

% Read the treaty's Articles VIII and VIII as mandating progressive convertibility and capital account liberalization. See the sovereignty defense reading as an anachronistic misreading that privileges Article IV's par-value obligations over the system's teleological direction. Their reading became institutionally dominant after 1978; it influences but does not foreclose the sovereignty defense framing.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, neoliberal_convertibility_advocates, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable nominal anchor (gold-dollar peg) and a multilateral payments clearing mechanism so that international trade and investment can occur without bilateral barter or chaotic floating. Solves the coordination problem of a common denominator for cross-border contracts.
% TRANSFER_FUNCTION: Moves real resources and policy autonomy from non-reserve-currency states to the United States via the asymmetric adjustment mechanism: U.S. deficits become others' reserve accumulation; U.S. monetary policy transmits globally without reciprocity; crisis costs are socialized onto deficit countries through IMF conditionality.
% ABSENT_VOICES: Colonial and post-colonial monetary authorities whose currencies were sterling- or franc-pegged, not dollar-pegged, and who were integrated into the system without representation. The Global South at Bandung (1955) and the NIEO demand (1974) articulated the victim position but were excluded from the governance architecture.
% DISAPPEARANCE_RATIONALE: If the par-value system and its enforcement (IMF surveillance, Article IV obligations, gold window) vanished overnight, exchange rates would float, the dollar's reserve role would be contested, developing countries would regain monetary policy autonomy, and the asymmetric adjustment burden would dissolve — but global trade invoicing and debt denomination would take decades to re-coordinate.
% FOUNDING_PROBLEM: The interwar collapse of the gold standard produced competitive devaluations, trade warfare, and capital flight that deepened the Great Depression. A rules-based system with an anchor currency and an international lender of last resort was designed to prevent 1930s-style beggar-thy-neighbor dynamics.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the 1944 conference record (Keynes, White, delegates). Its status is contested: U.S. officials and dollar-zone beneficiaries argue the interwar instability risk persists and the system's core logic remains live; non-reserve central banks, developing country governments, and the 1974 NIEO coalition argue the founding problem was solved by the system's own evolution (floating rates, SDRs, swap lines) and the arrangement now persists as extraction. The corroboration comes from the 1974 UN Declaration on the Establishment of a New International Economic Order and the 1976 Jamaica Accords — sources outside the beneficiary set.
narrative_ontology:disappearance_verdict(bretton_woods_treaty_substrate__sovereignty_defense, world_rearranges).
narrative_ontology:founding_problem_status(bretton_woods_treaty_substrate__sovereignty_defense, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bretton_woods_treaty_substrate__sovereignty_defense, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(bretton_woods_treaty_substrate__sovereignty_defense, 'none', 1).
narrative_ontology:epsilon_provenance(bretton_woods_treaty_substrate__sovereignty_defense, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bretton_woods_treaty_substrate__sovereignty_defense_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bretton_woods_treaty_substrate__sovereignty_defense, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bretton_woods_treaty_substrate__sovereignty_defense_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.68 at 1971) reflects the accumulated transfer: U.S. seigniorage, the Triffin dilemma's resolution always at others' expense, and IMF conditionality as the enforcement arm. Suppression (0.72) is high because the regime's persistence required active prevention of exit: capital controls were permitted but exit from the par-value system itself invited IMF censure and market punishment; the London Gold Pool (1961–68) was explicit suppression of the gold price to maintain the anchor. Theater ratio (0.31) rises over the interval as the 'gold standard' rhetoric increasingly covers a system where the center does not play by the rules it enforces on the periphery. Accessibility collapse (0.58) and resistance (0.61) are moderate: alternatives (floating, regional blocs, SDRs) were discussed but structurally blocked by the dollar's network effects and U.S. veto power.
 *
 * PERSPECTIVAL GAP:
 *   From the U.S. agenda-setter seat, the constraint is genuine coordination: a stable anchor enables global trade, the IMF provides liquidity, and the system prevented 1930s chaos. From the non-reserve central bank seat, the same structure is enforced extraction: they import U.S. inflation, lose policy autonomy, and bear adjustment costs. From the developing country seat, it is a snare: the coordination story is cover for a system that prices their commodities in dollars, denominates their debt in dollars, and disciplines their wages through IMF conditionality. The engine computes these per-seat divergences from the structural data; the claimed type (tangled_rope) acknowledges the hybrid reality.
 *
 * DIRECTIONALITY LOGIC:
 *   The U.S. Treasury/Fed is the structural beneficiary (d ≈ 0.15): collects seigniorage, sets the anchor, exits costlessly. Multinational corporations in the dollar zone are beneficiaries (d ≈ 0.25) with mobile exit. IMF staff are agenda-setters with identity-locked exit (d ≈ 0.35) — they administer but are trapped by institutional identity. Non-reserve central banks are payers with constrained exit (d ≈ 0.85): they must defend the parity or face crisis. Developing countries are payers with trapped exit (d ≈ 0.95): no voice, debt dependence, conditionality. Labor movements are payers with constrained exit (d ≈ 0.8): domestic politics blocks exit. The two observer seats are analytical (d = 0.5 by definition).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (interwar chaos) was live in 1944. By 1971, the problem had mutated: the Triffin dilemma made the gold anchor incompatible with global liquidity provision; the system's own success (dollar accumulation abroad) undermined its foundation. The arrangement persisted not because the founding problem remained live, but because the beneficiaries (U.S., dollar-zone corporations, IMF bureaucracy) had the power to block reform. The 1971 Nixon shock ended the gold convertibility constraint but preserved the dollar's reserve role — the extraction mechanism survived the coordination function's collapse. This is mandatrophy: the mandate (prevent 1930s chaos) was resolved by the system's evolution, but the constraint (asymmetric adjustment) persisted via institutional inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Where exactly does the sovereignty defense reading end and the embedded liberalism reading begin — are they distinct constraints or emphasis shifts on the same constraint?',
    'Decompose the treaty articles into operational obligations: Article IV (par values) vs. Article VI (capital controls) vs. Article VIII (convertibility). Test whether the victim/beneficiary sets differ structurally or only rhetorically.',
    'If the readings share the same operational obligations and same victim/beneficiary sets, they are emphasis shifts on one constraint. If the victim sets differ (e.g., embedded liberalism includes capital-control users as beneficiaries, sovereignty defense excludes them), they are distinct constraints requiring separate stories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether sibling readings of the Bretton Woods kernel are structurally distinct constraints or framing variants').

omega_variable(
    exorbitant_privilege_measurement,
    'What is the quantitative magnitude of U.S. exorbitant privilege over the 1944–1971 interval, and does it map linearly to the extractiveness metric?',
    'Estimate the seigniorage flow, the interest rate differential on U.S. liabilities vs. assets, and the avoided adjustment costs. Compare to the GDP of victim countries to calibrate the transfer function.',
    'If privilege is small relative to global GDP, the extractiveness score may overstate the transfer. If privilege is large and growing, the 0.68 endpoint may understate it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exorbitant_privilege_measurement, empirical, 'Quantitative calibration of the beneficiary transfer in the sovereignty defense reading').

omega_variable(
    gold_anchor_snare_mechanism,
    'Is the gold anchor''s snare character (disciplining the periphery while the center defects) an emergent property of the Triffin dilemma, or was it designed in from the White Plan?',
    'Compare the 1944 White Plan and Keynes Plan on the adjustment burden. Trace the negotiation record: did U.S. delegates resist symmetric adjustment mechanisms (Keynes''s ICU clearing union with penalities on surplus countries)?',
    'If designed in, the constraint is a snare from inception. If emergent, it starts as rope and degrades — affecting the temporal trajectory and the mandatrophy analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gold_anchor_snare_mechanism, conceptual, 'Origin of the asymmetric adjustment mechanism in the Bretton Woods design').

omega_variable(
    imf_staff_identity_lock,
    'Is the IMF staff''s identity_locked exit a genuine professional identity fusion, or a rational career calculation in a monopolistic labor market?',
    'Track IMF staff mobility after 1971 (floating rates) and after 1982 (debt crisis): do they leave the institution, or do they adapt the institution''s mission? Survey epistemic commitments vs. material incentives.',
    'If identity fusion, the constraint''s persistence has a cultural-cognitive dimension beyond material interest. If career calculation, the lock is contingent on the institution''s monopoly over development lending.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(imf_staff_identity_lock, preference, 'Mechanism of institutional identity lock for the IMF bureaucracy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bretton_woods_treaty_substrate__sovereignty_defense, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bwts_sd_tr_t1944, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1944, 0.12).
narrative_ontology:measurement(bwts_sd_tr_t1950, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1950, 0.15).
narrative_ontology:measurement(bwts_sd_tr_t1958, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1958, 0.18).
narrative_ontology:measurement(bwts_sd_tr_t1960, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1960, 0.21).
narrative_ontology:measurement(bwts_sd_tr_t1965, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1965, 0.25).
narrative_ontology:measurement(bwts_sd_tr_t1968, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1968, 0.28).
narrative_ontology:measurement(bwts_sd_tr_t1971, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1971, 0.31).

% Extraction over time
narrative_ontology:measurement(bwts_sd_be_t1944, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1944, 0.42).
narrative_ontology:measurement(bwts_sd_be_t1950, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1950, 0.48).
narrative_ontology:measurement(bwts_sd_be_t1958, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1958, 0.55).
narrative_ontology:measurement(bwts_sd_be_t1960, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1960, 0.58).
narrative_ontology:measurement(bwts_sd_be_t1965, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1965, 0.63).
narrative_ontology:measurement(bwts_sd_be_t1968, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1968, 0.67).
narrative_ontology:measurement(bwts_sd_be_t1971, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1971, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(bwts_sd_su_t1944, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1944, 0.55).
narrative_ontology:measurement(bwts_sd_su_t1950, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1950, 0.58).
narrative_ontology:measurement(bwts_sd_su_t1958, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1958, 0.62).
narrative_ontology:measurement(bwts_sd_su_t1960, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1960, 0.65).
narrative_ontology:measurement(bwts_sd_su_t1965, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1965, 0.68).
narrative_ontology:measurement(bwts_sd_su_t1968, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1968, 0.7).
narrative_ontology:measurement(bwts_sd_su_t1971, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1971, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bretton_woods_treaty_substrate__sovereignty_defense, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(bretton_woods_treaty_substrate__sovereignty_defense, 0.12).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__sovereignty_defense, bretton_woods_treaty_substrate__keynesian_embedded_liberalism).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__sovereignty_defense, bretton_woods_treaty_substrate__neoliberal_convertibility).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__sovereignty_defense, triffin_dilemma_constraint).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__sovereignty_defense, dollar_reserve_currency_exorbitant_privilege).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__sovereignty_defense, imf_conditionality_enforcement_mechanism).

% DUAL FORMULATION NOTE:
% Part of the Bretton Woods Treaty Substrate constraint family. This reading (sovereignty_defense) emphasizes Article IV par-value obligations and the asymmetric adjustment burden. The keynesian_embedded_liberalism reading emphasizes Article VI capital controls as policy space protection. The neoliberal_convertibility reading emphasizes Articles VIII convertibility obligations as a teleological mandate. The three readings share the same legal text but instantiate different constraints with different beneficiary/victim structures and different ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bretton_woods_treaty_substrate__sovereignty_defense, institutional, 0.15).
constraint_indexing:directionality_override(bretton_woods_treaty_substrate__sovereignty_defense, organized, 0.25).
constraint_indexing:directionality_override(bretton_woods_treaty_substrate__sovereignty_defense, moderate, 0.85).
constraint_indexing:directionality_override(bretton_woods_treaty_substrate__sovereignty_defense, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
