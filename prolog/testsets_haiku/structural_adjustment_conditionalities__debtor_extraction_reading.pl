% ============================================================================
% CONSTRAINT STORY: structural_adjustment_conditionalities__debtor_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: structural_adjustment_conditionalities__debtor_extraction_reading
 *   human_readable: Structural Adjustment Conditionalities as Debtor Extraction Regime
 *   domain: economic/political/institutional
 *
 * SUMMARY:
 *   Structural adjustment conditionalities are requirements imposed by the
 *   International Monetary Fund and World Bank on debtor countries as
 *   conditions for loan disbursement and debt restructuring. The stated
 *   rationale is fiscal discipline, market confidence, and sustainable
 *   development. This constraint story instantiates the DEBTOR EXTRACTION
 *   READING: conditionalities function as a coercive extraction regime that
 *   violently dismantles domestic social contracts, redirects fiscal capacity
 *   toward transnational creditors and commodity extractors, and forecloses
 *   alternative development paths. The reading construes the constraint as a
 *   neo-colonial instrument: formal sovereignty masks binding external
 *   control, while the technical framing of conditionalities obscures the
 *   fact that populations bearing the costs have no voice in their design.
 *   The claim/metric gap is intentional: the constraint is CLAIMED as snare
 *   (extractive, coercive, with identifiable victims) and the metrics
 *   describe a high-extraction, high-suppression regime that has intensified
 *   over 44 years. This reading is ONE of three structurally distinct
 *   readings of the same kernel (structural_adjustment_conditionalities); the
 *   sibling readings (creditor_coordination, hybrid_selectivity) would
 *   generate different constraint stories with different ε values,
 *   beneficiary/victim sets, and suppression mechanisms. They coexist as live
 *   competing interpretations held by different institutional seats
 *   (creditors, heterodox economists, geopolitically variated debtors).
 *
 * KEY AGENTS:
 *   - IMF/World Bank technocrats: institutional agenda-setters, design and enforce the policy menu without bearing its costs
 *   - Transnational creditor banks: institutional beneficiaries, collect repayment + interest enforced by conditionality discipline
 *   - Commodity extraction interests: beneficiaries, gain resource access and labor cost control through privatization and trade liberalization mandates
 *   - Debtor-country populations: powerless victims, trapped; bear direct costs through service cuts, subsidy removal, employment precarity
 *   - Public sector workers: organized victims, identity-locked; targeted by wage/hiring freezes and redundancy mandates
 *   - Rural subsistence communities: victims, doubly trapped (external: cannot immigrate; internal: resource dependence on enclosed commons)
 *   - Domestic industries: moderate-power victims, constrained; collapse under trade liberalization they cannot compete with
 *   - Debtor-country governments: moderate-power payers, constrained; structurally unable to reject conditions without triggering default cascade
 *   - Civil society, labor unions: excluded from design; would oppose but have no formal standing in state-to-creditor bilateral process
 *   - Alternative lenders (China, regional banks): excluded from rule-setting; constrained by creditor-dominated governance norms
 *   - Heterodox economists: analytical observers; document extraction logic and counterfactual welfare but lack institutional standing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_adjustment_conditionalities__debtor_extraction_reading, 0.87).
domain_priors:suppression_score(structural_adjustment_conditionalities__debtor_extraction_reading, 0.91).
domain_priors:theater_ratio(structural_adjustment_conditionalities__debtor_extraction_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, extractiveness, 0.87).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_adjustment_conditionalities__debtor_extraction_reading, snare).
narrative_ontology:human_readable(structural_adjustment_conditionalities__debtor_extraction_reading, "Structural Adjustment Conditionalities as Debtor Extraction Regime").
narrative_ontology:topic_domain(structural_adjustment_conditionalities__debtor_extraction_reading, "economic/political/institutional").

domain_priors:requires_active_enforcement(structural_adjustment_conditionalities__debtor_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(structural_adjustment_conditionalities__debtor_extraction_reading, '796eac2f-6bf8-41d4-890b-57aee2f61eff').
narrative_ontology:cs_kernel_codification('796eac2f-6bf8-41d4-890b-57aee2f61eff', formalized).
narrative_ontology:cs_authority_grounding('796eac2f-6bf8-41d4-890b-57aee2f61eff', extraction).
narrative_ontology:cs_interpretation_layer_present('796eac2f-6bf8-41d4-890b-57aee2f61eff').
narrative_ontology:cs_reading_relation('796eac2f-6bf8-41d4-890b-57aee2f61eff', structural_adjustment_conditionalities__creditor_coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('796eac2f-6bf8-41d4-890b-57aee2f61eff', structural_adjustment_conditionalities__hybrid_selectivity_reading, coexists_with).
narrative_ontology:cs_axiom('796eac2f-6bf8-41d4-890b-57aee2f61eff', foundational, conditionalities_function_as_extraction).
narrative_ontology:cs_axiom_status(conditionalities_function_as_extraction, holdable).
narrative_ontology:cs_axiom_grounding('796eac2f-6bf8-41d4-890b-57aee2f61eff', conditionalities_function_as_extraction, empirically_contingent).
narrative_ontology:cs_axiom('796eac2f-6bf8-41d4-890b-57aee2f61eff', foundational, founding_crisis_superseded_by_permanent_rent_capture).
narrative_ontology:cs_axiom_status(founding_crisis_superseded_by_permanent_rent_capture, holdable).
narrative_ontology:cs_axiom_grounding('796eac2f-6bf8-41d4-890b-57aee2f61eff', founding_crisis_superseded_by_permanent_rent_capture, empirically_contingent).
narrative_ontology:cs_reference_frame('796eac2f-6bf8-41d4-890b-57aee2f61eff', crisis_response_fiscal_stabilization).
narrative_ontology:cs_drift_state('796eac2f-6bf8-41d4-890b-57aee2f61eff', contemporary_2024, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('796eac2f-6bf8-41d4-890b-57aee2f61eff', '').
narrative_ontology:cs_kernel_id(structural_adjustment_conditionalities__debtor_extraction_reading, structural_adjustment_conditionalities).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__debtor_extraction_reading, transnational_creditor_banks).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__debtor_extraction_reading, global_financial_capital).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__debtor_extraction_reading, commodity_extraction_interests).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, debtor_country_populations).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, public_sector_workers).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, rural_subsistence_communities).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, domestic_industries).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(structural_adjustment_conditionalities__debtor_extraction_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(structural_adjustment_conditionalities__debtor_extraction_reading, 'none', 1).

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
 *   Extractiveness at 0.87 reflects that conditionalities are designed to transfer fiscal capacity, resource access, and labor discipline from debtor populations to transnational beneficiaries. The measurement series shows extraction accumulation over 44 years: starting at 0.45 (acute crisis response) to 0.87 (mature extraction regime). Suppression at 0.91 is extremely high because the constraint persists through structural coercion, not voluntary participation: governments face binary choice (accept conditions or face default cascade); populations face identity-lock (public sector careers, subsistence dependence, immobility) that make exit impossible. Theater at 0.62 (rising from 0.25) reflects that the functional justification for conditionalities (crisis management, fiscal discipline) has been increasingly detached from practice (permanent austerity, resource extraction continuing despite stabilized fiscal positions). The suppression trajectory shows steady intensification: 1990 (0.74) through 2024 (0.91). This reflects two mechanisms: (1) enforcement hardening as creditors perfected the machinery (debt conditionality linkage, surveillance expansion, expanded scope of conditions); (2) resistance hardening (labor unrest, electoral rejection, social crisis) requiring greater enforcement effort. The theater ratio rise (0.25 to 0.62) suggests Goodhart drift: early conditions were genuinely tied to fiscal metrics; later conditions (governance restructuring, privatization mandates, labor flexibilization) are justified by appeal to 'market confidence' and 'best practice' rather than measurable sustainable finance. These are smoke screens for resource extraction.
 *
 * PERSPECTIVAL GAP:
 *   From the creditor institutional seat (IMF/World Bank), conditionalities are necessary coordination: they provide assurance to lenders and ensure policy consistency that makes lending possible. From the debtor-country government seat, conditions are constraining but rational: accepting them preserves access to refinancing and avoids default. From the debtor-population seats (powerless, trapped, identity-locked), the same structure is pure coercion: policies are imposed without consent, costs are borne involuntarily, and resistance is suppressed. The engine computes seat-specific directionality: creditor seats derive d near 0.0 (beneficiary, arbitrage exit, power to shape rules); government seats derive d near 0.5 (constrained, moderate power, mixed costs and stability benefits); population seats derive d near 1.0 (victims, trapped/identity-locked, no exit, no voice in design). This structural divergence is the point: the same constraint maps to different effective extractions depending on power, exit, and beneficiary status.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (transnational banks, commodity interests) are declared on the structural fact that they collect from the arrangement: interest flows and principal payments go to lenders; resource rents and cheap labor go to commodity interests. They face no austerity, experience no service cuts, and bear zero enforcement cost. Victims are declared on the structural fact that they bear costs involuntarily: populations lose public services, subsistence commons, and employment security; domestic industries lose tariff protection and credit access; rural communities lose land and pastoral resources. The constraint persists because beneficiaries can externalize enforcement costs (IMF/World Bank do the coercion) while victims cannot exit. Directionality for each seat follows from power and exit: powerless + trapped/identity-locked + victim = d near 1.0 (full target). Institutional beneficiary + arbitrage exit + power to shape rules = d near 0.0 (full beneficiary). Moderate-power government + constrained exit + mixed roles = d around 0.5 (symmetric, pulled both ways by creditor pressure and constituent resistance).
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved (founding_problem_status=dead): the 1982–1985 debt crisis triggered conditions meant as temporary shock therapy. By 2000, debt-to-export ratios had stabilized, financial markets had resumed access, and the founding problem (acute refinancing crisis) was solved. Yet conditionalities persisted and expanded in scope (governance, labor, resource sectors). This is classic mandatrophy: the mandate has outlived the function. Creditors continue to defend conditions as necessary for 'market confidence,' but this is post-hoc rationalization — the founding problem is gone, yet the constraint persists by institutional inertia and extracted benefit. The theatrical justification (conditions = fiscal discipline) masks the fact that conditions now function as permanent extraction (resource access, labor cost control, debt servicing priority). The theater ratio rise from 0.25 to 0.62 is diagnostic of this drift: the functional story (fiscal crisis response) has been replaced by performance (conditions are 'best practice,' 'market expectations,' 'good governance') while the actual mechanism is creditor-enforced resource extraction. This is mandatrophy detection: constraint persists after its founding rationale is dead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_vs_persistence_gap,
    'The founding debt crisis (1982–1985) is solved; yet conditionality scope and intensity have increased. Is the constraint persisting due to institutional inertia/path dependence, or due to active extraction benefit capture?',
    'Historical analysis of IMF/World Bank mandate evolution, personnel testimony, and internal documents (released through FOIA/information access laws) comparing crisis-response design to post-2000 condition imposition. Counterfactual analysis: what would conditionalities look like if designed for solved crisis (tapering, targeted) versus designed for permanent extraction (expansion, scope creep)?',
    'If inertia dominates: the constraint is a Piton (degraded institution maintained theatrically). If extraction benefit dominates: the constraint is a Snare (active coercive extraction). The classification turns on this omega.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_vs_persistence_gap, empirical, 'Whether post-crisis condition persistence is institutional inertia or active extraction benefit.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression (0.91) primarily structural (legal/institutional barriers, default cascade threat, immobility) or internalized (debtor governments and populations believe conditions are necessary, inevitable, or legitimate)?',
    'Post-exit suppression trajectory: countries that exit (Argentina 2001 default, Iceland 2008, Jamaica conditional on alternative lenders) show whether suppression persists after escape from IMF machinery. Qualitative: belief surveys among debtor-country elites and populations before/after condition escape.',
    'If structural: the constraint''s effective suppression is the measured level. If internalized: populations carry the suppression with them even post-exit; the constraint''s power exceeds the measured level and persists through internalized discipline (structural adjustment as ideology). Hybrid mechanism (both) would mean institutional + cognitive lock.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether suppression is structural coercion or internalized discipline.').

omega_variable(
    alternative_development_pathways,
    'Are the policy alternatives to conditionalities (public investment, import substitution, capital controls, mixed economy) genuinely inferior to conditionality-mandated liberalization, or does the superiority claim depend on metrics controlled by creditors?',
    'Comparative development analysis: growth, welfare, inequality, service provision, and resilience in conditionality-compliant versus non-compliant debtors matched on initial conditions. Analysis must use independent metrics (human development, Gini, health/education access) not just GDP growth and financial market access (which creditors measure success by).',
    'If alternatives are genuinely inferior: conditionalities, while extractive, produce better material outcomes. If alternatives are superior: the constraint is purely extractive with no coordination benefit — the ''technical'' justification is false. If outcomes are mixed/path-dependent: the constraint involves genuine trade-offs and alternative readings (creditor_coordination, hybrid_selectivity) gain weight.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_development_pathways, conceptual, 'Whether conditionality-mandated policies are developmentally superior or whether superiority is an artifact of creditor-controlled metrics.').

omega_variable(
    coalition_capacity_of_debtor_populations,
    'Could debtor-country populations, if organized, mount sufficient resistance to compel condition reversal, or is the institutional/power asymmetry too large to overcome through domestic political action?',
    'Historical case analysis of resistance episodes (IMF riots, labor-led government capture, electoral rejection of conditionality-supporting parties). Assessment of whether these produced condition relaxation or merely spectacle/theater before creditor enforcement resumed.',
    'If coalition resistance can compel reversal: the constraint operates through suppression of actual latent power (high resistance potential, high enforcement needed). If institutional asymmetry prevents reversal: the constraint is a Snare with low contestability (high suppression, low active resistance because effort is futile). This affects the resistance metric calibration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_capacity_of_debtor_populations, empirical, 'Whether debtor-country resistance capacity could translate to institutional reversal or is structurally foreclosed.').

omega_variable(
    reading_contest_empirical_boundary,
    'What empirical evidence would adjudicate between the debtor_extraction_reading and the creditor_coordination_reading? What would disconfirm this reading''s core premise?',
    'This is a omega_c (conceptual) probe disguised as empirical: the two readings differ on the MOTIVE driving condition persistence (extraction vs. efficiency). Motive is empirically opaque. Disconfirming evidence would include: (1) demonstrable cases where conditions are relaxed on high-extraction debtors who pose no fiscal risk (contradicts extraction story); (2) cases where creditors maintain conditions despite clear welfare harm and political instability (supports extraction story); (3) counterfactual: conditions diverge sharply across geopolitically ranked debtors even controlling for fiscal metrics (supports hybrid_selectivity, suggests extraction).',
    'The reading''s claim is structural (conditions function as extraction machinery). The empirical boundary is thin: both extraction and coordination can coexist (conditions coordinate creditor expectations AND extract rents). The reading''s unique claim is that extraction dominates the function. Evidence of selective application by geopolitical standing (hybrid_selectivity reading) would support this reading''s contention that power and profit, not efficiency, drive the system.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest_empirical_boundary, conceptual, 'Kernel contest: whether the operative motive driving condition persistence is creditor extraction or efficiency coordination.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_adjustment_conditionalities__debtor_extraction_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stru_tr_t1980, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 1980, 0.25).
narrative_ontology:measurement(stru_tr_t1990, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 1990, 0.38).
narrative_ontology:measurement(stru_tr_t2000, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 2000, 0.48).
narrative_ontology:measurement(stru_tr_t2010, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 2010, 0.55).
narrative_ontology:measurement(stru_tr_t2018, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 2018, 0.61).
narrative_ontology:measurement(stru_tr_t2024, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 2024, 0.62).

% Extraction over time
narrative_ontology:measurement(stru_be_t1980, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 1980, 0.45).
narrative_ontology:measurement(stru_be_t1990, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 1990, 0.68).
narrative_ontology:measurement(stru_be_t2000, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 2000, 0.78).
narrative_ontology:measurement(stru_be_t2010, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 2010, 0.83).
narrative_ontology:measurement(stru_be_t2018, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 2018, 0.86).
narrative_ontology:measurement(stru_be_t2024, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 2024, 0.87).

% Suppression requirement over time
narrative_ontology:measurement(stru_su_t1980, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 1980, 0.55).
narrative_ontology:measurement(stru_su_t1990, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 1990, 0.74).
narrative_ontology:measurement(stru_su_t2000, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 2000, 0.82).
narrative_ontology:measurement(stru_su_t2010, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 2010, 0.87).
narrative_ontology:measurement(stru_su_t2018, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 2018, 0.9).
narrative_ontology:measurement(stru_su_t2024, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 2024, 0.91).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_adjustment_conditionalities__debtor_extraction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(structural_adjustment_conditionalities__debtor_extraction_reading, 0.25).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__debtor_extraction_reading, structural_adjustment_conditionalities__creditor_coordination_reading).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__debtor_extraction_reading, structural_adjustment_conditionalities__hybrid_selectivity_reading).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__debtor_extraction_reading, debt_trap_mechanism__asymmetric_repayment).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__debtor_extraction_reading, resource_curse__extractive_commodity_export).

% DUAL FORMULATION NOTE:
% This constraint is part of the structural_adjustment_conditionalities kernel family. Three structurally distinct readings instantiate different constraints from the same kernel: (1) debtor_extraction_reading (this story) — snare, high extraction (ε=0.87), coercive enforcement, neo-colonial framing; (2) creditor_coordination_reading — rope, moderate extraction (ε~0.45), coordination function dominates, efficiency/market discipline framing; (3) hybrid_selectivity_reading — tangled_rope, high extraction (ε~0.80) but applied selectively by geopolitical rank, suggesting power-based (not efficiency-based) differentiation. The ε values differ because the readings define different constraints: extraction-dominated versus efficiency-dominated versus selectively-applied. The family structure reflects the kernel contest: one commitment (IMF/World Bank governance authority over debtor fiscal policy) admits genuinely incommensurable readings depending on the interpreter's seat and empirical claims. This story (debtor_extraction) feeds the downstream constraints (debt_trap_mechanism, resource_curse) by establishing that conditionalities function as extraction infrastructure; those constraints are structurally downstream (condition-enforced debt service and resource privatization enable both debt-trap dynamics and commodity extraction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(structural_adjustment_conditionalities__debtor_extraction_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
