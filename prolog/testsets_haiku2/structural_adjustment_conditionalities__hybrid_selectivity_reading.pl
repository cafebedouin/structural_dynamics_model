% ============================================================================
% CONSTRAINT STORY: structural_adjustment_conditionalities__hybrid_selectivity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_structural_adjustment_conditionalities__hybrid_selectivity_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: structural_adjustment_conditionalities__hybrid_selectivity_reading
 *   human_readable: Selective Structural Adjustment Conditionalities (Hybrid Selectivity Reading)
 *   domain: international_political_economy/development_finance
 *
 * SUMMARY:
 *   Structural adjustment conditionalities — the bundle of fiscal austerity,
 *   labor-market deregulation, and privatization requirements imposed by the
 *   IMF and World Bank on debtor states seeking debt restructuring — are
 *   presented by creditors as neutral coordination mechanisms ensuring
 *   repayment capacity and market confidence. This reading instantiates the
 *   hybrid_selectivity thesis: the conditionalities do solve a real
 *   creditor-coordination problem (preventing runs and restructuring
 *   holdouts), but their application is sharply selective by debtor
 *   geopolitical alignment. States without G7 leverage (sub-Saharan Africa,
 *   non-aligned Latin America, the post-Soviet periphery) face full
 *   implementation; strategically important debtors (Cold War Turkey,
 *   post-1989 Russia, Egypt post-2013) negotiate exemptions and have
 *   conditionalities applied lightly or waived altogether. The mechanism is
 *   neither pure coordination (creditor_coordination_reading) nor pure
 *   extraction (debtor_extraction_reading), but hybrid: genuinely
 *   coordinating creditors' lending while selectively extracting from
 *   non-strategic periphery. This reading claims a Tangled Rope
 *   classification: both coordination (the core function) and asymmetric
 *   extraction (the variable application) are structurally real, and both are
 *   sustained by active enforcement (IMF restructuring conditionality is
 *   binding; non-compliance triggers liquidity cutoff). The founding
 *   coordination problem (runs on sovereigns, inability to orchestrate
 *   multilateral restructuring) was real in the 1980s-early 1990s. The
 *   enduring omega is whether that problem remains live (supporting
 *   coordination thesis) or has been solved by the 1990s onward while the
 *   apparatus persists for rent extraction (supporting piton or mandatrophy
 *   diagnosis). This reading resolves the ambiguity by situating selectivity
 *   as the pivot: as long as selectivity is driven by legitimate risk
 *   differentials (higher default risk justifies stricter discipline),
 *   coordination dominates. As selectivity becomes decoupled from risk and
 *   driven instead by geopolitical alignment, extraction dominates.
 *   Empirically, selectivity correlates far more strongly with G7 alignment
 *   than with debt-to-GDP or fiscal profiles, suggesting the extraction
 *   function is primary.
 *
 * KEY AGENTS:
 *   - IMF/World Bank: institutional agenda-setter, controls conditionality design and enforcement via debt restructuring authority
 *   - Hegemon-aligned creditor states: institutional beneficiaries, receive favorable conditionality treatment and exemptions in practice
 *   - Core creditor institutions: institutional beneficiaries, collect extraction via repayment prioritization and asset privatization
 *   - Non-strategic peripheral debtors (sub-Saharan Africa, non-aligned Latin America): powerless victims, bear full conditionality burden
 *   - Geopolitically strategic debtors (Turkey, Russia post-1989, Egypt post-2013): organized beneficiary-payers, receive lighter discipline in exchange for alignment
 *   - Domestic labor coalitions: moderate-power payers in non-strategic debtors, identity-locked into national state's debt position
 *   - Subsistence populations: powerless victims, experience direct extraction via subsidy removal
 *   - Hegemon governments (G7): institutional agenda-setters and indirect beneficiaries, shape IMF governance and enforce selective application
 *   - Alternative creditors: organized but excluded parties, locked out of restructuring authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.72).
domain_priors:suppression_score(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.78).
domain_priors:theater_ratio(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_adjustment_conditionalities__hybrid_selectivity_reading, tangled_rope).
narrative_ontology:human_readable(structural_adjustment_conditionalities__hybrid_selectivity_reading, "Selective Structural Adjustment Conditionalities (Hybrid Selectivity Reading)").
narrative_ontology:topic_domain(structural_adjustment_conditionalities__hybrid_selectivity_reading, "international_political_economy/development_finance").

domain_priors:requires_active_enforcement(structural_adjustment_conditionalities__hybrid_selectivity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(structural_adjustment_conditionalities__hybrid_selectivity_reading, '2624690b-4cf2-4516-992e-cbec3b81ba9b').
narrative_ontology:cs_kernel_codification('2624690b-4cf2-4516-992e-cbec3b81ba9b', fixed_text).
narrative_ontology:cs_authority_grounding('2624690b-4cf2-4516-992e-cbec3b81ba9b', extraction).
narrative_ontology:cs_interpretation_layer_present('2624690b-4cf2-4516-992e-cbec3b81ba9b').
narrative_ontology:cs_reading_relation('2624690b-4cf2-4516-992e-cbec3b81ba9b', structural_adjustment_conditionalities__creditor_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('2624690b-4cf2-4516-992e-cbec3b81ba9b', structural_adjustment_conditionalities__debtor_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('2624690b-4cf2-4516-992e-cbec3b81ba9b', foundational, conditioning_solves_creditor_coordination).
narrative_ontology:cs_axiom_status(conditioning_solves_creditor_coordination, holdable).
narrative_ontology:cs_axiom_grounding('2624690b-4cf2-4516-992e-cbec3b81ba9b', conditioning_solves_creditor_coordination, empirically_contingent).
narrative_ontology:cs_axiom('2624690b-4cf2-4516-992e-cbec3b81ba9b', foundational, conditioning_selectively_applied_by_geopolitical_position).
narrative_ontology:cs_axiom_status(conditioning_selectively_applied_by_geopolitical_position, holdable).
narrative_ontology:cs_axiom_grounding('2624690b-4cf2-4516-992e-cbec3b81ba9b', conditioning_selectively_applied_by_geopolitical_position, empirically_contingent).
narrative_ontology:cs_reference_frame('2624690b-4cf2-4516-992e-cbec3b81ba9b', multilateral_discipline_framework).
narrative_ontology:cs_drift_state('2624690b-4cf2-4516-992e-cbec3b81ba9b', contemporary_2010s_2020s, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2624690b-4cf2-4516-992e-cbec3b81ba9b', '').
narrative_ontology:cs_kernel_id(structural_adjustment_conditionalities__hybrid_selectivity_reading, structural_adjustment_conditionalities).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__hybrid_selectivity_reading, hegemon_aligned_creditor_states).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__hybrid_selectivity_reading, core_creditor_institutions).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__hybrid_selectivity_reading, geopolitically_strategic_debtors).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__hybrid_selectivity_reading, non_strategic_peripheral_debtors).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__hybrid_selectivity_reading, domestic_labor_coalitions).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__hybrid_selectivity_reading, subsistence_level_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__hybrid_selectivity_reading, hegemon_governments).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__hybrid_selectivity_reading, geopolitically_strategic_debtors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and administers conditionality packages for sovereign debt restructuring. Justifies them as necessary for market confidence and long-term sustainability. In practice, applies stringent labor flexibilization, privatization, and austerity requirements to politically weak debtors while exempting or relaxing requirements for states aligned with G7 geopolitical interests (e.g., Turkey 2001, Egypt post-2013). Controls the binding constraint: access to debt restructuring and new borrowing depends on compliance.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, imf_world_bank, agenda_setter,
    institutional, generational, arbitrage, global).

% G7 states and their allies receive favorable treatment when indebted: conditionalities are applied lightly, selectively waived for geopolitical alignment (Turkey's banking deregulation was waived despite prior IMF opposition; post-Cold-War Russia received rescheduling without labor discipline demands). They collect the benefit of debt restructuring access without the extraction cost imposed on non-aligned peers.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, hegemon_aligned_creditor_states, beneficiary,
    institutional, generational, arbitrage, global).

% Multilateral development banks and core country banks originating the credit. Conditionalities ensure repayment prioritization and de facto collateral extraction (asset privatization, tax base expansion, labor cost suppression) from debtor economies. Non-compliance triggers enforcement through liquidity cutoffs and credit downgrades.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, core_creditor_institutions, beneficiary,
    powerful, generational, arbitrage, global).

% States without geopolitical leverage or G7 alignment (sub-Saharan Africa, most of Latin America in the 1990s, post-Soviet periphery) face the full weight of conditionality: mandatory privatization of water, electricity, healthcare; labor market deregulation; removal of subsidy regimes; budget caps on education and health. Non-compliance triggers immediate liquidity cutoff and sovereign default. Exit from the framework requires either attracting alternative financing (rare and expensive) or accepting default, both carrying severe economic damage. Their situation embodies the extraction mechanism itself.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, non_strategic_peripheral_debtors, payer,
    powerless, biographical, trapped, national).

% States with geopolitical value (Cold War-era Turkey, post-1989 Russia, Egypt post-2013, Pakistan) face relaxed or selectively waived conditionalities despite comparable debt profiles to non-strategic peers. They negotiate exemptions from labor and welfare cuts while receiving debt relief; they are coordinated with rather than disciplined. The same restructuring framework applies to them in name while functioning differently in practice due to their bargaining position.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, geopolitically_strategic_debtors, beneficiary,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(structural_adjustment_conditionalities__hybrid_selectivity_reading, geopolitically_strategic_debtors, payer).

% Labor unions, public sector workers, and wage-earning populations in non-strategic debtors bear the direct extraction via conditionality-mandated labor market flexibilization, public sector downsizing, and subsidy removal. They lack representation in the debt negotiation and conditionality design process. In strategic debtor states, labor retains more bargaining power and often negotiates exemptions (Turkey's public sector was less aggressively cut than Mexico's under comparable IMF programs). Identity-locked: their interests as workers are constituted through the national state's fiscal position, making exit from the constraint require either capturing the state's debt policy or emigrating.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, domestic_labor_coalitions, payer,
    moderate, biographical, identity_locked, national).

% The poorest quintiles in non-strategic debtor states, dependent on subsidized food, water, electricity, and public health services. Conditionalities mandate removal of these subsidies to meet fiscal targets. They face increased malnutrition, water-borne disease, and preventable mortality when service costs rise beyond their purchasing power. Their geographic and economic position allows no exit; their voice is mediated through national governments they did not negotiate.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, subsistence_level_populations, payer,
    powerless, immediate, trapped, local).

% G7 governments (especially US) shape IMF/World Bank governance through voting shares and staff placement, setting the conditionality playbook. They enforce selective application by threatening withdrawal of IMF support for strategically important defaulters and insisting on exemptions for aligned states. They benefit both directly (as creditor states) and indirectly (as architects of a global financial order that subordinates non-aligned periphery).
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, hegemon_governments, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(structural_adjustment_conditionalities__hybrid_selectivity_reading, hegemon_governments, beneficiary).

% Non-IMF lenders (bilateral creditors, Chinese development banks, Islamic finance institutions) are structurally excluded from conditionality design and debt restructuring authority. IMF binding power over restructuring means excluded creditors cannot block conditionality terms or propose alternatives. They are trapped into accepting the IMF-designed framework or losing access to debtor coordination mechanisms (the Paris Club excludes them). This exclusion maintains the cartel structure enabling selective application.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, alternative_creditors, excluded,
    organized, generational, trapped, global).

% Analyze conditionality effects empirically. The community is split: some vindicate the coordination thesis (Gavin/Rodrik showing pro-cyclical cuts deepen recessions); others document extraction (Dreher et al. on regulatory capture; IMF staff's own 2015 evaluation noting labor-market harm). Their seat is analytical only; they do not negotiate or enforce.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, development_economists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(structural_adjustment_conditionalities__hybrid_selectivity_reading, core_creditor_institutions).
narrative_ontology:fixing_cost_class(structural_adjustment_conditionalities__hybrid_selectivity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates creditor lending decisions and debtor fiscal discipline: multilateral restructuring prevents a commons tragedy where debtors default when borrowing is restructured unequally, and prevents creditor runs where some creditors grab priority. It solves the collective action problem of coordinating simultaneous multilateral debt relief with fiscal adjustment, which individual bilateral negotiation cannot achieve.
% TRANSFER_FUNCTION: Moves wealth from peripheral debtors to core creditors via: (1) repayment prioritization over domestic spending, (2) asset privatization transfers (state enterprises sold below market value to foreign buyers), (3) labor cost suppression through deregulation raises creditor returns on future investment, (4) subsidy removal raises debt service as a share of government revenue. The transfer is enforced by conditioning new borrowing and restructuring relief on compliance. Strategic debtors negotiate partial or conditional waivers; non-strategic debtors face the full architecture.
% ABSENT_VOICES: Non-strategic debtor populations, subsistence-level labor, and alternative creditor states are excluded from conditionality design. Workers and the poor in debtor countries are neither consulted nor represented in IMF board decisions; they experience conditionalities as impositions. Alternative creditors are structurally excluded from restructuring authority. These absences are not accidents — they are the enforcement mechanism: non-representation ensures compliance cannot be blocked by those bearing costs.
% DISAPPEARANCE_RATIONALE: If selective conditionality enforcement vanished overnight, non-strategic debtors could restructure without labor-market deregulation and austerity, allowing fiscal space for public goods. Alternative financing sources would gain access to debtor markets. Strategic debtors would lose their conditionality-exemption bargaining advantage. Core creditors would face higher default risk and renegotiation pressure. The global financial hierarchy would reorganize around more symmetric creditor-debtor bargaining rather than IMF-adjudicated unilateral discipline.
% FOUNDING_PROBLEM: Sovereign debt crises create coordination failures: when multiple creditors compete to seize payment priority (a run dynamic), debtors cannot restructure; when debtors threaten default, creditors over-tighten lending, deepening crisis. Conditionalities were designed to solve this by centralizing restructuring authority and ensuring debtors' commitment to repayment through monitored fiscal discipline.
% FOUNDING_PROBLEM_CORROBORATION: The IMF and core creditor states attest the coordination problem is still live, citing ongoing default risk and the need for discipline mechanisms. Non-strategic debtor governments and development economists outside the creditor bloc attest that the founding problem was solved by the 1980s (serial restructurings reduced runs and panic defaults) while the conditionality apparatus persisted, morphing into rent extraction. The World Bank's own 2015 evaluation (OED study on labor markets) documents that conditionality produces pro-cyclical cuts and harms growth outcomes in peripheral debtors while being selectively waived for strategic debtors — corroborating the hypothesis that the apparatus's primary function shifted from coordination to selective extraction.
narrative_ontology:disappearance_verdict(structural_adjustment_conditionalities__hybrid_selectivity_reading, world_rearranges).
narrative_ontology:founding_problem_status(structural_adjustment_conditionalities__hybrid_selectivity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(structural_adjustment_conditionalities__hybrid_selectivity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(structural_adjustment_conditionalities__hybrid_selectivity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.72, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(structural_adjustment_conditionalities__hybrid_selectivity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(structural_adjustment_conditionalities__hybrid_selectivity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(structural_adjustment_conditionalities__hybrid_selectivity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness trajectory (0.48 → 0.72 over the interval) reflects the shift from coordination dominance (1980s-early 1990s) to selective extraction dominance (2000s-2020s). Early in the interval, conditionalities solved a real coordination problem and extracted moderately as the price of coordination. By the interval midpoint (t=17), selectivity had hardened into routine practice — G7-aligned states routinely received waived or lightened terms while non-strategic peers faced full discipline. By the endpoint, the theater ratio (0.48) is substantial: IMF documentation still emphasizes the coordination and sustainability narrative, but empirical implementation has become openly selective (IMF staff publications, World Bank evaluations, and academic studies document the divergence). Suppression is high (0.78) because the mechanism's persistence depends on preventing alternatives: IMF governance excludes alternative creditors, excluded creditors are locked out of restructuring authority by IMF binding power, and non-strategic debtor populations are excluded from the decision-making process entirely. Non-compliance triggers immediate liquidity cutoff with severe consequences (default, capital flight, hyperinflation), creating near-total suppression of exit. The theater ratio is moderate (0.48) rather than high because the constraint is not purely theatrical — real coordination still occurs, conditionalities do reduce default risk — but a growing share of IMF activity defends selective application rather than enforcing discipline uniformly. The one shared time grid ensures all three metrics are authored at each examined point, preventing misalignment drift. The measurement series track the observable hardening of selectivity (rising extractiveness) and the intensifying theater as the coordination rationale became less empirically defensible (rising theater ratio) while enforcement machinery had to work harder to maintain selective application against growing resistance (stable suppression at high level, indicating sustained coercive infrastructure).
 *
 * PERSPECTIVAL GAP:
 *   The IMF/creditor seat and the non-strategic debtor seat experience structurally incommensurable types. From the IMF perspective, conditionalities are coordination mechanisms with some unfortunate but necessary selectivity based on risk differentials and geopolitical context — a rope or coordination function with asymmetric risk pricing. From the non-strategic debtor perspective, the mechanism is enforced extraction: their state is forced to cut public spending, privatize assets, and deregulate labor markets while strategically important neighbors receive exemptions, revealing the inequality as structural, not justified. The engine computes this divergence from the directionality data: IMF and core creditors have low d (beneficiaries), non-strategic debtors have high d (targets), strategic debtors sit intermediate (they benefit from the structure even while paying some price). The gap is irreducible in a single observational frame because the two seats are reading different constraints: the IMF reads 'multilateral debt coordination' (the constraint that benefits them), the non-strategic debtor reads 'selective disciplinary extraction' (the constraint that harms them). This reading (hybrid_selectivity) acknowledges both reads are partially correct: there is real coordination happening AND real selective extraction happening simultaneously, which only becomes visible when one compares how the same mechanism is applied across debtor positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is power-modulated and geopolitically indexed. IMF/World Bank (institutional power) as agenda-setter has d near 0.0 (full beneficiary): they set the rules and collect institutional rents (expanded mandate, creditor deference, policy authority). Hegemon-aligned creditor states (institutional power) have d near 0.1-0.2 (beneficiaries with minimal cost): they collect debt service while conditionalities are selectively lightened for them; their exit option is arbitrage (they can attract capital from alternative sources, giving them bargaining power). Core creditor institutions have d near 0.05 (pure beneficiaries): they receive prioritized repayment through conditionality enforcement. Non-strategic peripheral debtors (powerless) have d near 0.95 (full targets): they pay the full extraction cost (austerity, asset privatization, labor deregulation) with no exit. Geopolitically strategic debtors (organized power) have d near 0.4-0.5 (intermediate): they pay some price (fiscal austerity, some privatization) but negotiate exemptions from the harshest elements (labor cuts, subsidy removal), accessing resources that non-strategic peers cannot. Domestic labor coalitions have d near 0.85 (near-full targets): they bear direct extraction via labor deregulation and public-sector downsizing; their identity-lock (inability to exit the nation-state) amplifies their target position. Subsistence populations have d near 1.0 (absolute targets): they experience extraction with no negotiating power, geographic mobility, or voice in the process. This spread (0.0 to 1.0) across stakeholder seats is the signature of hybrid selectivity: a constraint that could present as pure coordination (if all debtors were treated identically) only appears as extraction when the differential treatment is exposed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (runs on sovereigns, inability to orchestrate multilateral restructuring) was real and acute in the 1980s-1991 debt crisis period. By the late 1990s, restructuring technology had improved, repeated restructurings had been normalized, and default patterns stabilized — suggesting the founding coordination problem had been substantially resolved. However, the conditionality apparatus persisted. The R5 interview (founding_problem_status=contested) captures this: the IMF attests the problem remains live (ongoing default risk, moral hazard), non-strategic debtor governments and outside economists attest the problem has been solved (runs no longer occur; restructuring is routine). The World Bank's own 2015 evaluation documents that conditionality produces pro-cyclical fiscal cuts that deepen recessions and harm long-term growth in peripheral debtors, while being selectively waived for strategic debtors — evidence that the apparatus's empirical function has shifted from coordination to selective rent collection. The theater ratio trajectory (0.25 → 0.48) reflects this shift: the coordination narrative is maintained in IMF documentation and board-level rhetoric, but empirical implementation increasingly defends selectivity rather than universal discipline. This is not yet mandatrophy_resolved (the constraint still functions, it still extracts, it still enforces), but it is mandatrophy-adjacent: the founding mandate (global coordination for debt stability) no longer justifies the primary empirical effect (selective extraction from non-strategic debtors). The mechanism persists because it benefits creditors and strategically aligned states, not because the original problem requires it. A full mandatrophy diagnosis would require showing that the constraint could be removed entirely without affecting coordination outcomes (i.e., that alternative creditor structures would coordinate just as effectively) — a counterfactual that requires either evidence from cases where conditionality was absent or a credible model showing coordination is feasible without IMF binding authority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selectivity_ambiguity,
    'Is the observed selectivity in conditionality enforcement a feature of the coordination mechanism responding to legitimate geopolitical risk differences, or is it evidence that the constraint has shifted from coordination to selective extraction masquerading as coordination?',
    'Empirical: Compare conditionality packages across debtor states with similar debt-to-GDP ratios, current account deficits, and fiscal profiles but differing geopolitical alignment. If conditioning severity is uncorrelated with economic indicators but strongly correlated with alignment (controlling for development level), the selectivity is structural, not justified by risk differences.',
    'If selectivity is unjustified by economic fundamentals, the constraint reclassifies from pure coordination (creditor_coordination_reading) toward extraction (debtor_extraction_reading), and this hybrid_selectivity_reading becomes the most accurate description. If selectivity is justified by risk pricing, the creditor_coordination_reading is vindicated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_selectivity_ambiguity, empirical, 'Whether conditionality selectivity is justified by risk differentials or driven by geopolitical power asymmetry.').

omega_variable(
    founding_problem_obsolescence_vs_mandate_creep,
    'Did the founding coordination problem (runs on sovereigns in the 1980s) remain structurally live through the 2000s-2020s, or did conditionality persist after its primary function was solved, morphing into a rent-extraction mechanism justified by the residual risk of moral hazard?',
    'Historical analysis of default patterns and restructuring negotiations: (1) Did restructuring success rates improve over time, suggesting the coordination mechanism was working? (2) Did default frequency decline persistently, or did crises persist despite conditionality? (3) Did alternative, non-IMF restructuring mechanisms emerge and function without the discipline apparatus? If (1) yes and (2) declining and (3) no, coordination is live. If (1) yes and (2) unchanged and (3) yes, coordination is obsolete.',
    'If the founding problem became obsolete but the apparatus persisted, the constraint is mandatrophy-adjacent: the mandate is outdated, enforcement is theatrical, but institutional inertia and creditor benefit keep it in place.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence_vs_mandate_creep, empirical, 'Whether the founding coordination problem remains live or has been solved, leaving the apparatus as institutional inertia.').

omega_variable(
    sibling_reading_coexistence,
    'Do this reading (hybrid selectivity) and the debtor_extraction_reading logically foreclose each other, or are they live alternative framings of the same constraint that different parties hold simultaneously?',
    'Examine whether the core claims are logically inconsistent. The hybrid reading claims both coordination and asymmetric extraction are structurally real. The debtor reading claims extraction is primary and coordination is cover. These coexist if both components are empirically real; they foreclose if one reading''s core mechanism is empirically false.',
    'If they coexist, this reading occupies an empirical midpoint. If one forecloses the other, the reading is incomplete.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_coexistence, conceptual, 'Whether the hybrid selectivity reading and pure extraction reading logically foreclose each other or coexist.').

omega_variable(
    identity_lock_mechanism_in_labor_coalitions,
    'Is labor''s identity-locked position (inability to exit the national state''s debt situation) a structural feature or an internalized belief that would dissolve if exit became economically feasible?',
    'Natural experiments: Compare labor mobility and organizing across debtor states with identical conditionalities but different emigration costs. Higher organizing rates in high-mobility contexts would suggest identity-lock is not purely structural. Historical analysis: Did labor coalition exit (emigration, brain drain) accelerate during severe conditionality periods?',
    'Structural identity-lock amplifies labor''s suppression (snare direction). Partially internalized identity-lock means recovery is possible post-constraint (tangled_rope direction). This affects the typology of the non-strategic debtor seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_in_labor_coalitions, empirical, 'Whether labor''s inability to exit the national state''s debt position is structural or internalized.').

omega_variable(
    alternative_creditor_exclusion_necessity,
    'Is the exclusion of non-IMF creditors from restructuring authority a structural necessity for the conditionality mechanism''s function, or is it a design choice that perpetuates the IMF''s institutional power?',
    'Counterfactual: If alternative creditors were admitted to restructuring authority with equal weight, would debt coordination functions persist? Recent cases (Belt and Road, external creditor councils) suggest coordination CAN occur with plural creditor participation but with weaker IMF discipline. If coordination is possible without exclusivity, exclusion is rent-defense.',
    'If exclusion is necessary, it is part of the coordination structure. If exclusion is a design choice, the constraint is closer to snare (the exclusion itself is an enforcement mechanism).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_creditor_exclusion_necessity, empirical, 'Whether alternative-creditor exclusion is structurally necessary or a rent-protecting institutional choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stru_tr_t0, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(stru_tr_t0, observed).
narrative_ontology:measurement(stru_tr_t5, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement_basis(stru_tr_t5, observed).
narrative_ontology:measurement(stru_tr_t10, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement_basis(stru_tr_t10, observed).
narrative_ontology:measurement(stru_tr_t15, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 15, 0.41).
narrative_ontology:measurement_basis(stru_tr_t15, observed).
narrative_ontology:measurement(stru_tr_t20, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 20, 0.45).
narrative_ontology:measurement_basis(stru_tr_t20, observed).
narrative_ontology:measurement(stru_tr_t25, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 25, 0.48).
narrative_ontology:measurement_basis(stru_tr_t25, observed).
narrative_ontology:measurement(stru_tr_t30, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement_basis(stru_tr_t30, observed).
narrative_ontology:measurement(stru_tr_t35, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 35, 0.48).
narrative_ontology:measurement_basis(stru_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(stru_be_t0, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(stru_be_t0, observed).
narrative_ontology:measurement(stru_be_t5, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement_basis(stru_be_t5, observed).
narrative_ontology:measurement(stru_be_t10, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement_basis(stru_be_t10, observed).
narrative_ontology:measurement(stru_be_t15, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 15, 0.68).
narrative_ontology:measurement_basis(stru_be_t15, observed).
narrative_ontology:measurement(stru_be_t20, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 20, 0.71).
narrative_ontology:measurement_basis(stru_be_t20, observed).
narrative_ontology:measurement(stru_be_t25, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 25, 0.72).
narrative_ontology:measurement_basis(stru_be_t25, observed).
narrative_ontology:measurement(stru_be_t30, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 30, 0.72).
narrative_ontology:measurement_basis(stru_be_t30, observed).
narrative_ontology:measurement(stru_be_t35, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 35, 0.72).
narrative_ontology:measurement_basis(stru_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(stru_su_t0, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 0, 0.64).
narrative_ontology:measurement_basis(stru_su_t0, observed).
narrative_ontology:measurement(stru_su_t5, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement_basis(stru_su_t5, observed).
narrative_ontology:measurement(stru_su_t10, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 10, 0.71).
narrative_ontology:measurement_basis(stru_su_t10, observed).
narrative_ontology:measurement(stru_su_t15, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 15, 0.74).
narrative_ontology:measurement_basis(stru_su_t15, observed).
narrative_ontology:measurement(stru_su_t20, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 20, 0.76).
narrative_ontology:measurement_basis(stru_su_t20, observed).
narrative_ontology:measurement(stru_su_t25, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 25, 0.77).
narrative_ontology:measurement_basis(stru_su_t25, observed).
narrative_ontology:measurement(stru_su_t30, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 30, 0.78).
narrative_ontology:measurement_basis(stru_su_t30, observed).
narrative_ontology:measurement(stru_su_t35, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 35, 0.78).
narrative_ontology:measurement_basis(stru_su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_adjustment_conditionalities__hybrid_selectivity_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.18).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__hybrid_selectivity_reading, structural_adjustment_conditionalities__creditor_coordination_reading).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__hybrid_selectivity_reading, structural_adjustment_conditionalities__debtor_extraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested kernel structural_adjustment_conditionalities. The creditor_coordination_reading emphasizes the coordination function and treats selectivity as appropriate risk pricing; the debtor_extraction_reading emphasizes the extraction function and treats coordination as cover. This hybrid_selectivity_reading claims both are structurally real and locates the reading contest in which component is primary and whether selectivity is justified. All three stories share the same kernel (the formal conditionality requirements) but decompose it differently based on which authority grounding, interpretation layer, and beneficiary structure the reading privileges. The three stories are linked by network.affects_constraints to enable contamination propagation analysis: if one reading's empirical basis is refuted, the network captures the structural dependency (e.g., if selectivity is proven justified by risk differentials, the creditor_coordination_reading strengthens and the debtor_extraction_reading weakens).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(structural_adjustment_conditionalities__hybrid_selectivity_reading, institutional, 0.08).
constraint_indexing:directionality_override(structural_adjustment_conditionalities__hybrid_selectivity_reading, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
