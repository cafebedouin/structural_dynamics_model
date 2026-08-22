% ============================================================================
% CONSTRAINT STORY: structural_adjustment_conditionalities__creditor_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_structural_adjustment_conditionalities__creditor_coordination_reading, []).

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
 *   constraint_id: structural_adjustment_conditionalities__creditor_coordination_reading
 *   human_readable: Structural Adjustment Conditionalities (Creditor Coordination Reading)
 *   domain: international_political_economy/development_finance/institutional_economics
 *
 * SUMMARY:
 *   This constraint story represents the creditor coordination reading of
 *   structural adjustment conditionalities: the view that conditionalities
 *   are primarily a mechanism for solving the collective action problem among
 *   diverse creditors by providing a single, credible, monitorable reform
 *   program. In this reading, the extraction observed in other readings
 *   (debtor_extraction_reading, hybrid_selectivity_reading) is either the
 *   necessary cost of coordination (enforcement overhead) or a separate
 *   constraint misidentified as part of the same phenomenon. The claimed_type
 *   is rope because the structural function is genuine coordination with
 *   net-beneficiary participation; the metrics reflect low extractiveness
 *   (0.28 at interval end), low suppression (0.15), and moderate theater
 *   (0.22) — the coordination function is real but not pure.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_adjustment_conditionalities__creditor_coordination_reading, 0.28).
domain_priors:suppression_score(structural_adjustment_conditionalities__creditor_coordination_reading, 0.15).
domain_priors:theater_ratio(structural_adjustment_conditionalities__creditor_coordination_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_adjustment_conditionalities__creditor_coordination_reading, rope).
narrative_ontology:human_readable(structural_adjustment_conditionalities__creditor_coordination_reading, "Structural Adjustment Conditionalities (Creditor Coordination Reading)").
narrative_ontology:topic_domain(structural_adjustment_conditionalities__creditor_coordination_reading, "international_political_economy/development_finance/institutional_economics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(structural_adjustment_conditionalities__creditor_coordination_reading, '696da230-451e-4b40-b91a-c58be5c6200c').
narrative_ontology:cs_kernel_codification('696da230-451e-4b40-b91a-c58be5c6200c', formalized).
narrative_ontology:cs_authority_grounding('696da230-451e-4b40-b91a-c58be5c6200c', expertise).
narrative_ontology:cs_interpretation_layer_present('696da230-451e-4b40-b91a-c58be5c6200c').
narrative_ontology:cs_reading_relation('696da230-451e-4b40-b91a-c58be5c6200c', structural_adjustment_conditionalities__debtor_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('696da230-451e-4b40-b91a-c58be5c6200c', structural_adjustment_conditionalities__hybrid_selectivity_reading, influences).
narrative_ontology:cs_axiom('696da230-451e-4b40-b91a-c58be5c6200c', foundational, creditor_coordination_requires_conditionality).
narrative_ontology:cs_axiom_status(creditor_coordination_requires_conditionality, holdable).
narrative_ontology:cs_axiom_grounding('696da230-451e-4b40-b91a-c58be5c6200c', creditor_coordination_requires_conditionality, instrumental).
narrative_ontology:cs_axiom('696da230-451e-4b40-b91a-c58be5c6200c', secondary, fiscal_sustainability_precedes_social_spending).
narrative_ontology:cs_axiom_status(fiscal_sustainability_precedes_social_spending, holdable).
narrative_ontology:cs_axiom_grounding('696da230-451e-4b40-b91a-c58be5c6200c', fiscal_sustainability_precedes_social_spending, conventional).
narrative_ontology:cs_reference_frame('696da230-451e-4b40-b91a-c58be5c6200c', bretton_woods_crisis_management_framework).
narrative_ontology:cs_drift_state('696da230-451e-4b40-b91a-c58be5c6200c', post_2008_crisis_conditionality_expansion, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('696da230-451e-4b40-b91a-c58be5c6200c', '').
narrative_ontology:cs_kernel_id(structural_adjustment_conditionalities__creditor_coordination_reading, structural_adjustment_conditionalities).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, future_taxpayers).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, international_capital_markets).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, multilateral_development_banks).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__creditor_coordination_reading, inefficient_state_sectors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__creditor_coordination_reading, sovereign_debtor_governments).
narrative_ontology:constraint_vindicates(structural_adjustment_conditionalities__creditor_coordination_reading, fiscal_sustainability_conditionality).
narrative_ontology:constraint_vindicates(structural_adjustment_conditionalities__creditor_coordination_reading, market_confidence_signaling).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and administer conditionality programs as the primary mechanism for coordinating creditor expectations and debtor reform. They set the policy matrix, monitor compliance, and control disbursement. Their legitimacy depends on being seen as neutral coordinators rather than partisan enforcers. They can redirect resources across programs but face reputational costs if conditionalities systematically fail.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, imf_world_bank, agenda_setter,
    institutional, generational, arbitrage, global).

% Accept conditionality packages to access concessional financing and market re-entry. They bear the political cost of implementing reforms (subsidy cuts, privatization, tax increases) while the benefits accrue over longer horizons. Exit means default or seeking alternative financing (regional banks, bilateral lenders, capital markets) — all costlier or unavailable in crisis. Their negotiating power varies with reserve levels, geopolitical alignment, and domestic institutional capacity.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, sovereign_debtor_governments, payer,
    moderate, biographical, constrained, national).

% Gain a credible commitment device: conditionality signals that a sovereign will prioritize debt service and macro stability. This lowers risk premia, enables market access at sustainable rates, and creates a focal point for coordinated lending. They do not administer programs but their pricing behavior validates or undermines the coordination. Exit is trivial — capital reallocates instantly across sovereigns and asset classes.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, international_capital_markets, beneficiary,
    powerful, biographical, mobile, global).

% Inherit the fiscal space created by today's adjustment: lower debt service burdens, functional public services, and macro stability. They have no voice in current negotiations and cannot exit the polity whose debts are being restructured. Their benefit is real but delayed, diffuse, and contingent on reforms actually improving long-run capacity rather than merely extracting present resources.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, future_taxpayers, beneficiary,
    powerless, generational, identity_locked, national).

% State-owned enterprises, bloated bureaucracies, and protected industries that capture rents under the pre-reform equilibrium. Conditionalities target their subsidies, monopolies, and employment guarantees. They resist through political mobilization, strikes, and capture of implementation agencies. Exit means restructuring or privatization — they fight to preserve the rents the constraint threatens.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, inefficient_state_sectors, payer,
    organized, immediate, constrained, national).

% Represent constituencies affected by austerity measures (public sector workers, pensioners, users of subsidized services). They are consulted but not empowered in program design. Their objection is that conditionalities impose regressive cuts without protecting vulnerable populations. They can mobilize protests, litigation, and international advocacy but cannot veto programs. Exit means operating outside the formal process — shadow reporting, parallel monitoring.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, civil_society_organizations, excluded,
    moderate, biographical, mobile, national).

% Produces the evidence base on conditionality effectiveness: growth impacts, poverty outcomes, institutional change. Their research shapes the intellectual legitimacy of the coordination framework. They neither collect rents nor bear costs directly. Exit is irrelevant — their role is epistemic, not operational.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, development_economics_academy, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective action problem among heterogeneous creditors (multilaterals, bilaterals, private bondholders) by providing a single, credible reform program that all can rally around. Without it, each creditor would demand bilateral assurances, hold out for better terms, or free-ride on others' restraint — delaying adjustment and raising the cost of crisis resolution. The conditionality matrix internalizes the coordination externality.
% TRANSFER_FUNCTION: Moves policy autonomy from sovereign debtor governments to the multilateral institutions that design and monitor reform programs. In exchange, debtors receive concessional financing, debt relief, and restored market access. The transfer is intertemporal: present policy sovereignty for future fiscal sustainability. Inefficient state sectors lose captured rents; future taxpayers and international capital gain lower risk and higher credibility.
% ABSENT_VOICES: Communities directly impacted by subsidy removal, public sector retrenchment, and user fee introduction — particularly in rural areas and informal economies where state capacity to mitigate is weakest. They are not at the negotiating table; their representatives (civil society, opposition parties) are consulted but not empowered. Their absence is structural: the coordination problem is defined among creditors and the sovereign executive, not among all affected constituencies.
% DISAPPEARANCE_RATIONALE: If conditionalities vanished overnight, creditor coordination would collapse into bilateral bargaining and holdout problems. Sovereigns would face fragmented, inconsistent demands; crisis resolution would slow; market access would shrink for all but the strongest borrowers. The fiscal sustainability gains from coordinated adjustment would be lost. A new coordination mechanism (regional arrangements, sovereign debt restructuring frameworks) would eventually emerge but with higher transaction costs and weaker enforcement.
% FOUNDING_PROBLEM: The 1980s debt crisis revealed that uncoordinated creditor claims and sovereign discretion produced serial rescheduling without adjustment — debt grew, growth stalled, and markets stayed closed. Conditionalities were built to break this deadlock by tying financing to a credible, monitorable reform path that all creditors could accept.
% FOUNDING_PROBLEM_CORROBORATION: IMF and World Bank institutional histories attest the founding problem was creditor coordination failure in the 1980s. Independent economic historians (e.g., Eichengreen, Sachs) corroborate the coordination rationale but argue the design over-indexed on fiscal targets and under-indexed on institutional capacity. Debtor country policymakers from the era (Latin American finance ministers, African central bank governors) attest the problem was real but the solution was imposed asymmetrically. No single corroborating source outside the benefiting parties exists — the genealogy is itself contested.
narrative_ontology:disappearance_verdict(structural_adjustment_conditionalities__creditor_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(structural_adjustment_conditionalities__creditor_coordination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(structural_adjustment_conditionalities__creditor_coordination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(structural_adjustment_conditionalities__creditor_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(structural_adjustment_conditionalities__creditor_coordination_reading, 0.28, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(structural_adjustment_conditionalities__creditor_coordination_reading_tests).
:- end_tests(structural_adjustment_conditionalities__creditor_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low because the primary transfer is policy autonomy for financing access — a voluntary exchange at the sovereign level, even if domestic losers exist. Suppression is low because the constraint operates through market access and reputation rather than coercion; sovereigns can and do refuse programs (e.g., Malaysia 1998, Argentina 2001-2003). Theater is moderate because some conditionalities (structural benchmarks) become ritualistic — implemented formally without substantive change — but the core fiscal/monetary targets remain enforced. Accessibility_collapse (0.35) reflects that alternatives (capital controls, bilateral deals, default) exist but are costly. Resistance (0.45) captures organized opposition from inefficient state sectors and civil society.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (sovereign governments, inefficient state sectors) and the agenda_setter seat (IMF/World Bank) should compute differently. From the Fund/Bank perspective, the constraint is a coordination mechanism they built and maintain; from the sovereign perspective, it is a conditional exchange; from the inefficient sectors' perspective, it is targeted extraction. The engine computes this divergence from the structural data — the creditor coordination reading claims rope for all seats, but the debtor_extraction_reading would claim snare for the same constraint viewed from the inefficient_state_sectors seat.
 *
 * DIRECTIONALITY LOGIC:
 *   The IMF/World Bank (agenda_setter, institutional power, arbitrage exit) sits near the beneficiary end — they administer the coordination and gain legitimacy from successful programs. Sovereign debtors (payer, moderate power, constrained exit) bear the immediate political costs but gain market access; their d is near symmetric (0.5). International capital (beneficiary, powerful, mobile exit) is the clearest beneficiary — d near 0.0. Future taxpayers (beneficiary, powerless, identity_locked) benefit intergenerationally but cannot consent — d is low but identity_locked modulates it. Inefficient state sectors (payer, organized, constrained) are the concentrated losers — d near 1.0. Civil society (excluded, moderate, mobile) has voice but no veto.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (creditor coordination failure in serial debt crises) was real in the 1980s. By the 2000s, the coordination infrastructure (Paris Club, London Club, collective action clauses, IMF surveillance) had matured — the original coordination problem is substantially solved. Yet conditionalities persist and have expanded into structural, governance, and social domains. This reading acknowledges the mandate has drifted: the coordination function remains but now carries additional objectives (poverty reduction, climate, governance) that were not in the founding problem. The mandatrophy is unresolved — the constraint persists partly because the coordination infrastructure it created is now the only game in town for crisis lending.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction_boundary,
    'Where does the coordination function end and the extraction function begin in conditionality design? Are the structural benchmarks (privatization, labor reform, governance) necessary for creditor coordination, or do they serve creditor/private sector interests beyond coordination?',
    'Counterfactual analysis: compare programs with only macro-fiscal conditionality vs. programs with extensive structural conditionality. If creditor coordination (measured by program participation, market re-entry speed, risk premia compression) is achieved equally by both, the structural benchmarks are excess extraction.',
    'If structural benchmarks are coordination-unnecessary, the constraint decomposes: a rope (macro-fiscal conditionality) plus a snare/tangled_rope (structural conditionality). This would validate the debtor_extraction_reading''s claim for the structural component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, empirical, 'Whether the full conditionality matrix is necessary for the coordination function or contains extractive elements.').

omega_variable(
    counterfactual_coordination_mechanism,
    'Would a credible alternative coordination mechanism (sovereign debt restructuring framework with automatic stays, regional financing arrangements) achieve the same creditor coordination with lower extraction?',
    'Historical analysis of episodes where conditionality was absent or weakened (e.g., Brady Plan, Heavily Indebted Poor Countries Initiative, recent Common Framework). Compare coordination outcomes (creditor participation, holdout rates, market access restoration) and extraction levels (policy autonomy loss, social cost).',
    'If alternatives achieve coordination with lower extraction, the current constraint is not a minimal coordination mechanism — it embeds extractive design choices. This would shift classification toward tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_coordination_mechanism, conceptual, 'Whether the coordination function requires the specific institutional form of IMF/World Bank conditionality.').

omega_variable(
    committer_frame_disambiguation,
    'This constraint is one reading (creditor_coordination_reading) of the structural_adjustment_conditionalities kernel. The sibling readings (debtor_extraction_reading, hybrid_selectivity_reading) claim different ε, different victim/beneficiary structures, and different types for the same label. Does the ε-invariance principle hold — are these structurally distinct constraints, or one constraint with observer-dependent metrics?',
    'Decompose the label ''structural adjustment conditionalities'' into its empirically separable components: macro-fiscal targets, structural benchmarks, social protection floors, governance conditionality. Assign each component its own ε, stakeholders, and classification. If components have stable ε across readings, the kernel is a single constraint with contested metrics. If components split across readings, the kernel is a family of distinct constraints.',
    'If decomposition reveals distinct constraints, the kernel label is a false unity — each reading instantiates a different constraint (or set of constraints). The creditor_coordination_reading would map to the macro-fiscal component; the debtor_extraction_reading to the structural benchmarks component; the hybrid_selectivity_reading to the geopolitical waiver component.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_disambiguation, conceptual, 'Whether the kernel label covers one constraint or a family — the ε-invariance test for this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_adjustment_conditionalities__creditor_coordination_reading, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stru_tr_t1980, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 1980, 0.18).
narrative_ontology:measurement(stru_tr_t1990, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 1990, 0.22).
narrative_ontology:measurement(stru_tr_t2000, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 2000, 0.25).
narrative_ontology:measurement(stru_tr_t2010, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(stru_tr_t2020, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 2020, 0.21).
narrative_ontology:measurement(stru_tr_t2025, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 2025, 0.22).

% Extraction over time
narrative_ontology:measurement(stru_be_t1980, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 1980, 0.42).
narrative_ontology:measurement(stru_be_t1990, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 1990, 0.35).
narrative_ontology:measurement(stru_be_t2000, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 2000, 0.28).
narrative_ontology:measurement(stru_be_t2010, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 2010, 0.22).
narrative_ontology:measurement(stru_be_t2020, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 2020, 0.25).
narrative_ontology:measurement(stru_be_t2025, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 2025, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(stru_su_t1980, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 1980, 0.25).
narrative_ontology:measurement(stru_su_t1990, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 1990, 0.2).
narrative_ontology:measurement(stru_su_t2000, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 2000, 0.15).
narrative_ontology:measurement(stru_su_t2010, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 2010, 0.12).
narrative_ontology:measurement(stru_su_t2020, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 2020, 0.14).
narrative_ontology:measurement(stru_su_t2025, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 2025, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_adjustment_conditionalities__creditor_coordination_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(structural_adjustment_conditionalities__creditor_coordination_reading, 0.12).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__creditor_coordination_reading, sovereign_debt_restructuring_framework).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__creditor_coordination_reading, multilateral_surveillance_architecture).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__creditor_coordination_reading, capital_account_management_policies).

% DUAL FORMULATION NOTE:
% This reading (creditor_coordination_reading) and the debtor_extraction_reading decompose the kernel 'structural_adjustment_conditionalities' into distinct constraints. The creditor reading sees a rope (coordination function primary, extraction as overhead); the debtor reading sees a snare/tangled_rope (extraction primary, coordination as cover). The hybrid_selectivity_reading adds a geopolitical dimension: conditionalities as a tool applied selectively. All three share the kernel_id but instantiate different ε, different stakeholders, and different classifications. They are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(structural_adjustment_conditionalities__creditor_coordination_reading, powerless, 0.15).
constraint_indexing:directionality_override(structural_adjustment_conditionalities__creditor_coordination_reading, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
