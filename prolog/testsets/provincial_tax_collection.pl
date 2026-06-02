% ============================================================================
% CONSTRAINT STORY: provincial_tax_collection
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_provincial_tax_collection, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: provincial_tax_collection
 *   human_readable: Provincial Tax Collection System
 *   domain: fiscal_governance/political_economy
 *
 * SUMMARY:
 *   Provincial tax collection systems exemplify the structural entanglement
 *   of genuine public-goods coordination with extraction mechanisms.
 *   Populations require collective funding for infrastructure (roads, courts,
 *   schools, healthcare), which creates a coordination problem that taxation
 *   solves. Simultaneously, the taxing authority possesses structural
 *   advantages — power to set rates, audit capacity, monopoly on coercion —
 *   that enable extraction beyond what pure coordination requires. The
 *   constraint exhibits all characteristics of tangled rope: (1) genuine
 *   coordination function (public goods provision), (2) asymmetric extraction
 *   (taxpayers bear compliance burden; administrators capture discretionary
 *   power), (3) active enforcement (audit, penalty, collection mechanisms).
 *   The temporal measurements reveal increasing theater ratio (0.35 → 0.48)
 *   and rising suppression requirement (0.55 → 0.65), indicating that the
 *   system is accumulating performative complexity (tax code exemptions,
 *   deductions, special treatments) and enforcement intensity over the
 *   interval. This trajectory suggests the constraint is drifting from mixed
 *   coordination toward extraction dominance.
 *
 * KEY AGENTS:
 *   - Ordinary Taxpayers: Primary victims (powerless/trapped) — mandatory compliance with no viable exit; maximum suppression and experienced extraction
 *   - Middle-Class Property Holders: Secondary victims (moderate/constrained) — high compliance costs and audit risk, constrained exit through relocation barriers, mixed benefit from infrastructure
 *   - Business Sector: Mixed actor (organized/constrained) — benefits from infrastructure and legal system, faces significant corporate tax extraction and compliance burden, exit constrained by capital specificity
 *   - Wealthy Elite / Capital Holders: Primary beneficiaries (powerful/arbitrage) — disproportionate benefit from public goods protecting asset rights, strong exit options through tax planning and relocation, minimal experienced extraction
 *   - Provincial Administration: Primary beneficiary (institutional/arbitrage) — captures substantial revenue and monopoly enforcement power, experiences tax collection as coordination mechanism, extensive arbitrage through rate and enforcement adjustment
 *   - Tax Authority Officials: Mixed actors (powerful/arbitrage) — benefit from job security and institutional power, constrained by budget pressure and political oversight, mixed extraction through discretionary authority
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional choices as inherent to organized states
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(provincial_tax_collection, 0.52).
domain_priors:suppression_score(provincial_tax_collection, 0.65).
domain_priors:theater_ratio(provincial_tax_collection, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(provincial_tax_collection, extractiveness, 0.52).
narrative_ontology:constraint_metric(provincial_tax_collection, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(provincial_tax_collection, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(provincial_tax_collection, tangled_rope).
narrative_ontology:human_readable(provincial_tax_collection, "Provincial Tax Collection System").
narrative_ontology:topic_domain(provincial_tax_collection, "fiscal_governance/political_economy").

domain_priors:requires_active_enforcement(provincial_tax_collection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(provincial_tax_collection, provincial_administration).
narrative_ontology:constraint_beneficiary(provincial_tax_collection, public_infrastructure_beneficiaries).
narrative_ontology:constraint_victim(provincial_tax_collection, taxpayers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ORDINARY TAXPAYER (SNARE) — Citizens face mandatory tax collection with severe penalties for non-compliance (asset seizure, imprisonment, wage garnishment). Exit is structurally impossible: mobility within the same jurisdiction does not escape tax jurisdiction; migration to avoid taxes faces legal barriers (capital controls, residency requirements, tax treaties). The taxpayer experiences maximum suppression and effective extraction with minimal coordination benefit visible from their position.
constraint_indexing:constraint_classification(provincial_tax_collection, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: MIDDLE-CLASS PROPERTY HOLDER (TANGLED ROPE) — Faces high compliance costs (record-keeping, audit risk, penalty exposure) but also benefits from public infrastructure funded through tax collection (roads, schools, legal system). Exit is constrained by high relocation cost (property sales tax, transaction costs, establishing new community ties) but possible. Experiences both genuine coordination (infrastructure) and asymmetric extraction (audit burden concentrated on visible property owners, capital gains tax).
constraint_indexing:constraint_classification(provincial_tax_collection, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PROVINCIAL ADMINISTRATION (ROPE) — Benefits substantially from tax collection through legitimate revenue capture that funds operations and services. Experiences the constraint as a coordination mechanism: collecting taxes solves the real problem of funding public goods. Has arbitrage options (setting rates, adjusting collection intensity, revenue bonds) and effectively unlimited exit from enforcement pressure through monopoly on legitimate force. Experiences minimal suppression and negative extraction (revenue flows toward this agent).
constraint_indexing:constraint_classification(provincial_tax_collection, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: BUSINESS SECTOR (TANGLED ROPE) — Corporations and merchants benefit from infrastructure (port facilities, transportation networks, legal contract enforcement) yet face significant extraction through corporate income tax, value-added tax, and audit intensity targeted at high-revenue entities. Exit is constrained by capital specificity (manufacturing plants, retail locations, supply chains rooted in jurisdiction) but possible through relocation or corporate restructuring. Experience is mixed: genuine coordination function (roads enable commerce) alongside asymmetric extraction (corporate tax rates, transfer pricing scrutiny, compliance burden).
constraint_indexing:constraint_classification(provincial_tax_collection, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: WEALTHY ELITE / CAPITAL HOLDERS (ROPE) — High-net-worth individuals and large capital holders experience tax collection as a coordination mechanism from which they benefit disproportionately. They capture the infrastructure benefits (legal system protects contracts and property rights, police protect wealthy neighborhoods, courts enforce debt collection). Exit options are strong (offshore accounts, tax havens, capital flight, relocation to lower-tax jurisdictions). Effective extraction rates are low due to tax planning options (capital gains treatment, depreciation deductions, charitable structures). Many experience negative net extraction — the public goods funded by collective taxes subsidize their asset security.
constraint_indexing:constraint_classification(provincial_tax_collection, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: TAX AUTHORITY OFFICIALS (TANGLED ROPE) — Bureaucrats and enforcement officials benefit from job security, institutional status, and discretionary power (selective audits, penalty waivers, collection intensity). Simultaneously, they are subject to fiscal pressure (insufficient budgets), political pressure (tax targets set by politicians), and institutional constraints (fairness mandates, due process requirements). They experience mixed extraction: privileged access to institutional power alongside constraints from oversight, public scrutiny, and formal rules. Exit is constrained by career lock-in and institutional identity.
constraint_indexing:constraint_classification(provincial_tax_collection, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 7: REVENUE SYSTEM THEATER (PITON) — From the civilizational analytical perspective, much of the revenue system is performative: complex tax codes with exemptions, deductions, and special treatments that serve political constituencies rather than efficient revenue raising. Tax rates are calibrated to appear fair (progressive brackets) while collection mechanisms extract through compliance burden (accounting costs, audit risk, penalty exposure). The system persists through institutional inertia — the alternative (simplified taxation, clearer extraction) would be politically transparent. The performative character is visible in theater_ratio (0.48) — nearly half the system's energy goes to theater rather than function.
constraint_indexing:constraint_classification(provincial_tax_collection, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some form of public revenue collection is inherent to any organized state capable of providing public goods. The necessity of taxation can appear as an immutable feature of political order — states require revenue; taxation is the mechanism; all populations subject to states face this constraint. However, this classification triggers false-summit detection. The structural data reveals that taxation's specific form (rate structure, enforcement intensity, compliance burden distribution, exemptions) is contingent and contested, not inherent. The 'naturalness' claim masks political choices benefiting specific coalitions.
constraint_indexing:constraint_classification(provincial_tax_collection, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(provincial_tax_collection_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(provincial_tax_collection, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(provincial_tax_collection, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(provincial_tax_collection, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(provincial_tax_collection, TR),
    TR >= 0.70.

:- end_tests(provincial_tax_collection_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The system funds genuine public goods (roads, courts, schools) but the rate structure and compliance burden distribution suggest extraction beyond what pure coordination requires. Corporate tax rates (20-30%), capital gains taxation, and audit intensity concentrated on visible wealth indicate that some extraction is structural rather than functional. The upward trajectory (0.38 → 0.52) suggests rent-seeking layering onto coordination: politicians add tax preferences for constituencies, administrators expand compliance requirements, special interests capture exemptions. Theater ratio (0.48): Moderate. Nearly half the system's complexity serves performative rather than functional purposes: exemptions and deductions that appeal to constituencies but reduce revenue; complex code that enables tax planning; audit procedures that signal fairness but miss sophisticated avoidance. The upward trajectory (0.35 → 0.48) indicates that complexity is accumulating faster than functional necessity. Suppression (0.65): Moderate-high. Legal barriers (tax jurisdiction, residency requirements, penalties for evasion) combine with economic barriers (relocation cost, capital specificity, jurisdictional dependence) to limit exit. Penalties (asset seizure, wage garnishment, imprisonment for evasion) are severe. However, wealthy agents have arbitrage options (offshore accounts, tax havens, transfer pricing) that reduce experienced suppression. The upward trajectory (0.55 → 0.65) suggests enforcement machinery is intensifying — more audits, higher penalties, stricter capital controls.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates massive perspectival disagreement. The ordinary taxpayer sees pure extraction (snare) — mandatory compliance, severe penalties, minimal visible coordination benefit. The provincial administration sees coordination (rope) — legitimate revenue collection that funds operations and public goods. The middle class sees mixed constraint (tangled rope) — infrastructure benefits alongside compliance burden and audit risk. Wealthy elites see coordination (rope) — public goods protect their assets; exit options are available; experienced extraction is minimal. The tax authority officials see constrained power (tangled rope) — discretionary authority alongside budget pressure. The analytical observer risks naturalizing this system as inherent to organized states (mountain) when the structural data reveals political choices (rate structure, enforcement intensity, exemption allocation) that benefit specific coalitions. The perspectival gap reveals that 'taxation' is not a single constraint but a superposition of coordination and extraction mechanisms experienced differently by agents at different structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies substantially across perspectives depending on exit options and beneficiary/victim status. Powerless/trapped taxpayers derive d ≈ 0.95 (full target of extraction). Moderate/constrained middle-class agents derive d ≈ 0.60 (mixed burden and benefit, some exit capacity). Institutional beneficiaries (administration, wealthy elites) with arbitrage options derive d ≈ 0.15-0.30 (benefits exceed costs; extraction flows toward them). The engine's sigmoid function f(d) maps these d values to perceived extractiveness chi: trapped taxpayers experience chi ≈ 1.42 (maximum), institutional beneficiaries experience chi ≈ -0.01 to 0.40 (minimal to negative). The perspectival gap is substantial: the ordinary taxpayer sees snare; the provincial administration sees rope; the analytical observer risks seeing mountain. The actual classification (tangled rope) is the middle position — the system genuinely coordinates public goods AND extracts asymmetrically.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint is genuinely tangled rope: it coordinates public goods provision (solving a real coordination problem) while extracting asymmetrically (distributing burden and benefit unequally). The classification prevents three errors: (1) calling it rope (pure coordination) — this would ignore the real extraction and compliance burden on powerless agents; (2) calling it snare (pure extraction) — this would ignore the genuine public goods benefit; (3) calling it mountain (natural law) — this would naturalize political choices about rate structure, enforcement intensity, and exemption allocation. The tangled rope classification holds both truths: the system solves a coordination problem AND extracts asymmetrically. The mandatrophy dissolves when we recognize that no single type adequately captures the structural reality — the presheaf of perspectives IS the answer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraction_vs_coordination_boundary,
    'What proportion of provincial tax collection funds genuine public goods (roads, courts, education) versus rent-seeking (bureaucratic expansion, corporate subsidies, political patronage)?',
    'Comparative budget analysis across provinces and time periods; correlation between tax rates and public good provision; tracking of expenditure allocations to discretionary vs mandated functions',
    'If genuine public goods > 70%: classification shifts toward rope/tangled_rope across all perspectives. If genuine public goods < 40%: classification shifts toward snare/snare-dominant. Current assumption (moderate mix) supports tangled_rope claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_boundary, empirical, 'Boundary between coordination function and extraction/rent-seeking in tax expenditure').

omega_variable(
    exit_option_asymmetry,
    'How much of the suppression (0.65) derives from legal barriers to exit (residency laws, capital controls, tax treaties preventing escape) versus economic barriers (relocation cost, capital specificity, jurisdictional dependence)?',
    'Comparative analysis of cross-border migration patterns; tracking of capital flight and asset relocation; case law on residency and tax jurisdiction; economic analysis of relocation cost vs tax burden',
    'If suppression is primarily legal: the constraint is more extractive than measured (exit is truly blocked). If suppression is primarily economic: agents have latent arbitrage options and effective extraction is lower than measured. The distinction affects which perspectives perceive true escape routes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_option_asymmetry, empirical, 'Legal vs economic drivers of exit suppression in tax jurisdiction').

omega_variable(
    tax_rate_vs_compliance_cost_tradeoff,
    'What is the optimal tax rate that maximizes revenue while minimizing extraction through compliance burden (accounting costs, audit risk, penalty exposure)? Is current rate below, at, or above this optimum?',
    'Economic analysis of tax revenue curves and deadweight loss; comparative tax compliance cost studies across jurisdictions; correlation between rate changes and voluntary compliance rates',
    'If current rate > optimal: reducing rates would increase net revenue by reducing compliance burden; extraction is hidden in administrative overhead rather than visible tax rate. If current rate ≈ optimal: extraction is genuine and necessary to maintain public goods. If current rate < optimal: the system is under-levying and under-providing public goods.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tax_rate_vs_compliance_cost_tradeoff, empirical, 'Optimal tax rate for revenue maximization given compliance costs').

omega_variable(
    distributive_fairness_perception,
    'Do taxpayers perceive the tax system as fair (proportional to income/ability to pay) or as extractive (benefiting wealthy elites while burdening middle class)?',
    'Survey data on tax fairness perceptions across income brackets; analysis of actual tax incidence (who pays vs who benefits); comparison of perceived vs actual distribution of tax burden',
    'If perceived fairness > 60%: the constraint functions more like rope/coordination from the populace''s perspective — citizens believe they benefit. If perceived fairness < 40%: the constraint functions like snare from the populace''s perspective — citizens see pure extraction. Perception differences between classes reveal perspectival gaps in classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(distributive_fairness_perception, empirical, 'Taxpayer perception of distributive fairness in provincial tax system').

omega_variable(
    capital_mobility_trajectory,
    'Is capital mobility within and across provincial jurisdictions increasing or decreasing over time? Are exit options expanding (more arbitrage opportunity) or contracting (more entrapment)?',
    'Longitudinal tracking of capital flight, business relocation, wealth migration; analysis of tax haven usage and transfer pricing trends; monitoring of jurisdictional tax competition and harmonization efforts',
    'If mobility increasing: wealthy agents and corporations have expanding arbitrage; suppression decreases; the constraint becomes more purely extractive for immobile populations (workers, small property holders). If mobility decreasing: exit options narrow for all agents; suppression increases; the constraint tightens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capital_mobility_trajectory, empirical, 'Trajectory of capital mobility and exit arbitrage options').

omega_variable(
    false_summit_natural_law_claim,
    'Is taxation a natural law inherent to organized political order (mountain), or is the specific form of provincial tax collection (rate structure, enforcement intensity, compliance burden) a contingent institutional arrangement benefiting specific coalitions?',
    'Comparative institutional analysis across provinces and time periods; historical counterfactual analysis of alternative revenue mechanisms (land tax, consumption tax, debt finance); analysis of tax structure changes following political coalitions',
    'If mountain: taxation is inherent and inevitable; policy resistance is irrational. If tangled_rope: the current system is contingent; alternative distributions and enforcement regimes are possible; political contest is structural. FSM detection flags this as a false summit when beneficiaries are declared and strategic interests in maintaining the system are identified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law_claim, conceptual, 'Natural law vs contingent institutional arrangement claim for tax collection').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(provincial_tax_collection, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ptc_tr_t0, provincial_tax_collection, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ptc_tr_t10, provincial_tax_collection, theater_ratio, 10, 0.42).
narrative_ontology:measurement(ptc_tr_t20, provincial_tax_collection, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(ptc_be_t0, provincial_tax_collection, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(ptc_be_t10, provincial_tax_collection, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(ptc_be_t20, provincial_tax_collection, base_extractiveness, 20, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(ptc_su_t0, provincial_tax_collection, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(ptc_su_t10, provincial_tax_collection, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(ptc_su_t20, provincial_tax_collection, suppression_requirement, 20, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(provincial_tax_collection, resource_allocation).
narrative_ontology:affects_constraint(provincial_tax_collection, wealth_inequality_reproduction).
narrative_ontology:affects_constraint(provincial_tax_collection, bureaucratic_rent_seeking).
narrative_ontology:affects_constraint(provincial_tax_collection, capital_flight_arbitrage).

% DUAL FORMULATION NOTE:
% Provincial tax collection is the primary constraint in a family of fiscal governance constraints. Downstream constraints (wealth inequality, bureaucratic rent-seeking, capital flight) are affected by the specific form of tax collection because the rate structure, enforcement intensity, and exemption allocation determine how much extraction occurs and how it distributes across populations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(provincial_tax_collection, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
