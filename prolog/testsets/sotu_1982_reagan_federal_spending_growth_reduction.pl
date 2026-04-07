% ============================================================================
% CONSTRAINT STORY: sotu_1982_reagan_federal_spending_growth_reduction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1982_reagan_federal_spending_growth_reduction, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sotu_1982_reagan_federal_spending_growth_reduction
 *   human_readable: Federal Spending Growth Rate Reduction (Reagan 1982)
 *   domain: governance/fiscal_policy
 *
 * SUMMARY:
 *   The 1982 Reagan SOTU commitment to reduce the rate of growth in federal
 *   spending by approximately 50% represents a structural constraint on
 *   fiscal policy that simultaneously functions as an ideological device and
 *   an extraction mechanism. The constraint operates not through absolute
 *   spending cuts (which would be politically infeasible) but through
 *   growth-rate reduction, which produces real cost increases for federal
 *   program recipients and public sector workers over time. The mechanism
 *   aligns with supply-side theory: by reducing government's fiscal footprint
 *   and removing what Reagan framed as government's 'hidden incentive to grow
 *   larger at the expense of American workers,' the constraint creates fiscal
 *   space for tax reduction and validates the claim that government itself is
 *   extractive. The constraint exhibits Tangled Rope characteristics: it
 *   coordinates real benefits (inflation control narrative, deficit reduction
 *   as shared fiscal burden) alongside asymmetric extraction (costs fall
 *   almost entirely on powerless recipients and constrained public sector
 *   workers). Theater content is substantial and rising: the nominal
 *   growth-rate target is achievable only through definitional boundaries
 *   (excluding entitlements, counting inflation differently across programs),
 *   Congressional workarounds (emergency spending, appropriations bills), and
 *   narrative framing that obscures the distributional consequences. The
 *   false summit risk is high: from the analytical/civilizational
 *   perspective, spending constraints appear as mathematical inevitabilities
 *   of fiscal sustainability, naturalizing what is actually a contingent
 *   choice about whose programs to constrain and whose tax burdens to reduce.
 *
 * KEY AGENTS:
 *   - Federal Program Recipients (powerless/trapped): Medicaid beneficiaries, disability recipients, welfare recipients, SNAP participants — depend on federal assistance and face real benefit erosion as growth is constrained but inflation persists
 *   - Public Sector Workers (powerless/trapped): Federal employees, state and local government workers, defense contractors — face hiring freezes, wage constraints, and job losses during recession; private sector offers no exit during 1982-1983 downturn
 *   - Inflation-Hit Middle Class (moderate/constrained): Non-wealthy households experiencing 1970s inflation effects; benefit from constraint through narrative of deficit control and potential interest rate moderation; harmed through public investment cuts and job losses
 *   - Affluent Taxpayers and Supply-Side Coalition (institutional/arbitrage): Primary beneficiaries; constraint frames government spending as extraction from private sector; benefits from tax-cut complement and validation of supply-side theory; abundant exit options through capital reallocation
 *   - State and Local Governments (organized/constrained): Receive federal Medicaid and education funds; forced to absorb costs through local taxation or service cuts; participate in deficit-reduction narrative but face fiscal pressure
 *   - Federal Budgeting Apparatus (institutional/arbitrage): Congress, OMB, agency administrators who implement constraint; maintain theatrical appearance of constraint while using workarounds; piton classification reflects degradation of nominal constraint through procedural inertia
 *   - Analytical Observer (analytical/analytical): Risk of naturalizing contingent fiscal choices as inevitable mathematical limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1982_reagan_federal_spending_growth_reduction, 0.52).
domain_priors:suppression_score(sotu_1982_reagan_federal_spending_growth_reduction, 0.65).
domain_priors:theater_ratio(sotu_1982_reagan_federal_spending_growth_reduction, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1982_reagan_federal_spending_growth_reduction, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_1982_reagan_federal_spending_growth_reduction, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(sotu_1982_reagan_federal_spending_growth_reduction, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1982_reagan_federal_spending_growth_reduction, tangled_rope).
narrative_ontology:human_readable(sotu_1982_reagan_federal_spending_growth_reduction, "Federal Spending Growth Rate Reduction (Reagan 1982)").
narrative_ontology:topic_domain(sotu_1982_reagan_federal_spending_growth_reduction, "governance/fiscal_policy").

domain_priors:requires_active_enforcement(sotu_1982_reagan_federal_spending_growth_reduction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1982_reagan_federal_spending_growth_reduction, taxpayers_affluent).
narrative_ontology:constraint_beneficiary(sotu_1982_reagan_federal_spending_growth_reduction, deficit_reduction_narrative).
narrative_ontology:constraint_victim(sotu_1982_reagan_federal_spending_growth_reduction, federal_program_recipients).
narrative_ontology:constraint_victim(sotu_1982_reagan_federal_spending_growth_reduction, public_sector_workers).
narrative_ontology:constraint_victim(sotu_1982_reagan_federal_spending_growth_reduction, poor_and_disabled).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FEDERAL PROGRAM RECIPIENTS (SNARE) — Depend on federal assistance (Social Security supplements, disability, welfare, Medicaid, food assistance). Cannot exit government programs without relocation or family support. Face erosion of benefits in real terms as growth is constrained but inflation continues. Maximum suppression: alternative funding sources do not exist; no geographic arbitrage available.
constraint_indexing:constraint_classification(sotu_1982_reagan_federal_spending_growth_reduction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PUBLIC SECTOR WORKERS (SNARE) — Federal and state government employees face constrained wage growth and hiring freezes. Exit options severely limited by sectoral recession (1982 unemployment peak at 10.8%). Career path locked into government employment; private sector offers no alternative during downturn. Suppression: economic conditions eliminate mobility even when constraints theoretically permit it.
constraint_indexing:constraint_classification(sotu_1982_reagan_federal_spending_growth_reduction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: INFLATION-HIT MIDDLE CLASS (TANGLED ROPE) — Experiences real wage erosion from 1970s inflation (peaked 1980 at 13.5%). Constraint benefits them through deficit reduction narrative and interest rate stabilization potential. Also constrains them through reduced public investments (infrastructure, education) and public sector job loss. Mixed experience: genuine coordination benefit (controlling inflation) alongside real extraction (reduced services, public job losses).
constraint_indexing:constraint_classification(sotu_1982_reagan_federal_spending_growth_reduction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: AFFLUENT TAXPAYERS AND SUPPLY-SIDE COALITION (ROPE) — Primary beneficiaries. Constraint positions spending reduction as complement to tax cuts: smaller fiscal footprint removes 'hidden incentive to grow larger at the expense of American workers.' Reframes government spending as extraction from the private sector. Experiences constraint as coordination mechanism: creates fiscal space for tax reduction and validates supply-side theory. Exit options abundant: can relocate capital, adjust investment allocations, shift between public and private returns.
constraint_indexing:constraint_classification(sotu_1982_reagan_federal_spending_growth_reduction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: STATE AND LOCAL GOVERNMENTS (TANGLED ROPE) — Receive federal funds for Medicaid, education, infrastructure. Constraint forces them to absorb costs through increased local taxation, service cuts, or borrowing. Coordination benefit: participating in national deficit reduction narrative, federal matching requirements create some efficiency incentives. Extraction: unfunded mandates accumulate; inability to raise federal revenue forces difficult local tradeoffs. Exit options constrained by fiscal interdependence and political pressure to maintain service levels.
constraint_indexing:constraint_classification(sotu_1982_reagan_federal_spending_growth_reduction, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: FEDERAL BUDGETING APPARATUS (PITON) — The constraint operates through Congressional appropriations process, which becomes largely theatrical by the 1980s. Budget reconciliation bills, continuing resolutions, and emergency appropriations bypass growth-rate constraints in practice while maintaining the performative appearance of constraint. The actual mechanism (excluding 'uncontrollable' entitlements, defense exemptions, emergency spending) means the constraint's nominal force is substantially degraded by institutional inertia and structural workarounds.
constraint_indexing:constraint_classification(sotu_1982_reagan_federal_spending_growth_reduction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a civilizational perspective, spending growth constraints are natural laws of fiscal sustainability: aggregate spending cannot exceed revenue without deficit accumulation, which produces mathematical limits. The constraint appears as an immutable feature of budgetary arithmetic. However, beneficiary presence reveals false summit: the choice of HOW to reduce spending growth (which programs to constrain, which tax bases to reduce) is purely contingent. The 'natural law' naturalizes political choices about distributive consequences.
constraint_indexing:constraint_classification(sotu_1982_reagan_federal_spending_growth_reduction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1982_reagan_federal_spending_growth_reduction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1982_reagan_federal_spending_growth_reduction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1982_reagan_federal_spending_growth_reduction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1982_reagan_federal_spending_growth_reduction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1982_reagan_federal_spending_growth_reduction, TR),
    TR >= 0.70.

:- end_tests(sotu_1982_reagan_federal_spending_growth_reduction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint targets reduction in fiscal commitments to vulnerable populations and public sector employment. Unlike absolute cuts, growth-rate reduction obscures the real costs — benefits erode slowly through inflation rather than appearing as explicit program terminations. Initial extractiveness (0.38) reflects the short-term ambiguity about actual implementation; by year 10, extractiveness reaches 0.60 as the real effects of constrained growth become visible. The trajectory shows extraction accumulation as beneficiaries adapt to the narrative frame and victims' adaptive capacity is exhausted. Suppression (0.65): High. Federal program recipients face severe barriers to exit: they depend on government assistance for survival, cannot relocate to avoid effects, and have no alternative funding sources. Public sector workers during the 1982-1983 recession face unemployment rates exceeding 10%, eliminating private-sector mobility. Federal spending constraints are national in scope, eliminating geographic arbitrage. Theater ratio (0.58): Moderate-high and rising. The constraint is substantially performative: definitional boundaries (what counts as 'federal spending'), entitlement exclusions, inflation assumptions, and Congressional workarounds allow nominal compliance while real constraints vary across programs. Theater rises from 0.45 to 0.62 as Gramm-Rudman-Hollings (1985) introduces sequestration procedures that operate through continuing resolutions and emergency appropriations — increasingly theatrical mechanisms that maintain the appearance of constraint while actual spending trajectories are obscured.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence from a single set of structural metrics. The affluent taxpayer/supply-side coalition sees Rope: spending reduction solves the coordination problem of deficits and inflation. The federal program recipient sees Snare: growth-rate constraints translate into real benefit erosion with no escape. The public sector worker sees Snare: hiring freezes and wage constraints amid recession eliminate both exit options. The state and local government sees Tangled Rope: federal cost-shifting mixed with participation in deficit narrative. The federal budgeting apparatus sees Piton: the constraint operates through increasingly theatrical procedures (continuing resolutions, sequestration) that maintain appearance without functional enforcement. The analytical observer at civilizational scale risks seeing Mountain: fiscal sustainability appears as mathematical inevitability rather than political choice about distribution. The perspectival gap reflects that the SAME spending growth reduction is experienced as beneficial coordination by beneficiaries and devastating extraction by victims — the constraint is not naturally law-like but politically contingent in whose interests it serves.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values vary sharply across the two primary groups. Federal program recipients and public sector workers (trapped/powerless) experience d ≈ 0.95: they bear the full extraction cost with no exit capacity. The inflation-hit middle class (constrained/moderate) experiences d ≈ 0.60: they benefit from deficit-reduction narrative but lose from reduced public investment and job loss — symmetric harm and benefit. Affluent taxpayers (arbitrage/institutional) experience d ≈ 0.10: they are net beneficiaries through tax-cut complement and inflation control without bearing constraint costs. This creates strong perspectival divergence: the 50% reduction in spending growth rate appears as coordination mechanism to the beneficiary (rope/arbitrage perspective) and pure extraction to the victim (snare/trapped perspective). The organized institutional actors (federal budgeting apparatus, state/local governments) experience d ≈ 0.55-0.70: they participate in constraint through procedures but have some agency in implementation and workaround design.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy through explicit examination of beneficiary and victim distributions. The tension arises because: (A) The constraint frames itself as coordination (solving the shared fiscal problem of deficits), suggesting Rope classification. (B) The costs fall asymmetrically on vulnerable populations who have no exit options, suggesting Snare classification. Mandatrophy resolution: (1) Declare beneficiaries explicitly (affluent taxpayers, tax-cut beneficiaries, supply-side coalition) and victims explicitly (program recipients, public sector workers). (2) Compute directionality from structural position. (3) Accept that the same constraint is Rope from the beneficiary perspective and Snare from the victim perspective. (4) The Tangled Rope claimed_type represents the structural reality: genuine coordination benefit (deficit/inflation control) exists alongside asymmetric extraction (cost distribution). Theater ratio rising from 0.45 to 0.62 indicates procedural degradation — the constraint's nominal enforcement mechanism becomes increasingly theatrical, suggesting potential Piton transition over longer timescales if enforcement mechanisms (Gramm-Rudman) fail and continue as pure ritual.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    discretionary_vs_entitlement_boundary,
    'What constitutes ''federal spending'' for growth-rate limitation? Are entitlements (Social Security, Medicare, Medicaid) included, excluded, or partially constrained?',
    'Historical examination of budget authority documents; analysis of Congressional intent regarding entitlement treatment; comparison of nominal vs. effective spending constraints across fiscal years 1982-1990',
    'If entitlements excluded: constraint is political choice to reduce discretionary services; extraction falls entirely on federal program recipients and public workers. If entitlements included: political infeasibility creates de facto constraint primarily on defense and discretionary spending; constraints distribute differently. If partially included: institutional boundaries become the extraction mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(discretionary_vs_entitlement_boundary, empirical, 'Whether entitlement programs are included in spending growth constraints').

omega_variable(
    inflation_adjustment_definitional,
    'Is the ''rate of increase'' measured in nominal or real terms? Does it adjust for inflation, population growth, or structural cost increases in healthcare and defense?',
    'Comparison of budget documents specifying baseline year, inflation assumptions, and demographic adjustments; analysis of actual vs. planned spending trajectories under different baseline definitions',
    'If nominal: constraint appears severe but masks growing real spending. If inflation-adjusted: constraint is more restrictive and visible to voters. If adjusted for demographic growth: constraint is more lenient. Definition determines experienced extraction magnitude.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inflation_adjustment_definitional, empirical, 'Inflation and demographic adjustment assumptions for spending growth rate').

omega_variable(
    enforcement_mechanism_credibility,
    'What enforcement mechanism ensures growth-rate constraint? Are there sequestration triggers, appropriations caps, or legislative procedures that make constraint binding?',
    'Analysis of Gramm-Rudman-Hollings Act (1985) and successor mechanisms; examination of actual appropriations vs. statutory limits; tracking of waiver frequency and scope',
    'If enforcement is weak: constraint is primarily narrative/theater (piton classification strengthens). If enforcement is strong: constraint forces real tradeoffs and increases experienced extraction. Enforcement credibility determines whether constraint transitions from rope (coordination mechanism) to snare (extraction trap).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_mechanism_credibility, empirical, 'Actual enforcement credibility of spending growth constraints').

omega_variable(
    distributional_incidence_visibility,
    'Are the costs of spending growth reduction made visible to voters and recipients, or obscured through administrative mechanisms and delayed effects?',
    'Media analysis of budget discourse; tracking of recipient complaints and advocacy mobilization; comparison of perceived vs. actual service reductions by program type and geography',
    'If costs are visible: constraint becomes politically unsustainable and triggers coalition formation among victims (potential escape from snare). If costs are obscured: constraint is sustainable and extraction mechanisms are more effective. Visibility affects whether constraint maintains suppression over biographical timescales.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributional_incidence_visibility, empirical, 'Visibility and politicization of spending constraint costs').

omega_variable(
    beneficiary_narrative_alignment,
    'Do affluent taxpayers genuinely perceive spending reduction as beneficial to them, or is this a rhetorical frame disconnected from their actual interests?',
    'Analysis of campaign messaging, polling data, and voting patterns; comparison of stated preferences for spending reduction with revealed preferences for specific cuts (defense, Social Security, Medicare, etc.); tracking of tax cut distribution vs. spending reduction distribution',
    'If alignment is genuine: beneficiary group maintains support and suppression remains stable. If alignment is rhetorical: constraint requires constant narrative maintenance and is vulnerable to coalition building by victims. Narrative credibility affects whether constraint persists beyond the immediate political context.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_narrative_alignment, preference, 'Whether beneficiary support for spending reduction is genuine or rhetorical').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1982_reagan_federal_spending_growth_reduction, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sotu82_tr_t0, sotu_1982_reagan_federal_spending_growth_reduction, theater_ratio, 0, 0.45).
narrative_ontology:measurement(sotu82_tr_t2, sotu_1982_reagan_federal_spending_growth_reduction, theater_ratio, 2, 0.5).
narrative_ontology:measurement(sotu82_tr_t4, sotu_1982_reagan_federal_spending_growth_reduction, theater_ratio, 4, 0.55).
narrative_ontology:measurement(sotu82_tr_t6, sotu_1982_reagan_federal_spending_growth_reduction, theater_ratio, 6, 0.58).
narrative_ontology:measurement(sotu82_tr_t8, sotu_1982_reagan_federal_spending_growth_reduction, theater_ratio, 8, 0.6).
narrative_ontology:measurement(sotu82_tr_t10, sotu_1982_reagan_federal_spending_growth_reduction, theater_ratio, 10, 0.62).

% Extraction over time
narrative_ontology:measurement(sotu82_be_t0, sotu_1982_reagan_federal_spending_growth_reduction, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(sotu82_be_t2, sotu_1982_reagan_federal_spending_growth_reduction, base_extractiveness, 2, 0.46).
narrative_ontology:measurement(sotu82_be_t4, sotu_1982_reagan_federal_spending_growth_reduction, base_extractiveness, 4, 0.52).
narrative_ontology:measurement(sotu82_be_t6, sotu_1982_reagan_federal_spending_growth_reduction, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(sotu82_be_t8, sotu_1982_reagan_federal_spending_growth_reduction, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(sotu82_be_t10, sotu_1982_reagan_federal_spending_growth_reduction, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1982_reagan_federal_spending_growth_reduction, resource_allocation).
narrative_ontology:affects_constraint(sotu_1982_reagan_federal_spending_growth_reduction, reaganomics_tax_reduction_complement).
narrative_ontology:affects_constraint(sotu_1982_reagan_federal_spending_growth_reduction, federal_entitlement_indexing_constraints).
narrative_ontology:affects_constraint(sotu_1982_reagan_federal_spending_growth_reduction, defense_spending_exemption_mechanism).

% DUAL FORMULATION NOTE:
% The spending growth reduction constraint is downstream of supply-side fiscal ideology and upstream of specific program-level constraints (Medicaid growth, federal employee hiring, infrastructure investment). The growth-rate reduction mechanism has its own extractiveness (0.52) reflecting the combination of coordination benefit (inflation control narrative) and asymmetric distribution. Decomposition into program-specific constraints would show variation: defense exemptions have lower extractiveness, means-tested programs show higher extractiveness. The family includes complementary constraints on tax reduction (which creates fiscal space justifying spending cuts) and entitlement indexing (which distributes constraint burden). The unified story treats the spending growth mechanism as the primary structural constraint; downstream program stories inherit the architectural choices encoded here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
