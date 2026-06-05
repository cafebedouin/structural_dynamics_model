% ============================================================================
% CONSTRAINT STORY: sotu_1964_johnson_deficit_reduction_budget
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1964_johnson_deficit_reduction_budget, []).

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
 *   constraint_id: sotu_1964_johnson_deficit_reduction_budget
 *   human_readable: Johnson Administration Unified Federal Budget Constraint (1964)
 *   domain: economics/fiscal_policy
 *
 * SUMMARY:
 *   The Johnson Administration's unified federal budget constraint of 1964
 *   represents a structural coordination mechanism linking fiscal discipline
 *   to targeted social spending growth. The constraint reduces the federal
 *   deficit from $10 billion to $4.9 billion (51% reduction) while cutting
 *   total federal expenditures by $500+ million and reducing Department of
 *   Defense civilian personnel to 1950 levels. The austerity generates fiscal
 *   space for reallocation to education, health, unemployment retraining, and
 *   support for the handicapped. This is a Rope mechanism: the constraint
 *   solves a collective action problem (how to achieve credible fiscal
 *   discipline while expanding social investment) through shared commitment
 *   to a budget ceiling that applies uniformly across all agencies,
 *   benefiting both taxpayers (through deficit reduction and fiscal
 *   credibility) and beneficiary sectors (through guaranteed reallocation).
 *   No agent perceives significant extraction except the defense sector,
 *   which experiences proportional discipline in the context of broader
 *   constraint architecture.
 *
 * KEY AGENTS:
 *   - Executive Administration (Johnson): Primary beneficiary (powerful/arbitrage) — captures credibility gains from deficit reduction; able to pursue expansionary social agenda within disciplined fiscal envelope
 *   - Taxpayers and Fiscal Credibility Advocates: Secondary beneficiary (moderate/constrained) — benefit from deficit reduction and improved fiscal authority; constrained by taxation but aligned with discipline logic
 *   - Education, Health, Retraining Sectors: Tertiary beneficiary (institutional/mobile) — receive reallocation of savings; can pursue independent funding but benefit from federal prioritization and guaranteed growth
 *   - Department of Defense and Defense Contractors: Constrained actors (powerful/constrained) — face proportional spending discipline and personnel reduction; experience coordination (uniform budget discipline applies equally) alongside constraint (absolute reduction)
 *   - Organized Labor and Displaced Defense Workers: Retraining beneficiaries (moderate/constrained) — receive retraining and unemployment support for transition to growth sectors; constrained by labor market but supported through transition phase
 *   - Congress: Institutional enforcer (institutional/mobile) — implements budget discipline through appropriations; maintains arbitrage options on future fiscal priorities
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1964_johnson_deficit_reduction_budget, 0.32).
domain_priors:suppression_score(sotu_1964_johnson_deficit_reduction_budget, 0.28).
domain_priors:theater_ratio(sotu_1964_johnson_deficit_reduction_budget, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1964_johnson_deficit_reduction_budget, extractiveness, 0.32).
narrative_ontology:constraint_metric(sotu_1964_johnson_deficit_reduction_budget, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(sotu_1964_johnson_deficit_reduction_budget, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1964_johnson_deficit_reduction_budget, rope).
narrative_ontology:human_readable(sotu_1964_johnson_deficit_reduction_budget, "Johnson Administration Unified Federal Budget Constraint (1964)").
narrative_ontology:topic_domain(sotu_1964_johnson_deficit_reduction_budget, "economics/fiscal_policy").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1964_johnson_deficit_reduction_budget, taxpayers).
narrative_ontology:constraint_beneficiary(sotu_1964_johnson_deficit_reduction_budget, fiscal_credibility_advocates).
narrative_ontology:constraint_beneficiary(sotu_1964_johnson_deficit_reduction_budget, education_sector).
narrative_ontology:constraint_beneficiary(sotu_1964_johnson_deficit_reduction_budget, health_sector).
narrative_ontology:constraint_beneficiary(sotu_1964_johnson_deficit_reduction_budget, retraining_programs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXECUTIVE BUDGET DISCIPLINE (ROPE) — The Administration benefits from the constraint's credibility signal. Deficit reduction from $10B to $4.9B (51% decrease) establishes fiscal authority and enables the reallocation mechanism. This perspective sees the constraint as pure coordination: the discipline *enables* the desired spending growth in education, health, and retraining by establishing the credibility prerequisite. Exit option is arbitrage — the Administration can claim fiscal responsibility while maintaining expansionary social spending.
constraint_indexing:constraint_classification(sotu_1964_johnson_deficit_reduction_budget, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 2: BENEFICIARY AGENCIES (ROPE) — Education, health, and retraining sectors experience the constraint as enabling, not extractive. The discipline on total spending creates the fiscal space for their expansion. These agencies are beneficiaries with mobile exit options — they can pursue independent funding but benefit from federal prioritization. They experience the constraint as coordination that makes their expansion possible rather than barriers that block it.
constraint_indexing:constraint_classification(sotu_1964_johnson_deficit_reduction_budget, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: TAXPAYERS AND FISCAL CREDIBILITY (ROPE) — Taxpayers benefit from deficit reduction and reduced future tax burden from interest payments. They experience the constraint as coordination that protects fiscal sustainability. Exit options are constrained — they cannot opt out of federal taxation but benefit from the fiscal discipline mechanism. The constraint aligns their interests with program beneficiaries through the credibility linkage.
constraint_indexing:constraint_classification(sotu_1964_johnson_deficit_reduction_budget, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: DEFENSE SECTOR (TANGLED ROPE) — DoD personnel reduced to 1950 levels (significant cutback) while maintaining strategic capabilities. Defense contractors face procurement discipline and elimination of obsolete installations. This sector experiences genuine coordination (unified budget discipline applies to all agencies equally, creating transparency) alongside extraction (their headcount and spending are directly constrained). They have constrained exit options — they cannot avoid the budget discipline — but also participate in the new allocation logic (retraining programs may absorb displaced personnel).
constraint_indexing:constraint_classification(sotu_1964_johnson_deficit_reduction_budget, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ORGANIZED LABOR AND WORKFORCE (SCAFFOLD) — The retraining programs and unemployment support represent a temporary coordination mechanism with implicit sunset logic. As defense and federal employment shift toward educational and health sectors, retraining is a transitional support structure. Organized labor sees the constraint as enabling a generational workforce shift with support mechanisms for the transition period. Exit options are constrained by the labor market but improving as educational and health sectors expand.
constraint_indexing:constraint_classification(sotu_1964_johnson_deficit_reduction_budget, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (ROPE) — From a long-term fiscal and institutional perspective, the constraint demonstrates pure coordination: linking austerity discipline to targeted expansion creates a sustainable allocation mechanism. The 51% deficit reduction is severe but selective (defense cutbacks offset by social spending). The analytical perspective sees no extraction — all major constituencies benefit from credible fiscal constraint. Theater is low (0.35) because the mechanism is transparent: deficit reduction is clearly stated, agency cutbacks are named, reallocation is explicit.
constraint_indexing:constraint_classification(sotu_1964_johnson_deficit_reduction_budget, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1964_johnson_deficit_reduction_budget_tests).
:- end_tests(sotu_1964_johnson_deficit_reduction_budget_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32): Low-moderate. The constraint operates as a coordination mechanism rather than extraction. The deficit reduction (51%) is severe in nominal terms but distributed across all agencies proportionally. Defense receives the deepest cut (to 1950 personnel levels), but this is transparent and justified by reallocation logic. No hidden extraction — the budget mechanism makes all reductions and reallocations explicit. The low extractiveness reflects that all major constituencies view the constraint as beneficial: deficit reduction benefits fiscal credibility; defense discipline is proportional; reallocation benefits education/health/retraining. Suppression (0.28): Low. The constraint operates through transparent budgeting and legislative appropriation, not coercion. Agencies can propose alternatives through the normal budget process; Congress retains approval authority. The unified budget itself makes the constraint visible and subject to political negotiation. Theater ratio (0.35): Low. The constraint's mechanism is simple and transparent: deficit reduction target ($5.1B) is stated; spending reductions are named (defense personnel, obsolete installations); reallocation targets are specified (education, health, retraining, handicapped support). Limited theater because the constraint's logic is accessible and measurable. Theater increases slightly over the 2-year interval (0.20 → 0.35) as the constraint becomes institutionalized and political rhetoric emphasizes credibility signaling more than underlying budget mechanics.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows minimal perspectival gap across most constituencies because all major actors benefit from or perceive alignment with the deficit reduction logic. The Administration, taxpayers, and beneficiary sectors all classify the constraint as Rope (pure coordination). The only significant gap appears in the Defense sector perspective (Tangled Rope vs Rope) due to their constrained exit options and absolute spending reduction, even within proportional discipline. The analytical perspective reinforces the Rope classification across civilizational time, suggesting the constraint's coordination function is robust and durable. This low perspectival gap is diagnostic: if a constraint appears purely beneficial to all observers, verify that extraction is not hidden in asymmetric implementation or that beneficiary declarations are accurate. In this case, the low gap appears justified by the constraint's transparent architecture and proportional application of discipline.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies by agent's structural position. The Administration and executive branch actors are net beneficiaries: deficit reduction establishes fiscal authority (d ≈ 0.15, low extraction). Taxpayers benefit from reduced future interest burden but bear current discipline: constrained exit creates moderate extraction perception (d ≈ 0.45, symmetric). Defense sector faces 51% deficit reduction but within uniform budget discipline and with retraining support: proportional constraint rather than targeted extraction (d ≈ 0.50, symmetric). Beneficiary sectors (education, health, retraining) experience the constraint as enabling: reallocation flows toward them and discipline generates fiscal credibility supporting their expansion (d ≈ 0.25, low extraction). The Rope classification follows from low baseline extractiveness and absence of high suppression. The Defense sector's Tangled Rope classification reflects that they experience both coordination (uniform budget discipline applies equally) and constraint (absolute reduction from previous levels), with constrained exit options. All perspectives derive d from the same structural relationships (beneficiary/victim + power + exit), producing congruent classification outcomes except where exit options differentiate the perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy resolution: The constraint is NOT mandatrophic because extractiveness (0.32) is below the 0.46 threshold. However, the omega variables document three sources of potential mandatrophy in interpretation: (1) If fiscal multiplier effects are strong, the constraint's actual deficit impact may be lower than nominal, suggesting the credibility signaling mechanism is more theatrical than real. (2) If reallocation commitments depend entirely on sustained deficit discipline, the constraint is durable coordination; if they decouple, the constraint becomes temporary theater with shifting beneficiaries. (3) If defense base elimination is concentrated rather than distributed, affected regions/contractors experience extraction rather than proportional discipline, creating a hidden Snare mechanism within the broader Rope. The analysis recommends treating these omegas as early-warning indicators: if future measurements show rising theater ratio or evidence of multiplier effects without sustained reallocation, reclassify toward Scaffold (temporary coordination) or Piton (degraded institutional performance).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fiscal_multiplier_assumption,
    'Does deficit reduction via agency spending cuts and defense cutbacks activate the countercyclical fiscal multiplier, offsetting the deficit savings through reduced economic activity?',
    'Post-implementation economic growth tracking; comparison of actual GDP growth (1964-1965) to counterfactual projections; analysis of sectoral employment shifts',
    'If multiplier effect is strong: the constraint''s actual deficit impact is lower than the nominal $5.1B reduction, and the credibility gain may be partly illusory. If weak: the constraint''s disciplinary logic is robust across business cycles.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_multiplier_assumption, empirical, 'Whether deficit reduction via spending cuts triggers countercyclical fiscal multiplier').

omega_variable(
    reallocation_permanence,
    'Are the education, health, and retraining reallocations structurally permanent, or do they depend on sustained political commitment to the deficit discipline logic?',
    'Longitudinal tracking of education/health/retraining appropriations across subsequent administrations; correlation between deficit discipline advocates and program funding levels',
    'If reallocations are dependent on discipline: the constraint is a durable coordination mechanism only if the political coalition supporting deficit reduction persists. If reallocations decouple from discipline: the constraint becomes a temporary theater with shifting beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reallocation_permanence, empirical, 'Whether reallocation commitments persist independent of deficit discipline').

omega_variable(
    defense_base_elimination_extraction,
    'Do the eliminated defense installations and reduced DoD civilian personnel represent genuine efficiency improvements, or are they concentrated cuts that extract disproportionately from specific regions and contractor-dependent communities?',
    'Geographic analysis of base closures and civilian job losses; comparison of affected regions'' employment recovery rates; contractor dependence analysis for eliminated installations',
    'If distributed evenly: defense sector experiences symmetric discipline (rope classification holds). If concentrated: some regions/contractors bear asymmetric extraction (snare classification for affected constituencies).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(defense_base_elimination_extraction, empirical, 'Whether defense base eliminations are distributed or concentrated extraction').

omega_variable(
    credibility_signaling_mechanism,
    'Does the deficit reduction credibility signal actually reduce market borrowing costs and improve fiscal authority, or is it largely domestic political theater with minimal international economic effect?',
    'Analysis of US Treasury borrowing costs 1964-1965; comparison to international borrowing cost baselines; correlation between deficit reduction announcements and actual credit market behavior',
    'If credibility signal is effective: the constraint''s coordination function is durable because it creates real economic benefits (lower borrowing costs) that sustain political support. If theater: the constraint''s justification depends on sustained belief in the signal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credibility_signaling_mechanism, empirical, 'Whether deficit reduction produces real credibility signaling to credit markets').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1964_johnson_deficit_reduction_budget, 0, 2).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sotu64_tr_t0, sotu_1964_johnson_deficit_reduction_budget, theater_ratio, 0, 0.2).
narrative_ontology:measurement(sotu64_tr_t1, sotu_1964_johnson_deficit_reduction_budget, theater_ratio, 1, 0.28).
narrative_ontology:measurement(sotu64_tr_t2, sotu_1964_johnson_deficit_reduction_budget, theater_ratio, 2, 0.35).

% Extraction over time
narrative_ontology:measurement(sotu64_be_t0, sotu_1964_johnson_deficit_reduction_budget, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(sotu64_be_t1, sotu_1964_johnson_deficit_reduction_budget, base_extractiveness, 1, 0.28).
narrative_ontology:measurement(sotu64_be_t2, sotu_1964_johnson_deficit_reduction_budget, base_extractiveness, 2, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1964_johnson_deficit_reduction_budget, resource_allocation).
narrative_ontology:affects_constraint(sotu_1964_johnson_deficit_reduction_budget, defense_industrial_complex_consolidation).
narrative_ontology:affects_constraint(sotu_1964_johnson_deficit_reduction_budget, federal_education_expansion_1960s).
narrative_ontology:affects_constraint(sotu_1964_johnson_deficit_reduction_budget, great_society_program_funding).

% DUAL FORMULATION NOTE:
% The unified budget constraint is upstream to specific program expansions (education, health, retraining) and defense consolidation. The constraint's ε=0.32 reflects its role as enabling coordination; the downstream constraints have their own ε values reflecting the distributional asymmetries within each sector's implementation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
