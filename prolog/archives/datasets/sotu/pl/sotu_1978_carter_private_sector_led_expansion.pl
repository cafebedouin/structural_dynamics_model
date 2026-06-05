% ============================================================================
% CONSTRAINT STORY: sotu_1978_carter_private_sector_led_expansion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1978_carter_private_sector_led_expansion, []).

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
 *   constraint_id: sotu_1978_carter_private_sector_led_expansion
 *   human_readable: Private Sector-Led Economic Expansion (Carter 1978)
 *   domain: governance/economic_policy
 *
 * SUMMARY:
 *   The 1978 State of the Union speech establishes a structural constraint by
 *   positioning private enterprise as the primary economic driver,
 *   deliberately rejecting direct government management and production roles.
 *   This constraint exhibits the key features of Tangled Rope: genuine
 *   coordination function (market competition can mobilize capital and
 *   coordinate investment decisions) coexists with asymmetric extraction
 *   (benefits concentrate in capital and management; costs diffuse across
 *   workers and defunded public institutions). The theater ratio (0.64)
 *   reflects the gap between the constraint's rhetorical justification
 *   ('rising tide lifts all boats,' market efficiency) and empirical outcomes
 *   showing wage stagnation and income concentration. Extractiveness (0.52)
 *   is moderate rather than severe because some workers do benefit from
 *   expansion, but the benefit is distributed asymmetrically. Suppression
 *   (0.58) is high because alternatives to private expansion (public banking,
 *   government employment, sector-wide wage standards) are systematically
 *   defunded and delegitimized, forcing workers to accept private-sector wage
 *   and employment terms or exit the labor market entirely.
 *
 * KEY AGENTS:
 *   - Capital Investors and Large Corporations: Primary beneficiary (institutional/arbitrage) — capture expansion benefits, access capital, can relocate to maximize returns
 *   - Government Workers and Social Program Beneficiaries: Primary victim (powerless/trapped) — face systematic defunding; cannot exit public sector or reorganize effectively
 *   - Small Business Owners and Private Sector Workers: Secondary victim (moderate/constrained) — benefit from market access but constrained by wage pressure and large-firm dominance
 *   - Federal Government: Institutional actor (institutional/constrained) — constrained by ideological commitment to private leadership while retaining enforcement role
 *   - Democratic Demand for Shared Prosperity: Institutional performance (institutional/piton) — rhetorical function (meritocracy, market efficiency) persists despite empirical degradation
 *   - Labor Movement and Progressive Coalitions: Organized opposition (organized/constrained) — recognize constraint as temporary and build exit pathways through organization
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent policy choice as inherent capitalist law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1978_carter_private_sector_led_expansion, 0.52).
domain_priors:suppression_score(sotu_1978_carter_private_sector_led_expansion, 0.58).
domain_priors:theater_ratio(sotu_1978_carter_private_sector_led_expansion, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1978_carter_private_sector_led_expansion, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_1978_carter_private_sector_led_expansion, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(sotu_1978_carter_private_sector_led_expansion, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1978_carter_private_sector_led_expansion, tangled_rope).
narrative_ontology:human_readable(sotu_1978_carter_private_sector_led_expansion, "Private Sector-Led Economic Expansion (Carter 1978)").
narrative_ontology:topic_domain(sotu_1978_carter_private_sector_led_expansion, "governance/economic_policy").

domain_priors:requires_active_enforcement(sotu_1978_carter_private_sector_led_expansion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1978_carter_private_sector_led_expansion, capital_investors).
narrative_ontology:constraint_beneficiary(sotu_1978_carter_private_sector_led_expansion, large_corporations).
narrative_ontology:constraint_beneficiary(sotu_1978_carter_private_sector_led_expansion, business_management_class).
narrative_ontology:constraint_victim(sotu_1978_carter_private_sector_led_expansion, government_workers).
narrative_ontology:constraint_victim(sotu_1978_carter_private_sector_led_expansion, social_program_beneficiaries).
narrative_ontology:constraint_victim(sotu_1978_carter_private_sector_led_expansion, small_business_labor_force).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GOVERNMENT WORKERS & PROGRAM BENEFICIARIES (SNARE) — Cannot exit the constraint; face systematic defunding of agencies and programs. Their employment and services are explicitly repositioned as secondary to private sector growth. High suppression through budget elimination, hiring freezes, and institutional degradation. No coordination benefit — pure extraction of resources from public sector to private capital accumulation.
constraint_indexing:constraint_classification(sotu_1978_carter_private_sector_led_expansion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SMALL BUSINESS OWNERS & WORKERS (TANGLED ROPE) — Benefit from expanded market opportunity and capital availability, but constrained by wage pressure, reduced worker protections, and competition with large corporations favored by policy. Real coordination function (market competition drives innovation) exists alongside asymmetric extraction favoring large capital. Exit cost is high — leaving the market means unemployment in regions dependent on private expansion.
constraint_indexing:constraint_classification(sotu_1978_carter_private_sector_led_expansion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LARGE CORPORATIONS & CAPITAL INVESTORS (ROPE) — Primary beneficiaries with arbitrage options (can move capital, relocate operations, access new markets). Constraint provides coordination function: clear signal that private expansion is legitimate policy mechanism, reducing regulatory uncertainty and enabling capital mobilization. Net positive experience — extraction flows toward this agent.
constraint_indexing:constraint_classification(sotu_1978_carter_private_sector_led_expansion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FEDERAL GOVERNMENT (TANGLED ROPE) — Constrained by ideological commitment to private-sector leadership while retaining enforcement role (tariffs, tax policy, regulatory frameworks supporting business). Genuine coordination function exists (government enables market conditions), but extraction runs upward toward capital — government becomes mechanism for private accumulation rather than independent agent. Constrained exit: cannot abandon market coordination without economic collapse.
constraint_indexing:constraint_classification(sotu_1978_carter_private_sector_led_expansion, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: DEMOCRATIC DEMAND FOR SHARED PROSPERITY (PITON) — The constraint maintains rhetorical coordination function (appeals to meritocracy, individual enterprise, market efficiency) that has degraded empirically. Theater ratio high: the promise of 'rising tide lifts all boats' persists despite data showing wage stagnation, income concentration, and reduced intergenerational mobility. The constraint persists through institutional inertia and political ideology despite low functional verification of its prosperity-distribution claims.
constraint_indexing:constraint_classification(sotu_1978_carter_private_sector_led_expansion, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: LABOR & PUBLIC INTEREST COALITIONS (SCAFFOLD) — Organized opposition with exit-path awareness. The constraint is recognized as temporary — union organization, progressive taxation, and public investment can counter private-sector dominance. Low immediate extraction because the organizing coalition has agency and sees alternatives (public banking, cooperative enterprise, sector-wide wage standards). Sunset logic applies: if labor organization rebuilds and public investment restores legitimacy, the constraint loses force.
constraint_indexing:constraint_classification(sotu_1978_carter_private_sector_led_expansion, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURALIZATION VIEW (MOUNTAIN) — From civilizational/global perspective, risks classifying the constraint as inherent to capitalist economic organization: private enterprise naturally drives expansion because capital accumulation is the mechanism of growth. However, structural data reveals this as false summit — the constraint's existence depends on political choice (Carter's policy), beneficiary concentration (large capital), and suppression mechanisms (defunding alternatives). The mountain classification naturalizes what is a contingent institutional arrangement.
constraint_indexing:constraint_classification(sotu_1978_carter_private_sector_led_expansion, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1978_carter_private_sector_led_expansion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1978_carter_private_sector_led_expansion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1978_carter_private_sector_led_expansion, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1978_carter_private_sector_led_expansion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1978_carter_private_sector_led_expansion, TR),
    TR >= 0.70.

:- end_tests(sotu_1978_carter_private_sector_led_expansion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint generates asymmetric returns: capital investors gain access to labor at suppressed wages and reduced regulatory overhead; workers lose government employment, social services, and wage-setting power through unionization. However, extraction is not maximal (Snare-level) because some workers benefit from private expansion (job creation in growing sectors), and the constraint requires genuine coordination functions (market mechanisms do mobilize capital and direct investment). The 0.52 value reflects that the constraint is neither pure coordination (Rope, ~0.05-0.35) nor pure extraction (Snare, ~0.46-0.72), but a hybrid with asymmetric distribution. Suppression (0.58): High. Systematic defunding of alternatives creates barriers to exit — workers cannot access public employment, social programs are cut, and collective bargaining is weakened. However, suppression is not absolute (trapped-level, ~0.70+) because some organized labor maintains power and political opposition persists. Theater ratio (0.64): Moderate-high. The constraint's rhetorical justification (private enterprise drives growth, market efficiency, individual opportunity) is empirically partial — wage data shows stagnation, income distribution shows concentration, and intergenerational mobility shows decline. The gap between rhetoric and outcome grows over the interval as extractive effects accumulate.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates marked perspectival divergence across power levels. Capital investors (institutional/arbitrage) see Rope — genuine coordination mechanism with clear policy signal for investment. Small business and workers (moderate/constrained) see Tangled Rope — real coordination benefits but constrained by wage suppression and market concentration. Government workers (powerless/trapped) see Snare — pure extraction with no exit and no coordination benefit. Labor coalitions (organized/constrained) see Scaffold — constraint is temporary, alternatives exist, sunset is achievable through organizing. Federal government (institutional/constrained) sees itself in Tangled Rope — coordinates markets but loses autonomy. Democratic demand (institutional/piton) appears to see the original coordination mechanism, but theater ratio reveals that the rhetorical mechanism (meritocracy lifts all boats) has decoupled from outcomes. The analytical observer risks seeing Mountain — naturalizing the constraint as inherent to capitalist growth — but structural data reveals beneficiary concentration and suppression mechanisms that are contingent on policy choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values flow from agent structural position and beneficiary/victim status. Capital investors (beneficiaries, arbitrage options) derive low d (~0.10-0.20), experiencing negative effective extraction (they gain from the constraint). Small business (mixed — some benefit, some constrained) derives moderate d (~0.45-0.55), experiencing moderate extraction despite coordination benefits. Government workers (victims, trapped) derive high d (~0.85-0.95), experiencing maximum extraction. The Federal Government as institutional beneficiary derives low d (~0.15), but the analytical perspective at institutional power derives moderate-high d (~0.55-0.65) when constrained by ideological commitment, reflecting that the government has lost agency in the constraint structure. The piton perspective derives d from behavioral inertia (the performance persists despite low function), not from structural extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that Tangled Rope is the correct classification from the analytical perspective: genuine coordination function (capital mobilization, market competition) coexists with asymmetric extraction (benefits to capital, costs to workers and defunded institutions) and active enforcement (tax policy, regulatory frameworks, labor law changes supporting private expansion). The false summit risk is acute: the constraint risks being naturalized as an immutable feature of capitalism itself, when structural data shows it is a political choice enforced through specific mechanisms (defunding, wage suppression, regulatory arbitrage). The piton perspective reveals that the rhetorical coordination function has degraded over time — the 'rising tide' promise was never empirically true for most workers, and theater ratio shows the constraint is maintained through ideology rather than genuine coordination outcome. The scaffold perspective indicates the constraint is politically reversible — labor organizing and public investment can rebuild alternatives, creating exit pathways within the biographical time horizon.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    trickle_down_verification,
    'Do private sector expansion mechanisms actually distribute prosperity to workers and communities, or primarily concentrate it in capital and management?',
    'Longitudinal wage data, income distribution analysis, and regional economic outcomes correlation between private investment rates and worker income growth; comparison of 1978-1990 wage trends across income deciles',
    'If trickle-down verified: constraint is genuine coordination mechanism (Rope from powerless perspective). If falsified: constraint is pure extraction (Snare confirmed) and false summit dissolves.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(trickle_down_verification, empirical, 'Whether private expansion distributes prosperity to workers').

omega_variable(
    government_capacity_degradation,
    'Does systematic defunding of government agencies and programs create irreversible institutional damage, or can capacity be restored once political consensus shifts?',
    'Measurement of institutional capacity metrics (hiring, training, capital infrastructure) in defunded agencies; analysis of restoration timelines when funding resumes; comparison of 1978 vs 1993 institutional capacity in same agencies',
    'If degradation is reversible: constraint is temporary (Scaffold logic holds). If degradation is path-dependent and irreversible: constraint generates permanent lock-in effects (Piton mechanism becomes dominant, theater persists).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(government_capacity_degradation, empirical, 'Whether defunded government capacity can be restored').

omega_variable(
    small_business_displacement,
    'Does private-sector-led expansion primarily benefit established large corporations with capital mobility, or does it enable small business competition and growth?',
    'Analysis of business formation rates, survival rates by firm size, and capital access gaps (interest rates, collateral requirements) before and after constraint implementation; tracking of small-firm market share concentration',
    'If small business competitive: tangled rope classification for small business is accurate (real coordination benefit exists). If large-firm dominance: small business perception shifts to Snare, and constraint is pure extraction at scale.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(small_business_displacement, empirical, 'Whether expansion benefits small businesses or concentrates capital').

omega_variable(
    regulatory_arbitrage_extraction,
    'Does the private-sector-led framework enable regulatory arbitrage (firm relocation to lower-regulation jurisdictions) that extracts from workers and communities left behind?',
    'Tracking of firm relocations, wage outcomes in left-behind regions, tax revenue loss; comparison of deindustrialization rates in high-regulation vs low-regulation regions; analysis of environmental and labor compliance as driver of relocation decisions',
    'If arbitrage is significant: suppression mechanism is geographic (workers cannot follow capital), and Snare classification strengthens. If arbitrage is marginal: constraint functions more as Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_arbitrage_extraction, empirical, 'Magnitude of regulatory arbitrage-driven extraction').

omega_variable(
    democratic_mandate_duration,
    'How long does the political consensus supporting private-sector leadership persist, and what factors trigger reversal?',
    'Tracking of public opinion on government vs private sector roles; electoral outcomes favoring labor/progressive parties; policy reversals (job guarantee proposals, public banking, sectoral wage standards); timeline of institutional rebuilding movements',
    'If consensus fractures within 10-15 years: Scaffold sunset logic is realistic. If consensus persists 30+ years: constraint becomes structural feature, and theater ratio rises (Piton mechanism).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(democratic_mandate_duration, conceptual, 'Duration and reversibility of private-sector policy consensus').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1978_carter_private_sector_led_expansion, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(carter78_tr_t0, sotu_1978_carter_private_sector_led_expansion, theater_ratio, 0, 0.5).
narrative_ontology:measurement(carter78_tr_t3, sotu_1978_carter_private_sector_led_expansion, theater_ratio, 3, 0.58).
narrative_ontology:measurement(carter78_tr_t6, sotu_1978_carter_private_sector_led_expansion, theater_ratio, 6, 0.64).

% Extraction over time
narrative_ontology:measurement(carter78_be_t0, sotu_1978_carter_private_sector_led_expansion, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(carter78_be_t3, sotu_1978_carter_private_sector_led_expansion, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(carter78_be_t6, sotu_1978_carter_private_sector_led_expansion, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1978_carter_private_sector_led_expansion, resource_allocation).
narrative_ontology:affects_constraint(sotu_1978_carter_private_sector_led_expansion, regressive_tax_policy_accumulation).
narrative_ontology:affects_constraint(sotu_1978_carter_private_sector_led_expansion, labor_union_suppression_mechanisms).
narrative_ontology:affects_constraint(sotu_1978_carter_private_sector_led_expansion, public_sector_disinvestment_cascade).

% DUAL FORMULATION NOTE:
% This constraint is upstream of specific labor and tax policies, but represents a distinct structural commitment to private-sector primacy. Downstream constraints (regressive taxation, union suppression, public disinvestment) inherit their extractiveness from this constraint family. The network reflects causal dependency: the private-sector-led framework enables and justifies the downstream extraction mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1978_carter_private_sector_led_expansion, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
