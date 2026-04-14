% ============================================================================
% CONSTRAINT STORY: single_family_rental_maintenance_standards
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_single_family_rental_maintenance_standards, []).

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
 *   constraint_id: single_family_rental_maintenance_standards
 *   human_readable: Single-Family Rental Maintenance Standards Enforcement
 *   domain: housing/property_management/regulatory
 *
 * SUMMARY:
 *   Single-family rental maintenance standards enforce habitability
 *   requirements through municipal code enforcement, landlord liability, and
 *   tenant remedies. The constraint exhibits the core tension of housing
 *   regulation: legitimate coordination (maintaining safe living conditions
 *   across a distributed owner base) coexists with asymmetric extraction
 *   (compliance costs borne unequally by small landlords vs. institutional
 *   management, enforcement burden on tenants without resources). The
 *   constraint's classification varies sharply by observer position, making
 *   it a diagnostic exemplar for how distributional asymmetry creates
 *   perspectival divergence. The tenant experiences the constraint as a snare
 *   — trapped in substandard conditions with no exit mechanism. The small
 *   landlord experiences it as tangled rope — genuine coordination function
 *   (maintenance is necessary) alongside extraction (costs are high and
 *   concentrated). Institutional management companies and code enforcement
 *   agencies experience it as rope — solving a coordination problem with
 *   acceptable cost structure. Tenant advocacy coalitions see it as scaffold
 *   — private repair rights and tenant remedies are building exit pathways.
 *   The property tax system appears to coordinate maintenance incentives but
 *   functions as piton — theatrical without real force.
 *
 * KEY AGENTS:
 *   - Tenant: Primary victim (powerless/trapped) — bears cost of substandard conditions; no exit mechanism except homelessness or displacement
 *   - Small Landlord (1-5 units): Secondary victim (moderate/constrained) — faces compliance costs and enforcement action risk; also coordinates genuine maintenance; consolidation pressure from institutional competitors
 *   - Institutional Property Management: Primary beneficiary (institutional/arbitrage) — economies of scale enable compliance; arbitrage between landlords and tenants; concentration advantage from small landlord displacement
 *   - Municipal Code Enforcement: Primary beneficiary (institutional/arbitrage) — enforcement authority and discretionary budget flow to agency; stable institutional role
 *   - Tenant Advocacy Coalition: Organized agent (organized/constrained) — building alternative enforcement mechanisms (repair-and-deduct, private causes of action, habitability codes); represent potential sunset mechanism
 *   - Property Tax/Valuation System: Institutional observer (institutional/arbitrage) — ostensibly coordinates maintenance incentives through assessment; theater high, functional coupling low
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional enforcement as necessary to housing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(single_family_rental_maintenance_standards, 0.52).
domain_priors:suppression_score(single_family_rental_maintenance_standards, 0.58).
domain_priors:theater_ratio(single_family_rental_maintenance_standards, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(single_family_rental_maintenance_standards, extractiveness, 0.52).
narrative_ontology:constraint_metric(single_family_rental_maintenance_standards, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(single_family_rental_maintenance_standards, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(single_family_rental_maintenance_standards, tangled_rope).
narrative_ontology:human_readable(single_family_rental_maintenance_standards, "Single-Family Rental Maintenance Standards Enforcement").
narrative_ontology:topic_domain(single_family_rental_maintenance_standards, "housing/property_management/regulatory").

domain_priors:requires_active_enforcement(single_family_rental_maintenance_standards).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(single_family_rental_maintenance_standards, property_management_industry).
narrative_ontology:constraint_beneficiary(single_family_rental_maintenance_standards, municipal_code_enforcement).
narrative_ontology:constraint_victim(single_family_rental_maintenance_standards, tenant_habitability).
narrative_ontology:constraint_victim(single_family_rental_maintenance_standards, small_landlord_compliance_burden).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE TENANT (SNARE) — Trapped in the rental unit; cannot exit without losing housing security and bearing relocation costs. Bears full cost of substandard conditions (health, safety, quality of life). No collective organization or formal exit mechanism. Suppression is structural: homelessness or displacement is the cost of exit. Maximum experienced extraction — the tenant has no leverage.
constraint_indexing:constraint_classification(single_family_rental_maintenance_standards, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE SMALL LANDLORD (TANGLED ROPE) — Constrained by compliance costs, specialized knowledge requirements, and enforcement action risk. But also coordinates genuine maintenance — safety and habitability require landlord investment. The constraint has both a real coordination function (maintaining safe housing) and asymmetric extraction (disproportionate cost burden on small owners vs. institutional management). Exit options are constrained: selling triggers capital gains tax and opportunity cost; refusing maintenance triggers fines or tenant remedies.
constraint_indexing:constraint_classification(single_family_rental_maintenance_standards, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE PROPERTY MANAGEMENT COMPANY (ROPE) — Institutional actors with economies of scale, specialized compliance knowledge, and diversified portfolios. Experiences the constraint as coordination: maintenance standards are built into their service model. Can pass costs to owners and select properties by profitability. Net beneficiary — they arbitrage between landlords with high compliance costs and tenants.
constraint_indexing:constraint_classification(single_family_rental_maintenance_standards, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: MUNICIPAL CODE ENFORCEMENT (ROPE) — Institutional actor coordinating safety standards across residential stock. Experiences the constraint as solving a collective action problem: uncoordinated landlord decisions would produce substandard housing. Has stable authority structure, resource allocation, and career advancement tied to enforcement success. Net beneficiary — enforcement discretion and budget authority flow to them.
constraint_indexing:constraint_classification(single_family_rental_maintenance_standards, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: TENANT ADVOCACY COALITION (SCAFFOLD) — Organized agents (legal aid, housing nonprofits, tenant unions) see maintenance standards as a temporary coordination failure being solved through alternative mechanisms: right-to-repair laws, repair-and-deduct provisions, habitability codes with private causes of action. These pathways shift enforcement from institutional code agencies to individual tenants. Low effective extraction because organized agents have agency and see a structural exit path toward tenant-empowered enforcement. Theater is present but declining as legal tools shift enforcement burden.
constraint_indexing:constraint_classification(single_family_rental_maintenance_standards, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: THE PROPERTY TAX/VALUATION SYSTEM (PITON) — Ostensibly links property tax to maintenance condition (lower assessments for substandard units should incentivize landlord investment). In practice, the assessment-to-maintenance feedback loop is weak: political resistance to property tax changes, lag in assessment cycles, and opacity in valuation methodology create theatrical accountability without real incentive force. The system persists through inertia despite low functional coupling to actual maintenance outcomes. Theater ratio high because the system appears to price maintenance but doesn't.
constraint_indexing:constraint_classification(single_family_rental_maintenance_standards, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational view, housing requires maintenance; tenants and landlords are parties to exchange; some coordination mechanism must exist. The naturalizing framing claims: 'Standards enforcement is inherent to rental markets — you cannot have functioning housing without it.' However, structural data reveals this as false summit: the specific institutional arrangement (municipal code enforcement + landlord liability + compliance costs borne asymmetrically by small owners) is contingent, not natural. Alternative mechanisms exist (tenant-enforced repair rights, performance-based deposits, third-party inspection services) that achieve habitability with different extractiveness profiles.
constraint_indexing:constraint_classification(single_family_rental_maintenance_standards, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(single_family_rental_maintenance_standards_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(single_family_rental_maintenance_standards, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(single_family_rental_maintenance_standards, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(single_family_rental_maintenance_standards, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(single_family_rental_maintenance_standards, TR),
    TR >= 0.70.

:- end_tests(single_family_rental_maintenance_standards_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint has genuine coordination content (maintenance is required for safe housing), but asymmetric cost distribution and institutional consolidation effects amplify the extraction component. The initial value (0.35) reflects coordination dominance in small-owner markets with low enforcement; the increased value (0.52) reflects consolidation and heightened enforcement intensity over the interval. Suppression (0.58): Moderate-high. Structural barriers to tenant exit (housing insecurity, displacement cost) are high. Barriers to small landlord exit (capital gains tax, opportunity cost, liquidity constraints) are significant. But suppression is not total — some mobility exists for tenants who relocate, and some landlords can sell or exit. Theater ratio (0.64): Moderate-high. Code enforcement involves inspection rituals, violation citation processes, and documented remediation that serve accountability functions but also contain performative elements. The theater has increased over the interval as enforcement has become more systematic and paperwork-intensive. Repair-and-deduct remedies bypass much of this theater, suggesting theater is not strictly necessary for habitability outcomes.
 *
 * PERSPECTIVAL GAP:
 *   Snare (tenant) vs. Rope (management/enforcement) represents the maximum gap. The tenant perceives pure extraction with no coordination benefit because code enforcement delivers habitability standards but leaves them no agency and no recovery of compliance costs. Institutional management perceives coordination because economies of scale make compliance cost-effective, and they can pass costs to owners and tenants. The gap is not observational (both see the same enforcement actions) but distributional: enforcement benefits flow to management/agencies; costs flow to small landlords/tenants. The scaffold perspective (tenant advocates) represents an emerging perspectival shift — as repair-and-deduct rights mature, the constraint appears less like a snare and more like a temporary coordination failure being solved. The piton perspective (property tax) reveals theatrical accountability without real functional coupling, suggesting the constraint's apparent necessity (mountain view) is partially performative.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is determined by the agent's structural position relative to extraction flow. Tenants are victims with trapped exit: d ≈ 0.95, producing high f(d) ≈ 1.42, high effective extraction chi. Small landlords are victims with constrained exit (high cost but possible): d ≈ 0.70, producing moderate f(d) ≈ 1.00. Institutional management companies are beneficiaries with arbitrage exit: d ≈ 0.10, producing low f(d) ≈ -0.02, negative chi (they arbitrage the constraint). Code enforcement is institutional beneficiary with arbitrage: d ≈ 0.15, producing f(d) ≈ 0.00 (neutral experienced extraction). Tenant advocates are organized with constrained exit but growing alternatives: d ≈ 0.45, producing f(d) ≈ 0.52. The property tax system is institutional with arbitrage but theater masks true function: d ≈ 0.12. The analytical observer is analytical position: d ≈ 0.72. The scope modifier σ(local=0.8) dampens chi for local perspectives, σ(regional=0.9) for mid-scale. This produces Chi values: tenant snare chi = 0.52 × 1.42 × 0.8 ≈ 0.59 (high), small landlord tangled rope chi = 0.52 × 1.00 × 0.9 ≈ 0.47 (moderate), institutional management rope chi = 0.52 × (-0.02) × 1.0 ≈ negative (arbitrage benefit), code enforcement rope chi = 0.52 × 0.00 × 1.0 ≈ 0.0 (neutral), tenant advocates scaffold chi = 0.52 × 0.52 × 0.9 ≈ 0.24 (low, justifying scaffold classification).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by distinguishing coordination function from extraction function. The base claim is that maintenance standards require institutional enforcement — that without code enforcement, landlords would provide substandard housing. The counter-claim is that private repair-and-deduct remedies, habitability codes with private causes of action, and third-party inspection services achieve comparable outcomes with lower extractiveness and theater. The mandatrophy is resolved by recognizing that BOTH claims can be true: the coordination function (ensuring habitability) is real, but the specific institutional arrangement (municipal code enforcement) is one of several mechanisms for achieving it. The tangled rope classification captures this: the constraint has genuine coordination content (maintenance is required) AND asymmetric extraction (costs are borne unequally). The scaffold perspective operationalizes the resolution by identifying alternative enforcement pathways (repair-and-deduct, private causes of action) that deliver habitability with lower theater and lower institutional consolidation effects. The sunset is real: as tenant remedies mature, the extraction component of municipal enforcement declines. The piton perspective (property tax) reveals that some apparent 'coordination' mechanisms are theatrical — the property tax system claims to incentivize maintenance but demonstrates weak functional coupling.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    code_enforcement_selectivity,
    'Are code enforcement inspections distributed equitably across socioeconomic neighborhoods or concentrated in ways that correlate with tenant advocacy capacity and political visibility?',
    'Audit of code enforcement patterns: complaint-to-inspection ratios by neighborhood, demographic analysis of enforcement action distribution, temporal comparison of enforcement against advocacy presence',
    'If inequitable: the constraint functions as targeted extraction (snare) in low-advocacy areas and effective coordination in high-advocacy areas, splitting the classification. If equitable: snare classification is less justified; tangled rope classification is more appropriate for all tenants.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(code_enforcement_selectivity, empirical, 'Whether code enforcement is equitably distributed or concentrated by neighborhood advocacy capacity').

omega_variable(
    small_landlord_viability_threshold,
    'At what cumulative compliance cost do small (1-5 unit) landlords exit the market or sell to institutional management companies, triggering portfolio consolidation?',
    'Time-series analysis of small landlord demographic turnover; regression of exit rates against cumulative compliance cost indices; tracking of portfolio consolidation patterns pre- and post-enforcement intensity changes',
    'If threshold is crossed: the constraint functions as an institutional consolidation mechanism (accelerating transition from distributed small landlords to institutional management), changing the extraction pattern from directed-at-individuals to structural-market-concentration.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(small_landlord_viability_threshold, empirical, 'The compliance cost threshold at which small landlords exit the rental market').

omega_variable(
    tenant_repair_rights_substitutability,
    'Do private repair-and-deduct remedies and right-to-repair laws achieve comparable habitability outcomes to municipal code enforcement, with lower theater and lower extractiveness?',
    'Comparison of habitability outcomes (safety inspections, health code compliance, resident satisfaction) in jurisdictions with strong private repair rights vs. enforcement-dependent models; measurement of theater ratio in each pathway (compliance evidence vs. enforcement action)',
    'If substitutable: scaffold classification is justified, and the sunset is real — private remedies are alternative extraction pathways with lower institutional capture risk. If not substitutable: municipal enforcement remains necessary, reducing sunset prospect and strengthening tangled rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tenant_repair_rights_substitutability, empirical, 'Whether private repair-and-deduct remedies substitute for municipal code enforcement').

omega_variable(
    externality_asymmetry_magnitude,
    'What proportion of maintenance costs represent genuine coordination of shared externalities (foundation damage affecting adjacent units, health hazards spreading to neighborhood) vs. pure tenant-specific habitability (aesthetic conditions, minor repairs)?',
    'Structural engineering analysis of maintenance cost itemization; separation of inter-unit externality costs from tenant-specific habitability requirements; comparison against standard habitability code language',
    'If externalities dominate: the constraint is primarily coordination, justifying rope classification and beneficiary designation as genuine. If tenant-specific habitability dominates: the constraint is primarily protection, shifting beneficiary status and justifying snare classification for the tenant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_asymmetry_magnitude, conceptual, 'The relative magnitude of coordination externalities vs. pure tenant habitability in maintenance cost structure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(single_family_rental_maintenance_standards, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sfr_maint_tr_t0, single_family_rental_maintenance_standards, theater_ratio, 0, 0.48).
narrative_ontology:measurement(sfr_maint_tr_t5, single_family_rental_maintenance_standards, theater_ratio, 5, 0.58).
narrative_ontology:measurement(sfr_maint_tr_t10, single_family_rental_maintenance_standards, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(sfr_maint_be_t0, single_family_rental_maintenance_standards, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sfr_maint_be_t5, single_family_rental_maintenance_standards, base_extractiveness, 5, 0.44).
narrative_ontology:measurement(sfr_maint_be_t10, single_family_rental_maintenance_standards, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(single_family_rental_maintenance_standards, enforcement_mechanism).
narrative_ontology:affects_constraint(single_family_rental_maintenance_standards, tenant_remediation_rights).
narrative_ontology:affects_constraint(single_family_rental_maintenance_standards, landlord_portfolio_consolidation).
narrative_ontology:affects_constraint(single_family_rental_maintenance_standards, housing_cost_affordability).

% DUAL FORMULATION NOTE:
% Maintenance standards enforcement can be decomposed into coordination function (ensuring safe housing) and extraction mechanism (institutional consolidation and cost-shifting). The coordination component appears as rope/scaffold (alternative mechanisms achieve habitability). The extraction component appears as snare/tangled rope (unequal cost distribution and consolidation effects). This story addresses the hybrid constraint; separate stories could model pure-coordination repair-and-deduct mechanisms (lower extractiveness, higher tenant agency) and institutional consolidation dynamics (extraction-focused, pure institutional perspective).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(single_family_rental_maintenance_standards, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
