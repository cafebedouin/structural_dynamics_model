% ============================================================================
% CONSTRAINT STORY: raac_school_maintenance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_raac_school_maintenance, []).

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
 *   constraint_id: raac_school_maintenance
 *   human_readable: Systemic Response to RAAC Concrete Failures in UK Schools
 *   domain: economic/political
 *
 * SUMMARY:
 *   Reinforced Autoclaved Aerated Concrete (RAAC), a cost-effective building
 *   material widely used from the 1950s through 1990s, is experiencing
 *   systematic failure across hundreds of UK schools. The structural
 *   constraint operates at the intersection of infrastructure aging,
 *   institutional budget cycles, and regulatory compliance. Students and
 *   teachers remain physically trapped in deteriorating buildings while
 *   decision-making is deferred across local authorities and central
 *   government. The constraint exhibits a mixed coordination-extraction
 *   hybrid: genuine coordination problems exist (prioritizing high-risk
 *   sites, organizing remediation) alongside asymmetric extraction (local
 *   authorities deferring maintenance costs, central government stretching
 *   remediation timelines to preserve budget allocations, construction
 *   companies capturing margins on urgent repairs). The theater ratio
 *   reflects that much institutional response is performative: safety audits
 *   and closure protocols manage risk perception while actual remediation
 *   proceeds at budgetary rather than structural-necessity pace. The
 *   constraint's evolution shows increasing extractiveness and theater from
 *   2020-2025 as the scope of RAAC prevalence became undeniable, forcing
 *   public acknowledgment while remediation capacity remained inadequate.
 *
 * KEY AGENTS:
 *   - Students and Teachers in RAAC Schools: Primary victims (powerless/trapped) — enclosed in deteriorating facilities with no exit option short of forgoing education/employment
 *   - School Headteachers and Governors: Secondary agents (moderate/constrained) — bear enforcement burden of managing closures and continuity; benefit from guidance coordination but face liability exposure
 *   - Local Authorities: Primary beneficiaries (institutional/arbitrage) — extract budget delay value; coordinate remediation prioritization; can reallocate deferred maintenance capital
 *   - Central Government (Department for Education): Primary beneficiary (institutional/arbitrage) — preserves overall capital budget by phasing remediation; coordinates national prioritization framework
 *   - Construction and Remediation Companies: Secondary beneficiaries (powerful/mobile) — capture contract value and cost markups; benefit from sustained demand; constrained by supply chains and scheduling
 *   - Building Standards and Material Certification: Institutional actor (institutional/arbitrage) — maintains performative compliance despite documented failures; persists through regulatory inertia
 *   - School Rebuilding Programme: Organized coordinating agent (organized/constrained) — scaffold function with explicit sunset targets; enables remediation while constrained by budget allocations and construction capacity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(raac_school_maintenance, 0.58).
domain_priors:suppression_score(raac_school_maintenance, 0.68).
domain_priors:theater_ratio(raac_school_maintenance, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(raac_school_maintenance, extractiveness, 0.58).
narrative_ontology:constraint_metric(raac_school_maintenance, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(raac_school_maintenance, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(raac_school_maintenance, tangled_rope).
narrative_ontology:human_readable(raac_school_maintenance, "Systemic Response to RAAC Concrete Failures in UK Schools").
narrative_ontology:topic_domain(raac_school_maintenance, "economic/political").

domain_priors:requires_active_enforcement(raac_school_maintenance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(raac_school_maintenance, local_authorities_delaying_remediation).
narrative_ontology:constraint_beneficiary(raac_school_maintenance, construction_companies_contracted_for_repairs).
narrative_ontology:constraint_beneficiary(raac_school_maintenance, central_government_budget_preservation).
narrative_ontology:constraint_victim(raac_school_maintenance, students_and_teachers_in_affected_schools).
narrative_ontology:constraint_victim(raac_school_maintenance, school_operational_capacity).
narrative_ontology:constraint_victim(raac_school_maintenance, educational_continuity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STUDENTS/TEACHERS (SNARE) — Trapped within the school estate. Cannot exit the risk without forgoing education or employment. No voice in remediation decisions. Bear full structural cost: closure risk, health hazards, reduced learning time during repairs. Maximum extraction toward institutional actors.
constraint_indexing:constraint_classification(raac_school_maintenance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: HEADTEACHERS/GOVERNORS (TANGLED ROPE) — Constrained by duty-of-care obligations and local authority budgets. Benefit from coordination (shared risk data, remediation prioritization protocols). But also bear enforcement burden: managing closures, arranging temporary sites, reassuring parents. Mixed extraction and coordination function.
constraint_indexing:constraint_classification(raac_school_maintenance, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LOCAL AUTHORITIES/CENTRAL GOVT (ROPE) — Primary beneficiaries. Experience constraint as pure coordination: information-sharing protocols, risk prioritization frameworks, contracting mechanisms. Extract funding delays and budget optimization from the phased remediation timeline. Arbitrage: can reallocate capital by deferring non-RAAC maintenance.
constraint_indexing:constraint_classification(raac_school_maintenance, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSTRUCTION COMPANIES (TANGLED_ROPE) — Benefit from sustained remediation contracts. Constrained by material supply chains and regulatory inspection requirements. High agency but also dependent on government budget allocation and scheduling. Both extract (through cost markups) and enable coordination (technical solutions). Powerful but not fully free.
constraint_indexing:constraint_classification(raac_school_maintenance, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: BUILDING STANDARDS/CERTIFICATION (PITON) — The regulatory framework that certified RAAC as compliant persists despite its now-documented failure. Theater ratio: high. Certification bodies maintain performative compliance audits while acknowledging RAAC's degradation. The system remains because alternatives (retroactive decertification, liability cascade) are institutionally costly, not because it functions. Maintenance through inertia.
constraint_indexing:constraint_classification(raac_school_maintenance, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: GOVERNMENT REMEDIATION PROGRAM (SCAFFOLD) — The Department for Education's School Rebuilding Programme and RAAC remediation funding represent explicit scaffold: temporary coordination with a sunset clause. Phase 1 (2023-2025) focuses on high-risk schools; subsequent phases have sunset target dates. Coordination function (resource allocation, risk prioritization) with planned exit. Theater not minimal but declining as program matures.
constraint_indexing:constraint_classification(raac_school_maintenance, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN RISK) — Risks naturalizing the RAAC failure as an immutable property of infrastructure aging. From a civilizational view, material degradation is inherent to physical systems. However, base properties contradict mountain criteria: suppression (0.68) far exceeds the 0.05 threshold, extractiveness (0.58) exceeds 0.25, and requires_active_enforcement is true. This is a false summit — the naturalized framing obscures contingent institutional decisions (deferred inspection, delayed remediation, budget prioritization).
constraint_indexing:constraint_classification(raac_school_maintenance, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(raac_school_maintenance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(raac_school_maintenance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(raac_school_maintenance, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(raac_school_maintenance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(raac_school_maintenance, TR),
    TR >= 0.70.

:- end_tests(raac_school_maintenance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Base extraction reflects that the primary cost-bearing actors (students, teachers, school operations) have zero control over remediation timelines while beneficiary actors (local authorities, central government, construction firms) extract value through deferral (budget optimization), urgency markups (construction pricing), and performance credentialing (compliance audits that don't accelerate actual work). The trajectory shows increasing extractiveness over the interval as RAAC scope became undeniable — initially suppressed knowledge gave way to acknowledged crisis, but remediation pace remained constrained, making the extraction more explicit. Suppression (0.68): High. Trapped students have zero exit options within their educational context. Schools cannot close permanently without educational alternatives. Local authorities have limited exit (can privatize but contractual complexity is high). The binding suppression is the construction sector bottleneck: remediation speed is constrained by labor, materials, regulatory approval, and budget cycles regardless of urgency. Theater ratio (0.64): Moderate-high and increasing. Performative elements include: closure announcements without remediation start dates, risk assessments that manage public perception rather than accelerate repair, safety protocols that appear responsive but don't reduce timeline pressure. However, theater is not maximal — genuine coordination activity (site prioritization, cost estimation, contracting) has functional content. The ratio increases over time as performative response (announcements, task forces, guidance) outpaces actual remediation speed. Claimed type (Tangled Rope): Justified by presence of coordination function (prioritization protocols, resource allocation framework) AND asymmetric extraction (budget deferral, construction markups) AND active enforcement requirement (local authorities managing closures, central government allocating budgets).
 *
 * PERSPECTIVAL GAP:
 *   The constraint's core perspectival gap is between agents with budget-cycle autonomy and agents trapped in physical infrastructure. Local authorities see coordination (Rope) — they have exit options through budget reallocation and can benefit from phased remediation (preserving capital for other priorities). Central government sees its own remediation program as Scaffold — explicitly temporary with planned sunset. But students and teachers see Snare — no exit, all cost, zero benefit. Headteachers see Tangled Rope — managing both coordination (scheduling repairs) and extraction (bearing reputational risk of closures). Construction companies see Tangled Rope — benefit from contracts but constrained by material and labor supply, regulatory approval timelines. The building standards system sees Piton — maintaining performative compliance while the underlying system (RAAC certification) has failed. The gaps reflect genuine structural differences: agents can be categorized as (1) trapped without exit (students/teachers); (2) constrained by resources but with agency (headteachers, construction firms); (3) with budget-cycle autonomy (local authorities, central government); (4) with performative institutional role (building standards).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from beneficiary/victim status and exit options. Students/teachers: victim status + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high experienced extraction. Local authorities/central government: beneficiary status + arbitrage exit (can reallocate budgets, shift priorities) → d ≈ 0.05-0.15 → f(d) ≈ -0.12 to -0.01 → negative effective extraction (they benefit). Construction companies: beneficiary status (receive contracts) but constrained by supply/scheduling → d ≈ 0.35-0.45 → f(d) ≈ 0.30-0.40 → moderate extraction (they have agency but not full freedom). Headteachers: victim of enforcement burden (must manage closures) but with some institutional voice → d ≈ 0.60-0.70 → f(d) ≈ 1.00-1.15 → experienced extraction. The scope modifier σ(S) = 1.0 (national scope) leaves chi unscaled by scope; extraction is uniform across all school locations.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY CASE: TANGLED ROPE RESOLVES THE CLASSIFICATION. The initial framing of RAAC as a 'natural infrastructure aging problem' (Mountain) is a false summit — it naturalizes what is actually a contingent institutional decision to defer remediation. The constraint cannot be classified as pure Rope (coordination without extraction) because trapped students experience zero benefit. It cannot be classified as pure Snare (extraction without coordination) because genuine coordination functions exist (site prioritization, resource allocation, contracting). The Tangled Rope classification is mandated by the presence of: (1) coordination function (prioritization frameworks, risk assessment, remediation scheduling); (2) asymmetric extraction (budget deferral, construction cost markups, delayed timeline benefiting authorities while harming students); (3) active enforcement requirement (local authorities must manage closures, central government must allocate budgets and monitor progress). The government's School Rebuilding Programme and remediation funding represent explicit scaffold with sunset, showing institutional awareness that the constraint should be temporary. The theater ratio (0.64) reflects performative institutional response — announcements, task forces, guidance — that manages perception of crisis while remediation pace remains constrained by actual resource limits. The extractiveness trajectory (0.35 → 0.58) shows increasing extraction as RAAC prevalence became undeniable but remediation timelines remained budgetary rather than structural-necessity driven.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    remediation_timeline_sufficiency,
    'What timeline for full RAAC remediation prevents unacceptable health and safety risk accumulation in the interim?',
    'Correlation analysis between closure duration, structural failure rates, and student health outcomes; expert structural assessment of degradation acceleration curves',
    'If remediation can safely extend 10+ years: suppression is overstated, constraint is closer to Scaffold. If remediation must complete within 3-5 years: suppression is understated, constraint is closer to Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remediation_timeline_sufficiency, empirical, 'Timeline sufficiency for RAAC remediation under acceptable risk thresholds').

omega_variable(
    local_authority_fiscal_capacity,
    'Do local authorities have adequate fiscal capacity to fund accelerated RAAC remediation, or is central government budget allocation the binding constraint?',
    'Audit of local authority capital budgets; comparison of RAAC remediation costs to baseline maintenance budgets; analysis of competing capital priorities (social care, transport, housing)',
    'If local authorities can fund their share: constraint shifts toward Rope (coordination problem). If central government funding is binding constraint: constraint remains Snare for schools in underfunded authorities.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(local_authority_fiscal_capacity, empirical, 'Local authority fiscal capacity relative to RAAC remediation costs').

omega_variable(
    alternative_materials_readiness,
    'Are approved rapid-remediation materials and techniques mature and cost-competitive, or are supply and regulatory bottlenecks preventing fast deployment?',
    'Survey of construction companies on material availability and cost; analysis of Building Regulations approval timelines for alternative systems; case studies of accelerated remediation projects',
    'If alternatives are mature: remediation can proceed faster, suppression decreases, constraint approaches Scaffold. If bottlenecked: remediation speed is constrained, suppression remains high, constraint remains Snare/Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_materials_readiness, empirical, 'Readiness of alternative materials and rapid remediation techniques').

omega_variable(
    knowledge_of_raac_prevalence,
    'Was the extent of RAAC prevalence in the school estate known and suppressed by authorities, or genuinely unknown until recent assessments?',
    'Historical analysis of Building Regulations guidance, maintenance records, and inspection reports from 2000-2022; interviews with building control officers and local authority capital planners',
    'If known and suppressed: constraint is pure extraction (Snare). If genuinely unknown: constraint is a coordination failure with less asymmetric intent, closer to Rope or Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(knowledge_of_raac_prevalence, empirical, 'Extent of prior knowledge of RAAC failures by authorities').

omega_variable(
    construction_industry_capacity,
    'Can the construction industry execute remediation at scale without diverting resources from other critical infrastructure (NHS, social housing, transport)?',
    'Labor and materials availability forecasting; comparison of RAAC remediation scope to annual construction output; analysis of resource conflicts with other government capital programs',
    'If capacity is sufficient: constraint is primarily a budget allocation problem (Tangled Rope). If capacity is bottlenecked: constraint becomes scarcity-driven suppression (Snare across multiple sectors).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(construction_industry_capacity, empirical, 'Construction industry capacity to execute RAAC remediation at scale').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(raac_school_maintenance, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(raac_tr_t0, raac_school_maintenance, theater_ratio, 0, 0.48).
narrative_ontology:measurement(raac_tr_t5, raac_school_maintenance, theater_ratio, 5, 0.58).
narrative_ontology:measurement(raac_tr_t10, raac_school_maintenance, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(raac_be_t0, raac_school_maintenance, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(raac_be_t5, raac_school_maintenance, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(raac_be_t10, raac_school_maintenance, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(raac_school_maintenance, resource_allocation).
narrative_ontology:affects_constraint(raac_school_maintenance, uk_school_capital_budget_allocation).
narrative_ontology:affects_constraint(raac_school_maintenance, building_materials_certification_system).

% DUAL FORMULATION NOTE:
% RAAC concrete failure decomposes into two distinct constraints: (1) RAAC material physics (already-failed concrete in existing buildings — a Mountain from structural engineering perspective, ε ≈ 0.05); (2) Institutional response to failure (this story — a Tangled Rope, ε ≈ 0.58). The material failure is immutable physical fact. The institutional response is a contingent choice about deferral, budget allocation, and pace. This story models the institutional response constraint, not the material constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(raac_school_maintenance, moderate, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
