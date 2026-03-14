% ============================================================================
% CONSTRAINT STORY: software_license_lock_in
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_license_lock_in, []).

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
 *   constraint_id: software_license_lock_in
 *   human_readable: Software License Lock-In: Proprietary Vendor Control via Licensing Terms
 *   domain: technology/business/intellectual_property
 *
 * SUMMARY:
 *   Software license lock-in is a structural constraint where proprietary
 *   vendors use licensing terms, contractual restrictions, and technical
 *   integration to create switching costs that exceed customers' feasible
 *   alternatives. The constraint operates across enterprise computing,
 *   professional tools, and development platforms. It exhibits all six
 *   classification types from different perspectives because it combines a
 *   genuine coordination function (enabling software distribution,
 *   maintenance, and support) with substantial asymmetric extraction (vendor
 *   capture of user surplus via pricing power, contract terms, and
 *   enforcement). The constraint's evolution shows increasing extractiveness
 *   (0.42 → 0.58 over the interval) as vendors consolidate market position,
 *   offset partly by declining theater ratio (0.55 → 0.48) as subscription
 *   models reduce performative enforcement and replace it with technical
 *   constraints (account-based access, usage metering). The open-source
 *   movement represents a generational exit pathway: as open-source software
 *   matures in capability and adoption, proprietary lock-in's extractive
 *   power decreases through sustained competitive pressure rather than
 *   dramatic replacement.
 *
 * KEY AGENTS:
 *   - Proprietary Software Vendors: Primary beneficiary (institutional/arbitrage) — capture rent through licensing terms, enforcement, and switching cost asymmetry; can pivot pricing, licensing models, or market segments
 *   - Enterprise Customers: Primary victim (powerless/trapped) — bear full extraction via high switching costs; mission-critical deployment prevents exit despite high extraction
 *   - Small Business Users: Secondary victim (moderate/constrained) — face constraints on exit but retain negotiating leverage and partial migration options
 *   - Open Source Development Communities: Organized actors (organized/constrained) — building exit pathways that gradually erode proprietary lock-in; constrained by volunteer coordination and feature parity challenges
 *   - Corporate Compliance Departments: Institutional actors (institutional/arbitrage) — enforce license terms through audits and contractual monitoring; maintain procedural theater with declining real enforcement effectiveness
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing intellectual property lock-in as inherent to software innovation rather than a contingent institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_license_lock_in, 0.58).
domain_priors:suppression_score(software_license_lock_in, 0.65).
domain_priors:theater_ratio(software_license_lock_in, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_license_lock_in, extractiveness, 0.58).
narrative_ontology:constraint_metric(software_license_lock_in, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(software_license_lock_in, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_license_lock_in, tangled_rope).
narrative_ontology:human_readable(software_license_lock_in, "Software License Lock-In: Proprietary Vendor Control via Licensing Terms").
narrative_ontology:topic_domain(software_license_lock_in, "technology/business/intellectual_property").

domain_priors:requires_active_enforcement(software_license_lock_in).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_license_lock_in, proprietary_software_vendors).
narrative_ontology:constraint_victim(software_license_lock_in, enterprise_customers).
narrative_ontology:constraint_victim(software_license_lock_in, small_business_users).
narrative_ontology:constraint_victim(software_license_lock_in, open_source_ecosystem).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOCKED-IN ENTERPRISE CUSTOMER (SNARE) — An organization that has deployed mission-critical software across its infrastructure faces insurmountable barriers to switching. License terms restrict redistribution, modification, and concurrent installation; switching costs (retraining, system redesign, data migration) are prohibitive; and contractual terms impose penalties for early termination. The customer is trapped by both material barriers (technical integration, data lock-in) and legal/financial terms. Bears full extraction cost with no meaningful exit option.
constraint_indexing:constraint_classification(software_license_lock_in, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SMALL BUSINESS USER (TANGLED ROPE) — Smaller organizations face constraints but retain some agency through partial migration or negotiation. The license term (per-seat, per-instance, usage-based) creates coordination benefit: licensing enables access to professional tools without massive upfront capital. But asymmetric extraction exists: pricing discriminates against small users; compliance audits impose disproportionate burden; and contractual terms prevent resale or secondhand market. High suppression (legal enforcement of terms) with genuine but constrained exit options.
constraint_indexing:constraint_classification(software_license_lock_in, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PROPRIETARY SOFTWARE VENDOR (ROPE) — The vendor experiences licensing as a coordination mechanism: license terms enable software distribution, manage support burden, and signal professional legitimacy. The vendor benefits from enforcement (audits, legal action against unauthorized use) but also provides genuine coordination value: maintenance, updates, support, and protection of intellectual property investment. Arbitrage exit option: vendor can adjust terms, migrate customer base, or pivot to new market segments.
constraint_indexing:constraint_classification(software_license_lock_in, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN SOURCE MOVEMENT (SCAFFOLD) — Organized development communities (Linux, Apache, open-source alternatives) represent a temporary coordination solution with sunset logic. These projects build parallel capabilities to proprietary software; they provide exit pathways (migration to open-source equivalents) that reduce extraction's effectiveness over time. The constraint has a sunset clause: as open-source tools mature (estimated 5-15 years depending on software category), proprietary lock-in's extractive power diminishes. Organized agents can coordinate around alternative pathways.
constraint_indexing:constraint_classification(software_license_lock_in, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY LICENSE COMPLIANCE THEATER (PITON) — Corporate compliance departments enforce license terms (audit clauses, usage monitoring, contractual penalties) with substantial performative content. Much of the enforcement is procedural theater: license audits verify contract compliance but do not substantially improve product security, functionality, or performance. The compliance ritual persists through institutional inertia despite degraded enforcement (increasingly difficult to monitor all usage; open-source alternatives provide escape). Theater ratio is high; effective extraction via compliance is declining as vendors shift to subscription models that reduce enforcement burden.
constraint_indexing:constraint_classification(software_license_lock_in, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / INTELLECTUAL PROPERTY NATURALIZATION (MOUNTAIN) — From a civilizational perspective, some might argue that intellectual property licensing is a natural law: software code is a creative good; protecting the creator's investment through licensing is inherent to innovation incentives; therefore lock-in is an inevitable cost of software production. However, this classification is a false summit. The structural data contradicts the mountain classification: significant extraction (0.58), high suppression (0.65), and contingent institutional mechanisms (enforceable contracts, vendor lock-in strategies) are not unchangeable natural laws — they are policy choices. The mountain perspective naturalizes what is actually a tangled arrangement of extraction and coordination.
constraint_indexing:constraint_classification(software_license_lock_in, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_license_lock_in_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(software_license_lock_in, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(software_license_lock_in, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(software_license_lock_in, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(software_license_lock_in, TR),
    TR >= 0.70.

:- end_tests(software_license_lock_in_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, increasing. The vendor captures surplus through per-seat pricing, usage restrictions, and switching cost creation. The value has increased over the interval as vendors consolidate market share and implement more sophisticated licensing metering. However, extractiveness is not maximal (not 0.72+) because customers do retain some agency through negotiation (enterprise customers can negotiate volume licensing), partial migration (moving some workloads to open-source), or accepting high switching costs to exit. The upward trend reflects vendors' increasing market power and refinement of extraction mechanisms (from simple per-license to usage-based metering). Suppression (0.65): Moderate-high. Legal mechanisms (contract enforcement, copyright claims, license audits) create barriers, but suppression is not total because open-source alternatives exist and enforcement costs are rising. Customers are suppressed by contractual terms and switching cost, not by absolute technical prevention. Theater ratio (0.48): Moderate, declining. License compliance audits and enforcement procedures contain substantial performative content (demonstrating corporate diligence to auditors) but also genuine rent extraction (detected unlicensed use triggers penalties). The declining trend reflects the shift from perpetual licensing (which required ongoing enforcement to prevent unauthorized use) to subscription models (which rely on technical access control rather than legal enforcement theater).
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is maximum across the constraint: from the vendor's view (rope: coordination of legitimate software distribution) to the trapped customer's view (snare: pure extraction with no exit). This gap reveals the fundamental structural asymmetry: one agent (vendor) experiences the constraint as solving a coordination problem (software distribution, support, IP protection); the other (locked-in customer) experiences it as extraction with no coordination benefit. The middle perspectives (small business experiencing tangled rope, open-source coalition experiencing scaffold) show intermediate positions where coordination and extraction are genuinely mixed. The piton perspective (compliance theater) demonstrates how institutional inertia maintains enforcement procedures (license audits, penalty structures) that persist despite declining real enforcement effectiveness. The analytical observer's mountain classification is a false summit: it naturalizes a contingent policy arrangement (enforceable intellectual property licenses with vendor lock-in) as an unchangeable law of innovation. The structural data contradicts this naturalization.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from each agent's structural position. Enterprise customers classified as trapped experience d ≈ 0.95 (maximum victim status) because they bear extraction with no material exit capacity. Small business users classified as constrained experience d ≈ 0.60-0.75 (high victim status) because exit is possible at significant cost. Proprietary vendors classified as institutional beneficiaries with arbitrage exit experience d ≈ 0.10-0.20 (beneficiary status) because they extract surplus and can adjust terms or markets. Open-source coalition classified as organized agents with constrained exit (they can develop alternatives but face coordination challenges) experience d ≈ 0.45-0.55 (moderate victim/beneficiary hybrid) because they both bear costs (volunteer coordination burden) and benefit (building alternatives that reduce lock-in). The sigmoid f(d) transforms these d values into experienced extractiveness χ: trapped customers experience maximum χ (high f(d)); beneficiary vendors experience low or negative χ (low f(d)).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that all six types are legitimate perspectival readings. The classification is not 'which type is correct?' but 'which structural position are you measuring from?' The vendor genuinely experiences coordination function (rope). The trapped customer genuinely experiences pure extraction (snare). The open-source coalition genuinely experiences a temporary problem with sunset logic (scaffold). The compliance department genuinely experiences institutional ritual (piton). The small business genuinely experiences mixed coordination and extraction (tangled rope). The analytical observer naturalizes what is actually a policy choice (mountain). No single type is the 'true' classification — the presheaf over observation sites (vendor, customer, coalition, compliance, analyst) is the answer. Mandatrophy is resolved by rejecting the monism that seeks one true type and accepting the perspectival realism that different structural positions yield different legitimate classifications from identical base properties.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vendor_switching_cost_threshold,
    'At what absolute switching cost does a customer transition from trapped to constrained exit status?',
    'Empirical analysis of actual migration costs (system redesign, data conversion, retraining, downtime) versus customer revenue; comparison across software categories (ERP, database, development tools)',
    'If threshold < 10% annual software spend: many customers classified as constrained could exit → reduces Snare prevalence. If threshold > 40% annual spend: most enterprise customers are trapped → increases Snare prevalence and extraction effectiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vendor_switching_cost_threshold, empirical, 'Switching cost threshold determining exit status').

omega_variable(
    open_source_capability_parity,
    'Do open-source alternatives provide feature parity with proprietary tools in sufficient software categories to constitute a genuine mass-market exit pathway?',
    'Feature matrix comparison (functionality, performance, user experience, ecosystem support) across software categories; adoption rates of open-source substitutes by locked-in customer segments',
    'If parity achieved in > 50% of software market: scaffold perspective is structural, sunset is real → tangled_rope classification is stable. If parity < 30%: open-source alternative is aspirational, not practical exit → snare prevalence increases.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(open_source_capability_parity, empirical, 'Whether open-source provides practical alternatives to proprietary software').

omega_variable(
    contractual_enforcement_effectiveness,
    'How effectively do vendors actually enforce license terms against customers at different scales, and does enforcement cost exceed the rent extracted?',
    'Analysis of vendor audit frequency, enforcement rates, penalty revenue versus audit execution cost; customer compliance survey data',
    'If enforcement is economically viable (revenue > cost): suppression rating 0.65 is justified. If enforcement is net-negative (cost > revenue): suppression is theater-sustained rather than enforcement-sustained → reclassify piton component upward, overall theater_ratio increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contractual_enforcement_effectiveness, empirical, 'Cost-effectiveness of license term enforcement').

omega_variable(
    subscription_model_exit_effect,
    'Does the shift from perpetual licensing to subscription models (SaaS, recurring payments) increase or decrease lock-in extractiveness?',
    'Historical comparison of switching rates and costs: perpetual license era vs subscription era for same software categories; vendor pricing strategies and revenue concentration in subscription models',
    'If subscriptions increase lock-in: χ would increase (higher effective extraction) → Snare becomes dominant classification. If subscriptions decrease lock-in (lower switching costs, simpler contract exit): χ would decrease → Rope becomes more prevalent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subscription_model_exit_effect, empirical, 'How licensing models affect lock-in extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_license_lock_in, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(swlic_tr_t0, software_license_lock_in, theater_ratio, 0, 0.55).
narrative_ontology:measurement(swlic_tr_t5, software_license_lock_in, theater_ratio, 5, 0.51).
narrative_ontology:measurement(swlic_tr_t10, software_license_lock_in, theater_ratio, 10, 0.48).
narrative_ontology:measurement(swlic_tr_t15, software_license_lock_in, theater_ratio, 15, 0.48).

% Extraction over time
narrative_ontology:measurement(swlic_be_t0, software_license_lock_in, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(swlic_be_t5, software_license_lock_in, base_extractiveness, 5, 0.51).
narrative_ontology:measurement(swlic_be_t10, software_license_lock_in, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(swlic_be_t15, software_license_lock_in, base_extractiveness, 15, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_license_lock_in, resource_allocation).
narrative_ontology:affects_constraint(software_license_lock_in, software_interoperability_standards).
narrative_ontology:affects_constraint(software_license_lock_in, open_source_adoption_barriers).
narrative_ontology:affects_constraint(software_license_lock_in, intellectual_property_enforcement).

% DUAL FORMULATION NOTE:
% Software license lock-in decomposes into multiple structurally distinct constraints depending on observable: (1) vendor pricing power and margin extraction (ε ≈ 0.58, this story); (2) technical data lock-in and format incompatibility (ε varies by data type, separate story); (3) intellectual property enforcement mechanisms (ε ≈ 0.35, licensing-as-coordination story). The shared label 'lock-in' conflates these distinct mechanisms. This story focuses on licensing terms and contractual lock-in (observable: price discrimination, contract enforcement, switching cost asymmetry). Related stories address technical lock-in (data format, API dependence) and IP enforcement (copyright, patent mechanisms).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(software_license_lock_in, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
