% ============================================================================
% CONSTRAINT STORY: sotu_1976_ford_federalism_devolution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1976_ford_federalism_devolution, []).

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
 *   constraint_id: sotu_1976_ford_federalism_devolution
 *   human_readable: Federalism Devolution: Transfer of Domestic Policy Authority from Federal to State/Local
 *   domain: governance/political_economy
 *
 * SUMMARY:
 *   Ford's 1976 State of the Union proposal to devolve domestic policy
 *   authority from federal to state and local governments presents a
 *   constitutional redistributive choice that benefits high-capacity
 *   jurisdictions and organized business while imposing costs on low-capacity
 *   regions and marginalized populations. The constraint exhibits the
 *   structural hallmark of tangled rope: genuine coordination benefit
 *   (tailoring policy to regional conditions, reducing federal bureaucratic
 *   overhead, enabling policy innovation) coexists with asymmetric extraction
 *   (unfunded mandate burden shift, civil rights protection erosion,
 *   regulatory races to bottom). The mechanism operates through authority
 *   transfer without uniform fiscal transfer — states gain discretion but not
 *   necessarily resources. The extractiveness score reflects moderate but
 *   real burden shift; the suppression score reflects meaningful barriers to
 *   policy coordination (capacity constraints, geographic immobility,
 *   political vulnerability in low-capacity regions). Theater ratio rises
 *   from 0.42 to 0.58 as federal government maintains claim on national
 *   outcomes (employment, growth, welfare) while ceding policy control — a
 *   signature Piton dynamic in the federal apparatus itself. The constraint
 *   family includes separate stories for low-capacity jurisdictions (snare),
 *   high-capacity jurisdictions (rope), and business optimizing across state
 *   boundaries (tangled rope), but this unified story captures the
 *   federal-level devolution mechanism as experienced across all positions.
 *
 * KEY AGENTS:
 *   - State and Local Leaders (High-Capacity Regions): Primary beneficiary (institutional/arbitrage) — gain discretion, retain resources, exercise authority over responsive governance
 *   - Low-Capacity Jurisdiction Residents: Primary victim (powerless/trapped) — cannot relocate, cannot replicate service provision, bear costs of federal withdrawal
 *   - Marginalized Populations (Civil Rights Dependents): Primary victim (powerless/trapped) — lose federal floor protections, face state-level majoritarian suppression
 *   - Federal Bureaucracy: Secondary actor (institutional/arbitrage) — loses functional authority and resource flow; maintains formal responsibility with reduced actual power
 *   - Organized Business and Capital: Secondary beneficiary (organized/mobile) — exploit regulatory arbitrage opportunity; face compliance fragmentation costs
 *   - Uniform Standard Advocates (Labor, Environment, Civil Rights Organizations): Organized victims (organized/mobile) — perceive sunset mechanism via crisis-driven federal reversion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1976_ford_federalism_devolution, 0.52).
domain_priors:suppression_score(sotu_1976_ford_federalism_devolution, 0.48).
domain_priors:theater_ratio(sotu_1976_ford_federalism_devolution, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1976_ford_federalism_devolution, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_1976_ford_federalism_devolution, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(sotu_1976_ford_federalism_devolution, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1976_ford_federalism_devolution, tangled_rope).
narrative_ontology:human_readable(sotu_1976_ford_federalism_devolution, "Federalism Devolution: Transfer of Domestic Policy Authority from Federal to State/Local").
narrative_ontology:topic_domain(sotu_1976_ford_federalism_devolution, "governance/political_economy").

domain_priors:requires_active_enforcement(sotu_1976_ford_federalism_devolution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1976_ford_federalism_devolution, state_local_leaders).
narrative_ontology:constraint_beneficiary(sotu_1976_ford_federalism_devolution, high_capacity_jurisdictions).
narrative_ontology:constraint_victim(sotu_1976_ford_federalism_devolution, uniform_standard_seekers).
narrative_ontology:constraint_victim(sotu_1976_ford_federalism_devolution, low_capacity_jurisdictions).
narrative_ontology:constraint_victim(sotu_1976_ford_federalism_devolution, marginalized_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-CAPACITY JURISDICTION RESIDENTS (SNARE) — Trapped by geography and residency. When federal standards dissolve, these jurisdictions cannot replicate high-service provision — they lack tax base, institutional capacity, and economies of scale. Devolution shifts costs upward to individuals; federal safety net contracts. Maximum extraction: these agents bear costs of policy fragmentation but have no exit mechanism (relocating is prohibitively expensive). No coordination benefit — pure burden shift.
constraint_indexing:constraint_classification(sotu_1976_ford_federalism_devolution, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MARGINALIZED POPULATIONS (SNARE) — Civil rights protections, environmental standards, and social safety net programs established at federal level dissolve into state-level politics where minority preferences face majoritarian suppression. These agents are trapped by lack of exit options and face systematic extraction: loss of federal floor protections, variable enforcement, regulatory arbitrage against vulnerable groups. No beneficiary side — pure extraction mechanism via devolution.
constraint_indexing:constraint_classification(sotu_1976_ford_federalism_devolution, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE/LOCAL LEADERS—MID-CAPACITY REGIONS (TANGLED ROPE) — Experience genuine coordination benefit: tailoring policies to regional conditions, responsive governance, reduced bureaucratic overhead. But also face extraction: unfunded mandates (federal elimination of federal funding without eliminating state obligation), competitive dynamics (neighboring jurisdictions attract businesses via regulatory races to bottom), and responsibility burden (politically vulnerable when policy fails locally). Mixed experience: some agency and benefit, but also significant cost shifting and coordination challenge.
constraint_indexing:constraint_classification(sotu_1976_ford_federalism_devolution, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: HIGH-CAPACITY JURISDICTION LEADERS (ROPE) — Clear beneficiaries. These jurisdictions already have tax base, technical expertise, and institutional capacity. Devolution grants them discretion and control over resources that would otherwise flow to federal bureaucracy. Federal withdrawal becomes their gain: regulatory authority + resource retention + policy innovation freedom. Low extraction experienced — they are net beneficiaries of the coordination mechanism. Pure rope from their perspective: devolution solves the collective action problem of centralizing decision-making at the federal level.
constraint_indexing:constraint_classification(sotu_1976_ford_federalism_devolution, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: FEDERAL BUREAUCRACY (PITON) — Institutional actor losing functional authority and resource flow. Devolution degrades the federal system's capacity for coherent national policy while maintaining federal legal framework (enumerated powers still exist; states constrained by Constitution). The federal apparatus persists through inertia — Civil Service, regulatory agencies, federal courts — but exercises less actual authority. Theater persists: federal government still claims responsibility for national outcomes (GDP, employment, welfare) while ceding policy control. High theater ratio reflects ongoing federal performance-claiming despite reduced functional authority.
constraint_indexing:constraint_classification(sotu_1976_ford_federalism_devolution, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ORGANIZED BUSINESS AND CAPITAL (TANGLED ROPE) — Experience mixed extraction and benefit. Devolution creates regulatory arbitrage opportunity: firms can optimize across state jurisdictions, jurisdictions compete for business via regulatory concessions (labor, environmental, tax races to bottom). Genuine coordination benefit: policy flexibility, reduced federal regulatory burden. But also face extraction: fragmented regulatory landscape increases compliance costs across borders, prevents scale advantages of single national standard, creates regulatory uncertainty. Moderate experienced extraction — significant agency but also coordination burden.
constraint_indexing:constraint_classification(sotu_1976_ford_federalism_devolution, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: UNIFORM STANDARD ADVOCATES (SCAFFOLD) — Labor unions, environmental groups, civil rights organizations perceive devolution as temporary setback with eventual reversal mechanism. They see this constraint as provisional: devolution distributes authority until catastrophic failures (environmental races to bottom, civil rights violations, welfare races to bottom) generate political pressure for federal re-intervention. Sunset mechanism operates via crisis: when fragmentation produces unacceptable outcomes, coalition pressure re-centralizes. Moderate extraction because advocates perceive agency (organizing political coalition) and a visible exit path (crisis → federal re-intervention). Scaffold classification captures the provisional nature.
constraint_indexing:constraint_classification(sotu_1976_ford_federalism_devolution, scaffold,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER—STRUCTURAL FEDERALISM (MOUNTAIN) — From civilizational scale, devolution expresses fundamental structural tension in federal systems: How do you coordinate national-scale problems with local preference heterogeneity? Federalism itself emerges as an inherent feature of any large, diverse polity — the tension between centralization (efficiency, uniform standards) and decentralization (responsiveness, subsidiarity) is irreducible. This perspective risks naturalizing what is actually a contested institutional choice. However, the structural data (beneficiaries, victims, enforcement) reveals this as a false summit: devolution benefits specific actors (high-capacity jurisdictions, business) at cost to others (low-capacity jurisdictions, marginalized). The mountain framing ('federalism is fundamental') masks this asymmetry.
constraint_indexing:constraint_classification(sotu_1976_ford_federalism_devolution, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1976_ford_federalism_devolution_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1976_ford_federalism_devolution, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1976_ford_federalism_devolution, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1976_ford_federalism_devolution, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1976_ford_federalism_devolution, TR),
    TR >= 0.70.

:- end_tests(sotu_1976_ford_federalism_devolution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint produces measurable burden shift from federal to state/local level. Low-capacity jurisdictions cannot replicate service provision; federal resource withdrawal forces either local revenue increase (regressive taxation) or service reduction (extraction via deprivation). Marginalized populations lose federal civil rights protections; state-level enforcement is variable and often weaker. The extraction is not maximal (0.66+) because high-capacity jurisdictions genuinely benefit, creating coordination value that offsets some of the asymmetric burden. The constraint rises from 0.38 (pre-devolution baseline, some federal extraction via bureaucratic overhead) to 0.52 (post-devolution, extraction shifted from federal to local/individual level). Suppression (0.48): Moderate. Geographic immobility, political powerlessness in low-capacity jurisdictions, and capacity constraints suppress alternatives to federal devolution. But suppression is not absolute — advocates organize political coalitions, federal courts maintain some civil rights jurisdiction, and fiscal crises can drive federal reversion. Theater ratio (0.58): Moderate-high. Federal government maintains theatric responsibility for national outcomes (employment, growth, poverty reduction) while ceding policy authority. The gap between claimed responsibility and actual authority is the theater signature. Federal agencies continue performance reporting and policy pronouncements despite reduced actual control, creating performative asymmetry.
 *
 * PERSPECTIVAL GAP:
 *   This constraint generates maximum perspectival divergence from a unified structural base. High-capacity state leaders genuinely perceive coordination benefit (rope classification) — devolution solves the federal overhead problem. Federal bureaucracy sees its own degradation (piton) — maintained through inertia despite reduced authority. Low-capacity residents and marginalized populations see pure extraction (snare) — no beneficiary side, only burden shift. Organized business sees mixed (tangled rope) — arbitrage opportunity but compliance complexity. Uniform standard advocates see provisional burden (scaffold) — expect federal reversion via crisis. The analytical observer risks mountain classification (federalism is inherent to large polities) but the structural data reveals this as false summit: beneficiaries (high-capacity leaders) and extraction mechanisms (unfunded mandates, civil rights erosion) are identifiable, making the mountain a naturalization of political choice. The perspectival gaps are not measurement noise — they are diagnostic signals of the constraint's extractive structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from the structural position of each agent relative to extraction flow. High-capacity state leaders benefit (d ≈ 0.15, low extraction experienced); low-capacity residents bear costs (d ≈ 0.90, high extraction experienced). Marginalized populations dependent on federal protection face maximum structural vulnerability (d ≈ 0.95). Organized business experiences moderate extraction from compliance fragmentation but benefits from regulatory arbitrage (d ≈ 0.55). Federal bureaucracy loses authority but maintains formal responsibility (d ≈ 0.60, institutional constraints prevent full exit). The constraint's overall χ (effective extractiveness) is modulated by f(d) for each perspective and by spatial scope σ(S) at national level (σ=1.0). The perspectival gaps reflect these directionality differences: high-capacity leaders see rope (d low → negative χ); powerless trapped residents see snare (d high → high χ). The tangled_rope classification emerges from the coexistence of genuine coordination benefit (beneficiary side) and significant asymmetric extraction (victim side) within a single constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy here resolves through structural decomposition: devolution is not a single constraint but a constraint family operating across jurisdictional capacity tiers. High-capacity jurisdictions experience genuine coordination (rope); low-capacity jurisdictions experience extraction (snare); mid-capacity jurisdictions experience mixed (tangled_rope). The unified story captures the federal-level mechanism as tangled_rope because it genuinely offers coordination benefit (reducing federal overhead, enabling regional policy tailoring) while simultaneously imposing asymmetric extraction (unfunded mandate burden shift, civil rights protection erosion). This is not a mislabeling — tangled_rope is the analytically correct classification at the system level. The divergent perspectives are not contradictions; they are accurate readings from different structural positions. The false-summit risk is at the analytical/civilizational perspective: treating federalism as a natural law (mountain) rather than a contingent institutional choice whose extractive consequences are measurable and preventable through federal investment, equalization grants, and civil rights enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unfunded_mandate_severity,
    'Will devolution of authority without commensurate federal budget transfer constitute unfunded mandates that lower-capacity jurisdictions cannot absorb?',
    'Fiscal impact analysis: track federal budget reductions vs. state/local revenue capacity across jurisdictions. Measure program discontinuation vs. local provision costs.',
    'If unfunded: snare classification confirmed for low-capacity jurisdictions. If funded: tangled_rope classification (mixed costs/benefits) becomes more likely. If partial: varies by jurisdiction — true constraint family required.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unfunded_mandate_severity, empirical, 'Whether devolution transfers are genuinely funded or constitute unfunded mandate burden shift').

omega_variable(
    regulatory_race_to_bottom_mechanism,
    'Does devolution produce interstate regulatory competition that systematically advantages business interests over worker/environmental protection?',
    'Time-series analysis of state environmental, labor, and tax policy changes post-devolution. Cross-state correlation of race-to-bottom dynamics. Causal inference: does devolution announcement correlate with regulatory relaxation independent of other factors?',
    'If race-to-bottom occurs: snare classification for marginalized populations confirmed. If competition produces innovation (learning advantage): rope classification for some jurisdictions becomes stronger. If mixed: constraint family with separate stories for different policy domains required.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_race_to_bottom_mechanism, empirical, 'Whether devolution triggers regulatory races to bottom or produces beneficial policy competition').

omega_variable(
    federal_reversion_mechanism,
    'Does crisis-driven federal re-intervention represent a genuine sunset mechanism (scaffold) or a false stability expectation?',
    'Historical analysis: 1980-2026 pattern of federal-state authority shifts. Measure speed and magnitude of federal reversion following policy failures. Track political coalition durability and revisit likelihood.',
    'If federal reversion is reliable: scaffold classification for advocates is accurate. If reversion is slow/incomplete: scaffold is aspirational; actual constraint is tangled_rope or snare. If reversion never occurs: constraint ossifies into piton or degrades into stratified snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_reversion_mechanism, empirical, 'Whether federal reversion via crisis represents a functional sunset mechanism').

omega_variable(
    capacity_heterogeneity_structural,
    'Is the gap between high-capacity and low-capacity jurisdictions a structural feature of federalism (immutable) or a contingent outcome of prior fiscal/institutional choices (revisable)?',
    'Comparative federalism analysis: do capacity gaps exist in other federal systems (Canada, Australia, Germany)? Are gaps stable or dynamic? What institutional mechanisms (equalization grants, federal redistribution) affect capacity gaps?',
    'If structural/immutable: mountain perspective becomes more credible. If contingent: devolution without capacity-building is extractive choice, not natural law. If revisable: tangled_rope with engineering path (federal investment) becomes visible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(capacity_heterogeneity_structural, conceptual, 'Whether capacity heterogeneity across jurisdictions is structural or contingent').

omega_variable(
    civil_rights_protection_mechanism,
    'Will federal civil rights frameworks persist as effective constraints on state authority, or does devolution hollow federal protections through sub-federal nullification?',
    'Empirical tracking: do states violate federal civil rights mandates post-devolution? Do federal courts enforce protections? What is enforcement lag? Track disparities in civil rights protection across jurisdictions.',
    'If federal framework holds: snare classification for marginalized populations becomes less severe (residual federal floor). If nullification occurs: snare classification confirmed with maximum extraction. If varies by jurisdiction: constraint family required with separate stories for different institutional contexts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civil_rights_protection_mechanism, empirical, 'Whether federal civil rights protections persist as effective constraints under devolution').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1976_ford_federalism_devolution, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fed_dev_tr_t0, sotu_1976_ford_federalism_devolution, theater_ratio, 0, 0.42).
narrative_ontology:measurement(fed_dev_tr_t5, sotu_1976_ford_federalism_devolution, theater_ratio, 5, 0.52).
narrative_ontology:measurement(fed_dev_tr_t10, sotu_1976_ford_federalism_devolution, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(fed_dev_be_t0, sotu_1976_ford_federalism_devolution, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(fed_dev_be_t5, sotu_1976_ford_federalism_devolution, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(fed_dev_be_t10, sotu_1976_ford_federalism_devolution, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1976_ford_federalism_devolution, enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_1976_ford_federalism_devolution, unfunded_mandate_accumulation).
narrative_ontology:affects_constraint(sotu_1976_ford_federalism_devolution, civil_rights_protection_fragmentation).
narrative_ontology:affects_constraint(sotu_1976_ford_federalism_devolution, regulatory_race_to_bottom).
narrative_ontology:affects_constraint(sotu_1976_ford_federalism_devolution, federal_capacity_centralization).

% DUAL FORMULATION NOTE:
% Federalism devolution is upstream of multiple domain-specific extraction constraints. Unfunded mandates, civil rights fragmentation, and regulatory races to bottom all emerge as downstream consequences of the authority transfer mechanism. The upstream constraint (this story) captures the general structural devolution; downstream stories capture specific sectoral implementations (environmental, labor, welfare).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1976_ford_federalism_devolution, powerless, 0.9).
constraint_indexing:directionality_override(sotu_1976_ford_federalism_devolution, moderate, 0.7).
constraint_indexing:directionality_override(sotu_1976_ford_federalism_devolution, powerful, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
