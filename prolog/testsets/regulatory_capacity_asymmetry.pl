% ============================================================================
% CONSTRAINT STORY: regulatory_capacity_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_regulatory_capacity_asymmetry, []).

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
 *   constraint_id: regulatory_capacity_asymmetry
 *   human_readable: Regulatory Capacity Asymmetry Between Large Firms and Regulators
 *   domain: regulatory_economics/institutional_governance
 *
 * SUMMARY:
 *   Regulatory capacity asymmetry emerges when regulatory agencies lack
 *   sufficient resources or independence to enforce rules uniformly across
 *   firms of different sizes and sophistication. Large firms can afford
 *   specialized compliance teams, hire regulatory consultants, participate in
 *   rulemaking processes, and arbitrage across jurisdictions; small firms
 *   cannot. This creates a two-layer system: formal regulations apply
 *   universally, but effective compliance requirements differ dramatically by
 *   firm size. The constraint exhibits both genuine coordination function
 *   (regulations prevent market failures and externalities) and genuine
 *   extraction (large firms benefit from the asymmetry through reduced
 *   effective burden and reduced competition from entry-constrained small
 *   firms). Theater ratio is elevated (0.64) because regulatory compliance
 *   rituals—public comment periods, environmental reviews, safety
 *   certifications—are performed extensively even as enforcement capacity is
 *   too thin to ensure substantive compliance, especially for small violators
 *   that cannot afford litigation.
 *
 * KEY AGENTS:
 *   - Small Competitors: Primary victim (powerless/trapped) — cannot afford compliance infrastructure; regulatory barriers become effective barriers to entry
 *   - Large Regulated Firms: Primary beneficiary (powerful/arbitrage) — maintain compliance teams; shape regulatory interpretation; arbitrage across jurisdictions
 *   - Regulatory Agencies: Secondary institutional actor (institutional/arbitrage) — designed for coordination function but underfunded relative to regulated sector complexity; gatekeepers with career mobility to industry
 *   - Mid-Sized Firms: Secondary victim (moderate/constrained) — have some compliance capacity but higher cost burden than large firms; cannot exit sector
 *   - Public Interest Commons: Collective victim (powerless/trapped) — environmental protection, consumer safety, market competition all depend on effective enforcement; enforcement capacity concentrates on large-firm concerns
 *   - Regulatory System Itself: Institutional actor (institutional/constrained) — performs extensive theater (comment periods, reviews) but enforcement is too thin; persists through institutional inertia
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(regulatory_capacity_asymmetry, 0.52).
domain_priors:suppression_score(regulatory_capacity_asymmetry, 0.58).
domain_priors:theater_ratio(regulatory_capacity_asymmetry, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(regulatory_capacity_asymmetry, extractiveness, 0.52).
narrative_ontology:constraint_metric(regulatory_capacity_asymmetry, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(regulatory_capacity_asymmetry, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(regulatory_capacity_asymmetry, tangled_rope).
narrative_ontology:human_readable(regulatory_capacity_asymmetry, "Regulatory Capacity Asymmetry Between Large Firms and Regulators").
narrative_ontology:topic_domain(regulatory_capacity_asymmetry, "regulatory_economics/institutional_governance").

domain_priors:requires_active_enforcement(regulatory_capacity_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(regulatory_capacity_asymmetry, large_regulated_firms).
narrative_ontology:constraint_beneficiary(regulatory_capacity_asymmetry, regulatory_agencies).
narrative_ontology:constraint_victim(regulatory_capacity_asymmetry, small_competitors).
narrative_ontology:constraint_victim(regulatory_capacity_asymmetry, public_interest_beneficiaries).
narrative_ontology:constraint_victim(regulatory_capacity_asymmetry, regulatory_legitimacy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL COMPETITOR (SNARE) — Lacks resources to hire compliance expertise, engage in rulemaking comment processes, or litigate regulatory interpretations. Trapped by regulatory barriers to entry that large firms navigate easily. Experiences the constraint as pure extraction: compliance costs are fixed, so regulatory burden falls disproportionately on small firms.
constraint_indexing:constraint_classification(regulatory_capacity_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LARGE REGULATED FIRM (TANGLED ROPE) — Has sufficient scale to maintain compliance teams, shape regulatory interpretation through comment letters and lobbying, and arbitrage across jurisdictions. Experiences the constraint as mixed: genuine coordination function (firms need clear rules), but also asymmetric extraction (small competitors face higher burden). Benefits from the regulatory apparatus while bearing less of its cost.
constraint_indexing:constraint_classification(regulatory_capacity_asymmetry, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: REGULATORY AGENCY (ROPE) — Views regulation as coordination mechanism: establishing standards, preventing externalities, enabling market function. Has arbitrage options (regulatory capture provides career pathways to industry; budget authority over enforcement). Experiences the constraint as pure coordination — the asymmetry is a resource problem (underfunding), not an extraction mechanism.
constraint_indexing:constraint_classification(regulatory_capacity_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MID-SIZED FIRM (TANGLED_ROPE) — Has some compliance capacity but less than giants. Can engage regulatory process but at higher cost relative to revenue. Experiences mixed dynamics: genuine coordination benefit (regulations prevent predatory competitors), but extraction cost that large firms don't bear. Constrained by the burden but cannot exit the regulated sector.
constraint_indexing:constraint_classification(regulatory_capacity_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PUBLIC INTEREST COMMONS (SNARE) — Environmental protection, consumer safety, market competition all depend on effective enforcement, but enforcement capacity concentrates on large firms' concerns. The commons is trapped in the asymmetry: cannot exit, cannot organize, bears the full cost of regulatory capture without voice. The constraint prevents the public good from being delivered.
constraint_indexing:constraint_classification(regulatory_capacity_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 6: REGULATORY SYSTEM ITSELF (PITON) — The formal regulatory apparatus persists through institutional inertia despite degraded function. Theater ratio is high: compliance rituals (comment periods, environmental reviews, public hearings) are performed extensively but enforcement capacity is too thin to ensure substantive compliance. The system is maintained because formal alternatives haven't replaced it, not because it delivers the intended regulatory function.
constraint_indexing:constraint_classification(regulatory_capacity_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED_ROPE) — From civilizational scope, regulatory capacity asymmetry is a genuine coordination mechanism with embedded extraction. Firms need clear rules (coordination function); the asymmetry arises because enforcement capacity is insufficient and concentrates on large-firm interactions (extraction mechanism). The constraint is not a natural law or pure extraction, but a structurally hybrid system with both real functions and real asymmetries.
constraint_indexing:constraint_classification(regulatory_capacity_asymmetry, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(regulatory_capacity_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(regulatory_capacity_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(regulatory_capacity_asymmetry, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(regulatory_capacity_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(regulatory_capacity_asymmetry, TR),
    TR >= 0.70.

:- end_tests(regulatory_capacity_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint exhibits genuine extraction—large firms benefit from reduced effective burden while small competitors face inflated compliance costs relative to firm size. However, extractiveness is not maximal because the coordination function is real: regulations do prevent externalities and enable market function. The extraction is a side effect of asymmetric capacity, not the primary function. Suppression (0.58): Moderate-high. Small firms face significant barriers to exit—regulatory compliance is mandatory in most sectors—but not insurmountable. Some firms do exit regulated sectors, and entrepreneurship persists despite regulatory burden. Barriers are real and external but not absolute. Theater ratio (0.64): Elevated and rising. Regulatory compliance rituals are performed extensively (comment periods, safety reviews, environmental assessments) but enforcement capacity is thin relative to the number of regulated entities. The rise over the measurement interval reflects growing complexity of regulated sectors (pharmaceuticals, environmental protection, financial services) without corresponding increases in enforcement staffing. At time zero, enforcement could reasonably keep up; by time 20, formal compliance theater exceeds substantive capacity.
 *
 * PERSPECTIVAL GAP:
 *   The gap between large-firm and small-firm perspectives is the diagnostic core. Both face the same regulations, but their experienced extractiveness (chi) differs dramatically because exit options differ. The large firm's arbitrage option (can shift compliance strategy across jurisdictions, can afford regulatory expertise to negotiate interpretations, can exit if burden becomes unacceptable) reduces their experienced extraction even though they benefit from asymmetry. The small firm's trapped option (must comply with same rules at higher proportional cost, cannot afford expertise to negotiate, cannot exit without abandoning market opportunity) increases their experienced extraction even though the formal rules apply uniformly. The gap reveals that chi depends not just on the rule's design but on the agent's structural position relative to it.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural position relative to the asymmetry. Large firms with arbitrage options derive d from beneficiary status + arbitrage exit: they can walk away or shift jurisdiction, so their structural position is low-cost. Small firms with no exit option derive d from victim status + trapped exit: they bear compliance costs without escape. Regulatory agencies derive d from their coordination function—they see the system as solving problems, not creating extraction—but their arbitrage option (career mobility to industry) slightly raises d from the canonical institutional baseline, reflecting partial capture. The mid-sized firm's moderate power derives from having some compliance capacity (reducing constraint experience) but still being trapped in the sector (cannot exit). The public commons has no exit option and no organized voice, so d is maximal—the abstract collective cannot negotiate.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by distinguishing the coordination function (genuine) from the extraction mechanism (genuine but incidental). Regulatory capacity asymmetry is NOT pure extraction (Snare universally) because the regulations do coordinate real problems: preventing externalities, enabling market function, protecting consumers and the environment. But it is NOT pure coordination (Rope universally) because the enforcement asymmetry creates private benefit for large firms—they benefit from the rules themselves AND from the asymmetry in enforcement burden. The tangled rope classification at the analytical level captures this: both functions are real, and both are structurally necessary given the constraint's origin (regulatory agencies are genuinely underfunded relative to sector complexity). The constraint could be simplified to Rope if enforcement capacity were sufficient to apply rules uniformly, or degraded to Snare if the asymmetry were intentional capture rather than incidental triage. Current state: hybrid, requiring active enforcement to maintain the coordination function while the extraction mechanism persists as a structural artifact of capacity limits.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capacity_vs_capture_causality,
    'Is the asymmetry primarily caused by regulatory underfunding (capacity problem) or by regulatory capture (extraction mechanism)?',
    'Comparative analysis of underfunded but independent regulatory systems vs. well-funded but captured ones. Historical track record of enforcement actions against large firms vs. small firms controlling for violation severity.',
    'If primarily capacity: classify perspectives toward Rope (coordination with resource constraint). If primarily capture: classify toward Snare (intentional extraction mechanism). This determines whether asymmetry is fixable (additional funding) or structural (misaligned incentives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_vs_capture_causality, empirical, 'Whether asymmetry is caused by resource constraints or regulatory capture').

omega_variable(
    small_firm_exit_viability,
    'Can small firms realistically exit the regulated sector, or is regulatory compliance a true barrier to entry with no exit option?',
    'Analysis of startup formation rates pre-and post-regulation; comparison of compliance cost as percentage of revenue across firm sizes; survey of firms citing regulatory burden as exit reason.',
    'If exit is possible: small firms should be reclassified from ''trapped'' to ''constrained'' (high cost, not impossible). If exit is impossible: confirms mountain-like immutability of barrier. Affects whether powerless agent perspective should use ''trapped'' or ''constrained''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(small_firm_exit_viability, empirical, 'Whether exit from regulated sector is viable for small firms').

omega_variable(
    enforcement_concentration_intentionality,
    'Is enforcement concentration on large firms intentional (large-firm benefit maximizes regulatory effectiveness, small firms inherently lower-priority) or unintentional (capacity constraints create triage bias)?',
    'Analysis of regulatory budgets and staff allocation across firm-size cohorts; interviews with enforcement agency leadership on priority-setting; comparison of fine-to-violation ratios across firm sizes.',
    'If intentional: enforcement system is designed to enable regulatory capture (Snare strengthened). If unintentional: reflects genuine capacity limits (Rope with resource constraint). Determines whether the constraint is structural or incidental.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_concentration_intentionality, empirical, 'Whether enforcement concentration is intentional or capacity-driven').

omega_variable(
    regulatory_arbitrage_scope,
    'How much of large-firm advantage comes from arbitraging across jurisdictions vs. gaining preferential treatment within jurisdictions?',
    'Analysis of firm behavior: percentage of regulatory savings from multi-jurisdictional arbitrage vs. negotiated compliance standards within single jurisdiction. Case studies of large firms'' regulatory strategy.',
    'If arbitrage dominates: problem is global institutional fragmentation (Rope at global scope). If preferential treatment dominates: problem is capture (Tangled Rope at national scope). Scope assignment depends on this decomposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_arbitrage_scope, empirical, 'Whether large-firm advantage derives from arbitrage or preferential treatment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(regulatory_capacity_asymmetry, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(regcap_tr_t0, regulatory_capacity_asymmetry, theater_ratio, 0, 0.48).
narrative_ontology:measurement(regcap_tr_t10, regulatory_capacity_asymmetry, theater_ratio, 10, 0.58).
narrative_ontology:measurement(regcap_tr_t20, regulatory_capacity_asymmetry, theater_ratio, 20, 0.64).
narrative_ontology:measurement(regcap_tr_t5, regulatory_capacity_asymmetry, theater_ratio, 5, 0.53).

% Extraction over time
narrative_ontology:measurement(regcap_be_t0, regulatory_capacity_asymmetry, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(regcap_be_t10, regulatory_capacity_asymmetry, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(regcap_be_t20, regulatory_capacity_asymmetry, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(regcap_be_t5, regulatory_capacity_asymmetry, base_extractiveness, 5, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(regulatory_capacity_asymmetry, enforcement_mechanism).
narrative_ontology:affects_constraint(regulatory_capacity_asymmetry, regulatory_capture_in_finance).
narrative_ontology:affects_constraint(regulatory_capacity_asymmetry, small_business_compliance_burden).
narrative_ontology:affects_constraint(regulatory_capacity_asymmetry, enforcement_agency_budget_constraint).

% DUAL FORMULATION NOTE:
% Regulatory capacity asymmetry is downstream of enforcement agency underfunding (budget constraint) and upstream of specific sector-level capture dynamics (e.g., financial regulation, pharmaceutical approval). The constraint represents a structural pattern that manifests across multiple regulatory domains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(regulatory_capacity_asymmetry, institutional, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
