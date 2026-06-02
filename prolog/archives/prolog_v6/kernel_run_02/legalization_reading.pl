% ============================================================================
% CONSTRAINT STORY: legalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legalization_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: legalization_reading
 *   human_readable: State Authority to Regulate Drug Markets as Legal Commerce with Quality/Access Controls
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   The legalization reading treats state authority to regulate drug markets
 *   as legal commerce with quality/access controls as a deliberate
 *   institutional substitution of regulatory capacity for prohibition. This
 *   reading assumes that demand for intoxicating substances is inelastic and
 *   geographically distributed; prohibition attempts to eliminate supply
 *   (futile in an open market) while creating secondary harms (incarceration,
 *   criminal market violence, adulterated products). Legalization accepts
 *   demand as fixed, redirects it into state-regulated channels, and uses
 *   regulatory authority (licensing, quality testing, taxation, distribution
 *   controls) as the control mechanism. The constraint is tangled_rope: it
 *   coordinates legitimate commerce (producers can operate legally, users
 *   access regulated supply, public health agencies gain market visibility)
 *   while extracting via regulatory rents (licensing fees, tax rates 30-50%
 *   in mature markets, price-based access barriers). The reading is one
 *   position in the contested 'substance_control_authority' kernel, competing
 *   with prohibition readings (which deny that demand is inelastic and assert
 *   that criminalization deters at the margin) and harm-reduction readings
 *   (which accept demand as inelastic but reject both prohibition and full
 *   legalization in favor of supervised consumption, treatment expansion, and
 *   decriminalization without retail markets). The legalization reading's
 *   distinctive normative claim is that **state-regulated commercial markets
 *   are the most effective mechanism for protecting users and the public
 *   while collecting revenue for harm mitigation**. This claim coexists with
 *   prohibition and harm-reduction readings — parties genuinely disagree
 *   about whether regulation can prevent capture, whether legalization
 *   increases use volume harmfully, and whether revenue gains offset public
 *   health costs. The temporal measurements show extractiveness rising as
 *   regulatory capacity matures (t0=0.15 prelegalization, t10=0.42 mature +
 *   capture risk), while theater drops sharply (t0=0.65 prohibition's
 *   performative enforcement, t5=0.35 legalization's functional transparency)
 *   and then drifts slightly upward (t10=0.38) as compliance theater
 *   accumulates.
 *
 * KEY AGENTS:
 *   - Legalized Users: Powerless/mobile agent gaining exit from criminal markets and incarceration risk. Primary beneficiary of coordination function.
 *   - Price-Sensitive Populations: Moderate/constrained agent bearing regressive tax incidence embedded in retail prices; face access barriers similar to illegal markets.
 *   - Illegal Market Participants: Powerless/trapped agent — street dealers and black-market suppliers face elimination without alternative livelihoods. Primary victim class created by legalization.
 *   - Regulated Commercial Operators: Institutional/arbitrage agent gaining market access and legitimacy but facing regulatory extraction (fees, taxes, compliance costs). Mixed beneficiary-victim.
 *   - Public Health Authority: Institutional/arbitrage agent designing and enforcing the constraint; primary beneficiary of revenue and market visibility.
 *   - Harm Reduction Advocacy Coalition: Organized/constrained agent achieving decriminalization goal but vulnerable to regulatory capture by operators. Secondary beneficiary with extraction risk.
 *   - International Prohibition Regime: Powerful/arbitrage institutional actor maintaining formal UN treaty commitments while losing enforcement capacity. Degraded institutional inertia (piton).
 *   - Analytical Observer: Civilizational/analytical position risking false-summit naturalization of demand inelasticity as a law of nature rather than a policy choice.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legalization_reading, 0.38).
domain_priors:suppression_score(legalization_reading, 0.45).
domain_priors:theater_ratio(legalization_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legalization_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(legalization_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(legalization_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legalization_reading, tangled_rope).
narrative_ontology:human_readable(legalization_reading, "State Authority to Regulate Drug Markets as Legal Commerce with Quality/Access Controls").
narrative_ontology:topic_domain(legalization_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(legalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legalization_reading, '2edba4a6-2555-4db2-b395-7b311d4340a5').
narrative_ontology:cs_created_at('2edba4a6-2555-4db2-b395-7b311d4340a5', '').
narrative_ontology:cs_kernel_codification('2edba4a6-2555-4db2-b395-7b311d4340a5', formalized).
narrative_ontology:cs_authority_grounding('2edba4a6-2555-4db2-b395-7b311d4340a5', extraction).
narrative_ontology:cs_interpretation_layer_present('2edba4a6-2555-4db2-b395-7b311d4340a5').
narrative_ontology:cs_kernel_id(legalization_reading, substance_control_authority).
narrative_ontology:cs_reading_relation('2edba4a6-2555-4db2-b395-7b311d4340a5', prohibition_reading, coexists_with).
narrative_ontology:cs_reading_relation('2edba4a6-2555-4db2-b395-7b311d4340a5', harm_reduction_reading, coexists_with).
narrative_ontology:cs_axiom('2edba4a6-2555-4db2-b395-7b311d4340a5', foundational, demand_inelastic_to_policy).
narrative_ontology:cs_axiom_status(demand_inelastic_to_policy, holdable).
narrative_ontology:cs_axiom_grounding('2edba4a6-2555-4db2-b395-7b311d4340a5', demand_inelastic_to_policy, empirically_contingent).
narrative_ontology:cs_axiom('2edba4a6-2555-4db2-b395-7b311d4340a5', foundational, regulatory_capacity_superior_to_prohibition).
narrative_ontology:cs_axiom_status(regulatory_capacity_superior_to_prohibition, holdable).
narrative_ontology:cs_axiom_grounding('2edba4a6-2555-4db2-b395-7b311d4340a5', regulatory_capacity_superior_to_prohibition, empirically_contingent).
narrative_ontology:cs_reference_frame('2edba4a6-2555-4db2-b395-7b311d4340a5', regulated_commercial_market_mechanism).
narrative_ontology:cs_drift_state('2edba4a6-2555-4db2-b395-7b311d4340a5', contemporary_capture_era, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legalization_reading, regulated_commercial_operators).
narrative_ontology:constraint_beneficiary(legalization_reading, state_revenue_collection).
narrative_ontology:constraint_beneficiary(legalization_reading, public_health_agencies).
narrative_ontology:constraint_victim(legalization_reading, illegal_market_participants).
narrative_ontology:constraint_victim(legalization_reading, regulatory_capture_risk).
narrative_ontology:constraint_victim(legalization_reading, access_inequality_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LEGALIZED USER (ROPE) — Users gain access to regulated supply, eliminating criminal market dependency and incarceration risk. Exit options shift from trapped to mobile as legal purchase becomes possible. The constraint now functions as pure coordination (accessing regulated supply with quality assurance and pricing transparency). Zero-to-low extraction because the primary value (safe access) aligns with the regulator's goal. This user experiences the legalization framework as enabling, not constraining.
constraint_indexing:constraint_classification(legalization_reading, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 2: PRICE-SENSITIVE POPULATIONS (TANGLED ROPE) — Low-income users bear disproportionate incidence of regulatory taxes and licensing fees embedded in retail prices. The constraint coordinates legitimate commerce but extracts via price mechanisms and restricted availability (desert pharmacies, closing hours, geographic concentration). Constrained exit: cannot avoid the regulatory regime if they want legal supply, but can also access remaining gray/illegal markets if prices exceed tolerance. Mixed coordination and extraction — the regime solves the public health problem of unregulated supply while imposing regressive costs.
constraint_indexing:constraint_classification(legalization_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ILLEGAL MARKET PARTICIPANTS (SNARE) — Street-level dealers, informal suppliers, and black-market networks face elimination (or dramatic revenue reduction) without alternative livelihoods. No exit: cannot transition into regulated commerce without capital, licensing, and regulatory compliance. Trapped in a shrinking informal sector with incarceration risk if they persist. High extraction, high suppression. The legalization framework solves the user problem but creates a new victim class — those whose livelihoods depend on illegality.
constraint_indexing:constraint_classification(legalization_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 4: REGULATED COMMERCIAL OPERATORS (TANGLED ROPE) — Licensed producers, distributors, and retailers benefit from market access, volume growth, and legitimacy, but face regulatory extraction via licensing fees, tax rates, inventory controls, and mandatory quality testing. Arbitrage exit: can move operations, adjust supply, lobby for regulatory relief. Coordination function: operators establish reliability, quality control, and transparent pricing. Extraction function: states capture significant margin (30-50% marginal tax rates typical in legalized markets). Mixed function — the constraint both enables commercial viability and extracts rents.
constraint_indexing:constraint_classification(legalization_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: PUBLIC HEALTH AUTHORITY (ROPE) — Regulatory agencies gain revenue streams, visibility into consumption, and legitimate leverage to enforce quality/safety standards. Arbitrage options: reallocate budget, adjust enforcement intensity, renegotiate with operators. Pure coordination from this perspective: the constraint solves the problem of hidden markets and enables public health surveillance. No significant extraction experienced because the beneficiary (state) designs and enforces the constraint. Theater low — health monitoring is functional, not performative.
constraint_indexing:constraint_classification(legalization_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: HARM REDUCTION ADVOCACY COALITION (TANGLED ROPE) — Advocates for non-punitive drug policy achieve major goal (decriminalization, regulated access), but face regulatory capture risk: operators lobby for reduced testing, higher prices, restricted THC potency (alcohol analogue), or geographic concentration that replicates inequality. Constrained exit: cannot unilaterally prevent capture; rely on political pressure. Coordination function: the regime enables evidence-based policy. Extraction function: regulatory authority may be captured by operators, reversing the public health gains. Moderate extraction reflecting the structural vulnerability to regulatory hijacking.
constraint_indexing:constraint_classification(legalization_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: INTERNATIONAL PROHIBITION REGIME (PITON) — The UN Single Convention on Narcotic Drugs (1961) and related treaties formalize global prohibition as the baseline. Legalization reading treats these as degraded/inert constraints: they persist formally but have lost enforcement capacity (states ignore or withdraw). Theater high: signatories maintain rhetorical commitment while adopting legalization domestically (Uruguay, Canada, numerous US states). The international prohibition persists through institutional inertia and treaty mechanics, not because enforcement works. Piton classification: formalized but functionally obsolete, maintained through bureaucratic continuation rather than active suppression.
constraint_indexing:constraint_classification(legalization_reading, piton,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (MOUNTAIN / CONTESTED) — From a civilizational view, legalization faces an immutable law: demand for intoxication and altered states is a stable human preference that cannot be eliminated through policy. Prohibition attempts to eliminate demand; legalization accepts demand as fixed and redirects it into regulated channels. This reads as mountain: the constraint (demand) is unchangeable, so regulation is the only viable control mechanism. However, this perspective risks naturalizing a policy choice (accepting demand as fixed rather than contestable via prevention, addiction treatment, or cultural norms) as a law of nature. The engine's false summit detection may flag this as contingent institutional framing rather than natural necessity.
constraint_indexing:constraint_classification(legalization_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legalization_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(legalization_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(legalization_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(legalization_reading, TR),
    TR >= 0.70.

:- end_tests(legalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The legalization reading achieves significant extraction reduction compared to prohibition (which incarcerated millions, funded criminal enterprises, and produced adulterated supply). However, legalization does not eliminate extraction — it redirects it into regulatory rents. Taxes, licensing fees, and geographic/temporal controls on availability create price-based access barriers that disproportionately affect low-income users. The rise from t0 (0.15 prelegalization) to t10 (0.42) reflects two mechanisms: (1) regulatory maturation (initial uncertainty gives way to stable fee structures and tax rates), (2) regulatory capture risk (operators lobby for reduced competition, higher prices, restricted availability). The measured extractiveness at t10 (0.42) incorporates the assessment that capture is partially realized — some regulatory agencies show evidence of operator influence on licensing decisions and fee structures. Suppression (0.45): Moderate. Legal availability eliminates criminal market suppression (incarceration risk, supply uncertainty), but regulatory suppression replaces it (price barriers, geographic concentration, temporal restrictions on sales hours/days). The net suppression is lower than prohibition but still substantial for price-sensitive populations. Theater ratio (0.35 at t5, increasing to 0.38 at t10): Low-to-moderate. Legalization's primary functional advantage over prohibition is transparency — regulatory compliance is verifiable (testing labs, licensing registries, tax records), not performative. Prohibition's theater was high (0.65 at t0: enforcement raids, incarceration statistics, and seizures as metrics of success despite minimal impact on supply). Legalization's theater is low because success is measured by actual outcomes (use prevalence, quality metrics, revenue collection) rather than enforcement activity. The slight rise at t10 reflects compliance theater creeping in (mandatory testing that exceeds minimum safety standards, reporting requirements for compliance rather than detection).
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces one of the starkest perspectival gaps in the corpus. The legalized user (perspective 1) sees pure coordination (Rope): access to quality supply at known prices. The illegal market participant (perspective 3) sees pure extraction (Snare): their livelihoods are eliminated with no alternative. The public health authority (perspective 5) sees coordination (Rope): the constraint solves the surveillance problem. The regulated operator (perspective 4) sees tangled rope (coordination for access + extraction via taxes). The price-sensitive user (perspective 2) sees tangled rope (coordination for access + regressive extraction via price). The harm-reduction coalition (perspective 6) sees tangled rope (goal achieved + capture risk). The international prohibition regime (perspective 7) sees degraded institutional inertia (Piton). The analytical observer (perspective 8) risks seeing mountain (demand is inelastic natural law) but the false-summit detector should flag this as contingent policy framing. The perspectival gap is not noise — it reflects genuine structural disagreement about what the legalization framework accomplishes for different agents. The constraint **simultaneously solves the user problem, creates a victim class, enables state extraction, and risks regulatory capture**. All four phenomena are real.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's structural position within this constraint. Legalized users have d ≈ 0.15 (beneficiary with mobile exit; the constraint enables rather than restricts). Public health authority has d ≈ 0.05 (institutional beneficiary with arbitrage; the authority designs and benefits from the constraint). Regulated operators have d ≈ 0.45 (institutional actors both benefiting from market access and bearing extraction via regulatory rents; mixed position). Price-sensitive populations have d ≈ 0.65 (moderate agents bearing extraction via price barriers despite gaining access). Illegal market participants have d ≈ 0.95 (powerless agents trapped as their livelihoods are eliminated; maximum d). Harm reduction coalition has d ≈ 0.60 (organized agents with constrained exit; benefiting from decriminalization but vulnerable to capture). The sigmoid f(d) function translates these d values into experienced chi (effective extractiveness). Low-d beneficiaries experience negative chi (the constraint subsidizes them); high-d victims experience chi > 1.0 (the constraint extracts more than base ε suggests because their structural position amplifies extraction). The false-summit mountain perspective at d ≈ 0.72 (analytical observer seeing demand as immutable) produces the typical analytical f(d) ≈ 1.15, amplifying the mountain reading — but this is precisely why the mountain classification is flagged as contested. The reading's true structure is tangled rope, not mountain.
 *
 * MANDATROPHY ANALYSIS:
 *   The legalization reading resolves mandatrophy by explicitly acknowledging the constraint's hybrid function: it coordinates legitimate commerce (satisfies the Rope gate: low suppression of users' access to regulated supply) **and** extracts rents (satisfies the Snare/Tangled Rope gate: victims experience suppression via price and geographic barriers). The constraint is not 'coordination or extraction?' but 'coordination of what, extraction from whom?' For users, it is pure coordination (Rope perspective). For illegal market operators, it is pure extraction (Snare perspective). For operators and price-sensitive users, it is mixed (Tangled Rope perspective). The mandatrophy is dissolved by the indexical classification itself: the constraint's type is not a univocal property of the institution but a perspectival property that depends on the observer's structural position. The claimed_type (tangled_rope) captures the base truth: the constraint has genuine coordination function (regulating supply quality, enabling users to exit criminal markets) and genuine extraction function (regulatory rents, price barriers, capture risk). Neither function reduces to the other. Both are structural. This is exactly what tangled rope is designed to capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    use_volume_elasticity,
    'Does legal availability and price reduction increase aggregate use volume? By how much? For which subpopulations?',
    'Longitudinal comparison of use prevalence, consumption frequency, and per-capita quantity consumed before vs. after legalization; cohort analysis by age, income, and addiction severity',
    'If volume increase is substantial (>30%): public health framing shifts; the constraint may extract via health costs transferred to non-users. If minimal (<10%): the coordination framing is validated; legalization solves black-market problems without creating new harm. If differential by subpopulation (youth use ↑, adult use →): constraint has heterogeneous extraction profile requiring perspectival disaggregation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(use_volume_elasticity, empirical, 'Price elasticity of demand and use volume response to legalization').

omega_variable(
    regulatory_capture_trajectory,
    'Do legalized operators successfully capture regulators, resulting in weakened quality standards, higher prices, or reduced access (replicating illegal-market inequalities)?',
    'Longitudinal tracking of regulatory decisions, fee/tax levels, and compliance rates; comparative analysis of pre- vs. post-capture periods; interview data on regulator-operator relationship dynamics',
    'If capture occurs: legalization reading converts from tangled_rope to snare (the constraint becomes extraction mechanism disguised as public health policy). If capture is prevented: the rope perspective (public health authority) is validated and the constraint becomes stable coordination with extraction limited to legitimate tax incidence. The axis of vulnerability is the regulatory authority''s independence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_trajectory, empirical, 'Regulatory capture risk and preventability').

omega_variable(
    access_equality_achievability,
    'Can a legalized market deliver equitable geographic and price-point access, or does it structurally replicate the inequality of the illegal market (desert pharmacies, price-sensitive exit)?',
    'Geographic distribution analysis of legal dispensaries vs. population density and income; price comparison across income-segregated neighborhoods; simulation modeling of market equilibrium under various regulatory fee structures',
    'If access achieves equity: the constraint''s extraction is minimal and coordination function dominates. If access replicates inequality: the moderate agent perspective (constrained, bearing regressive tax incidence) becomes the dominant structural reality, and extractiveness climbs (price as coercive mechanism). This is the crux of whether legalization solves the public health problem or merely transfers the control mechanism from prohibition to price.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(access_equality_achievability, empirical, 'Whether legalized markets achieve equitable access').

omega_variable(
    reading_boundary_use_vs_distribution,
    'Does this reading''s scope include only regulation of end-user access, or does it extend to production/distribution supply chain control?',
    'Clarification of which regulatory layers are included: retail point-of-sale only vs. full upstream control (cultivation licensing, distributor oversight, chemical precursor regulation)',
    'If scope is retail only: constraint is moderate tangled_rope (users and operators regulate access; suppliers may remain criminal). If scope is full supply chain: constraint''s extractiveness increases (state captures entire value chain margins) and suppression of alternatives intensifies (supply-side barriers become total). This is a reading-boundary omega: different framings of ''legalization'' produce structurally different constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_boundary_use_vs_distribution, conceptual, 'Scope boundary: retail access only vs. full supply chain control').

omega_variable(
    kernel_reading_alternative_prohibition,
    'Is this reading (legalization) logically foreclosed by the alternative reading (strict prohibition), or do both remain live positions for different parties?',
    'Philosophical/logical analysis: do the foundational axioms of legalization (demand is inelastic; quality control is superior to black-market risk) and prohibition (drug use is categorically impermissible; criminalization deters at the margin) coexist within a single regulatory framework, or does acceptance of one axiom require rejection of the other?',
    'If foreclosed: the constraint is not truly reading-contingent; one position dominates the logic. If coexist: the constraint is genuinely kernel-dependent; different parties adopt different readings based on normative premises, not on empirical facts alone. This omega documents whether the ''substance_control_authority'' kernel is genuinely contested or whether legalization has become the dominant reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_alternative_prohibition, conceptual, 'Whether legalization and prohibition readings logically coexist or foreclose each other').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legalization_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legal_theater_t0_prohibition_era, legalization_reading, theater_ratio, 0, 0.65).
narrative_ontology:measurement(legal_theater_t2_implementation, legalization_reading, theater_ratio, 2, 0.45).
narrative_ontology:measurement(legal_theater_t5_stable, legalization_reading, theater_ratio, 5, 0.35).
narrative_ontology:measurement(legal_theater_t10_compliance_drift, legalization_reading, theater_ratio, 10, 0.38).

% Extraction over time
narrative_ontology:measurement(legal_extract_t0_prelegalization, legalization_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(legal_extract_t2_early_implementation, legalization_reading, base_extractiveness, 2, 0.28).
narrative_ontology:measurement(legal_extract_t5_mature_regulation, legalization_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(legal_extract_t10_capture_risk_realized, legalization_reading, base_extractiveness, 10, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legalization_reading, resource_allocation).
narrative_ontology:affects_constraint(legalization_reading, prohibition_reading).
narrative_ontology:affects_constraint(legalization_reading, harm_reduction_reading).
narrative_ontology:affects_constraint(legalization_reading, criminal_market_extraction).
narrative_ontology:affects_constraint(legalization_reading, regulatory_capture).

% DUAL FORMULATION NOTE:
% The legalization reading is the middle position in the substance_control_authority kernel family. Upstream: the prohibition reading (criminalization as the state's control mechanism). Downstream: the harm-reduction reading (decriminalization without full legalization). The three readings form a policy spectrum with different ε values and beneficiary/victim structures. Each is a distinct constraint because the control mechanism (criminal law vs. regulatory authority vs. treatment/decriminalization) produces structurally different extractiveness profiles. This story should be read in conjunction with prohibition_reading and harm_reduction_reading to understand the contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legalization_reading, organized, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
