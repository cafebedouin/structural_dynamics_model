% ============================================================================
% CONSTRAINT STORY: un_high_seas_treaty_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_un_high_seas_treaty_2026, []).

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
 *   constraint_id: un_high_seas_treaty_2026
 *   human_readable: UN High Seas Treaty for Marine Biodiversity (BBNJ)
 *   domain: geopolitical/environmental
 *
 * SUMMARY:
 *   The UN High Seas Treaty (BBNJ), effective in 2026, establishes a legal
 *   framework for marine biodiversity protection in areas beyond national
 *   jurisdiction. The treaty creates a complex institutional hybrid: it
 *   functions as genuine coordination (enabling collective action on high
 *   seas management, formalizing benefit-sharing for marine genetic
 *   resources) AND as an extraction mechanism (imposing conservation
 *   constraints on fishing communities, formalizing biotech access
 *   advantages, concentrating enforcement capacity in powerful states). The
 *   constraint exhibits all six DR types from different perspectives,
 *   revealing deep tensions between conservation imperatives, development
 *   equity, and state sovereignty. The theater ratio (0.65) reflects
 *   substantial performative content: many treaty commitments (50% MPAs by
 *   2050, genetic benefit-sharing targets) rely on weak enforcement
 *   mechanisms, reliance on self-reporting, and lack of independent
 *   verification capacity. Meanwhile, the constraint's extractiveness (0.58)
 *   derives from asymmetric impacts: industrial fishing fleets bear real
 *   costs (fishing ground restrictions), while pharmaceutical corporations
 *   gain formalized access to genetic resources. The core structural
 *   asymmetry: costs fall on fishing communities (often in developing states,
 *   often indigenous), while benefits accrue to already-wealthy biotech
 *   sectors and high-capacity coastal states. The constraint is tangled_rope
 *   at the aggregate level because it genuinely coordinates high seas
 *   governance (rope function) while simultaneously extracting from those
 *   least able to influence its terms (snare function for trapped victims).
 *
 * KEY AGENTS:
 *   - Small-Scale Fishing Communities: Primary victims (powerless/trapped) — subsistence and artisanal fishers bearing conservation costs without compensation or representation
 *   - Indigenous Pacific Island Nations: Primary victims (powerless/trapped) — geographically dependent on high seas resources, no exit options, food security threatened
 *   - Mid-Tier Fishing Nations: Mixed actor (organized/constrained) — industrial fishing states experiencing compliance costs and benefit-sharing obligations, but retaining some veto power and exemption options
 *   - Coastal Developing States (Environmental Coalition): Primary beneficiaries (institutional/arbitrage) — gain access to genetic resources, development financing, technology transfer; can arbitrage between conservation and extraction
 *   - Pharmaceutical and Biotech Corporations: Beneficiaries (institutional/arbitrage) — formalized access to marine genetic resources, benefit-sharing fund, monopoly protections; maintain multiple exit options
 *   - Major Fishing Nations (EU, China, Japan): Powerful mixed actors (powerful/mobile) — experience both coordination (reduced fleet conflicts) and extraction (fishing ground restrictions); can arbitrage through compliance variation or fleet relocation
 *   - International Environmental Governance Infrastructure: Scaffold institution (organized/constrained) — BBNJ secretariat and capacity-building mechanisms designed with 5-10 year review windows and regional maturation targets
 *   - Pre-Existing Ocean Institutions: Degraded actors (institutional/arbitrage) — regional fisheries management organizations and IMO maintain theater presence but reduced decision-making authority; persist through stakeholder engagement rather than functional necessity
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing tragedy-of-commons dynamics as immutable rather than recognizing treaty as contingent political choice about enforcement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(un_high_seas_treaty_2026, 0.58).
domain_priors:suppression_score(un_high_seas_treaty_2026, 0.62).
domain_priors:theater_ratio(un_high_seas_treaty_2026, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(un_high_seas_treaty_2026, extractiveness, 0.58).
narrative_ontology:constraint_metric(un_high_seas_treaty_2026, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(un_high_seas_treaty_2026, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(un_high_seas_treaty_2026, tangled_rope).
narrative_ontology:human_readable(un_high_seas_treaty_2026, "UN High Seas Treaty for Marine Biodiversity (BBNJ)").
narrative_ontology:topic_domain(un_high_seas_treaty_2026, "geopolitical/environmental").

domain_priors:requires_active_enforcement(un_high_seas_treaty_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(un_high_seas_treaty_2026, coastal_developing_states).
narrative_ontology:constraint_beneficiary(un_high_seas_treaty_2026, environmental_advocacy_coalitions).
narrative_ontology:constraint_beneficiary(un_high_seas_treaty_2026, marine_research_institutions).
narrative_ontology:constraint_victim(un_high_seas_treaty_2026, industrial_fishing_fleets).
narrative_ontology:constraint_victim(un_high_seas_treaty_2026, pharmaceutical_prospectors).
narrative_ontology:constraint_victim(un_high_seas_treaty_2026, resource_extraction_companies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL-SCALE FISHING COMMUNITIES (SNARE) — Trapped within the constraint without meaningful representation in negotiation or compliance mechanisms. Cannot exit fishing as livelihood. Treaty's marine protected area zones restrict traditional fishing grounds with minimal transition support or alternative income pathways. Maximum extraction relative to power: communities bear costs of conservation while industrial fleets receive compensation or exemptions.
constraint_indexing:constraint_classification(un_high_seas_treaty_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INDIGENOUS PACIFIC ISLAND NATIONS (SNARE) — Structurally dependent on high seas fisheries for food security and economic survival. Trapped by geography and limited economic alternatives. Treaty's biodiversity protection zones directly reduce accessible fishing territory without adequate compensation mechanisms or co-management rights. No credible exit option; costs concentrated on subsistence users.
constraint_indexing:constraint_classification(un_high_seas_treaty_2026, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: MID-TIER FISHING NATIONS (TANGLED ROPE) — Industrial fishing states with moderate organizational capacity and some negotiating power. Constrained by treaty requirements (marine protected areas, environmental impact assessments, benefit-sharing obligations) but retain some exit options through delayed implementation, exemptions for domestic food security, and ability to block enforcement mechanisms. Experience mixed extraction (compliance costs) and coordination (access to genetic resource benefits, research partnerships). Asymmetric: benefits accrue to nations with pharmaceutical/biotech capacity to monetize marine genetic resources.
constraint_indexing:constraint_classification(un_high_seas_treaty_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: COASTAL DEVELOPING STATES (ENVIRONMENTAL COALITION) (ROPE) — Primary beneficiaries through marine genetic resource access, technology transfer, and capacity-building funds. High institutional capacity to navigate treaty mechanisms. Broad exit options: can leverage treaty for development financing, can arbitrage between conservation commitments and resource extraction claims. The treaty functions as coordination mechanism for these actors — enabling collective action on biodiversity while distributing benefits through benefit-sharing fund. Low effective extraction because beneficiary position is strong.
constraint_indexing:constraint_classification(un_high_seas_treaty_2026, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: PHARMACEUTICAL AND BIOTECH CORPORATIONS (ROPE) — Benefit from treaty's benefit-sharing fund (2% of net revenues from marine genetic resources) and from formalized access rights to high seas genetic material. Maintain arbitrage options: can operate in different jurisdictions, can pursue terrestrial alternatives, can negotiate bilateral access agreements outside treaty. The treaty functions as coordination mechanism providing legal clarity and monopoly protections for marine genetic prospecting. Low effective extraction because these actors can arbitrage away; they also benefit from the treaty's coordination function.
constraint_indexing:constraint_classification(un_high_seas_treaty_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: MAJOR FISHING NATIONS (TANGLED ROPE) — Powerful institutional actors with significant global fishing fleets and capacity to implement treaty requirements or circumvent them. Mobile: can relocate fishing operations, lobby for exemptions, fund enforcement selectively. Experience mixed coordination (marine spatial planning reduces conflicts between fleets) and extraction (marine protected areas reduce accessible fishing grounds by 10-30%, though with transition periods). Asymmetric: costs fall on fleet operators; benefits accrue to governments through environmental credibility and genetic resource access. Some actors function as beneficiaries (capacity to monetize marine genetics), others as victims (fishing ground restrictions).
constraint_indexing:constraint_classification(un_high_seas_treaty_2026, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: INTERNATIONAL ENVIRONMENTAL GOVERNANCE (SCAFFOLD) — Treaty functions as temporary coordination mechanism with explicit sunset logic embedded: benefit-sharing fund, marine protected areas, and capacity-building programs have 5-10 year review windows. The institutional scaffolding (BBNJ conference secretariat, capacity-building programs, technology transfer mechanisms) is designed to be dismantled or restructured as marine governance matures toward regional management. Suppression is moderate because the sunset is institutional — compliance mechanisms are weak, enforcement relies on shame and market pressure, transparency provisions allow non-compliance to be observed. Theater is high: many commitments are aspirational (50% ocean in MPAs by 2050, benefit-sharing targets) with weak accountability.
constraint_indexing:constraint_classification(un_high_seas_treaty_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: PRE-EXISTING OCEAN GOVERNANCE (PITON) — Regional fisheries management organizations (RFMOs), International Maritime Organization, and multilateral environmental agreements now operate in parallel with BBNJ. The older institutions have largely been superseded in mandate but remain operational through institutional inertia. Their continued existence performs a coordination function (stakeholder engagement, historical continuity) but their primary enforcement and decision-making authority has degraded. Theater ratio high: many meetings and committees produce reports with limited real-world impact; genuine decisions migrate upward to BBNJ secretariat. The institutions persist because they maintain stakeholder constituencies and haven't formally dissolved, not because they function at full capacity.
constraint_indexing:constraint_classification(un_high_seas_treaty_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational timescale, the tragedy of the commons in global fisheries and marine biodiversity is a structural inevitability: without enforcement mechanisms (which are impossible to implement globally), rational actors overexploit shared resources until collapse. The treaty might be viewed as an attempt to impose order on inherent resource dynamics, but the underlying constraint—the tragedy of commons in the absence of property rights—is immutable. This perspective risks naturalizing what is actually a contingent political choice (not enforcing property-like rights, not empowering enforcement mechanisms).
constraint_indexing:constraint_classification(un_high_seas_treaty_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(un_high_seas_treaty_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(un_high_seas_treaty_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(un_high_seas_treaty_2026, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(un_high_seas_treaty_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(un_high_seas_treaty_2026, TR),
    TR >= 0.70.

:- end_tests(un_high_seas_treaty_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The treaty generates asymmetric costs and benefits. Fishing communities lose access to fishing grounds (direct cost); pharmaceutical corporations gain formalized access to genetic resources (direct benefit); developing coastal states gain development financing but often lack capacity to capture benefits intended for their vulnerable populations. The extraction is structural and enforced through treaty mechanisms but not complete because: (1) enforcement capacity is weak (~3-7% detection rates), (2) fishing communities retain some grandfathered access and transition support (limited), (3) powerful fishing nations can negotiate exemptions. Base extractiveness has increased over the interval (0.48→0.58) as implementation mechanisms have matured and MPA zones have become operational. Suppression (0.62): Moderate-high. Significant barriers to exit include: geographic dependence of island nations on high seas resources, limited livelihood alternatives in coastal regions, institutional dominance of powerful fishing states in treaty governance, weak transition support for affected communities. However, suppression is not maximal because: some communities do have potential to relocate fishing effort or switch livelihoods (though costly), some major fishing nations retain flexibility through flag state enforcement discretion, and surveillance of treaty violations is incomplete (high suppression requires that exit be nearly impossible; imperfect enforcement reduces effective suppression). Theater ratio (0.65): Moderate-high. Performative elements include: aspirational MPA targets (50% by 2050, largely unverified), benefit-sharing fund distribution (2% of revenues, often untraceable to actual economic impact on vulnerable populations), capacity-building commitments (frequently underfunded), genetic resource tracking (relies on patent self-reporting and company compliance). However, theater is not maximal because some treaty provisions have measurable enforcement (fishing vessel monitoring systems, port state inspections in major ports) and real restrictions on access (MPAs do exclude fishing, even if imperfectly monitored).
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is extreme—spanning from Snare (powerless/trapped) to Rope (institutional/arbitrage). This gap reveals the treaty's fundamental structural conflict: it serves as genuine coordination mechanism for high-capacity actors (developing states benefit from genetic resource access and development financing; pharmaceutical corporations benefit from legal clarity; all major actors benefit from reduced fishing conflicts) while simultaneously extracting from those least able to exit or negotiate (small-scale fishing communities, island nations). The gap is not a measurement ambiguity—it reflects real structural asymmetries in power and exit options. This is exactly the territory where mandatrophy detection matters: the temptation is to call the treaty a 'coordination mechanism' (Rope) and miss the extraction from powerless agents (Snare), or conversely to focus on extraction dynamics and miss the genuine coordination benefits for developing states. The tangled_rope classification at the aggregate level captures both: coordination function is real, asymmetric extraction is real, both are structural features of how the treaty operates.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each perspective derives from the agent's structural position relative to the extraction and coordination flows. Small-scale fishing communities and indigenous island nations are trapped victims with no exit options: d ≈ 0.95 (near 1.0), f(d) ≈ 1.42, maximum experienced extraction. Mid-tier fishing nations are constrained victims with some veto power in governance: d ≈ 0.65 (constrained exit + mixed victim/beneficiary status), f(d) ≈ 1.00, moderate extraction. Coastal developing states are beneficiaries with arbitrage options (can leverage treaty for financing, can negotiate resource access): d ≈ 0.15 (beneficiary + arbitrage), f(d) ≈ -0.01, low or negative effective extraction. Pharmaceutical corporations are beneficiaries with broad arbitrage options (multiple jurisdictions, alternative research pathways): d ≈ 0.05 (strong beneficiary + arbitrage), f(d) ≈ -0.12, institutional-level extraction (extraction runs toward them). Major fishing nations are powerful with mobile exit options; their d depends on whether they are classified as beneficiary (coordination function) or victim (fishing restrictions): classified as mixed, d ≈ 0.50 (symmetric), f(d) ≈ 0.65, moderate extraction. Directive overrides are unnecessary because the structural data is unambiguous: the beneficiary-victim distinction is clear, exit options are differentiated by agent capacity and geographic circumstance.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RISK — COORDINATION DISGUISED AS EXTRACTION (OR VICE VERSA): The treaty's beneficiary coalition (coastal developing states + environmental advocates) frames BBNJ as pure coordination: 'We are solving the tragedy of the commons in high seas fisheries through collective governance.' The fishing industry frames it as extraction: 'Wealthy nations and environmentalists are imposing conservation costs on fishing communities.' The analytical truth: both frames capture real structure. The treaty IS solving a coordination problem (high seas overfishing, lack of governance mechanisms for shared resources). AND it IS imposing asymmetric extraction (conservation costs concentrate on fishing communities, benefits concentrate on biotech and finance sectors, developing states gain development money but don't control distribution to vulnerable populations). Tangled Rope classification resolves the mandatrophy by insisting: genuine coordination function (Rope element: collective action on high seas governance, formal benefit-sharing mechanism, reduced fishing conflicts between fleets), genuine asymmetric extraction (Snare elements: trapped victims in small-scale fishing, victims bear costs of MPA enforcement, beneficiaries gain genetic resource access), and active enforcement required (treaty implementation through BBNJ secretariat, national enforcement of MPA zones, genetic resource tracking systems—all enforceable, all depend on active institutional work). The theater ratio (0.65) tracks the gap between aspirational commitments (50% MPAs by 2050) and actual enforcement capacity (~5-7% violation detection). This gap is crucial to mandatrophy resolution: high theater indicates that some of the 'coordination benefit' is theater (aspirational), and some of the 'enforcement' is theater (weak). But enough real structure exists (satellite monitoring of fishing, genetic resource permits, MPA designations) that the constraint is not pure theater (Piton threshold: theater ≥ 0.70). Mandatrophy is resolved by acknowledging that the treaty is tangled_rope with scaffolding: it solves a real coordination problem while extracting from vulnerable populations, and the extraction is tolerated because the coordination benefit is substantial (preventing fisheries collapse) and because the treaty includes sunset provisions (designed with regional maturation timeline, with expected transition to mature regional governance).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_capacity_threshold,
    'What detection and enforcement capacity threshold distinguishes treaty coordination from theater compliance?',
    'Monitoring satellite data, port state inspection statistics, flag state prosecution rates; correlation between enforcement effort and actual compliance in marine protected areas and genetic resource tracking',
    'If enforcement capacity < 5% of violations detected: treaty functions as piton (institutional inertia with aspirational theater). If > 20%: begins functioning as genuine tangled_rope with real extraction costs. Current estimates ~3-7%.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_capacity_threshold, empirical, 'Detection and enforcement capacity threshold').

omega_variable(
    benefit_sharing_fund_sufficiency,
    'Do the benefit-sharing fund transfers (2% of marine genetic resource revenues) constitute genuine compensation for developing states or merely performative distribution?',
    'Longitudinal analysis of fund disbursement rates, allocation patterns, and economic impact on primary victim populations (small-scale fishing communities); comparison to actual extraction value in coastal developing state economies',
    'If fund transfers < 0.5% of actual resource extraction value: compensation is theater, classification stays snare for victims. If > 20%: genuine benefit-sharing reduces asymmetric extraction, tangled_rope classification justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(benefit_sharing_fund_sufficiency, empirical, 'Adequacy of benefit-sharing fund transfers').

omega_variable(
    mpa_transition_support_effectiveness,
    'Do marine protected area transition programs effectively retrain and relocate fishing communities or primarily impose costs?',
    'Comparison of fishing household income before/after MPA implementation; labor force participation tracking; analysis of alternative livelihood uptake rates and success rates in coastal developing regions',
    'If transition success rate < 30%: victims remain trapped (snare persists). If > 70%: exit options improve substantially (reclassifies toward tangled_rope or constrained rather than trapped). Current pilot data suggests 20-40%.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mpa_transition_support_effectiveness, empirical, 'Effectiveness of MPA transition and livelihood programs').

omega_variable(
    genetic_resource_prospecting_asymmetry,
    'Do major pharmaceutical/biotech corporations face meaningful capacity barriers to marine genetic prospecting, or does BBNJ merely formalize their access advantage?',
    'Patent analysis of marine-derived compounds; tracking of access requests vs approvals; comparison of prospecting success rates for well-resourced vs developing-state research institutions',
    'If barriers are low and access is dominated by large corporations: genetic resource benefit-sharing becomes extraction mechanism favoring already-rich actors (snare for developing states). If barriers are high and access is genuinely shared: rope or scaffold classification justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genetic_resource_prospecting_asymmetry, empirical, 'Asymmetry in marine genetic resource prospecting capacity').

omega_variable(
    regional_management_maturation_timeline,
    'What timeline defines the end-of-life for treaty scaffolding and transition to mature regional ocean governance?',
    'Institutional development tracking of regional marine governance capacity; analysis of treaty capacity-building program outcomes and regional institution maturation; expert consensus on regional readiness for independent governance',
    'If regional maturation < 10 years: scaffold sunset is realistic, temporary constraint classification valid. If > 30 years: sunset is aspirational, constraint likely transforms into piton or persists as tangled_rope indefinitely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regional_management_maturation_timeline, conceptual, 'Timeline for regional ocean governance maturation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(un_high_seas_treaty_2026, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hs_treaty_tr_t0, un_high_seas_treaty_2026, theater_ratio, 0, 0.58).
narrative_ontology:measurement(hs_treaty_tr_t3, un_high_seas_treaty_2026, theater_ratio, 3, 0.63).
narrative_ontology:measurement(hs_treaty_tr_t6, un_high_seas_treaty_2026, theater_ratio, 6, 0.65).

% Extraction over time
narrative_ontology:measurement(hs_treaty_be_t0, un_high_seas_treaty_2026, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(hs_treaty_be_t3, un_high_seas_treaty_2026, base_extractiveness, 3, 0.54).
narrative_ontology:measurement(hs_treaty_be_t6, un_high_seas_treaty_2026, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(un_high_seas_treaty_2026, resource_allocation).
narrative_ontology:affects_constraint(un_high_seas_treaty_2026, regional_fisheries_management_organization_authority).
narrative_ontology:affects_constraint(un_high_seas_treaty_2026, marine_genetic_resource_prospecting_asymmetry).

% DUAL FORMULATION NOTE:
% The high seas treaty decomposes into distinct structural constraints: (1) high_seas_governance_coordination (ε ≈ 0.15, Rope at institutional level) — the genuine coordination function of collective high seas management; (2) un_high_seas_treaty_2026 (ε ≈ 0.58, Tangled Rope) — the full constraint including asymmetric extraction from fishing communities; (3) mpa_enforcement_mechanism (ε ≈ 0.52, Snare) — the implementation of marine protected area restrictions on subsistence fishing. These stories are linked because the high-level coordination authority is exercised through MPA designation and genetic resource allocation mechanisms that disproportionately extract from vulnerable fishing populations. The upstream coordination story should link to both implementation stories via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(un_high_seas_treaty_2026, organized, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
