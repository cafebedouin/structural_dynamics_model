% ============================================================================
% CONSTRAINT STORY: regional_fisheries_management_organization_authority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_regional_fisheries_management_organization_authority, []).

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
 *   constraint_id: regional_fisheries_management_organization_authority
 *   human_readable: Regional Fisheries Management Organization Authority
 *   domain: maritime_governance/resource_extraction
 *
 * SUMMARY:
 *   Regional Fisheries Management Organizations (RFMOs) coordinate harvesting
 *   of shared fish stocks across national boundaries through quota systems,
 *   enforcement mechanisms, and scientific assessments. The constraint
 *   exhibits the characteristic tension between genuine coordination
 *   (preventing tragedy of commons overfishing) and asymmetric extraction
 *   (concentrating benefits to industrial operators while constraining
 *   small-scale fishers and coastal communities). RFMOs were established to
 *   solve a real collective-action problem: unregulated open-access fishing
 *   leads to stock collapse and economic destruction across all participants.
 *   However, the institutional form that emerged privileges capital-intensive
 *   industrial fishing through quota allocation mechanisms, enforcement
 *   asymmetries, and exclusion of small-scale operators. The constraint's
 *   extractiveness has increased over the 20-year interval as: (1) industrial
 *   consolidation accelerated, (2) quota markets emerged favoring capitalized
 *   operators, and (3) IUU fishing persisted despite compliance theater.
 *   Theater ratio has also increased as enforcement documentation becomes
 *   more elaborate while actual monitoring effectiveness stagnates. This is
 *   the diagnostic signature of institutional degradation: the apparatus of
 *   governance becomes more theatrical as its functional effectiveness
 *   plateaus.
 *
 * KEY AGENTS:
 *   - Small-Scale Fishers: Primary victims (powerless/trapped) — lack capital for equipment, formal participation requirements, and alternative occupations; bear full cost of quota restrictions without quota access
 *   - Coastal Communities: Secondary victims (moderate/constrained) — economically dependent on fishing, bear costs of industrial consolidation and stock pressure, constrained but not entirely trapped
 *   - Industrial Fishing Corporations: Primary beneficiaries (institutional/arbitrage) — benefit from quota allocation, enforcement against illegal competition, and price stability; experience constraint as coordination mechanism
 *   - Conservation Coalition: Organized secondary actors (organized/constrained) — benefit from coordination function but lack enforcement authority; constrained by RFMO governance structures dominated by fishing interests
 *   - RFMO Bureaucracy: Institutional actor (institutional/arbitrage) — maintains governance apparatus through member state dependencies; sees own process as degraded (piton perspective)
 *   - Fish Stock Sustainability: Abstract victim — benefits from coordination but extraction mechanisms incentivize overfishing at margin; cannot organize or exit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(regional_fisheries_management_organization_authority, 0.52).
domain_priors:suppression_score(regional_fisheries_management_organization_authority, 0.58).
domain_priors:theater_ratio(regional_fisheries_management_organization_authority, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(regional_fisheries_management_organization_authority, extractiveness, 0.52).
narrative_ontology:constraint_metric(regional_fisheries_management_organization_authority, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(regional_fisheries_management_organization_authority, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(regional_fisheries_management_organization_authority, tangled_rope).
narrative_ontology:human_readable(regional_fisheries_management_organization_authority, "Regional Fisheries Management Organization Authority").
narrative_ontology:topic_domain(regional_fisheries_management_organization_authority, "maritime_governance/resource_extraction").

domain_priors:requires_active_enforcement(regional_fisheries_management_organization_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(regional_fisheries_management_organization_authority, industrial_fishing_fleets).
narrative_ontology:constraint_beneficiary(regional_fisheries_management_organization_authority, capital_intensive_operators).
narrative_ontology:constraint_victim(regional_fisheries_management_organization_authority, small_scale_fishers).
narrative_ontology:constraint_victim(regional_fisheries_management_organization_authority, coastal_communities).
narrative_ontology:constraint_victim(regional_fisheries_management_organization_authority, fish_stock_sustainability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ARTISANAL FISHER (SNARE) — Trapped within RFMO quota systems that allocate most allowable catch to industrial operators. Small-scale fisher cannot exit: fishing is their livelihood, alternative occupations are unavailable in coastal communities, and RFMO rules prohibit informal fishing. Suppression is structural: legal barriers, enforcement patrols, and equipment requirements for formal participation. No coordination benefit — the system exists to extract maximum value to industrial operators. Maximum experienced extraction.
constraint_indexing:constraint_classification(regional_fisheries_management_organization_authority, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: COASTAL COMMUNITY (TANGLED ROPE) — Constrained by economic dependency on fishing but also benefits from RFMO's coordination function: fish stock sustainability prevents complete collapse. However, benefits accrue primarily to industrial operators; coastal communities bear disproportionate costs (reduced catch, exclusion from quotas, environmental damage from industrial methods). Genuine coordination (preventing tragedy of commons) is asymmetrically extracted. Exit costs are high (migration, economic transition) but not impossible.
constraint_indexing:constraint_classification(regional_fisheries_management_organization_authority, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INDUSTRIAL FISHING CORPORATION (ROPE) — Benefits substantially from RFMO quota allocation and enforcement against illegal fishing. Experiences the constraint as pure coordination: the RFMO prevents overfishing by competitors, protects their capital investments in vessels and technology, and creates stable market conditions. Exit is available (fish in unregulated waters) but arbitrage within the system is more profitable. Net beneficiary — experiences the system as enabling and stabilizing.
constraint_indexing:constraint_classification(regional_fisheries_management_organization_authority, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CONSERVATION COALITION (TANGLED ROPE) — Organized actors (environmental NGOs, scientific bodies) benefit from RFMO's coordinating function on sustainability but are constrained by lack of direct enforcement authority. The coalition must work through RFMO governance structures dominated by fishing states. Genuine coordination function (stock assessment, harvest limits) exists alongside extraction of conservation authority by industrial states. Coalition experiences the constraint as mixed: real coordination on sustainability, but asymmetric power to implement stricter limits.
constraint_indexing:constraint_classification(regional_fisheries_management_organization_authority, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: RFMO BUREAUCRACY (PITON) — The RFMO itself exhibits piton characteristics: theater_ratio 0.64 reflects that compliance monitoring, port state control documentation, and catch reporting are substantially performative. Illegal unreported unregulated (IUU) fishing persists at estimated 10-30% of declared catch; documentation systems are gamed; enforcement is spotty. The RFMO sees its own process as degraded — the bureaucratic machinery persists through institutional inertia and member state dependency, not because monitoring actually prevents overfishing effectively. The institution maintains itself as the authority on fisheries governance despite low functional effectiveness.
constraint_indexing:constraint_classification(regional_fisheries_management_organization_authority, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some central coordination of shared fish stocks is a natural limit: tragedy of the commons in open-access fisheries is a structural inevitability. RFMO authority appears as an immutable requirement of ocean governance. However, the structural data reveals this as a false summit: the 'inevitability' naturalizes the specific institutional form (state-centered RFMOs favoring industrial actors) rather than the abstract coordination requirement. Alternative coordination mechanisms (community-based management, tradeable rights systems, international commons frameworks) are possible.
constraint_indexing:constraint_classification(regional_fisheries_management_organization_authority, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(regional_fisheries_management_organization_authority_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(regional_fisheries_management_organization_authority, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(regional_fisheries_management_organization_authority, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(regional_fisheries_management_organization_authority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(regional_fisheries_management_organization_authority, TR),
    TR >= 0.70.

:- end_tests(regional_fisheries_management_organization_authority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The RFMO system does extract value to industrial operators through quota concentration, but extraction is not maximal (0.70+) because coordination function is genuinely valuable — stock collapse would devastate all parties including beneficiaries. The measured extractiveness reflects that benefits to industrial operators are substantial and asymmetrically distributed, while costs to powerless agents are severe. Trajectory from 0.35 to 0.52 reflects increasing concentration as quota trading markets emerged (2000s-2010s) and small-scale fisher exclusion became institutionalized. Suppression (0.58): Moderate-high. Small-scale fishers face legal barriers (licensing, reporting requirements), enforcement barriers (patrol boats, port controls), and economic barriers (equipment costs, quota purchase prices). Suppression is not total because: (1) informal fishing persists, (2) some countries have achieved co-management arrangements, (3) exit through migration is possible though costly. Theater ratio (0.64): Moderate-high. RFMO compliance monitoring (catch documentation, port state control, observer programs) creates appearance of strict enforcement, but IUU fishing estimates suggest 10-30% of actual harvest escapes reporting. The theater has increased as documentation systems became more elaborate without corresponding enforcement improvement.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. Small-scale fisher sees pure snare (trapped, no coordination benefit, maximum extraction cost). Industrial corporation sees pure rope (beneficiary, coordination solves their problem, no extraction experienced). Coastal community sees tangled rope (constrained, real coordination benefit from stock sustainability, but asymmetrically extracted). Conservation coalition sees tangled rope (benefits from coordination function, constrained by governance), but with different constraint focus (authority, not exit cost). RFMO bureaucracy sees piton (degraded institution, performative monitoring). Analytical observer risks seeing mountain (coordination requirement is natural law) but structural data reveals false summit: the coordination requirement (preventing commons tragedy) is natural, but the specific institutional form (RFMO with industrial bias) is contingent. This perspectival map is the engine's primary diagnostic value: it shows that 'RFMO authority' is not a monolithic constraint but a bundle of distinct extraction and coordination mechanisms experienced differently by structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective derives from structural position. Small-scale fishers: trapped exit (0.95) + victim status → d ≈ 0.95 → maximum f(d). Industrial corporations: arbitrage exit (0.05) + beneficiary status → d ≈ 0.05 → minimum f(d). Coastal communities: constrained exit (0.65) + mixed beneficiary/victim → d ≈ 0.58. Conservation coalition: constrained exit (0.65) + victim status (authority constraints) → d ≈ 0.68. The directionality formula captures why beneficiaries perceive rope (low extraction experienced) while victims perceive snare or tangled rope (high extraction experienced) from identical structural constraint. At regional scope (σ=0.9), effective extraction for victims is scaled: χ ≈ ε × f(d) × 0.9. For powerless fisher: χ ≈ 0.52 × 1.42 × 0.9 ≈ 0.67. For institutional beneficiary: χ ≈ 0.52 × (-0.12) × 0.9 ≈ -0.06 (negative extraction = coordination benefit).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by decomposing the RFMO authority into separable mechanisms: (1) Coordination mechanism: preventing open-access overfishing (Rope, genuine benefit to all). (2) Allocation mechanism: distributing quotas among beneficiaries (Tangled Rope, asymmetric extraction). (3) Enforcement mechanism: monitoring compliance (Piton, increasingly theatrical). (4) Exclusion mechanism: restricting small-scale fisher participation (Snare, pure extraction). These can be analyzed separately. Coordination and enforcement could theoretically be improved without the extractive allocation and exclusion — redesigned quota systems (e.g., ITQ systems with small-scale access, community-based allocation) could achieve sustainability without concentrated benefits. The mandatrophy is resolved by recognizing that different analytical questions map to different constraint types: 'Is regional coordination necessary?' maps to Mountain/Rope (yes, fundamental). 'Is the current RFMO form the only way to coordinate?' maps to Tangled Rope/Piton (no, alternatives exist). 'Do artisanal fishers benefit from this specific RFMO structure?' maps to Snare (no, they bear extraction costs with minimal coordination benefit). The integrated system as actually implemented is Tangled Rope: genuine coordination with asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    quota_allocation_mechanism,
    'Is the RFMO quota system fundamentally biased toward industrial operators, or does bias arise from how individual member states implement allocations?',
    'Comparative analysis of RFMO allocation rules (historical rights, capacity-based, etc.) vs actual quota distributions across member states; examination of allocation decision records and stakeholder input processes',
    'If systemic RFMO bias: classification remains Tangled Rope with high extraction term. If implementation bias: classification may shift toward Scaffold (coordination with degraded allocation mechanism that could be reformed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quota_allocation_mechanism, empirical, 'Whether quota bias is inherent to RFMO design or contingent implementation').

omega_variable(
    iuu_fishing_persistence,
    'What proportion of apparent RFMO effectiveness in stock management is actual enforcement vs. voluntary compliance from industrial operators benefiting from price stability?',
    'Analysis of stock recovery in high-compliance vs low-compliance RFMO zones; comparison of fish stock trajectories before/after RFMO establishment; estimation of IUU fishing impact on sustainability outcomes',
    'If IUU undermines stock recovery: RFMO coordination function is weaker than claimed (theater increases, snare characteristics intensify). If stock recovery is real: coordination benefits are genuine but asymmetrically distributed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(iuu_fishing_persistence, empirical, 'Whether RFMO coordination actually improves stock sustainability or creates illusion of management').

omega_variable(
    alternative_governance_viability,
    'Could community-based fisheries management, tradeable individual transferable quotas (ITQs), or coastal-state-centered frameworks achieve sustainability without the industrial bias?',
    'Case studies of successful alternative management systems (Baja California fisheries, Pacific communities, ITQ-based systems); analysis of why alternatives remain marginal in RFMO framework; identification of coordination problems that require regional scale',
    'If alternatives are viable: RFMO authority is contingent institutional form, not natural limit (scaffold or tangled rope with sunset potential). If alternatives fail at regional scale: mountain characteristics intensify.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_governance_viability, empirical, 'Viability of governance alternatives to RFMO structure').

omega_variable(
    coastal_state_capacity_constraint,
    'Is small-scale fisher exclusion from RFMO quota systems a coordination requirement (preventing cheating/overfishing) or an extractive mechanism (protecting industrial monopoly)?',
    'Analysis of enforcement costs and cheating rates in systems with vs without small-scale participation; examination of co-management arrangements that include artisanal sectors; comparison of stock outcomes',
    'If coordination requirement: suppression is justified (mountain-shift). If extractive mechanism: suppression is pure restriction (snare-shift for powerless perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coastal_state_capacity_constraint, empirical, 'Whether small-scale fisher exclusion is coordination requirement or extraction mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(regional_fisheries_management_organization_authority, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rfmo_tr_t0, regional_fisheries_management_organization_authority, theater_ratio, 0, 0.48).
narrative_ontology:measurement(rfmo_tr_t10, regional_fisheries_management_organization_authority, theater_ratio, 10, 0.58).
narrative_ontology:measurement(rfmo_tr_t20, regional_fisheries_management_organization_authority, theater_ratio, 20, 0.64).
narrative_ontology:measurement(rfmo_tr_t5, regional_fisheries_management_organization_authority, theater_ratio, 5, 0.53).

% Extraction over time
narrative_ontology:measurement(rfmo_be_t0, regional_fisheries_management_organization_authority, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(rfmo_be_t10, regional_fisheries_management_organization_authority, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(rfmo_be_t20, regional_fisheries_management_organization_authority, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(rfmo_be_t5, regional_fisheries_management_organization_authority, base_extractiveness, 5, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(regional_fisheries_management_organization_authority, resource_allocation).
narrative_ontology:affects_constraint(regional_fisheries_management_organization_authority, illegal_unreported_unregulated_fishing).
narrative_ontology:affects_constraint(regional_fisheries_management_organization_authority, ocean_acidification_marine_ecosystem_collapse).

% DUAL FORMULATION NOTE:
% RFMO authority operates at multiple structural levels. The coordination problem (preventing commons tragedy) is distinct from the extraction mechanism (quota concentration). IUU fishing is downstream: it exploits gaps in RFMO enforcement. Ocean acidification represents an orthogonal constraint that the RFMO framework is not designed to address, creating degradation (piton) as the RFMO's nominal scope increasingly mismatches actual sustainability challenges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(regional_fisheries_management_organization_authority, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
