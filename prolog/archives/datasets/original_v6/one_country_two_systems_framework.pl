% ============================================================================
% CONSTRAINT STORY: one_country_two_systems_framework
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_one_country_two_systems_framework, []).

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
 *   constraint_id: one_country_two_systems_framework
 *   human_readable: One Country Two Systems Framework (Hong Kong, Macau, Taiwan)
 *   domain: political/constitutional/geopolitical
 *
 * SUMMARY:
 *   The One Country Two Systems framework, instantiated in Hong Kong (since
 *   1997) and Macau (since 1999), and proposed for Taiwan, represents a
 *   constitutional arrangement designed to preserve distinct legal, economic,
 *   and institutional systems within a unified state structure. The framework
 *   exhibits simultaneous coordination and extraction: it genuinely preserves
 *   economic functionality and international trade relationships
 *   (coordination function) while systematically subordinating local
 *   democratic autonomy and civil liberties (extraction function). The
 *   constraint's theater ratio (0.65) reflects an increasing gap between
 *   formal legal distinctness (Hong Kong maintains separate currency, common
 *   law system, independent judiciary per the Basic Law) and operational
 *   convergence (National Security Law, extradition treaty expansion,
 *   judicial deference to central authority). The framework operates
 *   differently for different agents: central authority experiences pure
 *   coordination, international business experiences mixed
 *   coordination-extraction, local democratic actors experience pure
 *   extraction, and the international legal order treats it as increasingly
 *   performative (piton). The measurement trajectory shows extractiveness
 *   rising from 0.35 (1997, early period with genuine autonomy) to 0.62
 *   (2027, post-National Security Law intensification), while theater ratio
 *   rises from 0.40 to 0.65, indicating that formal distinctness persists
 *   while operational meaning erodes—a classic drift toward piton
 *   degradation.
 *
 * KEY AGENTS:
 *   - Central Authority: Primary beneficiary (institutional/arbitrage) — uses framework to maintain international credibility and trade relationships while reasserting sovereignty and preventing independence movements
 *   - Local Democratic Institutions: Primary victim (powerless/trapped) — civil society, independent judiciary, opposition parties face escalating constraints on autonomous decision-making with no exit option
 *   - Local Business Sector: Secondary beneficiary and victim (moderate/constrained) — benefits from distinct legal framework for commerce but faces rising political pressure and regulatory uncertainty
 *   - International Business Community: Organized beneficiary (organized/constrained) — multinational corporations benefit from Hong Kong's financial distinctness but face geopolitical risk and capital controls
 *   - Diaspora and Capital Networks: Exit-oriented agent (powerful/mobile) — most adaptive agents drain from jurisdiction as extraction intensifies, representing natural sunset mechanism
 *   - International Legal Order: Institutional observer (institutional/arbitrage) — maintains treaty obligations recognizing framework but increasingly treats distinctness as performative artifact
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees simultaneous coordination and extraction without resolution possible at this time horizon
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(one_country_two_systems_framework, 0.62).
domain_priors:suppression_score(one_country_two_systems_framework, 0.68).
domain_priors:theater_ratio(one_country_two_systems_framework, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework, extractiveness, 0.62).
narrative_ontology:constraint_metric(one_country_two_systems_framework, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(one_country_two_systems_framework, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(one_country_two_systems_framework, tangled_rope).
narrative_ontology:human_readable(one_country_two_systems_framework, "One Country Two Systems Framework (Hong Kong, Macau, Taiwan)").
narrative_ontology:topic_domain(one_country_two_systems_framework, "political/constitutional/geopolitical").

domain_priors:requires_active_enforcement(one_country_two_systems_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework, central_authority_credibility).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework, financial_hub_preservation).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework, international_trade_corridors).
narrative_ontology:constraint_victim(one_country_two_systems_framework, local_democratic_autonomy).
narrative_ontology:constraint_victim(one_country_two_systems_framework, civil_liberties_consensus).
narrative_ontology:constraint_victim(one_country_two_systems_framework, institutional_independence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOCAL DEMOCRATIC ACTOR (SNARE) — Civil society organizations, independent judges, journalists, and opposition politicians face escalating constraints on autonomous action. Exit options are severely limited: physical departure, loss of livelihood, loss of citizenship, or self-censorship. The framework extracts political autonomy while suppressing alternatives. No genuine coordination function exists from this vantage — only extraction masked by constitutional language.
constraint_indexing:constraint_classification(one_country_two_systems_framework, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: LOCAL BUSINESS SECTOR (TANGLED ROPE) — Benefits from distinct legal frameworks (property rights, contract law, separate currency/financial systems) that enable commercial activity and capital flows. But faces escalating political pressure, regulatory uncertainty, and risk of expropriation. The constraint genuinely coordinates cross-border commerce AND asymmetrically extracts political compliance. Suppression is high (regulatory pressure, surveillance, capital controls) but not total — business can operate, though with declining autonomy.
constraint_indexing:constraint_classification(one_country_two_systems_framework, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CENTRAL AUTHORITY (ROPE) — Experiences the framework as pure coordination: maintaining distinct legal systems in Hong Kong and Macau while reasserting sovereignty eliminates the risk of independent statehood claims, preserves international trade relationships (especially through Hong Kong), and avoids the economic disruption of forced unification. The extraction (political subordination) flows toward the central authority. No suppression required from this perspective — the framework is self-reinforcing through institutional momentum and international business incentives.
constraint_indexing:constraint_classification(one_country_two_systems_framework, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERNATIONAL BUSINESS COMMUNITY (TANGLED ROPE) — Organized multinational corporations, financial institutions, and trade networks benefit significantly from Hong Kong's distinct legal framework (common law, independent judiciary, currency separateness) which enables low-friction international finance and commerce. But increasingly faces political risk, capital controls, extradition treaty expansion, and regulatory unpredictability. The constraint coordinates global commerce AND extracts compliance with sovereignty claims. Suppression comes from geopolitical pressure and capital controls, but not from internal barriers — exit is possible at escalating cost (relocation, regulatory punishment).
constraint_indexing:constraint_classification(one_country_two_systems_framework, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL LEGAL ORDER (PITON) — The framework maintains vestigial status within international law (Hong Kong's separate seat in WTO, Macau's autonomous customs area, Taiwan's quasi-diplomatic relationships) that has largely become performative. The legal exceptions are acknowledged by treaty and tradition but increasingly disconnected from effective autonomy. The international order treats the framework as a historical artifact maintained by institutional inertia rather than as a functioning guarantee of meaningful difference. Theater ratio reflects the gap between the formal legal distinctness and the actual convergence in operational control.
constraint_indexing:constraint_classification(one_country_two_systems_framework, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: DIASPORA AND CAPITAL FLIGHT NETWORKS (SCAFFOLD) — Seen as temporary -- organized movements of capital, talent, and persons out of the region represent a sunset mechanism. As the framework's extractive mechanisms intensify, the most mobile agents (entrepreneurs, professionals, capital holders) exit into international networks. This drains the jurisdiction of its most adaptive agents and reduces the framework's coordination function. The framework sees this exit as a constraint on state power, but from the network perspective, it is a natural sunset: as extraction rises, exit becomes rationally permanent, effectively decomposing the Two Systems into One.
constraint_indexing:constraint_classification(one_country_two_systems_framework, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — The framework simultaneously coordinates genuine functional distinctness (separate legal systems enabling commerce and institutional autonomy) AND asymmetrically extracts political subordination and civil liberties. The extraction is not total (economic and business activity continue) but is systematic and rising. The suppression (restrictions on democratic voice, judicial independence, press freedom, capital mobility) prevents exit and alternatives. The analytical perspective sees both the real coordination function (economic preservation, trade facilitation) and the real extraction (political absorption, civil liberties erosion). Neither can be eliminated without structural failure.
constraint_indexing:constraint_classification(one_country_two_systems_framework, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(one_country_two_systems_framework_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(one_country_two_systems_framework, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(one_country_two_systems_framework, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(one_country_two_systems_framework, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(one_country_two_systems_framework, TR),
    TR >= 0.70.

:- end_tests(one_country_two_systems_framework_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High but not maximal. The framework initially preserved genuine autonomy (post-1997 Hong Kong had independent judiciary, free press, democratic elections for functional constituencies, capital mobility). Extractiveness has risen substantially following the 2019 protests and 2020 National Security Law, which curtailed judicial independence, press freedom, and electoral autonomy. But extraction is not total — Hong Kong maintains separate currency (HKD), distinct tax system, common law courts, and significant financial autonomy. Economic and business activity continue. The trajectory reflects rent-seeking layered onto coordination: early years coordination dominated, but extraction has accumulated over time. Suppression (0.68): High. Multiple mechanisms suppress alternatives: (1) legal suppression (NSL prohibits independence advocacy, sedition law, expanded extradition), (2) institutional suppression (courts increasingly defer to national security claims, executive expands relative to judiciary), (3) economic suppression (capital controls, asset freezes for political opposition, regulatory punishment for dissenting businesses), (4) informational suppression (media self-censorship, control of education curriculum, algorithmic control). Suppression is systematic across all exit options. Theater ratio (0.65): Moderately high and rising. The Basic Law (Hong Kong) and framework documents promise judicial independence, but courts increasingly rule consistent with central authority preferences on politically sensitive cases. Elections continue but with reduced franchise and approved-candidate lists. Autonomy is ritually declared but operationally constrained. The gap between promised and delivered distinctness reflects theater growth.
 *
 * PERSPECTIVAL GAP:
 *   The framework produces maximal perspectival divergence across the observation site. Central authority sees coordination (Rope): solving the problem of maintaining sovereignty while preserving economic functionality. Local democratic actor sees extraction (Snare): political autonomy vanishes with no exit. International business sees mixed coordination-extraction (Tangled Rope): benefits from distinctness are real but political risk is rising. Diaspora sees sunset mechanism (Scaffold): as extraction rises, voluntary exit accelerates, naturally decomposing the framework. International law sees degraded ritual (Piton): formal distinctness persists through treaty obligation but operational meaning has eroded. The analytical observer sees both coordination and extraction simultaneously existing in the same structure, unresolvable at the civilizational time horizon. This perspectival gap is diagnostic: if all perspectives converged on a single type, the framework would be either purely coordinative (Rope) or purely extractive (Snare). The gap itself is evidence that both functions operate — the mandatrophy is resolved by accepting that Tangled Rope is the correct classification despite the temptation to collapse it toward one type.
 *
 * DIRECTIONALITY LOGIC:
 *   The framework's directionality varies sharply by agent position. Central authority (d ≈ 0.05): Positioned as pure beneficiary; extraction flows toward them; they experience arbitrage options (can maintain or intensify the framework with little cost). Local democratic actors (d ≈ 0.92): Positioned as victims; extraction flows away from them; trapped exit (cannot leave Hong Kong identity or abandon democracy commitment without becoming someone else). International business (d ≈ 0.58): Mixed position; genuinely benefits from financial distinctness but faces rising political extraction; constrained exit (can relocate but at high cost). The sigmoid f(d) amplifies this asymmetry in perceived extractiveness: beneficiaries perceive low χ regardless of base ε, while victims perceive high χ. This drives the perspectival gap: same ε (0.62) produces rope for d=0.05, tangled rope for d=0.58, and snare for d=0.92. The directionality asymmetry is the core extraction mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The framework is classified as Tangled Rope, which requires demonstrating BOTH coordination function AND asymmetric extraction. The coordination function is genuine: Hong Kong's distinct legal system, currency, trade status, and capital autonomy enable commerce that would not exist under full national integration. The international financial system depends on this distinctness. The extractive function is genuine: the framework subordinates local democratic autonomy, constrains civil liberties, and prevents exit from these constraints. Both functions exist simultaneously and cannot be separated. The mandatrophy is avoided by rejecting the temptation to classify this as either pure coordination (Rope) or pure extraction (Snare). The framework is neither. It is explicitly defined by the Basic Law as a 'one country, two systems' duality — two functions in one structure. The rising theater ratio and extraction measurements over time indicate drift toward Snare, but current state is Tangled Rope with clear asymmetry. Suppression alone does not determine this — Rope constraints also have suppression. The presence of victims alongside beneficiaries, and the requirement for active enforcement to maintain the coordination function, are diagnostic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    convergence_timeline_acceleration,
    'At what rate will formal legal distinctness converge toward operational integration, and what triggers accelerate this timeline?',
    'Longitudinal measurement of: (1) frequency of overriding Hong Kong/Macau legal rulings through national security law, (2) capital flight rates, (3) brain drain metrics, (4) transaction costs for doing business across the boundary',
    'If convergence accelerates (< 10 years): scaffold perspective dominates and framework becomes de facto snare. If convergence plateaus: tangled rope equilibrium may stabilize. If reversal occurs: rope perspective becomes credible again.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(convergence_timeline_acceleration, empirical, 'Timeline and acceleration of formal vs operational convergence').

omega_variable(
    international_business_substitutability,
    'Are Hong Kong''s financial and commercial advantages (rule of law, currency, legal distinctness) substitutable by Singapore, Dubai, or other financial hubs, or are they genuinely irreplaceable for specific asset classes and trading relationships?',
    'Cross-correlation analysis of capital flows, trading volumes, and business registrations between Hong Kong and competing financial centers; identification of lock-in mechanisms and switching costs',
    'If highly substitutable: international business exit accelerates, framework collapses into purely extractive snare, suppression rises to compensate. If irreplaceable: rent extraction can continue indefinitely, framework stabilizes as tangled rope with sustainable asymmetric extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(international_business_substitutability, empirical, 'Substitutability of Hong Kong''s financial center status').

omega_variable(
    internal_legitimacy_bifurcation,
    'Is suppression sustained by internal legitimacy (local acceptance of the framework as necessary/beneficial) or purely by coercive capacity? What is the ratio?',
    'Public opinion polling, survey data on willingness-to-comply with specific framework constraints, correlation between protest cycles and suppression intensity, analysis of voluntary vs coerced compliance in judicial, business, and civil society sectors',
    'If legitimacy-based: framework can continue without rising suppression costs. If coercion-based: suppression must increase over time to maintain compliance, creating a feedback loop toward snare. If bifurcated: elite legitimacy can coexist with mass delegitimacy, destabilizing the framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internal_legitimacy_bifurcation, empirical, 'Ratio of legitimacy-based to coercion-based suppression').

omega_variable(
    taiwan_asymmetry,
    'Why does Taiwan not accept the One Country Two Systems framework when Hong Kong and Macau (ostensibly) do, and what does this reveal about the framework''s underlying constraint structure?',
    'Comparative structural analysis: Taiwan''s different entry conditions (never administratively integrated, separate state institutions, democratic transition during period of distinctness), different exit costs (military capacity, geopolitical alignment options), different beneficiary/victim maps. Assess whether Taiwan''s rejection proves the framework is extractive-only or whether Taiwan''s refusal to integrate is itself a choice variable.',
    'If Taiwan''s rejection proves extraction: framework is contingent on powerless/trapped populations, invalidating the coordination narrative. If Taiwan''s refusal reflects rational calculation under different conditions: framework may be temporarily stable but unstable to power shifts (Taiwan acquiring defense capacity, central authority declining). If framework is genuinely voluntary for Taiwan: snare and scaffold perspectives collapse, framework reclassifies as rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(taiwan_asymmetry, conceptual, 'Why Taiwan rejects One Country Two Systems framework').

omega_variable(
    national_security_mission_creep,
    'Does the National Security Law (Hong Kong 2020, Macau 2009) represent emergency temporary enforcement or permanent structural shift in the framework''s enforcement mechanism? Is the shift reversible?',
    'Textual analysis of statutory language (sunset clauses, temporal framing, reversibility conditions); historical comparison to other national security regimes and whether they have ever been voluntarily narrowed; measurement of whether NSL prosecutions are expanding or narrowing in scope over time',
    'If temporary/reversible: suppression may be modulated downward, framework could revert toward rope. If permanent/expanding: NSL represents extraction mechanism rather than coordination mechanism, framework has crossed from tangled rope toward snare, theater ratio rises as formal distinctness persists while operational autonomy vanishes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(national_security_mission_creep, empirical, 'Whether National Security Laws represent temporary or permanent enforcement shift').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(one_country_two_systems_framework, 1997, 2027).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(octs_tr_t0, one_country_two_systems_framework, theater_ratio, 0, 0.4).
narrative_ontology:measurement(octs_tr_t10, one_country_two_systems_framework, theater_ratio, 10, 0.55).
narrative_ontology:measurement(octs_tr_t20, one_country_two_systems_framework, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(octs_be_t0, one_country_two_systems_framework, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(octs_be_t10, one_country_two_systems_framework, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(octs_be_t20, one_country_two_systems_framework, base_extractiveness, 20, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(one_country_two_systems_framework, resource_allocation).
narrative_ontology:affects_constraint(one_country_two_systems_framework, hong_kong_judicial_independence).
narrative_ontology:affects_constraint(one_country_two_systems_framework, macau_autonomy_formal_vs_operational).
narrative_ontology:affects_constraint(one_country_two_systems_framework, taiwan_political_status_ambiguity).
narrative_ontology:affects_constraint(one_country_two_systems_framework, international_treaty_obligation_enforcement).

% DUAL FORMULATION NOTE:
% The One Country Two Systems framework coordinates across multiple distinct claims: financial system distinctness (high coordination, low extraction), judicial independence (high extraction post-2020), electoral autonomy (increasing extraction), civil liberties (high extraction), capital mobility (increasing extraction). These could be decomposed into separate constraint stories per the ε-invariance principle. The unified story presented here treats the framework as a single constraint because the binding mechanism is constitutional structure, not domain-specific mechanics. The network links show downstream constraints that depend on the framework's stability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(one_country_two_systems_framework, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
