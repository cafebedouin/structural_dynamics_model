% ============================================================================
% CONSTRAINT STORY: us_canada_geopolitical_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_canada_geopolitical_asymmetry, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: us_canada_geopolitical_asymmetry
 *   human_readable: The Geopolitical Constraint of US Proximity on Canadian Sovereignty
 *   domain: geopolitical/international_relations
 *
 * SUMMARY:
 *   The geopolitical relationship between the United States and Canada
 *   represents a classic case of structural asymmetry tempered by integration
 *   and mutual benefit. Canada is a nation-state of 39 million people sharing
 *   a continent with a superpower of 330 million; the economic, military, and
 *   technological asymmetry is not merely significant but fundamental to
 *   North American geopolitics. The constraint operates through multiple
 *   mechanisms: economic integration (USMCA makes Canada dependent on US
 *   market access), defense integration (NORAD embeds Canadian military in US
 *   command structure), technology dependence (Canadian digital
 *   infrastructure relies on US platforms and standards), and explicit
 *   strategic alignment requirements (China policy, Russia sanctions, NATO
 *   contributions). Yet the relationship is not one of pure extraction —
 *   there are genuine coordination benefits (Arctic security, shared missile
 *   defense against Russian threats, intelligence partnership that
 *   strengthens Canadian security), and Canada retains formal sovereignty and
 *   substantial agency. The constraint classifies as Tangled Rope from the
 *   Canadian institutional perspective: hybrid coordination
 *   (security/economic efficiency gains) bundled with asymmetric extraction
 *   (strategic autonomy reduced, policy alignment required). The powerless
 *   victim in this story is not a human agent but Canadian strategic
 *   independence itself — the abstract capacity to choose foreign policy
 *   without reference to US preference. The constraint extracts from that
 *   pool of autonomy consistently, though the extraction is often invisible
 *   because Canadian interests and US interests frequently align.
 *
 * KEY AGENTS:
 *   - United States Strategic Establishment: Primary beneficiary (institutional/arbitrage) — derives security advantage, economic benefits, and ability to project power northward; can arbitrage between Canadian and other relationships
 *   - Canadian Strategic Autonomy (abstract): Primary victim (powerless/trapped) — structural reduction in policy choice space; no exit option available
 *   - Canadian Government Institutions: Secondary actor (organized/constrained) — benefits from security guarantees and economic integration while bearing extraction on foreign policy independence; retains some agency but operates within asymmetric constraints
 *   - North American Defense Integration (NORAD/NATO): Institutional beneficiary (institutional/arbitrage) — represents genuine coordination function that benefits both parties but primarily serves US strategic preferences
 *   - Canadian Economic Sectors: Mixed victim/beneficiary (powerful/mobile to arbitrage) — manufacturing sectors benefit from integrated supply chains; extractive resource sectors benefit from US market access; tech sectors constrained by US standards/sanctions requirements
 *   - Canadian Economic Diversification Movement: Organized reformer (organized/constrained) — pursuing CPTPP, Arctic partnerships, indigenous economic development to reduce US dependence; creating potential sunset mechanism for constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_canada_geopolitical_asymmetry, 0.52).
domain_priors:suppression_score(us_canada_geopolitical_asymmetry, 0.65).
domain_priors:theater_ratio(us_canada_geopolitical_asymmetry, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_canada_geopolitical_asymmetry, extractiveness, 0.52).
narrative_ontology:constraint_metric(us_canada_geopolitical_asymmetry, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(us_canada_geopolitical_asymmetry, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_canada_geopolitical_asymmetry, tangled_rope).
narrative_ontology:human_readable(us_canada_geopolitical_asymmetry, "The Geopolitical Constraint of US Proximity on Canadian Sovereignty").
narrative_ontology:topic_domain(us_canada_geopolitical_asymmetry, "geopolitical/international_relations").

domain_priors:requires_active_enforcement(us_canada_geopolitical_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_canada_geopolitical_asymmetry, united_states_strategic_interests).
narrative_ontology:constraint_beneficiary(us_canada_geopolitical_asymmetry, north_american_defense_integration).
narrative_ontology:constraint_victim(us_canada_geopolitical_asymmetry, canadian_strategic_autonomy).
narrative_ontology:constraint_victim(us_canada_geopolitical_asymmetry, canadian_foreign_policy_independence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CANADIAN FOREIGN POLICY INDEPENDENCE (SNARE) — Canada cannot exit the geopolitical asymmetry without catastrophic economic and security costs. Strategic autonomy is systematically constrained by proximity to a superpower: NATO/NORAD integration, defense industrial dependence, resource export dominance, and cultural/technological hegemony create irreversible lock-in. Exit options are theoretically available but politically impossible — severing defense ties would expose Canada to Russian/Chinese pressure while destroying the North American economic zone. The constraint extracts policy compliance (alignment on China, Russia, Iran, climate frameworks) with minimal Canadian veto power. Canadian governments experience this as maximum structural extraction despite formal sovereignty.
constraint_indexing:constraint_classification(us_canada_geopolitical_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: CANADIAN GOVERNMENT (TANGLED ROPE) — Canada benefits from security guarantees, defense cost-sharing, economic integration, and access to US technology/intelligence. NORAD integration prevents external threats; US military umbrella provides public goods Canada could not afford independently. Simultaneously, Canada bears extraction: defense spending is constrained by US interoperability requirements, foreign policy must align with US strategic interests (Iran sanctions, China tech restrictions), and trade dependency means Canada faces implicit threats of economic retaliation if it deviates. The constraint is hybrid — genuine coordination on shared North American defense, asymmetric extraction on strategic choice. Canada retains some agency but operates within asymmetric boundaries.
constraint_indexing:constraint_classification(us_canada_geopolitical_asymmetry, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: UNITED STATES STRATEGIC ESTABLISHMENT (ROPE) — The US experiences the constraint primarily as coordination: Canada is a predictable ally, secure northern border eliminates military expenditure, integrated supply chains reduce US dependencies on hostile powers, and defense cooperation extends US power projection. The US has repeatedly offered Canada integration opportunities (defense industrial consolidation, security partnerships) that are mutually beneficial. From the US perspective, this is a low-extraction coordination mechanism — the US gets benefits without needing to actively suppress alternatives because Canadian self-interest aligns with integration. Arbitrage exit (US can always pivot to other alliances) makes this a cooperation zone rather than extraction.
constraint_indexing:constraint_classification(us_canada_geopolitical_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DEFENSE INTEGRATION INSTITUTIONS (TANGLED ROPE) — NORAD and NATO represent genuine coordination for Arctic security, missile defense, and continental defense. These institutions benefit both parties through burden-sharing and interoperability. However, they also extract from Canadian autonomy: integrated command structures mean Canadian military choices are constrained by alliance commitments; defense procurement must follow US-compatible standards (cost multiplier); and strategic doctrine must align (missile defense, China containment). The institutions have active enforcement (treaty obligations, interoperability requirements). The constraint is hybrid — real coordination function bundled with asymmetric extraction that favors US strategic preference.
constraint_indexing:constraint_classification(us_canada_geopolitical_asymmetry, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: CANADIAN ECONOMIC DIVERSIFICATION (SCAFFOLD) — Over the past two decades, Canadian governments have pursued explicit diversification strategies: CPTPP membership (reducing US-only trade dependence), Arctic indigenous partnerships, critical minerals investments, and technology sector development independent of US frameworks. These are not full exit mechanisms but gradual reduction of asymmetric dependence. The constraint here appears as temporary — sunset logic applies because diversification initiatives create genuine alternatives. If Canada successfully builds trade relationships with non-US partners, reduces resource export concentration on the US market, and develops indigenous defense/technology capacity, the constraint's extraction mechanism weakens. Theater remains low (0.48) because enforcement is implicit rather than performed. Sunset timeline: 15-25 years if diversification succeeds; indefinite if it stalls.
constraint_indexing:constraint_classification(us_canada_geopolitical_asymmetry, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: HISTORICAL DOMINION FRAMEWORK (PITON) — The formal institutional structure inherited from British imperial dominion status (Dominion of Canada, Westminster parliamentary system, shared monarchy) persists as largely performative. Legal continuities in Commonwealth relationships and royal symbolism maintain theatrical institutional continuity with 19th-century imperial subordination. However, the real power relationship is no longer structured through formal dominion hierarchy but through economic integration and security dependence. The old imperial framework is degraded — it persists through institutional inertia rather than functional enforcement. Modern constraints operate through NAFTA/USMCA, NORAD, and asymmetric economic dependence, not through formal political subordination. Theater ratio reflects this: enforcement is economic and strategic, not ceremonial. The piton classification captures the gap between formal sovereignty (theater) and functional constraint (structural).
constraint_indexing:constraint_classification(us_canada_geopolitical_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 7: GEOGRAPHIC DETERMINISM (MOUNTAIN) — From a pure geographic/structural perspective, Canada's constraint by US proximity appears as a natural law: geography determines vulnerability. A nation-state adjacent to a superpower, sharing a 5,525-mile border, with 90% of population within 200 miles of the border, cannot escape geopolitical asymmetry any more than a satellite can escape gravitational attraction. Under this view, the constraint is Mountain-type — irreducible, emerges naturally from territorial arrangement, zero degrees of freedom. However, the base properties contradict this classification: the constraint requires active enforcement (bilateral trade policy, defense integration agreements, intelligence coordination), is not immutable (diversification can weaken it), and suppression is contingent on political choice (0.65, not near 1.0). This is a FALSE SUMMIT — naturalization of what is actually a hybrid coordination-extraction system into immutable geography.
constraint_indexing:constraint_classification(us_canada_geopolitical_asymmetry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_canada_geopolitical_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_canada_geopolitical_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_canada_geopolitical_asymmetry, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_canada_geopolitical_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_canada_geopolitical_asymmetry, TR),
    TR >= 0.70.

:- end_tests(us_canada_geopolitical_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The base measure reflects that Canada bears significant strategic costs — policy autonomy is constrained, defense spending must align with US interoperability requirements, and foreign policy must accommodate US strategic preferences (China tech restrictions, Iran sanctions, Russia coordination). However, extraction is not maximal (0.66+) because Canada also receives genuine benefits: security guarantees Canada could not afford independently, economic integration that increases prosperity, and intelligence access that enhances Canadian security. The measurement shows upward trend from 0.38 (1990) to 0.52 (2020) because US hegemonic pressure increased post-9/11 (mandatory security integration, China containment policy) and is higher in contemporary period (technology decoupling, sanctions coordination). Suppression (0.65): Moderate-high. Multiple suppression mechanisms operate: (a) Economic: 75-90% of Canadian exports go to US, making trade retaliation a credible enforcement threat; (b) Strategic: Russian and Chinese military capabilities make independent Canadian defense economically impossible; (c) Institutional: NORAD/NATO integration creates path dependence — exiting would require rebuilding entire defense establishment; (d) Cultural/informational: US dominance of media, platforms, and information systems makes Canadian autonomous narrative space limited. Suppression is substantial but not total (not 0.85+) because Canada retains formal sovereignty, has some alternative trading partners, and could theoretically pursue independent defense. Theater ratio (0.48): Moderate-low. The constraint operates through substantive mechanisms (economic integration, military dependence, strategic alignment) rather than through performative theater. Unlike the degraded institutional theater of imperial dominion status, modern enforcement is structural and real. Theater_ratio has DECREASED from 0.62 (1990) when formal institutional theater from Commonwealth/dominion heritage was higher, to 0.48 (2020) as constraint became more economically/strategically substantive and less ceremonial. This trajectory contradicts piton classification — the constraint is becoming more functionally real, not more theatrical.
 *
 * PERSPECTIVAL GAP:
 *   The central perspectival gap lies between the Canadian and US institutional views. Canada experiences the constraint as Tangled Rope (mixed coordination/extraction) because: (a) Canada genuinely benefits from US security guarantees that it could not afford independently; (b) But Canada also bears extraction through constrained strategic autonomy and policy alignment requirements. The US experiences the constraint primarily as Rope (coordination) because: (a) US derives security benefits from Canadian partnership without needing to actively enforce conformity (Canadian self-interest aligns with US interests much of the time); (b) Canadian compliance is perceived as natural alignment rather than extracted enforcement. The powerless victim perspective (Canadian foreign policy independence) sees pure Snare — an irreversible lock-in with no exit option. The diversification movement perspective (scaffold) sees the constraint as temporary — deliberate institutional and economic restructuring can create real alternatives within 15-25 years. The geographic determinism perspective (mountain) sees the constraint as a law of nature — proximity to a superpower inevitably constrains smaller nations. But the base properties contradict the mountain view: the constraint requires active enforcement, has changed over time (extractiveness increasing, theater decreasing), and is contingent on political and economic choices, not on geography alone. The false summit classification reveals how geopolitical asymmetry can be naturalized as geographic destiny when it is actually a contingent institutional and economic arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality in this constraint flows from two structural sources: (1) Asymmetric power capacity: The US has 8.5x Canada's GDP, 6x the population, and hegemonic military capability. This creates a baseline power asymmetry that determines d values. (2) Structural relationship to the constraint: The US benefits from the constraint (beneficiary status, arbitrage exit options → low/negative d → negative effective extraction experienced by US). Canada bears costs of the constraint (victim of autonomy loss, trapped/constrained exit options → high d → high effective extraction experienced by Canada). The canonical d values flow from this: US institutional perspective derives d ≈ 0.00-0.15 (beneficiary + arbitrage → low extraction experience). Canadian institutional perspective derives d ≈ 0.50-0.70 (mixed beneficiary and victim status, constrained exit → moderate to high extraction experience). Canadian powerless perspective derives d ≈ 0.95 (victim + trapped → near-maximum extraction). No overrides are necessary — the structural relationship (who benefits, who bears costs, what exits are available) produces accurate d values through the derivation chain. The directionality captures the reality: the same constraint structure is experienced by the US as low-cost coordination and by Canada as extraction with coordination side-effects.
 *
 * MANDATROPHY ANALYSIS:
 *   The Mandatrophy problem asks: 'How do we avoid false positive Tangled Rope classifications that mislabel pure extraction as hybrid coordination?' In this case, the tangled_rope classification is justified by genuine structural coordination benefits bundled with asymmetric extraction. The resolution: (1) Canadian government DOES benefit from NORAD security guarantees that Canada could not afford independently — this is real coordination benefit, not pure theater. (2) Canada DOES bear extraction — policy autonomy is constrained, foreign policy must align with US preference, defense doctrine must interoperate with US systems. (3) Active enforcement IS required — bilateral treaties, defense integration agreements, and sanctions coordination mechanisms actively maintain the constraint. Without these three elements, classification would degrade to Snare (pure extraction). The Tangled Rope classification is accurate because the coordination component is genuine (not performative) and the extraction component is real (not hypothetical). The powerless victim perspective (Canadian strategic autonomy) sees pure Snare — extraction with zero coordination benefit for the victim — but this is perspectival truth, not structural error. From the powerless victim's vantage point, all coordination benefits accrue to the US and beneficiary Canadians (exporters, defense contractors, intelligence agencies), while strategic autonomy is extracted universally. The mandatrophy is RESOLVED by showing that the same constraint legitimately classifies as different types from different perspectives — Rope (US view), Tangled Rope (Canadian government view), Snare (Canadian autonomy victim view), Scaffold (diversification reformer view) — because the perspectives genuinely perceive different mixes of extraction and coordination depending on their structural position. No single type is 'correct'; the presheaf over all perspectives reveals the constraint's true structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_definition_threshold,
    'At what level of economic/strategic alignment does ''influence'' become structural ''constraint'' that violates sovereignty principles?',
    'Comparative analysis with other asymmetric bilateral relationships (UK-US, Germany-US, Mexico-US, Australia-US); correlation between alignment scores and quantified policy deviations from declared independence; legal scholars'' consensus on sovereignty thresholds',
    'If threshold is high (extreme alignment = constraint): Canada retains effective sovereignty classification. If threshold is low (moderate alignment = constraint): Canada''s functional sovereignty status is degraded to dependency. Currently contested between Canadian and US legal/diplomatic traditions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_definition_threshold, conceptual, 'Definitional threshold between justified influence and sovereignty-violating constraint').

omega_variable(
    enforcement_mechanism_visibility,
    'Is the constraint enforced through explicit threats, implicit retaliation expectations, or genuine mutual interest alignment?',
    'Declassified diplomatic cables analysis; game-theoretic modeling of Canadian compliance incentives; interviews with Canadian foreign policy decision-makers on threat perception vs. alignment belief; correlation between US pressure points (trade, defense cooperation, intelligence access) and Canadian policy shifts',
    'If enforcement is explicit (threats/retaliation): Snare classification from Canadian perspective is correct. If enforcement is implicit (retaliation expectations): Tangled Rope is accurate. If alignment is genuine (mutual interest): Canadian government sees Rope. Classification shifts across 0.30-0.65 extractiveness depending on this resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_mechanism_visibility, empirical, 'Whether constraint enforcement is explicit threat, implicit expectation, or genuine alignment').

omega_variable(
    diversification_viability,
    'Can Canadian economic and strategic diversification succeed in substantially reducing US dependency, or is geographic/economic structure permanently locking in asymmetry?',
    'Projection modeling of Canadian CPTPP trade growth vs. US trade concentration; assessment of critical minerals supply chains independent of US processing; evaluation of indigenous Arctic defense capacity development; historical precedent analysis (how much did post-WWII decolonization reduce dominion constraints?)',
    'If diversification succeeds: Scaffold perspective is correct and constraint has true sunset (15-25 years). If diversification fails: Mountain/Snare perspectives are correct and constraint is permanent. Theater ratio trajectory depends entirely on this — success maintains theater_ratio < 0.50, failure drives it toward 0.70+.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diversification_viability, empirical, 'Whether Canadian diversification can achieve structural independence from US asymmetry').

omega_variable(
    shared_threat_coordination_purity,
    'Do NORAD and NATO represent genuine collective defense (pure coordination) or do they primarily serve US strategic interests with Canadian participation as subordinate?',
    'Structural analysis of decision-making authority in NORAD/NATO; comparison of strategic priorities as declared by US vs. Canada vs. institutional positions; assessment of burden-sharing equity vs. benefits distribution; case studies of conflicts between US strategy and Canadian security preferences',
    'If genuinely shared threat coordination: Rope perspective dominates and constraint is primarily cooperative. If primarily US strategic vehicle: Tangled Rope or Snare perspective dominates and constraint is primarily extractive. This determines whether beneficiary_extracted value is 0.10-0.25 (rope) or 0.45-0.65 (tangled_rope/snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(shared_threat_coordination_purity, empirical, 'Whether defense integration serves shared threat response or US strategic hegemony').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_canada_geopolitical_asymmetry, 1990, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usca_tr_t0, us_canada_geopolitical_asymmetry, theater_ratio, 0, 0.62).
narrative_ontology:measurement(usca_tr_t5, us_canada_geopolitical_asymmetry, theater_ratio, 5, 0.55).
narrative_ontology:measurement(usca_tr_t10, us_canada_geopolitical_asymmetry, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(usca_be_t0, us_canada_geopolitical_asymmetry, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(usca_be_t5, us_canada_geopolitical_asymmetry, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(usca_be_t10, us_canada_geopolitical_asymmetry, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_canada_geopolitical_asymmetry, enforcement_mechanism).
narrative_ontology:affects_constraint(us_canada_geopolitical_asymmetry, arctic_resource_competition).
narrative_ontology:affects_constraint(us_canada_geopolitical_asymmetry, north_american_supply_chain_dependence).
narrative_ontology:affects_constraint(us_canada_geopolitical_asymmetry, us_sanctions_coordination_requirement).
narrative_ontology:affects_constraint(us_canada_geopolitical_asymmetry, canadian_technology_sovereignty).

% DUAL FORMULATION NOTE:
% The US-Canada geopolitical asymmetry decomposes into several constraint families: (1) Resource extraction constraints (Arctic, rare earth, agricultural exports) with different ε profiles; (2) Supply chain dependencies (integrated manufacturing, energy, semiconductors) that each have independent extractiveness measures; (3) Defense integration specifics (NORAD command authority, NATO burden-sharing) with distinct enforcement mechanisms; (4) Technology sovereignty constraints (5G/Huawei, cloud infrastructure, AI standards) with rapidly changing ε values. The parent constraint (us_canada_geopolitical_asymmetry) is upstream of all these; its ε=0.52 represents the general asymmetry. Specific domain constraints may have different ε values (e.g., technology sovereignty might be ε=0.68 due to active decoupling enforcement; Arctic resources might be ε=0.45 due to symmetrical competition rather than pure extraction). Each domain constraint receives its own story and links back to this parent via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
