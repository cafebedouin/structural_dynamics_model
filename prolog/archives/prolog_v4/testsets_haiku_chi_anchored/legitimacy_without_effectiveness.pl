% ============================================================================
% CONSTRAINT STORY: legitimacy_without_effectiveness
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_without_effectiveness, []).

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
 *   constraint_id: legitimacy_without_effectiveness
 *   human_readable: The Hollow Mandate
 *   domain: political/governance
 *
 * SUMMARY:
 *   The Hollow Mandate describes a structural constraint that arises when a
 *   state retains legal and social recognition as the legitimate authority
 *   but has lost the capacity to fulfill its primary functions — security,
 *   infrastructure, economic stability, justice administration. This
 *   condition is neither total state collapse (which triggers international
 *   intervention) nor functional governance (which sustains legitimacy
 *   through real delivery). Instead, the state apparatus persists through a
 *   hybrid mechanism: international recognition gatekeeping sustains formal
 *   legitimacy; incumbent elites extract resources through the legitimacy
 *   claim itself (taxation, debt servicing, privatization terms); the general
 *   population remains trapped because exit is costly and no alternative
 *   authority structure has emerged. The constraint exhibits all six DR types
 *   depending on the observer's structural position. The hollow state is
 *   simultaneously a snare for service-dependent citizens, a tangled rope for
 *   opposition movements, a rope for international creditors, a scaffold for
 *   reform-minded technocrats, a piton for the state apparatus itself
 *   (maintained through ritual), and a false mountain for those who
 *   naturalize the Westphalian separation of formal legitimacy from
 *   functional capacity. The theater ratio has risen from 0.48 (when services
 *   were degrading but state apparatus still performed some functions) to
 *   0.81 (when state legitimacy persists almost entirely through ceremonial
 *   authority and international recognition). The constraint is not
 *   mandatrophy-resolved because the gap between coordination benefit and
 *   extraction remains structurally ambiguous: the state does provide some
 *   minimal coordination (legal framework, currency, international
 *   recognition), but this coordination benefit is dwarfed by the extraction
 *   (taxation of destroyed infrastructure, debt service on loans for failed
 *   development) and the performativity (state ceremonies and institutions
 *   without real function).
 *
 * KEY AGENTS:
 *   - Service-Dependent Citizens: Primary victims (powerless/trapped) — bear full cost of state's inability to deliver security, water, electricity, healthcare; cannot exit the polity; required to remit taxes/rents to state that provides no reciprocal services
 *   - Incumbent State Apparatus: Primary beneficiary (institutional/arbitrage) — captures legitimacy-based rents (taxation, debt accumulation, international credit access); can reallocate resources to private gain via corruption and patronage; maintains position through formal authority claim despite functional collapse
 *   - International Legitimacy Gatekeepers: Secondary beneficiary (institutional/arbitrage) — IMF, World Bank, foreign governments, UN recognition apparatus sustain state's credibility; enable debt servicing, conditionality compliance, investment terms favorable to external capital
 *   - Organized Opposition Movement: Secondary victim (moderate/constrained) — face security risks from state apparatus despite its weakness; constrained by lack of alternative authority; benefit from mobilization opportunities created by legitimacy void
 *   - Reform-Oriented Technocrats: Tertiary actor (organized/constrained) — see hollowness as temporary design failure; advocate institutional reform and capacity-building; partially benefit from reform projects; constrained by state apparatus resistance
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the formal/functional decoupling as an inherent feature of sovereignty rather than a contingent institutional failure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_without_effectiveness, 0.58).
domain_priors:suppression_score(legitimacy_without_effectiveness, 0.68).
domain_priors:theater_ratio(legitimacy_without_effectiveness, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_without_effectiveness, extractiveness, 0.58).
narrative_ontology:constraint_metric(legitimacy_without_effectiveness, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(legitimacy_without_effectiveness, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_without_effectiveness, tangled_rope).
narrative_ontology:human_readable(legitimacy_without_effectiveness, "The Hollow Mandate").
narrative_ontology:topic_domain(legitimacy_without_effectiveness, "political/governance").

domain_priors:requires_active_enforcement(legitimacy_without_effectiveness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_without_effectiveness, incumbent_state_apparatus).
narrative_ontology:constraint_beneficiary(legitimacy_without_effectiveness, institutional_creditors).
narrative_ontology:constraint_beneficiary(legitimacy_without_effectiveness, international_legitimacy_gatekeepers).
narrative_ontology:constraint_victim(legitimacy_without_effectiveness, general_population).
narrative_ontology:constraint_victim(legitimacy_without_effectiveness, service_dependent_groups).
narrative_ontology:constraint_victim(legitimacy_without_effectiveness, fiscal_accountability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SERVICE-DEPENDENT CITIZEN (SNARE) — Cannot exit the governance structure; depends on services (water, electricity, police, healthcare) that the state claims to provide but cannot. Trapped within a polity that collects taxes/rents under the legitimacy claim while failing delivery. d≈0.92, f(d)≈1.40, σ=1.0 → χ≈0.81.
constraint_indexing:constraint_classification(legitimacy_without_effectiveness, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ORGANIZED OPPOSITION (TANGLED ROPE) — Constrained by security risks and lack of alternative authority structure, but also benefits from the hollowness: legitimacy void enables political mobilization and coalition-building. d≈0.68, f(d)≈1.05, σ=1.0 → χ≈0.61.
constraint_indexing:constraint_classification(legitimacy_without_effectiveness, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INTERNATIONAL LEGITIMACY GATEKEEPER (ROPE) — IMF, World Bank, foreign governments, UN recognition maintain the state's formal status despite dysfunction. Benefits from coordination: state apparatus continues extracting resources (via debt servicing, privatization, FDI terms) even as it hollows out. Recognizes the state as credible partner. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.06.
constraint_indexing:constraint_classification(legitimacy_without_effectiveness, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REFORM-ORIENTED TECHNOCRATS (SCAFFOLD) — Institutional reformers (independent agency heads, NGO administrators, local governance innovators) see the hollowness as a temporary design failure with a sunset: capacity-building, decentralization, and new institutional arrangements can restore effectiveness. d≈0.42, f(d)≈0.42, σ=1.0 → χ≈0.24.
constraint_indexing:constraint_classification(legitimacy_without_effectiveness, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: STATE APPARATUS (PITON) — Maintains performative functionality (ceremonies, symbolism, bureaucratic theater) despite loss of real capacity. The state as institution persists through inertia and international recognition, not through actual service delivery or security provision. theater_ratio=0.81 exceeds piton threshold (≥0.70). The apparatus knows it is degraded but is locked into perpetuating the hollow ritual.
constraint_indexing:constraint_classification(legitimacy_without_effectiveness, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / WESTPHALIAN VIEW (MOUNTAIN) — From a civilizational view, the sovereignty system requires that legitimacy and effectiveness sometimes decouple: formal statehood is legally defined (treaties, recognition), not functionally defined (capacity to deliver). A state can be legitimate yet ineffective because Westphalian law defines legitimacy through recognition, not through results. However, the structural data (ε=0.58, suppression=0.68, theater=0.81) contradicts the mountain classification — the engine will detect this as a false summit, revealing that the legal/functional decoupling is a contingent institutional choice, not a law of nature.
constraint_indexing:constraint_classification(legitimacy_without_effectiveness, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_without_effectiveness_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(legitimacy_without_effectiveness, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(legitimacy_without_effectiveness, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_without_effectiveness, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(legitimacy_without_effectiveness, TR),
    TR >= 0.70.

:- end_tests(legitimacy_without_effectiveness_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The state extracts through taxation (resource collection) despite failing to provide services, and through debt servicing (fiscal resources redirected to international creditors) despite economic collapse. The extraction is significant but not maximal because some minimal state functions persist (legal framework, currency, international representation), providing weak coordination benefit that prevents pure snare classification at the national aggregate level. However, individual service-dependent populations experience near-maximal extraction (snare perspective). Suppression (0.68): High. Citizens have severely limited exit options — cannot easily migrate (visa barriers, economic barriers), cannot opt out of taxation (legal enforcement), cannot withdraw from currency system without unacceptable cost. Informal alternative authorities exist but are fragmented and often predatory themselves. The suppression is substantial but not total because informal governance structures do provide some escape routes and because international pressure (however weak) creates some accountability mechanisms. Theater ratio (0.81): Very high. The state apparatus persists primarily through performative functions: ceremonies of legitimacy (state holidays, official symbols, bureaucratic theater), international recognition theater (summits, treaty signings, UN representation), and performative institutions (legislatures, courts, agencies) that lack real enforcement capacity. The theater has risen over the measurement interval as actual service delivery has collapsed — the gap between legitimacy claim and functional reality has widened. Claimed type (Tangled Rope): The constraint exhibits genuine coordination function (legal framework, currency, international representation enable some economic activity) AND asymmetric extraction (incumbent elites capture legitimacy-based rents while citizens bear service failure costs). Both coordination and extraction are required for the hybrid classification.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full range of DR classification from a single set of structural metrics, showing how indexical position determines observed type. Service-dependent citizens perceive a snare — they are trapped in a system that extracts resources while delivering nothing in return. The international legitimacy gatekeeper perceives a rope — from their position, the hollow state is a functioning coordination mechanism that enables debt servicing and resource flow predictability. The state apparatus itself perceives a piton — it recognizes its own functions as largely performative but maintains the ritual because exit (losing international recognition) would be catastrophic. The reform-oriented technocrat perceives a scaffold — they believe the hollowness is a temporary institutional failure that can be remedied through capacity-building and sunset clauses. The organized opposition perceives a tangled rope — the state's weakness creates both constraints (security risks) and opportunities (mobilization pathways). The civilizational analytical observer risks perceiving a mountain — naturalizing the legitimacy/capacity decoupling as an inherent feature of Westphalian sovereignty rather than a contingent institutional failure. The perspectival gap reveals that the 'hollowness' is not an objective property of the state, but a relational property: the state is hollow for those depending on services, functional for those extracting rents through legitimacy claims, and degraded for those maintaining its institutions.
 *
 * DIRECTIONALITY LOGIC:
 *   Service-dependent citizens: Victim + trapped → d≈0.92, f(d)≈1.40. Maximal extraction because citizens cannot exit and bear full cost of state collapse. Incumbent state apparatus: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary because they can reallocate legitimacy-based resources to private gain and can exit via capital flight or international relocation. International legitimacy gatekeeper: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Benefits from predictable debt servicing and resource flow. Organized opposition: Victim + constrained (but with some mobility through alliance-building) → d≈0.68, f(d)≈1.05. Significant extraction (security constraints, limited authority alternatives) but not maximal (can organize, can mobilize). Reform-oriented technocrats: Mixed + constrained → d≈0.42, f(d)≈0.42. Moderate directionality because they both experience institutional constraints and benefit from reform opportunities. State apparatus as agent: Institutional + arbitrage → d≈0.08, f(d)≈-0.10. Piton classification comes from theater gate (high performativity), not from high chi (which would indicate a powerful beneficiary). The apparatus is degraded, not powerful.
 *
 * MANDATROPHY ANALYSIS:
 *   UNRESOLVED MANDATROPHY: This constraint presents a genuine classification ambiguity that cannot be resolved without additional structural data. The core question is whether the state's legitimacy claim constitutes a genuine coordination mechanism (making the constraint a tangled rope or scaffold) or whether it is pure extraction enabled by international institutional gatekeeping (making it a snare with elevated theater). The measurement data shows theater ratio rising from 0.48 to 0.81 and extractiveness rising from 0.35 to 0.58 over 20 years. If the theater ratio continues rising and extractiveness plateaus or declines, the constraint is degrading from tangled rope toward piton (former coordination becoming pure ritual). If extractiveness continues rising while theater stabilizes, the constraint is consolidating as a snare that uses legitimacy theater as its primary suppression mechanism. The omega variables (especially 'mandate_hollowing_directionality' and 'international_recognition_independence') must be resolved empirically before final classification. The tentative tangled_rope classification assumes that the international legitimacy gatekeeping provides some genuine coordination value (enabling debt instruments, investment, currency credibility), but if this gatekeeping is revealed as pure extraction enablement (conditioning on privatization, austerity, resource extraction), the constraint should reclassify to snare. Conversely, if reform-oriented technocrats successfully rebuild state capacity within 10-15 years (resolving the scaffold perspective), the constraint will degrade toward rope. The hollow mandate's classification is therefore perspectival and temporal: which position you occupy (and when you measure) determines the observed type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_collapse_threshold,
    'At what level of service failure does formal legitimacy cease to function as extraction mechanism?',
    'Cross-national comparison of state capacity indices vs. citizen compliance with taxation and legal authority; tracking of delegitimization events (mass protests, exit of compliance) relative to service provision metrics',
    'If collapse occurs early (≤20% capacity): snare classification becomes mountain (trapped citizens realize no coordination benefit exists). If collapse occurs late (≥60% capacity): tangled_rope classification persists longer (some coordination benefits sustain cooperation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_collapse_threshold, empirical, 'Threshold at which formal legitimacy loses extractive force').

omega_variable(
    international_recognition_independence,
    'Does international legitimacy gatekeeping (UN seat, IMF status, foreign aid) constitute genuine coordination benefit or pure extraction enablement?',
    'Analysis of international support conditions and enforcement; tracking whether international legitimacy enables external extraction (debt service, conditionality compliance) vs. enables internal recovery (technical assistance, institutional capacity-building)',
    'If pure enablement of extraction: international gatekeeper should reclassify to snare. If mixed: rope classification confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(international_recognition_independence, empirical, 'Whether international legitimacy support enables extraction or recovery').

omega_variable(
    informal_authority_substitution,
    'Do informal governance structures (tribal authority, criminal syndicates, community organizations) constitute alternative effective authority that reduces the hollow mandate''s extractive grip?',
    'Ethnographic and structural analysis of service delivery, taxation, and dispute resolution outside the formal state; comparison of effective authority distribution with formal legitimacy distribution',
    'If informal alternatives provide significant service delivery: suppression value is lower (exit options improve), and the constraint degrades toward scaffold (temporary hollow state). If informal authority exploits the hollow state: suppression increases (trapped citizens have no alternatives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(informal_authority_substitution, empirical, 'Extent to which informal authority structures provide alternative governance').

omega_variable(
    mandate_hollowing_directionality,
    'Is the hollowing a deliberate extraction mechanism (state deliberately underfunds public goods to privatize rents), or is it structural degradation (capacity loss due to war, economic collapse, institutional decay)?',
    'Historical analysis of state budget allocation, intentionality of public sector erosion, correlation between privatization cycles and service collapse; interview evidence from state apparatus regarding knowledge of hollowness',
    'If deliberate extraction: extractiveness should increase (ε→0.65+), tangled_rope confirms as hybrid of coordination facade + rent extraction. If structural: extractiveness may be lower if state apparatus is genuinely trying to recover (ε→0.45), scaffold becomes more probable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_hollowing_directionality, empirical, 'Whether hollowing is deliberate extraction or structural degradation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_without_effectiveness, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hollow_tr_t0, legitimacy_without_effectiveness, theater_ratio, 0, 0.48).
narrative_ontology:measurement(hollow_tr_t10, legitimacy_without_effectiveness, theater_ratio, 10, 0.65).
narrative_ontology:measurement(hollow_tr_t20, legitimacy_without_effectiveness, theater_ratio, 20, 0.81).

% Extraction over time
narrative_ontology:measurement(hollow_be_t0, legitimacy_without_effectiveness, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hollow_be_t10, legitimacy_without_effectiveness, base_extractiveness, 10, 0.47).
narrative_ontology:measurement(hollow_be_t20, legitimacy_without_effectiveness, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_without_effectiveness, enforcement_mechanism).
narrative_ontology:affects_constraint(legitimacy_without_effectiveness, currency_hyperinflation).
narrative_ontology:affects_constraint(legitimacy_without_effectiveness, informal_authority_substitution).
narrative_ontology:affects_constraint(legitimacy_without_effectiveness, conditional_lending_sovereignty_loss).

% DUAL FORMULATION NOTE:
% The hollow mandate is distinct from but structurally related to currency hyperinflation (downstream: state's inability to maintain fiscal discipline causes currency collapse) and conditional lending arrangements (upstream: international creditors' legitimacy gatekeeping sustains hollowed state as extractive vehicle). The informal authority substitution is co-produced by the hollowness: as state capacity fails, informal structures expand to fill governance void. These constraints form a causal chain but have distinct ε values and classification types.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimacy_without_effectiveness, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
