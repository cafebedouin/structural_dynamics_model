% ============================================================================
% CONSTRAINT STORY: preparedness_retention__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_retention__hybrid_reading, []).

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
    narrative_ontology:cs_story_uid/2,
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
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: preparedness_retention__hybrid_reading
 *   human_readable: Preparedness Retention—Hybrid Stratified Model (Competence Centralization with Ceremonial Periphery)
 *   domain: disaster_preparedness/institutional_memory/governance
 *
 * SUMMARY:
 *   Dutch and German water management institutions have evolved a two-tier
 *   system: core technical staff at Rijkswaterstaat, Rhine Commission, and
 *   specialized water boards maintain live expertise through continuous
 *   hydrological monitoring, dam operation, real-time sensor networks, and
 *   infrastructure inspection. Peripheral actors—local governments, community
 *   volunteer networks, school-based preparedness programs—participate in
 *   ceremonial drills, evacuation exercises, and preparedness communication
 *   campaigns that create the appearance of distributed readiness. The hybrid
 *   reading claims this stratification is not accidental but functional: it
 *   concentrates genuine competence where it is most needed while using
 *   ceremonial participation to maintain public confidence and distribute
 *   responsibility across institutional layers. The constraint is Tangled
 *   Rope because the system genuinely coordinates water management (core
 *   institutions + peripheral actors working together) while simultaneously
 *   extracting legitimacy benefit for core institutions (who position
 *   themselves as the only source of real competence) and imposing ceremonial
 *   burden on peripheral actors (who perform preparedness without real
 *   authority). The theater_ratio rises over the interval (0.55 → 0.68) as
 *   climate change increases extreme-event frequency, forcing more drills and
 *   messaging campaigns to compensate for actual infrastructure gaps.
 *   Extractiveness rises slowly (0.28 → 0.38) as the knowledge gap between
 *   core and peripheral actors widens—the more the system relies on
 *   ceremonial performance, the more benefit accrues to institutions
 *   controlling actual competence.
 *
 * KEY AGENTS:
 *   - Rijkswaterstaat and Core Water Boards: Institutional/arbitrage — maintain live hydrological expertise, conduct real-time monitoring, control early warning systems, benefit from concentrated authority
 *   - Local Governments and Regional Water Authorities: Organized/constrained — receive training and protocols from core institutions, implement cascade procedures, perform ceremonial readiness, dependent on technical information asymmetry
 *   - Community Preparedness Networks: Powerless/trapped — organize drills, distribute evacuation guidelines, maintain public readiness through ceremonies, have no independent verification capacity, bear full cost if core institutions fail
 *   - Public Communication and Media: Institutional/constrained — broadcast preparedness messages, amplify drill importance, create symbolic coherence, constrained by mandate to maintain public confidence without revealing knowledge gaps
 *   - EU Flood Management Standards: Powerful/mobile — international pressure for distributed competence and transparency, external sunset pressure on the centralized model
 *   - Analytical Observer: Analytical/analytical — risks naturalizing complexity as justifying centralization; sees possible false summit where institutional convenience is presented as technical necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_retention__hybrid_reading, 0.38).
domain_priors:suppression_score(preparedness_retention__hybrid_reading, 0.52).
domain_priors:theater_ratio(preparedness_retention__hybrid_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_retention__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_retention__hybrid_reading, "Preparedness Retention—Hybrid Stratified Model (Competence Centralization with Ceremonial Periphery)").
narrative_ontology:topic_domain(preparedness_retention__hybrid_reading, "disaster_preparedness/institutional_memory/governance").

domain_priors:requires_active_enforcement(preparedness_retention__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_retention__hybrid_reading, '1843e52e-e47b-43e3-97bb-97117c82bf0a').
narrative_ontology:cs_kernel_codification('1843e52e-e47b-43e3-97bb-97117c82bf0a', implicit).
narrative_ontology:cs_authority_grounding('1843e52e-e47b-43e3-97bb-97117c82bf0a', extraction).
narrative_ontology:cs_interpretation_layer_present('1843e52e-e47b-43e3-97bb-97117c82bf0a').
narrative_ontology:cs_reading_relation('1843e52e-e47b-43e3-97bb-97117c82bf0a', preparedness_retention__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('1843e52e-e47b-43e3-97bb-97117c82bf0a', preparedness_retention__husk_reading, coexists_with).
narrative_ontology:cs_axiom('1843e52e-e47b-43e3-97bb-97117c82bf0a', foundational, preparedness_stratified_by_institutional_layer).
narrative_ontology:cs_axiom_status(preparedness_stratified_by_institutional_layer, holdable).
narrative_ontology:cs_axiom_grounding('1843e52e-e47b-43e3-97bb-97117c82bf0a', preparedness_stratified_by_institutional_layer, empirically_contingent).
narrative_ontology:cs_axiom('1843e52e-e47b-43e3-97bb-97117c82bf0a', foundational, institutional_concentration_extracts_legitimacy).
narrative_ontology:cs_axiom_status(institutional_concentration_extracts_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('1843e52e-e47b-43e3-97bb-97117c82bf0a', institutional_concentration_extracts_legitimacy, empirically_contingent).
narrative_ontology:cs_reference_frame('1843e52e-e47b-43e3-97bb-97117c82bf0a', integrated_national_preparedness).
narrative_ontology:cs_drift_state('1843e52e-e47b-43e3-97bb-97117c82bf0a', contemporary_climate_pressure, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1843e52e-e47b-43e3-97bb-97117c82bf0a', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(preparedness_retention__hybrid_reading, preparedness_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_retention__hybrid_reading, core_technical_institutions).
narrative_ontology:constraint_beneficiary(preparedness_retention__hybrid_reading, institutional_continuity_apparatus).
narrative_ontology:constraint_victim(preparedness_retention__hybrid_reading, distributed_societal_resilience).
narrative_ontology:constraint_victim(preparedness_retention__hybrid_reading, peripheral_actors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISTRIBUTED COMMUNITIES (SNARE) — Peripheral actors (local governments, community volunteers, general population) participate in ceremonial drills and preparedness rituals that feel comprehensive but lack connection to live technical competence. Cannot exit or independently verify whether their preparedness is real. Trapped by geography and power asymmetry; bear the full cost if core institutions fail or knowledge gap emerges during actual crisis.
constraint_indexing:constraint_classification(preparedness_retention__hybrid_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REGIONAL WATER BOARDS AND LOCAL GOVERNMENT (TANGLED ROPE) — Occupy middle position: receive some technical transmission from core institutions (Rijkswaterstaat) but also bear ceremonial burdens disproportionate to their actual competence. Constrained by funding and technical dependency; experience both coordination (receiving training and protocols) and extraction (tasked with cascade implementation without true understanding). Some agency through incremental skill-building but locked into hierarchical information flow.
constraint_indexing:constraint_classification(preparedness_retention__hybrid_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RIJKSWATERSTAAT AND CORE TECHNICAL INSTITUTIONS (ROPE) — Primary beneficiary. Maintains live technical competence, conducts real inspections, operates genuine early warning systems. The stratified system serves them as coordination: peripheral actors perform ceremonial compliance while core institutions focus on actual hydrological monitoring and infrastructure maintenance. Experience this as pure coordination—their role is to enable the system to function. Arbitrage: can redirect competence to other institutional contexts (EU collaborations, international consulting) if domestic constraints tighten.
constraint_indexing:constraint_classification(preparedness_retention__hybrid_reading, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PUBLIC COMMUNICATION AND PREPAREDNESS MESSAGING (PITON) — Ceremonial apparatus. Government agencies, media, schools conduct flood drills, distribute preparedness guidelines, broadcast evacuation procedures. Theater ratio is high—these activities create visible legitimacy ('we are prepared') while actual emergency response capacity depends on specialized technical institutions. Constrained by mandate to maintain public confidence; theater persists through inertia rather than function. If community discovered that real competence is concentrated elsewhere, theater would collapse—maintained by not asking whether it works.
constraint_indexing:constraint_classification(preparedness_retention__hybrid_reading, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: INTERNATIONAL KNOWLEDGE TRANSFER AND EU FLOOD MANAGEMENT STANDARDS (SCAFFOLD) — External actor with sunset logic. EU Floods Directive and international best-practice frameworks (ICPDR Danube coordination, Rhine Commission) create gradual pressure to distribute competence beyond core institutions. The constraint's coherence depends on information isolation; as transnational coordination increases, the centralized competence model faces pressure to democratize. Sunset is implicit: as compliance pressure mounts, peripheral actors gain leverage to access technical knowledge directly, bypassing Rijkswaterstaat mediation.
constraint_indexing:constraint_classification(preparedness_retention__hybrid_reading, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER—NATURAL COMPLEXITY VIEW (MOUNTAIN) — From civilizational scope, complex flood systems require centralized expertise: hydrological modeling, real-time sensor networks, infrastructure engineering. Distributing competence creates inefficiency and delay. The stratified system reflects an immutable constraint: core knowledge cannot be fully distributed without losing coherence. However, this perspective naturalizes what the hybrid reading identifies as a contingent institutional choice. The engine will flag this as a false summit: centralization is presented as natural law but is actually a distribution of benefits that could be otherwise arranged.
constraint_indexing:constraint_classification(preparedness_retention__hybrid_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_retention__hybrid_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(preparedness_retention__hybrid_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(preparedness_retention__hybrid_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(preparedness_retention__hybrid_reading, TR),
    TR >= 0.70.

:- end_tests(preparedness_retention__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The system does coordinate water management—core institutions genuinely need to concentrate expertise, and peripheral actors benefit from coherent early warning. But the extraction is real: core institutions gain status, authority, and budget priority by positioning themselves as irreplaceable, while peripheral actors perform work (drills, messaging) that does not translate to genuine preparedness capacity. The extractiveness is not as high as a pure snare (0.66+) because the core competence is real and functional; it is Tangled Rope because the functional coordination is entangled with asymmetric extraction of legitimacy. Suppression (0.52): Moderate-high. Suppression comes from information asymmetry: peripheral actors cannot independently verify whether their preparedness is real because technical details are opaque (hydrological models, sensor networks, infrastructure assessments are controlled by core institutions). Institutional barriers prevent distributed competence: funding flows to core institutions, training is mediated through core institutions, decision-making authority is concentrated. Some escape is possible—EU standards, international networks, private consultants—but exit costs are high. Theater ratio (0.68): High and rising. Drills, evacuation exercises, preparedness campaigns, public messaging are largely performative: they create symbolic coherence and public confidence but do not address the underlying knowledge gap. As climate pressure increases (more extreme events, more uncertainty), peripheral actors conduct more drills to compensate—theater rises faster than actual competence. The trend from 0.55 to 0.68 reflects this: real competence grows slowly (influenced by recruitment, infrastructure investment, research), while ceremonial burden grows rapidly (response to climate anxiety, liability concerns, political pressure).
 *
 * PERSPECTIVAL GAP:
 *   The gap between the core institution perspective (Rope—pure coordination) and the peripheral perspective (Snare—pure extraction) is the diagnostic signature of the hybrid reading. Core institutions see the system as legitimate coordination: their expertise enables peripheral actors to participate meaningfully in a shared safety project. Peripheral actors experience extraction: their labor (drills, training participation, messaging dissemination) contributes nothing to their own preparedness capacity, only to the legitimacy of a system that concentrates expertise elsewhere. The piton perspective (ceremonial apparatus) shows that even the public communication layer recognizes its own theater—preparedness campaigns exist to maintain confidence, not to build actual capacity. The scaffold perspective (international pressure) suggests the system is not stable: EU standards and transnational coordination create gradual pressure to distribute competence, implying a sunset to the current stratification. The false-summit mountain perspective reveals the naturalization risk: framing centralization as a natural law of complex systems obscures that it is a distribution choice that benefits core institutions.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality (d) for each perspective derives from beneficiary/victim status and exit options. Core institutions are beneficiaries with arbitrage (can exit to consulting, EU roles, international markets)—low d, negative chi. Peripheral actors are victims with constrained or trapped exits (cannot independently build hydrological modeling capacity, cannot exit their geographic vulnerability)—high d, high chi. The organized water boards occupy middle ground (constrained exit, mixed beneficiary/victim role)—moderate d, moderate chi. The piton's constrained exit with no victim status (ceremonial apparatus has no external cost-bearing) produces low chi despite high theater. The difference between snare (powerless/trapped) and tangled_rope (organized/constrained) is captured in d: trapped agents experience higher d (0.95) than organized agents (0.40), producing the classification gap. The analytical mountain perspective's d is canonical analytical (0.73), high f(d), but the base extraction is presented as natural law—the engine's false summit detector flags the beneficiary declarations and triggers FSM evaluation.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading resolves mandatrophy by decomposing the contested kernel into layers. Rather than asking 'is preparedness real or ceremonial?', the reading claims both are true and asks how they are distributed. The core institution exercises real competence (competence_reading is true for them); peripheral actors perform ceremony (husk_reading is true for them). The system's coherence depends on this stratification being stable—if peripheral actors discovered they have no real preparedness capacity and core institutions recognized they depend on peripheral actors for legitimacy, the stratification would collapse into conflict. The hybrid reading captures this tension: it is Tangled Rope because both coordination (real need for centralized expertise) and extraction (real asymmetry in legitimacy and authority) are simultaneously operative. The mandatrophy is not 'which reading is correct' but 'how stable is the stratified system and at what cost?'—leading directly to the omegas about knowledge distribution possibility, single-point-of-failure risk, and ceremonial decay rates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    knowledge_distribution_possibility,
    'Is the concentration of technical competence in Rijkswaterstaat and core water boards structurally necessary for system coherence, or a contingent institutional choice?',
    'Comparative analysis: examine water management systems with distributed competence (e.g., Swiss canton-level autonomous systems, Swedish volunteer-led networks, Japanese community-based flood warning in smaller basins). Do distributed systems fail at measurable higher rates, or do they succeed at different performance-cost tradeoffs?',
    'If structurally necessary: mountain classification confirmed—distribution is impossible. If contingent: hybrid reading is a false summit naturalizing an institutional choice that serves core institutions. Central actors have incentive to maintain naturalization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(knowledge_distribution_possibility, empirical, 'Whether competence concentration is structurally necessary or contingent institutional choice').

omega_variable(
    single_point_of_failure_severity,
    'How does the vulnerability profile (concentrated expertise, single-point-of-failure risk) compare between the stratified system and a distributed competence alternative?',
    'Failure-mode analysis: historical cases where core institutions failed (understaffing, political capture, knowledge loss through retirement). Compare actual outcomes to scenarios where peripheral actors had independent verification capacity. Monte Carlo simulation of competence loss under recruitment/retirement/organizational disruption.',
    'If concentrated model is more robust: stratification is justified and extraction is reduced. If distributed model is more robust: extraction is larger than measured—system design choice increases fragility to benefit institutional continuity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(single_point_of_failure_severity, empirical, 'Single-point-of-failure risk in stratified vs distributed preparedness models').

omega_variable(
    ceremonial_competence_decay_rate,
    'At what rate does peripheral-actor competence decay when disconnected from live technical practice? Does ceremonial participation preserve any functional knowledge or is it purely performative?',
    'Longitudinal testing: measure actual response capacity of local governments and volunteers after varying durations without core-institution contact. Compare drills with real incidents. Track knowledge retention curves for peripheral actors under isolation (pandemic lockdowns, disrupted training cycles).',
    'If decay is rapid and complete: peripheral resilience is illusory, extraction is severe. If some competence persists: system has redundancy value the hybrid reading under-estimates. Theater ratio implications.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ceremonial_competence_decay_rate, empirical, 'Decay rate and functional residue of ceremonial preparedness practice').

omega_variable(
    reading_contest_empirical_window,
    'Which sibling reading (competence_reading, husk_reading, or this hybrid_reading) best predicts crisis outcomes and hidden vulnerabilities when tested against historical flood events in Dutch and German water management?',
    'Post-hoc analysis: select 5-10 significant flood events (1995 Rhine flooding, 2002 summer floods, 2013 Danube events, recent climate-driven extremes). Code each event''s outcomes against predicted failure modes of each reading. Which reading''s vulnerabilities appear in the actual incident reports?',
    'If competence_reading best predicts: hybrid reading overstates the ceremonial component—system is more robust. If husk_reading best predicts: hybrid reading overstates core institution capability—system is more fragile than measured. If hybrid_reading best predicts: classification stands. If mixed: readings coexist without empirical resolution; uncertainty remains.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_empirical_window, empirical, 'Which preparedness reading best predicts crisis outcomes in historical flood events').

omega_variable(
    kernel_framing_contest,
    'Is ''preparedness'' best understood as a contested kernel with multiple simultaneous readings (hybrid reading), or do the readings foreclose each other (one reading is true, others are false)?',
    'Structural analysis: the hybrid reading claims all three readings coexist—core competence is live (competence_reading), peripheral participation is ceremonial (husk_reading), and the system is stratified (hybrid_reading). This assumes preparedness can be simultaneously real and performative at different levels. But the competence_reading and husk_reading each make universal claims (''preparedness IS live exercised knowledge'' vs ''preparedness IS memorial performance''). Do these claims foreclosure each other, or do they apply to different layers of a single institution?',
    'If readings foreclose: the kernel is not stratified but contested; one reading is true, the others are false. If readings coexist at different institutional levels: hybrid reading is correct, and the committer frame (multiple readings of a single kernel) applies. This determines engine behavior for cross-reading coupling and foreclosure detection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_contest, conceptual, 'Whether preparedness readings coexist (hybrid) or foreclose (contested) each other').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_retention__hybrid_reading, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_hybrid_tr_t0, preparedness_retention__hybrid_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(prep_hybrid_tr_t4, preparedness_retention__hybrid_reading, theater_ratio, 4, 0.62).
narrative_ontology:measurement(prep_hybrid_tr_t8, preparedness_retention__hybrid_reading, theater_ratio, 8, 0.68).

% Extraction over time
narrative_ontology:measurement(prep_hybrid_be_t0, preparedness_retention__hybrid_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(prep_hybrid_be_t4, preparedness_retention__hybrid_reading, base_extractiveness, 4, 0.34).
narrative_ontology:measurement(prep_hybrid_be_t8, preparedness_retention__hybrid_reading, base_extractiveness, 8, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(prep_hybrid_su_t0, preparedness_retention__hybrid_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(prep_hybrid_su_t4, preparedness_retention__hybrid_reading, suppression_requirement, 4, 0.49).
narrative_ontology:measurement(prep_hybrid_su_t8, preparedness_retention__hybrid_reading, suppression_requirement, 8, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_retention__hybrid_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(preparedness_retention__hybrid_reading, preparedness_retention__competence_reading).
narrative_ontology:affects_constraint(preparedness_retention__hybrid_reading, preparedness_retention__husk_reading).

% DUAL FORMULATION NOTE:
% The preparedness_retention kernel decomposes into three structurally distinct constraints: competence_reading (ε ≈ 0.15, Mountain—real expertise is irreducible), husk_reading (ε ≈ 0.62, Snare—ceremonial participation without competence), and hybrid_reading (ε ≈ 0.38, Tangled Rope—stratified system with both real and ceremonial components). The hybrid reading is downstream of the reading contest itself: its coherence depends on both sibling readings being partially true at different institutional levels. If empirical testing resolves which reading best predicts crisis outcomes, the hybrid reading may collapse into one of its siblings. Network links enable contamination propagation: if core institutional competence degrades (affecting competence_reading), the hybrid stratification fails and the system shifts toward husk_reading (pure ceremony). If peripheral actors gain independent competence (external pressure from EU standards), the stratification collapses toward competence_reading (distributed real preparedness).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(preparedness_retention__hybrid_reading, institutional, 0.15).
constraint_indexing:directionality_override(preparedness_retention__hybrid_reading, organized, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
