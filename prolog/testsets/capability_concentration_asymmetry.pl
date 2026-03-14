% ============================================================================
% CONSTRAINT STORY: capability_concentration_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_capability_concentration_asymmetry, []).

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
 *   constraint_id: capability_concentration_asymmetry
 *   human_readable: Capability Concentration Asymmetry
 *   domain: general/institutional_dynamics
 *
 * SUMMARY:
 *   Capability concentration asymmetry describes the structural constraint
 *   where access to critical capabilities (technical knowledge,
 *   infrastructure, capital, credentialing authority) is concentrated among
 *   institutions or agents who extract asymmetric value from those seeking
 *   capability accumulation. This constraint exhibits characteristics of pure
 *   extraction (Snare from the powerless perspective) but combines genuine
 *   coordination functions (maintaining standards, infrastructure, ecosystem
 *   interoperability) with enforcement of asymmetric access. The constraint
 *   operates across multiple domains — technological capability, financial
 *   access, educational credentials, institutional authority — and manifests
 *   at scales ranging from interpersonal mentorship asymmetries to global
 *   institutional gatekeeping. The core mechanism: capability concentrators
 *   benefit from maintaining scarcity and asymmetric access terms, while
 *   resource-constrained agents must accept extractive terms to access the
 *   capabilities they need for participation. Theater has increased over the
 *   measurement interval as formal credentialism (ritual institutional
 *   authority) has increasingly decoupled from actual capability, yet the
 *   ritual persists as an enforcement mechanism.
 *
 * KEY AGENTS:
 *   - Capability Concentrators: Primary beneficiaries (institutional/arbitrage) — maintain control over access pathways; extract asymmetric value through licensing, credentialing, proprietary knowledge; can arbitrage capabilities across multiple domains
 *   - Resource-Constrained Agents: Primary victims (powerless/trapped) — cannot accumulate capabilities without accepting asymmetric access terms; lack capital, prior access, or foundational knowledge; experience maximum extraction
 *   - Competing Capability Developers: Secondary victims (moderate/constrained) — face high development costs and patent/licensing barriers; can accumulate capabilities but at generational timescales; derive some coordination benefits from shared infrastructure
 *   - Decentralization Coalition: Organized agents (organized/mobile) — open-source communities, peer-to-peer networks, alternative educational platforms; building exit pathways that reduce dependency on institutional concentrators; suppression is declining as alternatives mature
 *   - Legacy Institutional Gatekeepers: Institutional actors (institutional/arbitrage) — maintain formal authority through credentialism and certification even as functional necessity has declined; perception of own role as vestigial but continuing enforcement through institutional inertia
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing institutional concentration as inherent technical requirement, obscuring contingent enforcement mechanisms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(capability_concentration_asymmetry, 0.68).
domain_priors:suppression_score(capability_concentration_asymmetry, 0.72).
domain_priors:theater_ratio(capability_concentration_asymmetry, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(capability_concentration_asymmetry, extractiveness, 0.68).
narrative_ontology:constraint_metric(capability_concentration_asymmetry, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(capability_concentration_asymmetry, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(capability_concentration_asymmetry, snare).
narrative_ontology:human_readable(capability_concentration_asymmetry, "Capability Concentration Asymmetry").
narrative_ontology:topic_domain(capability_concentration_asymmetry, "general/institutional_dynamics").

domain_priors:requires_active_enforcement(capability_concentration_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(capability_concentration_asymmetry, capability_concentrators).
narrative_ontology:constraint_beneficiary(capability_concentration_asymmetry, institutional_gatekeepers).
narrative_ontology:constraint_victim(capability_concentration_asymmetry, capability_dispersed_agents).
narrative_ontology:constraint_victim(capability_concentration_asymmetry, resource_constrained_populations).
narrative_ontology:constraint_victim(capability_concentration_asymmetry, emergent_capability_competitors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RESOURCE-CONSTRAINED AGENT (SNARE) — Cannot accumulate capabilities without accepting asymmetric terms. Trapped by lack of capital, access, or foundational resources. Experiences maximum extraction: must cede disproportionate value to access capability infrastructure controlled by concentrators. No meaningful exit path within biographical time.
constraint_indexing:constraint_classification(capability_concentration_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COMPETING CAPABILITY DEVELOPER (TANGLED ROPE) — Faces high but surmountable barriers to capability accumulation. Derives genuine coordination benefits from shared infrastructure (standards, protocols, foundational knowledge) while simultaneously bearing asymmetric extraction: must license, patent-navigate, or reverse-engineer. Exit is possible at generational scale but constrained by development timelines and market lock-in.
constraint_indexing:constraint_classification(capability_concentration_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: CAPABILITY CONCENTRATOR (ROPE) — Experiences the constraint as pure coordination: maintaining capability concentration requires solving collective action problems around standards, interoperability, and ecosystem health. High exit optionality — can arbitrage capability advantages across domains and geographies. Extraction flows toward this agent, not away; classification remains Rope because the primary function is coordination, not coercion.
constraint_indexing:constraint_classification(capability_concentration_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DECENTRALIZATION COALITION (SCAFFOLD) — Organized agents (open-source communities, decentralized networks, alternative capability platforms) are building exit pathways that reduce dependency on concentrators. High suppression currently, but suppression is declining as alternatives mature. Sees the concentration asymmetry as temporary, with a generational sunset clause as distributed capability accumulation becomes viable.
constraint_indexing:constraint_classification(capability_concentration_asymmetry, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY INSTITUTIONAL GATEKEEPER (PITON) — Maintains formal gatekeeping authority through inertia and ritual (credentials, certification, institutional affiliation) even as capability concentration mechanisms have substantially degraded. Theater ratio is moderate-high: credentialism persists despite declining correlation with actual capability. The gatekeeper perceives its own role as vestigial but continues enforcement through institutional momentum.
constraint_indexing:constraint_classification(capability_concentration_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FALSE NATURAL LAW VIEW (MOUNTAIN) — Risks classifying capability concentration as an inevitable natural law: 'complex capabilities require centralized coordination,' 'specialization demands hierarchy,' 'scale requires concentration.' This perspective naturalizes what is actually a contingent institutional arrangement. The engine's false summit detector will identify this mountain as spurious because structural data shows the concentration is maintained through enforcement, not through immutable physical/logical limits.
constraint_indexing:constraint_classification(capability_concentration_asymmetry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(capability_concentration_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(capability_concentration_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(capability_concentration_asymmetry, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(capability_concentration_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(capability_concentration_asymmetry, TR),
    TR >= 0.70.

:- end_tests(capability_concentration_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and increasing. The constraint concentrates control over capability access and extracts asymmetric value through licensing, credentialing, proprietary lock-in, and opportunity gate-keeping. The measured increase from 0.52 to 0.68 reflects intensifying concentration as digital and institutional barriers have become more sophisticated. However, the value is not maximal (0.90+) because alternative accumulation pathways are viable at longer timescales and some capabilities are becoming increasingly distributed. Suppression (0.72): High. Significant barriers to independent capability accumulation include: capital requirements for infrastructure, prior knowledge prerequisites, credentialing requirements, network effects, patent/IP restrictions, institutional access controls, and social/economic penalties for bypassing formal pathways. But suppression is not absolute — determined agents can and do accumulate capabilities outside formal pathways, though at higher personal cost. Theater ratio (0.58): Moderate-high and increasing. Credentialism has become increasingly performative as institutional credentials show declining correlation with actual capability in many domains. Formal gatekeeping (degrees, certifications, institutional affiliation) persists despite diminished predictive power, suggesting ritualization. The theater increase reflects growing disconnect between formal authority and functional necessity.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence. The resource-constrained agent sees a Snare: inescapable extraction with no exit. The competing developer sees a Tangled Rope: genuine benefits from shared infrastructure (standards, foundational knowledge) combined with significant asymmetric extraction. The concentrator sees a Rope: solving the coordination problem of maintaining capability access and ecosystem health. The decentralization coalition sees a Scaffold: a temporary institutional arrangement with a generational sunset as alternative pathways mature. The legacy gatekeeper sees a Piton: formal authority maintained through ritual despite declining functional necessity. The civilizational analyst risks seeing a Mountain: 'capability coordination inevitably requires concentration' — but the structural data reveals this as a false summit. The constraint is maintained through enforcement, not through immutable limits.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from the agent's structural position in the extraction flow. Capability concentrators (beneficiaries with arbitrage exit) have low d (~0.1-0.2) — extraction flows away from them. Resource-constrained victims with trapped exit have high d (~0.85-0.95) — maximum experienced extraction. Competing developers (moderate power, constrained exit) have mid-range d (~0.55-0.65) — mixed extraction with some coordination benefits. Organized decentralization agents (mobile exit) have moderate d (~0.35-0.45) — suppression is high but they can perceive and articulate exit pathways. Legacy gatekeepers (beneficiary institutional actors) have low d (~0.10-0.20) — extraction benefits accrue to them, but they are constrained by inertia and declining functional necessity. The analytical observer has neutral d (~0.72) typical of meta-level analysis.
 *
 * MANDATROPHY ANALYSIS:
 *   PARTIAL RESOLUTION: The capability concentration asymmetry resolves the mandatrophy by exposing how institutional perspective creates classification divergence. The Snare classification (primary type) is the most structural reading: resource-constrained victims cannot exit and bear maximum extraction. However, the Tangled Rope and Scaffold perspectives are not merely perceptual distortions — they identify genuine structural features (coordination benefits exist, exit pathways are viable at longer timescales). The Piton classification captures theatrical degradation: credentialism persists without functional necessity. The false Mountain reveals the naturalizing impulse: treating contingent institutional arrangements as immutable laws. Mandatrophy is not fully resolved because the constraint exhibits simultaneous Snare (for trapped victims), Tangled Rope (mixed coordination and extraction), Scaffold (declining suppression, viable sunset), and Piton (rising theater) characteristics. This simultaneous multi-type classification indicates that the constraint is transitional: currently a Snare maintained by high suppression and extractive enforcement, but evolving toward Piton (ritualized) or toward lower extractiveness as alternatives mature. The omega variables identify the key empirical questions that would resolve which trajectory is occurring.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    concentration_necessity_threshold,
    'What proportion of current capability concentration is technically necessary vs. institutionally maintained?',
    'Comparative analysis of capability accumulation in decentralized networks (open-source, peer-to-peer) vs. centralized institutions; measurement of redundancy and coordination overhead in both systems',
    'If > 60% is institutionally maintained: concentration asymmetry is primarily a Snare. If > 60% is technically necessary: concentration is closer to a natural law with coordination benefits (Rope). Current estimates suggest 50-70% is institutional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(concentration_necessity_threshold, empirical, 'Technical necessity vs institutional maintenance of capability concentration').

omega_variable(
    alternative_accumulation_viability,
    'Can resource-constrained agents meaningfully accumulate capabilities outside concentrator-controlled pathways within a generational timescale?',
    'Longitudinal tracking of capability development in alternative ecosystems (open-source, community-driven, decentralized); comparison of time-to-parity vs institutional pathway time',
    'If viable at < 20 years: scaffold sunset is real and suppression is declining. If viable at > 50 years: suppression remains high and mountain classification risk increases. Current evidence suggests 15-40 year window.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_accumulation_viability, empirical, 'Viability of alternative capability accumulation pathways').

omega_variable(
    capability_definition_ambiguity,
    'Does ''capability concentration'' refer to knowledge, infrastructure, capital access, institutional authority, or some weighted combination?',
    'Decomposition of the constraint by capability type; separate analysis for each domain (technical knowledge, infrastructure, financial access, credentialing authority)',
    'If primarily knowledge: decentralization is more feasible. If primarily infrastructure/capital: concentration may be more structurally necessary. If primarily credentialing authority: concentration is purely institutional.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capability_definition_ambiguity, conceptual, 'Specification of which type of capability is being concentrated').

omega_variable(
    suppression_mechanism_degradation,
    'Is measured suppression structural (real barriers to capability accumulation) or increasingly theatrical (formal exclusions despite declining functional necessity)?',
    'Measurement of suppression over time; decomposition into structural barriers vs. ritual/theatrical enforcement; tracking of de facto vs de jure capability access',
    'If structural: Snare classification is robust. If increasingly theatrical: transition to Piton is underway. High theater ratio (0.58) suggests mixed mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_degradation, empirical, 'Whether suppression is structural or increasingly theatrical').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(capability_concentration_asymmetry, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(capcon_tr_t0, capability_concentration_asymmetry, theater_ratio, 0, 0.42).
narrative_ontology:measurement(capcon_tr_t5, capability_concentration_asymmetry, theater_ratio, 5, 0.5).
narrative_ontology:measurement(capcon_tr_t10, capability_concentration_asymmetry, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(capcon_be_t0, capability_concentration_asymmetry, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(capcon_be_t5, capability_concentration_asymmetry, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(capcon_be_t10, capability_concentration_asymmetry, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(capability_concentration_asymmetry, global_infrastructure).
narrative_ontology:affects_constraint(capability_concentration_asymmetry, knowledge_asymmetry).
narrative_ontology:affects_constraint(capability_concentration_asymmetry, capital_access_restriction).
narrative_ontology:affects_constraint(capability_concentration_asymmetry, credentialing_gatekeeping).
narrative_ontology:affects_constraint(capability_concentration_asymmetry, institutional_lock_in).

% DUAL FORMULATION NOTE:
% Capability concentration asymmetry is the meta-constraint governing the entire family of access-restriction constraints. Upstream: fundamental asymmetries in knowledge distribution and capital availability. Downstream: specific institutional mechanisms (credentialing systems, IP regimes, access controls) that enforce and maintain the concentration. This story focuses on the structural constraint itself (the asymmetry mechanism), not on specific institutional implementations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(capability_concentration_asymmetry, moderate, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
