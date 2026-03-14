% ============================================================================
% CONSTRAINT STORY: intellectual_property_lock_in
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_intellectual_property_lock_in, []).

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
 *   constraint_id: intellectual_property_lock_in
 *   human_readable: Intellectual Property Lock-In and Ecosystem Dependency
 *   domain: economic/legal/technological
 *
 * SUMMARY:
 *   Intellectual property lock-in creates a structural extraction mechanism
 *   where creators benefit from temporary monopolies, users and developers
 *   pay switching costs, and the entire innovation ecosystem is constrained
 *   by control over proprietary infrastructure. The constraint operates
 *   across multiple levels: legal (patent/copyright enforcement), technical
 *   (API incompatibility, proprietary formats), economic (network effects,
 *   switching costs), and institutional (licensing restrictions, strategic
 *   gatekeeping). This constraint exemplifies how a coordination mechanism
 *   (IP protection as innovation incentive) has been layered with extraction
 *   (lock-in as control mechanism), creating a hybrid Tangled Rope from
 *   multiple perspectives while appearing to dependent developers as a pure
 *   Snare. The theater_ratio (0.52) reflects that patent enforcement for
 *   software and business methods has become increasingly performative — the
 *   actual lock-in operates through network effects and technical switching
 *   costs, while patent litigation provides a ritual justification.
 *
 * KEY AGENTS:
 *   - IP Holder: Primary beneficiary (institutional/arbitrage) — captures monopoly rents, controls platform evolution, benefits from network effects and switching costs
 *   - Dependent Developers: Primary victim (powerless/trapped) — locked into proprietary ecosystems; exit requires complete technical rebuild; bear suppression costs through licensing restrictions and forced upgrades
 *   - Open Source Community: Secondary victim (moderate/constrained) — can theoretically exit but face resource and coordination barriers; benefit from ecosystem access but constrained by IP licensing and patent threats
 *   - Downstream Innovation: Victim (powerless/trapped) — constrained by IP holder's control over foundational infrastructure; feature velocity controlled by rent extraction rather than genuine coordination needs
 *   - Interoperability Movement: Organized agents (organized/constrained) — regulators, standards bodies, open-source foundations building alternative pathways; see constraint as temporary if regulatory momentum sustains
 *   - Legacy Patent System: Institutional actor (institutional/arbitrage) — maintains performative patent enforcement while real lock-in operates through network effects; persists through institutional inertia
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(intellectual_property_lock_in, 0.58).
domain_priors:suppression_score(intellectual_property_lock_in, 0.65).
domain_priors:theater_ratio(intellectual_property_lock_in, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(intellectual_property_lock_in, extractiveness, 0.58).
narrative_ontology:constraint_metric(intellectual_property_lock_in, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(intellectual_property_lock_in, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(intellectual_property_lock_in, tangled_rope).
narrative_ontology:human_readable(intellectual_property_lock_in, "Intellectual Property Lock-In and Ecosystem Dependency").
narrative_ontology:topic_domain(intellectual_property_lock_in, "economic/legal/technological").

domain_priors:requires_active_enforcement(intellectual_property_lock_in).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(intellectual_property_lock_in, ip_holder).
narrative_ontology:constraint_beneficiary(intellectual_property_lock_in, ecosystem_gatekeeper).
narrative_ontology:constraint_victim(intellectual_property_lock_in, dependent_developers).
narrative_ontology:constraint_victim(intellectual_property_lock_in, downstream_innovation).
narrative_ontology:constraint_victim(intellectual_property_lock_in, user_choice).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT DEVELOPER (SNARE) — Developer whose products, tools, or infrastructure depend on proprietary IP. Cannot exit without complete rebuild of technical stack; migration costs are prohibitive. High suppression through lock-in: switching requires rewriting code, retraining teams, losing network effects. No meaningful alternatives exist when IP holder controls critical infrastructure.
constraint_indexing:constraint_classification(intellectual_property_lock_in, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: OPEN SOURCE COMMUNITY (TANGLED ROPE) — Benefits from access to proprietary APIs and platforms that enable rapid development and market reach. Also constrained by IP licensing restrictions, patent threats, and vendor control over platform evolution. Mixed coordination (API standardization, ecosystem tools) and extraction (restrictive licensing, patent enforcement). Exit is possible but costly — replicating proprietary functionality requires substantial coordination effort.
constraint_indexing:constraint_classification(intellectual_property_lock_in, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: IP HOLDER (ROPE) — Experiences the constraint as pure coordination mechanism. IP protection enables network effects, ecosystem development, and platform stability. Licensing arrangements appear as mutual benefit: IP holder gains revenue and platform control; licensees gain access to valuable IP. From the holder's perspective, the system solves the coordination problem of incentivizing creation. Arbitrage options available (licensing, sublicensing, strategic partnerships).
constraint_indexing:constraint_classification(intellectual_property_lock_in, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTEROPERABILITY MOVEMENT (SCAFFOLD) — Organized actors (regulatory bodies, standards organizations, open-source foundations) working to create alternative pathways through interoperability mandates, compulsory licensing, and open standards. The constraint appears as temporary because technical/legal alternatives are being systematized. Sunset clause is real: Digital Markets Act (EU), proposed interoperability legislation (US), and open standards development create declining enforcement pathways. However, this assumes continued regulatory pressure and technical progress.
constraint_indexing:constraint_classification(intellectual_property_lock_in, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY PATENT SYSTEM (PITON) — Patent protection for software and business methods has largely become performative theater. Patents rarely prevent determined competitors; enforcement is expensive and slow; the actual lock-in mechanism is network effects and switching costs, not patent infringement. The patent system persists through institutional inertia (USPTO continues issuing patents, lawyers continue filing) but does little of its intended work — incentivizing innovation. The core extraction mechanism is now platform control and network lock-in, which the patent theater obscures.
constraint_indexing:constraint_classification(intellectual_property_lock_in, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, some IP lock-in appears inherent to innovation: the creator faces a fundamental choice between disclosure (enabling imitation, reducing incentive) and secrecy (enabling extraction but preventing knowledge diffusion). This dilemma is presented as naturally unchangeable — a law of information economics. However, the structural data reveals contingency: alternative incentive structures (prize systems, grant-based research, knowledge commons) exist and work. The 'mountain' framing naturalizes a particular institutional choice.
constraint_indexing:constraint_classification(intellectual_property_lock_in, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(intellectual_property_lock_in_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(intellectual_property_lock_in, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(intellectual_property_lock_in, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(intellectual_property_lock_in, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(intellectual_property_lock_in, TR),
    TR >= 0.70.

:- end_tests(intellectual_property_lock_in_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. IP lock-in extracts substantial value from dependent developers through forced upgrade cycles, licensing costs, and switching costs. However, the extraction is not as severe as pure platform monopolies (0.75+) because alternative ecosystems exist, open-source substitutes reduce leverage, and regulatory pressure is increasing. The trajectory shows extraction increasing over 30 years as network effects strengthen and switching costs compound. Suppression (0.65): High. Substantial barriers to exit include technical lock-in (proprietary APIs, formats), economic lock-in (switching costs, network effects), legal lock-in (licensing restrictions, patent enforcement), and psychological lock-in (ecosystem lock-in makes alternatives invisible). However, suppression is not total — some developers successfully exit, open-source alternatives exist, and regulatory pressure is weakening IP holder control. Theater ratio (0.52): Moderate. Patent enforcement for software provides ritual justification for lock-in, but the actual extraction mechanism is technical and economic, not legal. The theater has increased over time as patent enforcement becomes more litigious while genuine lock-in operates through network effects and switching costs.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows stark perspectival divergence. The IP holder sees coordination (Rope) — incentive alignment, ecosystem stability, innovation rewards. Dependent developers see extraction (Snare) — forced upgrades, licensing costs, inability to migrate. The interoperability movement sees temporary constraint with sunset (Scaffold) — regulatory mandates and open standards creating alternative pathways. The patent system sees its own degradation (Piton) — patents as performative theater, real lock-in through network effects. Open source communities see mixed value and constraint (Tangled Rope) — ecosystem benefits alongside extraction. The analytical observer risks seeing inherent limits (Mountain) — information economics as natural law — but the structural data reveals contingency: alternative incentive systems exist and work. The perspectival gap reveals that IP lock-in is not a neutral coordination mechanism but an institutionally contingent extraction arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from the agent's structural position relative to the extraction flow. IP holders are beneficiaries with arbitrage options (exit via licensing, partnerships, acquisitions) — they experience low or negative d, producing negative effective extraction (net benefit from the constraint). Dependent developers are trapped victims with no meaningful exit — they experience high d, producing high f(d) and high experienced extraction chi. Open source communities and interoperability movements are organized but constrained — they have some agency and some benefit from ecosystem access, so they experience moderate d and mixed chi signatures. The piton perspective derives from the theater gate: patent protection persists despite providing little verification of lock-in's actual mechanism (network effects). The mountain perspective risks naturalizing a contingent institutional choice (IP-based innovation incentives vs. alternative systems like prizes, grants, or knowledge commons).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint resolves by demonstrating that IP lock-in combines genuine coordination (incentivizing creation, enabling ecosystem development) with genuine extraction (monopoly rents, switching costs, gatekeeper control). The Tangled Rope classification is not a failure to decide but a correct structural assessment: the constraint does both functions simultaneously. The mandatrophy resolves by rejecting the false binary — 'Is IP protection good or bad?' — and instead identifying which constituencies experience which function. IP holders experience coordination (Rope from their perspective). Dependent developers experience pure extraction (Snare from their perspective). The analytical observer needs to analyze from multiple positions to see the hybrid structure. The mandatrophy analysis also reveals that the patent system's performative theater (piton perspective) enables the extraction to persist even as patent enforcement becomes increasingly ineffective — the ritual of patent protection justifies lock-in that is actually maintained through network effects and technical switching costs. This explains why patent reform alone cannot resolve IP lock-in: the real mechanism is technical and economic, not legal. True resolution would require addressing network effects (interoperability mandates, platform decentralization) and switching costs (open standards, data portability), not just patent reform.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    switching_cost_measurement,
    'What proportion of measured exit costs are pure switching costs vs. genuine IP incompatibility?',
    'Direct measurement: cost of switching to open-source alternative + cost of genuine IP reimplementation (patent-blocking functions vs. non-IP-protected features)',
    'If high IP proportion: lock-in is authentically IP-based (patent/copyright enforcement). If low IP proportion: lock-in is primarily network effects and switching costs; IP is theatrical. This determines whether patent reform affects the constraint significantly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(switching_cost_measurement, empirical, 'Proportion of exit cost driven by IP vs. other switching factors').

omega_variable(
    ecosystem_coordination_necessity,
    'How much of the ecosystem benefit attributable to IP holders is genuine coordination (enabling interoperation, standardization) vs. extraction rent?',
    'Comparative analysis: open-source ecosystem equivalent features vs. proprietary ecosystem; measurement of innovation rates in walled vs. open sections; user welfare metrics under different IP regimes',
    'If coordination dominates: constraint is Rope from most perspectives; IP protection is genuine incentive. If extraction dominates: constraint is Snare/Tangled Rope; IP is cover story for rent extraction. Changes classification distribution significantly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecosystem_coordination_necessity, empirical, 'Whether ecosystem value derives from IP coordination or rent extraction').

omega_variable(
    regulatory_transition_speed,
    'Can interoperability mandates (Digital Markets Act, compulsory licensing, open standards) actually override technical lock-in faster than IP holders can rebuild proprietary advantages?',
    'Real-time monitoring: timeline from regulatory mandate → actual implementation → developer exit vs. IP holder adaptation speed. Case study: GDPR compliance costs vs. data portability impact.',
    'If regulators are faster: scaffold sunset is real (15-20 year transition). If IP holders faster: scaffold is aspirational, constraint persists. Determines whether temporal classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_transition_speed, empirical, 'Speed of regulatory interoperability mandates vs. IP holder technical adaptation').

omega_variable(
    patent_enforcement_vs_network_lock,
    'In cases where developers leave (exit occurs despite lock-in claims), is it because patents stop being enforced or because network effects have weakened?',
    'Historical case analysis: Linux vs. proprietary Unix, Android vs. iOS restrictiveness, open-source alternatives to enterprise software. What actually triggered exodus — patent fatigue or network effect decay?',
    'If patents: IP reform directly affects constraint severity. If network effects: IP reform is decorative; constraint persists through switching costs alone. Informs which omega variable is actually binding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(patent_enforcement_vs_network_lock, empirical, 'Whether patent enforcement or network effects drive actual lock-in').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(intellectual_property_lock_in, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ipl_tr_t0, intellectual_property_lock_in, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ipl_tr_t10, intellectual_property_lock_in, theater_ratio, 10, 0.48).
narrative_ontology:measurement(ipl_tr_t20, intellectual_property_lock_in, theater_ratio, 20, 0.52).
narrative_ontology:measurement(ipl_tr_t30, intellectual_property_lock_in, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(ipl_be_t0, intellectual_property_lock_in, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ipl_be_t10, intellectual_property_lock_in, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(ipl_be_t20, intellectual_property_lock_in, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(ipl_be_t30, intellectual_property_lock_in, base_extractiveness, 30, 0.54).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(intellectual_property_lock_in, resource_allocation).
narrative_ontology:affects_constraint(intellectual_property_lock_in, software_vendor_lock_in).
narrative_ontology:affects_constraint(intellectual_property_lock_in, cloud_platform_dependency).
narrative_ontology:affects_constraint(intellectual_property_lock_in, data_portability_barriers).
narrative_ontology:affects_constraint(intellectual_property_lock_in, open_standards_fragmentation).

% DUAL FORMULATION NOTE:
% IP lock-in exists in tension with multiple related constraints: vendor lock-in (which involves lock-in through contractual and technical means even without IP protection), platform dependency (which adds ecosystem network effects on top of IP), and data portability barriers (which create lock-in through data format and API incompatibility). These form a constraint family where IP lock-in is upstream — IP protection justifies and enables technical lock-in mechanisms that would otherwise face interoperability pressure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(intellectual_property_lock_in, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
