% ============================================================================
% CONSTRAINT STORY: technological_substrate_lock_in
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technological_substrate_lock_in, []).

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
 *   constraint_id: technological_substrate_lock_in
 *   human_readable: Technological Substrate Lock-In
 *   domain: technology_policy/infrastructure
 *
 * SUMMARY:
 *   Technological substrate lock-in describes the structural constraint that
 *   emerges when an incumbent technology platform achieves dominance such
 *   that network effects, switching costs, and ecosystem density make
 *   alternatives economically or operationally infeasible for developers,
 *   integrators, and end-users. The constraint exhibits characteristics of
 *   both genuine coordination (standards reduce fragmentation; established
 *   ecosystems provide tools, training, and compatibility) and asymmetric
 *   extraction (incumbents capture rents through pricing power, mandatory
 *   upgrades, and suppression of alternative pathways). The extractiveness
 *   value has risen from 0.35 to 0.58 over the measurement interval as
 *   vendors have actively leveraged platform dominance to extract value
 *   beyond coordination costs, and the theater ratio has risen from 0.42 to
 *   0.68 as regulatory and open-source responses have become increasingly
 *   performative (APIs exist but switching costs persist, open standards are
 *   adopted but new layers of lock-in emerge). The constraint is generational
 *   in its scope — individual developers face biographical trap, but
 *   technological transitions (infrastructure platform changes) operate on
 *   10-30 year timescales. This makes it particularly amenable to scaffold
 *   framing: sunset clauses exist (containerization, WASM, Linux abstraction
 *   layers, open standards) but their effectiveness depends on whether they
 *   genuinely reduce switching costs or merely displace lock-in to new
 *   technical layers.
 *
 * KEY AGENTS:
 *   - Locked-Out Developer / Startup: Powerless/trapped (local scope) — individual trying to build alternative; faces coordination costs too large to overcome without existing market position
 *   - Alternative Technology Projects: Powerless/identity_locked (global scope) — RISC-V, open-source hardware, alternative clouds; motivated by ideology of openness but functionally trapped by network effects even when technology is superior
 *   - Downstream Integrators: Moderate/constrained (national to global) — enterprises, system integrators, regional cloud providers; benefit from ecosystem stability but bear costs of mandatory upgrades and locked-in data migration
 *   - Incumbent Platform Vendors: Institutional/arbitrage (global) — AWS, Intel, Apple, Google, Microsoft; primary beneficiaries experiencing constraint as coordination problem; have optionality to leverage or cooperate
 *   - Interoperability Coalition: Organized/constrained (global) — standards bodies, open-source foundations, Linux Foundation, RISC-V; see substrate lock-in as solvable through open standards; building alternative pathways with generational sunset logic
 *   - Regulatory Authorities: Institutional/arbitrage (global) — antitrust, DMA, FTC; attempting to govern through interoperability mandates but becoming increasingly performative
 *   - Ecosystem Diversity: Powerless/trapped (universal) — abstract future innovation space; constrained by current platform dominance preventing exploration of alternative technological pathways
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technological_substrate_lock_in, 0.58).
domain_priors:suppression_score(technological_substrate_lock_in, 0.65).
domain_priors:theater_ratio(technological_substrate_lock_in, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technological_substrate_lock_in, extractiveness, 0.58).
narrative_ontology:constraint_metric(technological_substrate_lock_in, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(technological_substrate_lock_in, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technological_substrate_lock_in, tangled_rope).
narrative_ontology:human_readable(technological_substrate_lock_in, "Technological Substrate Lock-In").
narrative_ontology:topic_domain(technological_substrate_lock_in, "technology_policy/infrastructure").

domain_priors:requires_active_enforcement(technological_substrate_lock_in).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technological_substrate_lock_in, incumbent_platform_vendors).
narrative_ontology:constraint_beneficiary(technological_substrate_lock_in, network_effect_winners).
narrative_ontology:constraint_victim(technological_substrate_lock_in, alternative_technology_developers).
narrative_ontology:constraint_victim(technological_substrate_lock_in, ecosystem_diversity).
narrative_ontology:constraint_victim(technological_substrate_lock_in, future_innovation_pathways).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOCKED-OUT DEVELOPER (SNARE) — Individual developer or startup attempting to build on alternative substrate (e.g., non-AWS cloud, non-Intel CPU, non-Chrome browser engine) faces insurmountable coordination costs. Trapped: cannot migrate users without ecosystem participation they cannot access; cannot build ecosystem without users. Maximum experienced extraction — no exit path.
constraint_indexing:constraint_classification(technological_substrate_lock_in, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DOWNSTREAM INTEGRATOR (TANGLED ROPE) — System integrators, regional cloud providers, or enterprise IT teams experience genuine coordination benefit (standard interfaces, tool ecosystem, training availability) alongside asymmetric extraction (lock-in pricing, mandatory upgrades, data portability costs). Constrained by switching costs and ecosystem dependency, but some agency through negotiation and internal compatibility layers.
constraint_indexing:constraint_classification(technological_substrate_lock_in, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT PLATFORM VENDOR (ROPE) — Experiences the substrate lock-in as pure coordination: standardizing on their architecture solves collective action problems (developers want one target, users want one ecosystem, enterprises want compatibility). Net beneficiary through arbitrage optionality — can leverage dominance to extract or to coordinate depending on market dynamics.
constraint_indexing:constraint_classification(technological_substrate_lock_in, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTEROPERABILITY COALITION (SCAFFOLD) — Organized actors (open standards bodies, RISC-V consortium, WebAssembly advocates, open-source foundations) recognize substrate lock-in as a solvable coordination problem. Sunset clause: standardized APIs, container technologies, and vendor-neutral runtimes (WASM, containerization, Linux kernel abstraction) are creating exit pathways. Effective extraction is lower because organized agents have agency and see a functional exit strategy.
constraint_indexing:constraint_classification(technological_substrate_lock_in, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY APPARATUS (PITON) — Antitrust authorities, data protection frameworks, and interoperability mandates (DMA, GDPR, right-to-repair) attempt to govern substrate lock-in but become largely performative. Theater ratio high: compliance measures create appearance of choice without addressing underlying switching costs. Regulators see their own interventions as degraded — mandatory APIs exist, but network effects render them functionally weak.
constraint_indexing:constraint_classification(technological_substrate_lock_in, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From civilizational/universal scope, network effects and switching costs appear as immutable laws of technology: larger networks have increasing returns to scale, heterogeneous systems are harder to coordinate than homogeneous ones, and compatibility requires standardization. This perspective risks naturalizing what is actually a contingent institutional arrangement (investor structures, intellectual property norms, data portability economics). The engine's false summit detector identifies this as naturalization of market structure, not technological inevitability.
constraint_indexing:constraint_classification(technological_substrate_lock_in, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technological_substrate_lock_in_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(technological_substrate_lock_in, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(technological_substrate_lock_in, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(technological_substrate_lock_in, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(technological_substrate_lock_in, TR),
    TR >= 0.70.

:- end_tests(technological_substrate_lock_in_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. The constraint exhibits both genuine coordination function (ecosystem density provides value) and extraction (pricing power, upgrade requirements, data lock-in). The rising trajectory reflects deliberate vendor moves to monetize dominance beyond coordination costs. Suppression (0.65): High. Multiple barriers: switching costs (technical retraining, API migration, data migration); network effects (users/developers where the ecosystem is already established); ecosystem lock-in (tools, libraries, training for incumbent platform); switching risk (integration failures, performance penalties, support gaps). But suppression is not total — organized actors can build alternatives (Linux, open standards) and governments can mandate interoperability. Theater ratio (0.68): High. Regulatory mandates (APIs, data portability) and open-source alternatives create appearance of choice without addressing underlying economics of ecosystem dominance. Compliance theater increases as alternatives are adopted but prove insufficient to overcome network effects. Claimed type is tangled_rope because the constraint combines genuine coordination (solved technical heterogeneity) with asymmetric extraction (rents captured by incumbents) and active enforcement (vendor strategies to maintain dominance).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same technological arrangement produces fundamentally different experienced constraints depending on structural position. The vendor sees coordination (rope); the developer sees trap (snare); the integrator sees mixed (tangled_rope); the open-source movement sees solvable problem (scaffold); the regulator sees degraded intervention (piton). The analytical observer risks seeing immutable law (mountain) — 'larger networks have increasing returns, therefore centralized platforms are inevitable' — but the structural data contradicts this: the constraint's properties (extractiveness rising, theater rising) reflect deliberate vendor moves to maintain dominance, not technological inevitability.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from their structural position relative to extraction flow. Incumbent vendors (beneficiaries with arbitrage exit) have low d (~0.10), experiencing weak effective extraction because they can leverage dominance or cooperate flexibly. Downstream integrators (both beneficiary and victim aspects, constrained exit) have moderate d (~0.55), experiencing mixed extraction through lock-in costs and dependency. Locked-out developers (victims with trapped exit) have high d (~0.95), bearing maximum extraction. The open-source coalition (organized agents with constrained exit but agency) have moderate d (~0.40) despite victim classification because their organizational power reduces experienced extraction. The regulatory apparatus (institutional beneficiary of compliance theater, arbitrage exit) has low d (~0.15) but high chi because the theater_ratio is high, reflecting that regulatory visibility is decoupled from functional effect.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that substrate lock-in is neither pure coordination (rope) nor pure extraction (snare), but a genuine hybrid (tangled_rope) where the coordination function and extraction mechanism are structurally coupled. The incumbent's platform coordination solves real problems AND enables extraction through network effects. The exit paths being built (open standards, containerization, regulatory mandates) address the coordination function without addressing the extraction mechanism (network effects persist even after technical interoperability is achieved). True resolution requires either (a) breaking the network effects through regulatory intervention powerful enough to overcome switching costs (difficult empirically), (b) building superior alternatives with stronger network effects (rare — most alternatives remain marginalized), or (c) accepting the tangled rope as a permanent feature of technological development with periodic substrate transitions (generational timescale). The piton perspective (regulatory theater) and scaffold perspective (open standards sunset) both represent attempts at mandatrophy resolution, but their effectiveness remains empirically contested in the omega variables.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    switching_cost_arbitrage_boundary,
    'At what switching cost threshold does network effect dominance become structurally irreversible vs. strategically contingent?',
    'Empirical analysis of historical substrate transitions (Intel to ARM in mobile, gasoline to electric vehicles, SQL to NoSQL databases); measurement of switching cost ratios relative to lifetime user value; comparison of coordinated transitions (government mandate) vs. organic competition (superior alternative emerges)',
    'If threshold low (< 20% of lifetime value): lock-in is contingent on competitive dynamics — strong alternative can break dominance. If threshold high (> 60%): lock-in is quasi-structural — incumbents have near-permanent advantage regardless of alternative quality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(switching_cost_arbitrage_boundary, empirical, 'Switching cost threshold distinguishing contingent from structural lock-in').

omega_variable(
    open_standard_viability,
    'Can genuinely open standards (WASM, Linux, open-source ecosystems) reduce substrate lock-in sufficiently to create real exit pathways, or do they recapitulate lock-in at a different layer?',
    'Longitudinal comparison of actual developer migration rates to open alternatives; analysis of whether open standards themselves become platforms with new lock-in (e.g., WASM locks to browser runtimes; Linux locks to particular distributions)',
    'If viable: scaffold perspective is correct — open alternatives create genuine sunset. If not viable: lock-in is recursive — exits lead to new lock-in at different layers, and tangled rope properly captures the hybrid extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(open_standard_viability, empirical, 'Whether open standards provide functional exit from platform lock-in').

omega_variable(
    regulatory_mandate_effectiveness,
    'Do interoperability mandates (mandatory APIs, data portability requirements, sideloading permissions) actually reduce switching costs and enable genuine exit, or do they create theater without structural change?',
    'Post-DMA analysis of European tech market: measurement of actual user switching rates to regulated alternatives; analysis of whether mandatory APIs have identical switching costs as before (costs moved from technical to business layer); comparison of regulatory compliance (APIs exist) vs. functional interoperability (APIs are usable)',
    'If effective: piton perspective is incorrect — regulation can break the theater. If theater: piton classification confirmed — regulation creates appearance of choice while underlying network effects persist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_mandate_effectiveness, empirical, 'Effectiveness of regulatory mandates in reducing substrate lock-in').

omega_variable(
    incompatibility_as_feature,
    'Is substrate incompatibility deliberately maintained as an extraction mechanism, or does it emerge from legitimate technical and coordination constraints?',
    'Comparative analysis of vendor API design choices: measurement of incompatibility prevalence in competitive vs. monopolistic markets; analysis of whether incompatibility aligns with documented vendor incentives (vendor lock-in value) vs. technical necessity (genuine interoperability costs); examination of deliberate compatibility choices (e.g., AWS compatible APIs) when the vendor chose not to lock',
    'If largely deliberate: extraction is voluntary, and alternatives are suppressed by active enforcement (snare/tangled rope). If largely technical: lock-in is genuine coordination cost (rope), and extraction is secondary effect of legitimate network effects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incompatibility_as_feature, empirical, 'Whether substrate incompatibility is deliberate extraction mechanism or technical necessity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technological_substrate_lock_in, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(techlock_tr_t0, technological_substrate_lock_in, theater_ratio, 0, 0.42).
narrative_ontology:measurement(techlock_tr_t10, technological_substrate_lock_in, theater_ratio, 10, 0.58).
narrative_ontology:measurement(techlock_tr_t20, technological_substrate_lock_in, theater_ratio, 20, 0.68).
narrative_ontology:measurement(techlock_tr_t5, technological_substrate_lock_in, theater_ratio, 5, 0.5).
narrative_ontology:measurement(techlock_tr_t15, technological_substrate_lock_in, theater_ratio, 15, 0.63).

% Extraction over time
narrative_ontology:measurement(techlock_be_t0, technological_substrate_lock_in, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(techlock_be_t10, technological_substrate_lock_in, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(techlock_be_t20, technological_substrate_lock_in, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(techlock_be_t5, technological_substrate_lock_in, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(techlock_be_t15, technological_substrate_lock_in, base_extractiveness, 15, 0.53).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technological_substrate_lock_in, global_infrastructure).
narrative_ontology:affects_constraint(technological_substrate_lock_in, platform_monopoly_power).
narrative_ontology:affects_constraint(technological_substrate_lock_in, innovation_pathway_suppression).
narrative_ontology:affects_constraint(technological_substrate_lock_in, vendor_lock_in_pricing).

% DUAL FORMULATION NOTE:
% Technological substrate lock-in is upstream of platform monopoly effects and innovation suppression. The substrate lock-in constraint creates the structural conditions enabling monopoly pricing and pathway closure. Downstream constraints inherit the suppression and network effect properties of their upstream technological substrate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(technological_substrate_lock_in, institutional, 0.1).
constraint_indexing:directionality_override(technological_substrate_lock_in, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
