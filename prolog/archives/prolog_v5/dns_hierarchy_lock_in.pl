% ============================================================================
% CONSTRAINT STORY: dns_hierarchy_lock_in
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dns_hierarchy_lock_in, []).

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
 *   constraint_id: dns_hierarchy_lock_in
 *   human_readable: DNS Hierarchy Lock-In and Root Server Governance
 *   domain: internet_infrastructure/governance
 *
 * SUMMARY:
 *   The DNS hierarchy lock-in represents a structural constraint embedded in
 *   internet governance that combines genuine technical coordination with
 *   institutional capture and path dependence. The Domain Name System's
 *   hierarchical architecture — with 13 root servers controlled through
 *   ICANN, a US-founded nonprofit with significant US government influence —
 *   creates a single global namespace that prevents domain name collisions
 *   and coordinates internet addressing. However, this coordination mechanism
 *   also concentrates governance authority, creates barriers to alternative
 *   root systems, and reflects historical contingencies (NTIA delegation in
 *   1998, ICANN's nonprofit structure) rather than technical inevitability.
 *   The constraint manifests differently across observer positions: incumbent
 *   operators perceive it as essential coordination infrastructure;
 *   alternative root advocates experience it as extractive lock-in;
 *   nation-states see both coordination benefits and governance exclusion;
 *   decentralized DNS projects experience it as a technical and social
 *   barrier; and the legacy DNS protocol itself operates as a degraded system
 *   maintained through patches rather than fundamental redesign.
 *
 * KEY AGENTS:
 *   - ICANN (Internet Corporation for Assigned Names and Numbers): Institutional beneficiary (institutional/arbitrage) — captures governance authority, funding, and regulatory legitimacy through root zone control
 *   - Root Server Operators (VeriSign, Cogent, University of Maryland, etc.): Institutional beneficiary (institutional/arbitrage) — operate critical infrastructure with regulatory protection and coordination privilege
 *   - Alternative Root Developers (OpenNIC, Namecoin, Handshake): Primary victim (powerless/trapped) — cannot achieve network effects without displacing the incumbent; fragmentation is the only exit path
 *   - Nation-States (especially non-Western): Secondary victim (moderate/constrained) — benefit from DNS coordination but excluded from governance decisions; constrained by interoperability requirements
 *   - Decentralized DNS Coalition (ENS, Unstoppable Domains): Organized challenger (organized/constrained) — solving coordination through consensus; experiencing extraction through ICANN's regulatory advantage
 *   - Legacy DNS Protocol: Institutional degradation (institutional/arbitrage) — maintained through patches and workarounds; performance at scale compromised by architectural choices made in 1983
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dns_hierarchy_lock_in, 0.52).
domain_priors:suppression_score(dns_hierarchy_lock_in, 0.65).
domain_priors:theater_ratio(dns_hierarchy_lock_in, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dns_hierarchy_lock_in, extractiveness, 0.52).
narrative_ontology:constraint_metric(dns_hierarchy_lock_in, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(dns_hierarchy_lock_in, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dns_hierarchy_lock_in, tangled_rope).
narrative_ontology:human_readable(dns_hierarchy_lock_in, "DNS Hierarchy Lock-In and Root Server Governance").
narrative_ontology:topic_domain(dns_hierarchy_lock_in, "internet_infrastructure/governance").

domain_priors:requires_active_enforcement(dns_hierarchy_lock_in).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dns_hierarchy_lock_in, root_server_operators).
narrative_ontology:constraint_beneficiary(dns_hierarchy_lock_in, icann).
narrative_ontology:constraint_beneficiary(dns_hierarchy_lock_in, incumbent_registrars).
narrative_ontology:constraint_victim(dns_hierarchy_lock_in, alternative_root_developers).
narrative_ontology:constraint_victim(dns_hierarchy_lock_in, internet_sovereignty_advocates).
narrative_ontology:constraint_victim(dns_hierarchy_lock_in, decentralized_dns_participants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ALTERNATIVE ROOT INITIATIVE (SNARE) — Cannot exit the ICANN hierarchy without abandoning global interoperability. Trapped by network effects: every domain must resolve through the 13 root servers to reach 99.9% of internet users. Alternative roots (OpenNIC, Namecoin) remain marginalized despite technical viability. Maximum extraction: captured by the incumbent system with no meaningful exit.
constraint_indexing:constraint_classification(dns_hierarchy_lock_in, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NATION-STATE SOVEREIGNTY (TANGLED ROPE) — Constrained by technical and diplomatic barriers. Derives genuine coordination benefit: DNS hierarchy provides global namespace unity and prevents domain collisions. Simultaneously extracts: governance decisions exclude non-ICANN actors, and root zone authority is concentrated in US-aligned institutions. Costly exit (national intranet, fragmented namespace) but possible — some nations maintain parallel DNS infrastructure.
constraint_indexing:constraint_classification(dns_hierarchy_lock_in, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ICANN AND ROOT OPERATORS (ROPE) — Net beneficiary. Experiences the constraint as coordination: root servers provide essential namespace management, and ICANN's governance role solves the coordination problem of a single global namespace. Exit is available through arbitrage (sell governance authority, transition to competing root system) but unnecessary — the current position yields authority and funding. Classification as rope reflects genuine coordination function alongside institutional capture.
constraint_indexing:constraint_classification(dns_hierarchy_lock_in, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: BLOCKCHAIN DNS COALITION (TANGLED ROPE) — Organized agents (ENS, Unstoppable Domains, Handshake) see the DNS hierarchy as solvable through decentralized consensus. They derive coordination benefit from the single namespace problem that DNS solves and the technical standards it provides. Simultaneously constrained: blockchain DNS systems must either fork the root zone (fragmentation) or achieve critical mass adoption (chicken-and-egg problem). Requires active enforcement of their own rules; sees ICANN enforcement as extractive gatekeeping.
constraint_indexing:constraint_classification(dns_hierarchy_lock_in, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY DNS PROTOCOL (PITON) — The DNS protocol itself (RFC 1035 core architecture) is maintained as a degraded system. Modern DNS operates through security patches (DNSSEC), performance hacks (query caching, content delivery), and workarounds (DNS over HTTPS/TLS). The protocol persists through institutional inertia: replacing DNS entirely is too disruptive, so legacy infrastructure is patched indefinitely. Theater ratio reflects performative DNSSEC deployments that do not achieve their stated security goals.
constraint_indexing:constraint_classification(dns_hierarchy_lock_in, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ICANN REFORM (SCAFFOLD) — ICANN itself operates as a temporary coordination mechanism with a sunset clause. The organization was created to transition DNS governance from the US Department of Commerce (NTIA) to a multistakeholder body. Its legitimacy rests on implementing that transition fully. As governance matures (IANA stewardship transition, community board empowerment), the need for ICANN's specific hierarchical control diminishes. Low theater: reform advocates see a visible path to alternative governance. Extraction is constrained because the scaffold's mandate is explicit — it must sunset or transform.
constraint_indexing:constraint_classification(dns_hierarchy_lock_in, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the DNS hierarchy appears as an immutable law: a single global namespace requires a single root authority; distributed consensus cannot coordinate the namespace without fragmentation; the technical constraint of preventing conflicting domain assignments is inherent to the problem space. However, this naturalization masks contingent institutional choices (US-centric governance, commercial incentives, path dependence).
constraint_indexing:constraint_classification(dns_hierarchy_lock_in, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dns_hierarchy_lock_in_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dns_hierarchy_lock_in, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dns_hierarchy_lock_in, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(dns_hierarchy_lock_in, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(dns_hierarchy_lock_in, TR),
    TR >= 0.70.

:- end_tests(dns_hierarchy_lock_in_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The DNS hierarchy does extract from alternative root developers and nation-states seeking sovereignty, but the extraction is not maximal because genuine coordination benefits justify some of the extraction. The incumbent operators gain significant authority and de facto regulatory power, but this power is constrained by ICANN's multistakeholder model and international pressure. The extractiveness value reflects the asymmetry between coordination benefit (global namespace unity) and institutional capture (concentrated governance). Suppression (0.65): High. Multiple barriers prevent exit: network effects lock users into the ICANN root, technical standards require root zone delegation, and diplomatic pressure constrains parallel root systems. However, suppression is not total — alternative roots exist technically and some nation-states maintain parallel infrastructure. Theater ratio (0.48): Moderate. DNSSEC deployments often perform theater (signature validation without enforcement), but DNS operations are largely functional — the core role (name-to-IP translation) works reliably. Theater increased over time as security patches accumulated without fundamental redesign.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. ICANN experiences rope (coordination function with benefits), while alternative roots experience snare (pure extraction via lock-in). Nation-states see tangled rope (mixed coordination and governance exclusion). The ICANN reform movement sees scaffold (temporary coordination with sunset). The analytical observer risks seeing mountain (inherent technical law), but this is a false summit — the hierarchy is contingent on institutional arrangements, not technical necessity. The gap between ICANN's rope perception and alternative roots' snare perception reveals the full extractive mechanism: the same institutional structure that coordinates the global namespace simultaneously prevents competition through network effects and regulatory capture.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values reflect each agent's structural position in the extraction flow. Root server operators and ICANN experience low d (around 0.10-0.15) — they are beneficiaries with arbitrage options, deriving authority and funding. Alternative root developers experience high d (around 0.90) — they are powerless victims trapped by network effects with no cost-free exit. Nation-states are at intermediate d (around 0.55) — they benefit from coordination but experience governance exclusion and constrained exit options. Blockchain DNS coalitions are at moderate d (around 0.50) — they are organized enough to build alternatives but face chicken-and-egg problems and collision-avoidance trade-offs. The scaffold perspective on ICANN reform shows lower d (around 0.35) because reform advocates perceive an exit path through governance transition. The analytical mountain perspective risks d around 0.72 (full technical objectivity), but the structural data reveals this naturalizes contingent institutional choices.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY MECHANISM: The DNS hierarchy lock-in is a case where the coordination function (preventing domain collisions, providing single global namespace) is real and valuable, but the institutional capture (ICANN governance concentration, US-aligned control, barriers to alternatives) is also real. Neither function can be cleanly separated. The constraint is genuinely tangled rope, not pure extraction. However, the mandatrophy risk emerges when stakeholders claim the constraint is mountain (technically necessary) to justify institutional capture, or claim it is snare (pure extraction) to advocate for complete system replacement without accounting for coordination costs. The framework resolves mandatrophy by showing that all six types are valid perspectival readings — the constraint IS rope from ICANN's position, IS snare from alternative developers' position, IS scaffold from reform advocates' position, etc. The question is not 'which type is correct?' but 'which governance structure best serves the coordination function while minimizing extractive capture?' This moves the analysis from classification to design: can the namespace coordination function be achieved with lower institutional capture (e.g., through decentralized consensus, regional diversity in root operators, or governance reform)?
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decentralized_consensus_namespace,
    'Can decentralized consensus mechanisms (blockchain, distributed hash tables) coordinate a globally unique namespace without fragmenting into competing roots?',
    'Long-term empirical observation of blockchain DNS systems (ENS, Handshake); measurement of adoption rates and fragmentation events; comparison of collision resolution mechanisms across systems',
    'If yes: the mountain perspective is false — DNS hierarchy is contingent, not inherent. The lock-in is institutional, not technical. If no: the technical constraint is real, and the hierarchy is partially naturalizable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decentralized_consensus_namespace, empirical, 'Whether decentralized consensus can coordinate global namespace without fragmentation').

omega_variable(
    exit_cost_magnitude,
    'What fraction of internet users and services would be disrupted by a complete migration from ICANN DNS to an alternative root system?',
    'Technical analysis of dependency chains; measurement of services that can and cannot function on alternative roots; user survey on awareness and switching costs',
    'If < 5%: exit costs are overstated; the lock-in is weaker than suppression metrics suggest. If > 40%: the lock-in is primarily technical, not institutional. If 15-30%: the lock-in is genuinely mixed (institutional capture on top of network effects).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_cost_magnitude, empirical, 'Magnitude of disruption from complete migration to alternative DNS root').

omega_variable(
    icann_governance_legitimacy,
    'Is ICANN''s multistakeholder governance model perceived as legitimate by non-Western states, developing economies, and alternative root advocates?',
    'Survey of nation-state representatives, ICANN meeting participation analysis, measurement of policy adoption rates, comparison with parallel governance initiatives (Russia''s DNS mirror, China''s gTLD ecosystem)',
    'If low legitimacy: the constraint may fragment into regional DNS hierarchies (tangled rope becomes multiple snares). If high: the rope classification is more defensible. If moderate with geographic variance: scaffold sunset may be delayed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(icann_governance_legitimacy, empirical, 'Perceived legitimacy of ICANN multistakeholder governance').

omega_variable(
    protocol_replacement_feasibility,
    'Is DNS replacement technically feasible at internet scale, or is the protocol architecture sufficiently embedded in infrastructure that replacement requires civilization-scale disruption?',
    'Analysis of DNS-dependent infrastructure layers; comparison with prior major protocol transitions (IPv4 to IPv6); technical roadmap analysis for next-generation identifier systems',
    'If replacement is feasible: the hierarchy is contingent and institutional lock-in is the primary mechanism. If replacement requires 50+ year transition: the technical constraint dominates and the mountain perspective has merit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protocol_replacement_feasibility, empirical, 'Technical feasibility of DNS protocol replacement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dns_hierarchy_lock_in, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dns_tr_t0, dns_hierarchy_lock_in, theater_ratio, 0, 0.35).
narrative_ontology:measurement(dns_tr_t10, dns_hierarchy_lock_in, theater_ratio, 10, 0.42).
narrative_ontology:measurement(dns_tr_t20, dns_hierarchy_lock_in, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(dns_be_t0, dns_hierarchy_lock_in, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(dns_be_t10, dns_hierarchy_lock_in, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(dns_be_t20, dns_hierarchy_lock_in, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dns_hierarchy_lock_in, global_infrastructure).
narrative_ontology:affects_constraint(dns_hierarchy_lock_in, icann_multistakeholder_model).
narrative_ontology:affects_constraint(dns_hierarchy_lock_in, internet_governance_fragmentation).
narrative_ontology:affects_constraint(dns_hierarchy_lock_in, certificate_authority_lock_in).

% DUAL FORMULATION NOTE:
% The DNS hierarchy lock-in decomposes into technical coordination (name-to-IP resolution) and institutional capture (governance concentration). The technical coordination story (ε ≈ 0.08, rope) is distinct from the governance extraction story (ε ≈ 0.52, tangled rope). This story addresses the combined institutional-technical constraint. Network links show how DNS lock-in affects broader internet governance fragmentation and how it depends on ICANN's governance model.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dns_hierarchy_lock_in, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
