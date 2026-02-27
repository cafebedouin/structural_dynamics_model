% ============================================================================
% CONSTRAINT STORY: internet_evolution_lifecycle
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_internet_evolution_lifecycle, []).

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
 *   constraint_id: internet_evolution_lifecycle
 *   human_readable: The Lifecycle of the Open Internet
 *   domain: technological/social
 *
 * SUMMARY:
 *   The open internet emerged as a coordination mechanism for distributed
 *   communication (1990s-early 2000s): TCP/IP, DNS, email protocols solved
 *   collective action problems with minimal extraction. Over three decades,
 *   this infrastructure has been captured by centralized platforms (Google,
 *   Facebook, Amazon, Apple, Microsoft) that extract value through behavioral
 *   surveillance, algorithmic gatekeeping, and rent-seeking on creator
 *   economics. The constraint exhibits all six DR types depending on
 *   structural position. From the user's perspective, it is a snare: trapped
 *   in network effects with no viable exit. From the platform's perspective,
 *   it is a rope: they genuinely solved the coordination problem of global
 *   communication and deserve legitimate rent. From the internet governance
 *   coalition's perspective, it is a tangled rope: both coordination and
 *   extraction mechanisms are present and require active enforcement to
 *   balance. Net neutrality regulation has degraded into piton (performative
 *   theater). Decentralized alternatives represent a scaffold with potential
 *   sunset logic if UX parity emerges. The natural law perspective (mountain)
 *   risks naturalizing economic consolidation as inevitable technological
 *   necessity. The constraint is defined by the tension between the open
 *   internet's original coordination function and its transformation into an
 *   extraction mechanism through platform monopolism and surveillance
 *   capitalism.
 *
 * KEY AGENTS:
 *   - Ordinary Internet User: Primary victim (powerless/trapped) — data extracted, choices constrained by platform control, no exit option
 *   - Internet Privacy as Collective: Abstract victim (powerless/trapped) — no voice in architecture, no organizing agent, maximum extraction
 *   - Independent Content Creator: Secondary victim (moderate/constrained) — benefits from distribution access, harmed by algorithmic gatekeeping and revenue extraction
 *   - Content Distribution Platform (Google, Meta, Amazon, Apple): Primary beneficiary (institutional/arbitrage) — captures coordination function, extracts surveillance data and creator revenue
 *   - Internet Governance Coalition: Organized actor (organized/constrained) — ICANN, IETF, open-source communities trying to balance coordination and extraction through standards and decentralized architectures
 *   - Net Neutrality Regulator: Institutional actor (institutional/arbitrage) — maintains performance of neutrality; enforcement inconsistent, mechanism degraded
 *   - Decentralized Internet Movement: Organized actor (organized/mobile) — IPFS, ActivityPub, mesh networks building alternative infrastructure with exit potential
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing platform monopolism as inherent to network effects
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(internet_evolution_lifecycle, 0.52).
domain_priors:suppression_score(internet_evolution_lifecycle, 0.58).
domain_priors:theater_ratio(internet_evolution_lifecycle, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(internet_evolution_lifecycle, extractiveness, 0.52).
narrative_ontology:constraint_metric(internet_evolution_lifecycle, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(internet_evolution_lifecycle, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(internet_evolution_lifecycle, tangled_rope).
narrative_ontology:human_readable(internet_evolution_lifecycle, "The Lifecycle of the Open Internet").
narrative_ontology:topic_domain(internet_evolution_lifecycle, "technological/social").

domain_priors:requires_active_enforcement(internet_evolution_lifecycle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(internet_evolution_lifecycle, content_distribution_platforms).
narrative_ontology:constraint_beneficiary(internet_evolution_lifecycle, advertising_networks).
narrative_ontology:constraint_beneficiary(internet_evolution_lifecycle, surveillance_infrastructure_operators).
narrative_ontology:constraint_victim(internet_evolution_lifecycle, end_users).
narrative_ontology:constraint_victim(internet_evolution_lifecycle, privacy_advocates).
narrative_ontology:constraint_victim(internet_evolution_lifecycle, independent_content_creators).
narrative_ontology:constraint_victim(internet_evolution_lifecycle, internet_openness_principle).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ORDINARY INTERNET USER (SNARE) — Trapped in platform ecosystems with no viable exit. Cannot communicate without ceding data to centralized intermediaries. Career, social connection, and commerce all require participation. d≈0.92, f(d)≈1.40, σ=1.2 → χ≈0.76.
constraint_indexing:constraint_classification(internet_evolution_lifecycle, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INTERNET PRIVACY COLLECTIVE (SNARE) — Anonymous victim: no organizing agent, no exit option, no voice in architecture decisions. Surveillance infrastructure extracts behavioral data with zero consent mechanisms. d≈0.98, f(d)≈1.50, σ=1.2 → χ≈0.93.
constraint_indexing:constraint_classification(internet_evolution_lifecycle, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: INDEPENDENT CONTENT CREATOR (TANGLED ROPE) — Benefits from global distribution platform access; trapped by algorithmic gatekeeping, revenue extraction (30% platform cuts), and suppression of competing distribution models. d≈0.68, f(d)≈1.05, σ=1.1 → χ≈0.57.
constraint_indexing:constraint_classification(internet_evolution_lifecycle, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CONTENT DISTRIBUTION PLATFORM (ROPE) — Captures coordination function: matches creators to audiences globally, reduces transaction costs, enables scale. Extraction is legitimate first-mover rent. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.06.
constraint_indexing:constraint_classification(internet_evolution_lifecycle, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNET GOVERNANCE COALITION (TANGLED ROPE) — ICANN, IETF, open-source communities recognize both coordination function (protocol standards, decentralized architectures) and extraction problem (corporate consolidation, surveillance infrastructure embedded in DNS/IP stack). Constrained by network effects and switching costs. d≈0.55, f(d)≈0.75, σ=1.2 → χ≈0.46.
constraint_indexing:constraint_classification(internet_evolution_lifecycle, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: NET NEUTRALITY REGULATION (PITON) — Originally functional coordination mechanism (equal treatment of packets); now largely performative theater. Regulatory enforcement inconsistent across jurisdictions; actual data-throttling practices persist despite nominally neutral classification. theater_ratio=0.65 indicates substantial degradation. d≈0.10, f(d)≈-0.08, σ=1.2 → χ≈-0.06.
constraint_indexing:constraint_classification(internet_evolution_lifecycle, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: DECENTRALIZED INTERNET MOVEMENT (SCAFFOLD) — IPFS, ActivityPub, mesh networks, blockchain-based alternatives represent temporary scaffolding with sunset clause: if network effects reverse toward decentralization, these alternatives become primary. Currently low adoption but architectural sunrise path exists. χ≤0.30 (low extraction); sunset mechanism: user migration once UX reaches parity. d≈0.35, f(d)≈0.35, σ=0.9 → χ≈0.11.
constraint_indexing:constraint_classification(internet_evolution_lifecycle, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational scale, some centralization appears inevitable: network effects and switching costs create natural monopolies in communication infrastructure. This perspective risks naturalizing contingent economic arrangements as immutable laws. However, structural data (ε=0.52, suppression=0.58) contradicts mountain classification — this is a false summit revealing how economic power is naturalized as technological necessity.
constraint_indexing:constraint_classification(internet_evolution_lifecycle, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(internet_evolution_lifecycle_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(internet_evolution_lifecycle, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(internet_evolution_lifecycle, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(internet_evolution_lifecycle, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(internet_evolution_lifecycle, TR),
    TR >= 0.70.

:- end_tests(internet_evolution_lifecycle_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The internet has shifted from open coordination (early era, ε≈0.15) to platform-mediated extraction. Current measurement reflects: behavioral surveillance (ubiquitous data harvesting), algorithmic gatekeeping (suppression of competitor services), creator economy extraction (30% platform cuts + recommendation algorithm control). This is not maximal extraction because decentralized alternatives exist (low adoption, but technically feasible), and users retain some agency (can use multiple platforms, though costly). Suppression (0.58): Moderate-high. Network effects and switching costs create real barriers to exit. Users cannot easily migrate to alternatives without losing social graph and economic opportunity. However, suppression is not complete (technical alternatives exist, migration is possible with coordination, some users successfully use multiple platforms). Theater ratio (0.65): High and rising. Net neutrality regulations perform equality while platforms engage in de facto throttling. Platform recommendation algorithms present themselves as objective discovery mechanisms while actually performing aggressive amplification/suppression. Content moderation policies claim to serve community safety while extracting control over speech. Theater has increased as platforms have grown larger and facing more scrutiny — more performative legitimacy work required.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates profound perspectival divergence. The ordinary user sees a snare (trapped in network effects). The platform beneficiary sees a rope (legitimate coordination function). The internet privacy collective sees maximal snare (zero agency, zero voice). The independent creator sees tangled rope (both benefits and extraction). The internet governance coalition sees tangled rope with active enforcement requirements (standards, interoperability, decentralization support). Net neutrality regulators perform piton (ritual without function). The decentralized internet movement sees a scaffold with real sunset logic (UX parity could reverse network effects). The civilizational analytical observer risks seeing a mountain (network effects are inevitable, centralization is natural) — but the structural data reveals this as a false summit. The perspectival gap is not about different facts; it is about different structural relationships to the same extraction mechanism. This is the key diagnostic signature of a tangled rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Ordinary internet user: Victim + trapped → d≈0.92, f(d)≈1.40. Nearly maximal extraction. Content distribution platform: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; legitimate rent on coordination function. Internet privacy collective: Victim + trapped → d≈0.98, f(d)≈1.50. Maximal extraction (zero agency, zero exit, zero voice). Independent creator: Victim + constrained → d≈0.68, f(d)≈1.05. Significant extraction but not total; some agency through content production. Internet governance coalition: Both beneficiary (standards enable coordination) + victim (captured by platforms) + organized + constrained → d≈0.55, f(d)≈0.75. Mixed directionality. Decentralized internet movement: Organized + mobile → d≈0.35, f(d)≈0.35. Low effective extraction because mobility is real (decentralized alternatives exist, though adoption is low). Net neutrality regulator: Institutional + arbitrage → d≈0.10, f(d)≈-0.08. Appears as rope, but piton classification comes from theater_ratio gate, not chi.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY ANALYSIS — NOT YET RESOLVED: The constraint is claimed as tangled_rope but mandatrophy_resolved=false because the structural classification is currently contested. The constraint exhibits genuine coordination function (platforms solved real distribution problems that decentralized systems struggled with) AND asymmetric extraction (surveillance, algorithmic control, creator revenue extraction). However, the balance between coordination and extraction is the subject of ongoing debate: (1) Platform advocates argue the extraction is legitimate rent on coordination service and that user agency is overstated (users voluntarily choose to use platforms). (2) Decentralization advocates argue the extraction is increasingly predatory and that alternatives are now technically mature enough to provide coordination without surveillance overhead. (3) Regulators attempt to preserve coordination function while reducing extraction (GDPR, DMA, interoperability mandates). The mandatrophy will be resolved when one of three events occurs: (A) Decentralized alternatives achieve UX parity and adopt sufficiently that network effects reverse toward decentralization (constraint becomes Scaffold with real sunset). (B) Regulation successfully caps extraction while preserving coordination (constraint becomes Rope with governance overlay). (C) Extraction mechanisms become undeniable and irreducible (constraint becomes pure Snare with no coordination justification). Current status: tangled rope with live mandatrophy. The constraint's theater_ratio (0.65) is the leading indicator — as performative legitimacy work increases, the likelihood of pure snare classification rises.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    network_effects_reversibility,
    'Are network effects in centralized platforms reversible? Can users coordinate migration to decentralized alternatives once UX parity is achieved?',
    'Historical case studies of platform migrations (early web fragmentation, email federation adoption); empirical testing of decentralized UX parity thresholds; adoption rate analysis of Signal vs WhatsApp, Mastodon vs Twitter',
    'If reversible: scaffold perspective is structurally sound and open internet has real sunset path. If irreversible: constraint becomes longer-term tangled rope or even snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effects_reversibility, empirical, 'Whether network effects trap users irreversibly or permit coordination reversal').

omega_variable(
    surveillance_infrastructure_necessity,
    'Is behavioral data extraction technically necessary for platform economics or contingent on current business model choices?',
    'Comparison of platform revenue models; analysis of subscription-only services (Apple iCloud) vs ad-supported models; technology audit of privacy-preserving alternatives (differential privacy, federated learning)',
    'If necessary: extraction is structural, constraint is deeper snare. If contingent: extraction could be eliminated by policy/business model change.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(surveillance_infrastructure_necessity, conceptual, 'Whether surveillance infrastructure is technically necessary or model-dependent').

omega_variable(
    interoperability_protocol_feasibility,
    'Can interoperability standards (ActivityPub-style federation) reach technical maturity and user adoption within a single generation?',
    'Technical benchmark of federation protocol completeness; adoption metrics for ActivityPub instances; user experience parity testing vs centralized platforms',
    'If achievable: decentralization scaffold timeline is realistic, constraint becomes transitional. If not: open internet remains aspirational, constraint persists as tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interoperability_protocol_feasibility, empirical, 'Whether interoperability standards can mature within generational timescale').

omega_variable(
    state_surveillance_integration,
    'To what degree are commercial surveillance systems and state surveillance infrastructure integrated or dependent?',
    'Documentation of data-sharing agreements between platforms and government agencies; technical analysis of surveillance API integration; regulatory analysis of data transfer frameworks (GDPR, international agreements)',
    'If highly integrated: surveillance extraction becomes doubly constrained (commercial + state), snare classification deepens. If separable: decentralization path becomes viable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_surveillance_integration, empirical, 'Degree of integration between commercial and state surveillance infrastructure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(internet_evolution_lifecycle, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inet_tr_t0, internet_evolution_lifecycle, theater_ratio, 0, 0.2).
narrative_ontology:measurement(inet_tr_t15, internet_evolution_lifecycle, theater_ratio, 15, 0.45).
narrative_ontology:measurement(inet_tr_t30, internet_evolution_lifecycle, theater_ratio, 30, 0.65).

% Extraction over time
narrative_ontology:measurement(inet_be_t0, internet_evolution_lifecycle, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(inet_be_t15, internet_evolution_lifecycle, base_extractiveness, 15, 0.35).
narrative_ontology:measurement(inet_be_t30, internet_evolution_lifecycle, base_extractiveness, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(internet_evolution_lifecycle, global_infrastructure).
narrative_ontology:affects_constraint(internet_evolution_lifecycle, algorithmic_gatekeeping).
narrative_ontology:affects_constraint(internet_evolution_lifecycle, surveillance_capitalism_extraction).
narrative_ontology:affects_constraint(internet_evolution_lifecycle, network_effects_lock_in).
narrative_ontology:affects_constraint(internet_evolution_lifecycle, digital_public_square_control).

% DUAL FORMULATION NOTE:
% The open internet lifecycle decomposes into structurally distinct constraints: the original open protocols (TCP/IP, DNS, SMTP) form a Mountain/Rope family with low extraction; the platform monopolies that captured those protocols form a separate Snare/Tangled Rope family with high extraction. This story models the lifecycle as a single constraint to capture the transformation process. For empirical precision, downstream stories should decompose this into (1) open_internet_protocol_architecture (ε≈0.08, Mountain), (2) platform_surveillance_extraction (ε≈0.68, Snare), and (3) internet_governance_coordination (ε≈0.35, Tangled Rope). These stories are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(internet_evolution_lifecycle, analytical, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
