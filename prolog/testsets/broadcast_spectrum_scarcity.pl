% ============================================================================
% CONSTRAINT STORY: broadcast_spectrum_scarcity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_broadcast_spectrum_scarcity, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: broadcast_spectrum_scarcity
 *   human_readable: Broadcast Spectrum Scarcity and Regulatory Allocation
 *   domain: telecommunications/regulatory_policy
 *
 * SUMMARY:
 *   Broadcast spectrum is a finite physical resource managed through
 *   regulatory allocation. The constraint operates at the intersection of
 *   natural scarcity (only a limited portion of the electromagnetic spectrum
 *   is suitable for terrestrial broadcasting) and regulatory choice
 *   (government agencies grant exclusive licenses to specific frequency
 *   bands). This creates a structural hybrid: genuine coordination function
 *   (preventing interference between transmitters) layered with extractive
 *   mechanisms (incumbent protection, rent extraction through licensing
 *   scarcity). The constraint exhibits classic tangled rope structure:
 *   beneficiaries (incumbent broadcasters and the regulatory authority)
 *   depend on the allocation mechanism for coordination and profit; victims
 *   (new entrants, public interest broadcasters, emergency services) face
 *   suppression through barriers to spectrum access. Over the measurement
 *   interval, extractiveness and theater ratio have increased, reflecting the
 *   divergence between technical capability (digital technologies enable much
 *   higher spectrum reuse) and regulatory choice (allocation mechanisms
 *   remain locked to legacy analog-era assumptions). The piton perspective
 *   reflects that traditional broadcast television's technical coordination
 *   problem (interference prevention between analog transmitters) has been
 *   substantially solved by digital technologies and spectrum-sharing
 *   systems, yet regulatory protections persist through institutional
 *   inertia.
 *
 * KEY AGENTS:
 *   - Incumbent Broadcasters: Primary beneficiaries (institutional/arbitrage) — hold exclusive spectrum licenses with indefinite renewal; profits depend on spectrum scarcity; arbitrage regulatory relationships
 *   - New Market Entrants: Primary victims (powerless/trapped) — blocked from spectrum access by regulatory barriers and incumbent consolidation; cannot exit or compete within the constraint
 *   - Public Interest Broadcasters: Secondary victims (moderate/constrained) — limited spectrum access restricts community broadcasting and public interest content; can advocate but face organized incumbent opposition
 *   - Emergency Services: Secondary victims (moderate/constrained) — spectrum scarcity constrains emergency communication capacity during crises; face coordination challenges with civilian broadcast systems
 *   - Regulatory Authority: Institutional actor (institutional/arbitrage) — manages spectrum allocation; maintains power through rule definition; captures rents from licensing; experiences low extraction because authority defines the constraints
 *   - Legacy Broadcast Industry: Organized institutional actor (organized/constrained) — collective incumbent interest in maintaining regulatory protections; sees technical coordination problem as solved but supports preservation of allocation mechanism
 *   - Analytical Observer: Civilizational view (analytical/analytical) — observes hybrid of natural law (finite spectrum) and manufactured constraint (regulatory allocation); identifies increasingly unjustified suppression as technology enables spectrum sharing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(broadcast_spectrum_scarcity, 0.58).
domain_priors:suppression_score(broadcast_spectrum_scarcity, 0.68).
domain_priors:theater_ratio(broadcast_spectrum_scarcity, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(broadcast_spectrum_scarcity, extractiveness, 0.58).
narrative_ontology:constraint_metric(broadcast_spectrum_scarcity, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(broadcast_spectrum_scarcity, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(broadcast_spectrum_scarcity, tangled_rope).
narrative_ontology:human_readable(broadcast_spectrum_scarcity, "Broadcast Spectrum Scarcity and Regulatory Allocation").
narrative_ontology:topic_domain(broadcast_spectrum_scarcity, "telecommunications/regulatory_policy").

domain_priors:requires_active_enforcement(broadcast_spectrum_scarcity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(broadcast_spectrum_scarcity, incumbent_broadcasters).
narrative_ontology:constraint_beneficiary(broadcast_spectrum_scarcity, regulatory_authority).
narrative_ontology:constraint_victim(broadcast_spectrum_scarcity, new_market_entrants).
narrative_ontology:constraint_victim(broadcast_spectrum_scarcity, public_spectrum_access).
narrative_ontology:constraint_victim(broadcast_spectrum_scarcity, emergency_services).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NEW MARKET ENTRANT (SNARE) — Faces absolute barrier to spectrum access; cannot broadcast without license; regulatory allocation mechanism is opaque and favors incumbents. No exit option exists within the constraint. Maximum suppression experienced — trapped by regulatory gate and lack of capital to challenge established players.
constraint_indexing:constraint_classification(broadcast_spectrum_scarcity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PUBLIC INTEREST / EMERGENCY SERVICES (TANGLED ROPE) — Experiences both coordination benefits (spectrum prevents chaos and interference) and extraction (spectrum scarcity limits emergency communication capacity and public interest broadcasting). Can advocate for policy change but faces organized incumbent resistance. High suppression, moderate agency.
constraint_indexing:constraint_classification(broadcast_spectrum_scarcity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT BROADCASTER (ROPE) — Holds valuable spectrum license; experiences the constraint as coordination: clear frequency allocation prevents interference and enables profitable operation. Can arbitrage regulatory relationships to maintain position. Benefits from scarcity itself — license value increases as spectrum remains constrained.
constraint_indexing:constraint_classification(broadcast_spectrum_scarcity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REGULATORY AUTHORITY (ROPE) — Manages spectrum as a public resource but operates through allocation mechanism that concentrates benefits with incumbents and creates regulatory capture risk. Primary coordination function: preventing interference. Experiences low extraction because authority maintains power through rule definition. Theater ratio reflects performative consultation with public.
constraint_indexing:constraint_classification(broadcast_spectrum_scarcity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGACY BROADCAST INDUSTRY (PITON) — Historically dominant coordinating function (preventing interference) has been largely displaced by digital technologies and software-defined radio, yet regulatory protections for spectrum allocation remain through institutional inertia. Theater ratio is high because much regulatory activity around broadcast spectrum is now primarily performative — the technical problem was solved decades ago, but incumbent protections persist.
constraint_indexing:constraint_classification(broadcast_spectrum_scarcity, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — Spectrum scarcity is partially natural law (finite radio frequency bandwidth) and partially manufactured constraint (regulatory allocation mechanism). From a civilizational view, this is a hybrid: genuine coordination function (interference prevention) embedded in extractive regulatory capture. The natural scarcity of spectrum is real; the artificial scarcity created by spectrum licensing is contingent and increasingly unjustified by technological change.
constraint_indexing:constraint_classification(broadcast_spectrum_scarcity, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(broadcast_spectrum_scarcity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(broadcast_spectrum_scarcity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(broadcast_spectrum_scarcity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(broadcast_spectrum_scarcity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(broadcast_spectrum_scarcity, TR),
    TR >= 0.70.

:- end_tests(broadcast_spectrum_scarcity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and increasing. Initial extractiveness (0.35) reflected genuine coordination costs in managing terrestrial broadcast interference. Current extractiveness (0.58) reflects significant rent extraction through artificial scarcity. The incumbent broadcaster captures economic rents from exclusive spectrum access; new entrants face insurmountable barriers; public interest broadcasting is constrained by limited allocation. The increase over time reflects growing divergence between technical capability and regulatory choice — digital technologies have eliminated most technical scarcity, but regulatory mechanisms preserve artificial scarcity to protect incumbent value. Suppression (0.68): High. Barriers to spectrum access include: (a) regulatory licensing gate controlled by agency with incumbent capture risk, (b) high capital cost of competing against established broadcasters, (c) technical standards that favor incumbent technology choices, (d) political economy: incumbent broadcasters have organized lobbying capacity that new entrants lack. These barriers are enforced through legal penalties for unlicensed broadcasting. Theater ratio (0.55): Moderate and increasing. Regulatory processes (frequency reallocation studies, public consultation periods, environmental assessments) are partially performative. Many regulatory activities address technical problems that are already solved by software-defined radio and spectrum-sharing technologies. Theater has increased as the gap widens between what regulation claims to do (prevent interference, serve public interest) and what it actually does (protect incumbent rents, preserve administrative simplicity).
 *
 * PERSPECTIVAL GAP:
 *   The gap between incumbent broadcaster and new entrant perspectives is maximal. The incumbent sees coordination, safety (interference prevention), and beneficial scarcity. The entrant sees barriers, exclusion, and unjust scarcity. This gap reflects genuine disagreement about the constraint's function — is spectrum scarcity an immutable coordination problem or a manufactured barrier? The piton classification signals that the legacy broadcasting system has experienced technological obsolescence: software-defined radio and spectrum-sharing systems have solved the interference problem that originally justified spectrum allocation, yet regulatory protections persist. The analytical observer's tangled rope classification reframes the disagreement as a factual question: how much of the scarcity is physics vs. regulatory choice? As technology advances, the manufactured component increases, and the constraint should shift toward Snare classification unless regulatory reform decouples scarcity from allocation.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values determine experienced extractiveness for each agent through the sigmoid f(d). Incumbent broadcasters derive d from beneficiary status + arbitrage exit options → low d (~0.15) → negative or minimal f(d) → they experience low effective extraction, benefiting from the constraint. Regulatory authority derives d from beneficiary status + arbitrage exit options → low d (~0.15) → they maintain power through rule definition and low experienced extraction. New entrants derive d from victim status + trapped exit options → high d (~0.95) → high f(d) (~1.42) → they experience maximum effective extraction. Public interest/emergency services derive d from victim status + constrained exit options → high d (~0.85) → high f(d) (~1.15) → they experience significant effective extraction. The analytical observer derives d from neutral position + analytical exit options → d (~0.72) → moderate f(d) (~1.15) → sees the structure holistically but cannot resolve the natural vs. regulatory scarcity question from structural data alone.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by clarifying that 'spectrum scarcity' conflates two structurally distinct phenomena: (1) Natural electromagnetic scarcity (finite bandwidth suitable for terrestrial broadcasting), and (2) Regulatory artificial scarcity (exclusive licensing that restricts access below technical capacity). The constraint story integrates both: the natural scarcity justifies coordination (Rope classification from some perspectives), but the regulatory mechanism enables extraction (Snare classification from victim perspectives). The increasing theater ratio and extractiveness over the measurement interval reflects the growing gap between technical capability (spectrum-sharing technologies) and regulatory choice (unchanged allocation mechanism). Mandatrophy resolves by noting that the constraint's classification is not fixed — as technology enables spectrum reuse, the justified suppression component decreases and the extractive component increases. Current classification (Tangled Rope) is accurate but unstable: either regulation must modernize to enable spectrum sharing (sunset the artificial scarcity, shift toward Scaffold or Rope), or the constraint will degrade further toward Snare as the natural law justification evaporates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_regulatory_scarcity,
    'How much of the observed spectrum scarcity is physical law vs. regulatory choice?',
    'Technical analysis of spectrum utilization rates; comparison of licensed vs. unlicensed band efficiency; measurement of actual interference rates in dense spectrum use scenarios (e.g., WiFi, Bluetooth coexistence in unlicensed bands). Dynamic spectrum access systems that reallocate in real-time provide empirical upper bound on spectrum reuse potential.',
    'If regulatory choice dominates: constraint classification shifts toward Snare (manufactured scarcity enables extraction). If physical law dominates: classification remains Tangled Rope (genuine coordination with embedded extraction). Current evidence suggests 60-70% regulatory, 30-40% physical.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_vs_regulatory_scarcity, empirical, 'Proportion of scarcity attributable to physics vs. regulatory mechanism').

omega_variable(
    incumbent_protection_vs_public_benefit,
    'Does incumbent broadcaster protection serve genuine interference prevention or primarily extract economic rent?',
    'Historical analysis of interference incidents before and after deregulation in other markets (e.g., mobile spectrum auctions, unlicensed band opening). International comparison of spectrum efficiency in regulated vs. lightly-regulated regimes. Measurement of public interest broadcasting output under incumbent-protective vs. competitive allocation models.',
    'If genuine public benefit: suppression justified, classification remains Tangled Rope. If primarily rent extraction: suppression unjustified, classification degrades toward Snare. Current evidence suggests mixed: coordination benefit real but modest compared to extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbent_protection_vs_public_benefit, empirical, 'Whether incumbent protections serve coordination or extraction').

omega_variable(
    technology_substitution_timeline,
    'Will software-defined radio, cognitive radio, and dynamic spectrum access technologies eliminate the scarcity constraint entirely within this regulatory regime''s lifetime?',
    'Technical roadmap assessment; deployment timelines for spectrum-sharing technologies; measurement of spectrum utilization in experimental testbeds. Regulatory adoption timeline for technologies that would render traditional allocation obsolete.',
    'If technologies mature rapidly: constraint is a Scaffold with sunset (technological disruption will force reallocation). If regulatory inertia prevents adoption: constraint persists as Piton (degraded but maintained through institutional resistance).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_substitution_timeline, empirical, 'Timeline and feasibility of technology-driven spectrum constraint elimination').

omega_variable(
    regulatory_capture_depth,
    'Is the regulatory authority captured by incumbents, or does it maintain genuine independence in serving the public interest?',
    'Analysis of licensing decisions against stated criteria; comparison of incumbent renewal rates vs. challenger approval rates; measurement of regulatory responsiveness to public interest petitions vs. incumbent lobbying. Cross-national comparison of regulatory capture in spectrum allocation.',
    'If heavily captured: extractiveness increases, Snare perspective becomes more salient. If independent: Rope perspective is accurate, coordination function is genuine. Current evidence suggests significant but incomplete capture in most jurisdictions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_depth, empirical, 'Degree of regulatory capture by incumbent broadcasters').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(broadcast_spectrum_scarcity, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bss_tr_t0, broadcast_spectrum_scarcity, theater_ratio, 0, 0.35).
narrative_ontology:measurement(bss_tr_t15, broadcast_spectrum_scarcity, theater_ratio, 15, 0.48).
narrative_ontology:measurement(bss_tr_t30, broadcast_spectrum_scarcity, theater_ratio, 30, 0.55).
narrative_ontology:measurement(bss_tr_t50, broadcast_spectrum_scarcity, theater_ratio, 50, 0.6).

% Extraction over time
narrative_ontology:measurement(bss_be_t0, broadcast_spectrum_scarcity, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(bss_be_t15, broadcast_spectrum_scarcity, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(bss_be_t30, broadcast_spectrum_scarcity, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(bss_be_t50, broadcast_spectrum_scarcity, base_extractiveness, 50, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(broadcast_spectrum_scarcity, information_standard).
narrative_ontology:boltzmann_floor_override(broadcast_spectrum_scarcity, 0.08).
narrative_ontology:affects_constraint(broadcast_spectrum_scarcity, wireless_device_ecosystem).
narrative_ontology:affects_constraint(broadcast_spectrum_scarcity, internet_access_inequality).
narrative_ontology:affects_constraint(broadcast_spectrum_scarcity, emergency_communication_reliability).

% DUAL FORMULATION NOTE:
% Broadcast spectrum scarcity is a constraint family with three members: (1) broadcast_spectrum_scarcity (this story) — regulatory allocation mechanism, (2) wireless_device_ecosystem — how spectrum constraints drive mobile technology monopolies, (3) emergency_communication_reliability — how spectrum allocation affects public safety. Each has distinct ε value reflecting different measurement observable: regulatory extraction, technological lock-in, and emergency system capacity respectively.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(broadcast_spectrum_scarcity, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
