% ============================================================================
% CONSTRAINT STORY: iss_ped_ban
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_iss_ped_ban, []).

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
 *   constraint_id: iss_ped_ban
 *   human_readable: NASA's historical ban on personal electronic devices on the ISS
 *   domain: technological/institutional
 *
 * SUMMARY:
 *   NASA's ban on personal electronic devices on the ISS spans over two
 *   decades of crewed missions, reflecting an institutional approach to
 *   information control that combines genuine coordination needs with
 *   institutional extraction of astronaut autonomy. The constraint exhibits
 *   the full spectrum of indexical classification depending on the observer's
 *   structural position. For astronauts, the ban is a snare: they are trapped
 *   by contractual obligation with no exit option, bearing the full
 *   psychological cost of isolation while mission control captures all
 *   communication. For mission control, the ban is rope: it solves genuine
 *   coordination problems (unified communication protocols, bandwidth
 *   management, emergency response). For the open communications movement
 *   (crew unions, mental health advocates), the ban is a scaffold with a
 *   sunset — commercial spaceflight is proving that personal devices can
 *   coexist with operational security, creating an exit path within a
 *   generational timeframe. For the security protocols apparatus, the ban is
 *   a piton: the original justification (preventing espionage, ensuring
 *   operational security) is increasingly performative as cybersecurity
 *   capabilities have evolved, yet the ban persists through institutional
 *   inertia. The theater ratio has risen from 0.42 to 0.58 over twenty years
 *   as the functional security argument has weakened but the policy ritual
 *   has persisted. This constraint is a diagnostic exemplar of how
 *   institutional control mechanisms can be simultaneously genuine
 *   coordination (from the institution's perspective) and genuine extraction
 *   (from the isolated agent's perspective).
 *
 * KEY AGENTS:
 *   - Astronauts/Crew: Primary victims (powerless/trapped) — bear full psychological cost of isolation and communication control
 *   - Mission Control (NASA): Primary beneficiary (institutional/arbitrage) — benefits from unified communication protocols and information control authority
 *   - Crew unions and mental health advocates: Organized reformers (organized/constrained) — pushing for policy change with realistic sunset horizon
 *   - Security protocols division: Institutional maintainer (institutional/arbitrage) — preserves the ban through ritual compliance despite weakened functional justification
 *   - Commercial spaceflight operators: Competitive alternative (institutional/arbitrage) — demonstrating viable personal device policies, creating policy pressure
 *   - Analytical observer: Civilizational perspective (analytical/analytical) — risks naturalizing a contingent policy as an immutable constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(iss_ped_ban, 0.38).
domain_priors:suppression_score(iss_ped_ban, 0.65).
domain_priors:theater_ratio(iss_ped_ban, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(iss_ped_ban, extractiveness, 0.38).
narrative_ontology:constraint_metric(iss_ped_ban, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(iss_ped_ban, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(iss_ped_ban, tangled_rope).
narrative_ontology:human_readable(iss_ped_ban, "NASA's historical ban on personal electronic devices on the ISS").
narrative_ontology:topic_domain(iss_ped_ban, "technological/institutional").

domain_priors:requires_active_enforcement(iss_ped_ban).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(iss_ped_ban, mission_control).
narrative_ontology:constraint_beneficiary(iss_ped_ban, crew_safety_protocols).
narrative_ontology:constraint_beneficiary(iss_ped_ban, bandwidth_allocation).
narrative_ontology:constraint_victim(iss_ped_ban, astronaut_autonomy).
narrative_ontology:constraint_victim(iss_ped_ban, crew_morale_and_mental_health).
narrative_ontology:constraint_victim(iss_ped_ban, scientific_communication).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE ISOLATED ASTRONAUT (SNARE) — Six-month missions with no personal communication device. Trapped by contractual obligations and orbital mechanics; cannot exit. Full extraction of autonomy and connection to family. Maximum suppression: alternatives are zero. The astronaut bears the psychological cost of isolation while the institution captures control over all communication flows.
constraint_indexing:constraint_classification(iss_ped_ban, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE CREW COHORT (TANGLED ROPE) — Constrained by mission assignment but benefits from collective morale-building mechanisms (shared recreation time, Earth communication protocols). The ban coordinates crew focus but extracts individual autonomy. Mixed experience: some genuine coordination benefit (crew bonding) paired with substantial extraction (communication control).
constraint_indexing:constraint_classification(iss_ped_ban, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MISSION CONTROL (ROPE) — Experiences the ban as pure coordination: unified communication protocols, bandwidth management, emergency response time. No personal devices means predictable, monitorable information flows. Zero extraction experienced — the institution designed the constraint to benefit itself and genuinely solves collective action problems (who communicates what, when, through which channel). Net beneficiary with exit optionality via regulatory authority.
constraint_indexing:constraint_classification(iss_ped_ban, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN COMMUNICATIONS MOVEMENT (SCAFFOLD) — Organized advocates (crew unions, mental health researchers, commercial spaceflight operators) see the ban as a temporary institutional limitation with a sunset. SpaceX's Crew Dragon era and international partnerships are normalizing personal communications for crew morale and family connection. The constraint has an exit path: policy change is achievable within a decade as alternative protocols mature. Suppression is declining as norms shift.
constraint_indexing:constraint_classification(iss_ped_ban, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: SECURITY PROTOCOLS APPARATUS (PITON) — The rationale for the ban (preventing espionage, ensuring operational security) is increasingly performative. Modern cybersecurity, encryption, and monitored networks enable personal devices within a secure envelope. The original justification — 'personal devices compromise operational security' — is no longer structurally compelling but persists through institutional inertia. Security reviews are conducted; the findings are documented; the ban is maintained anyway. Theater ratio is high because the functional security benefit has degraded while the ritual persists.
constraint_indexing:constraint_classification(iss_ped_ban, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a pure communications theory perspective, the ISS is a closed system with finite bandwidth and limited power resources. Any personal device competes for these resources; at scale, personal communications would genuinely degrade mission-critical traffic. This constraint appears immutable — a consequence of orbital physics, not policy. However, the actual implementation (near-total ban rather than managed allocation) reveals this as naturalization of a policy choice, not a physical law. The engine's false summit detector will flag this.
constraint_indexing:constraint_classification(iss_ped_ban, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(iss_ped_ban_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(iss_ped_ban, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(iss_ped_ban, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(iss_ped_ban, TR),
    TR >= 0.70.

:- end_tests(iss_ped_ban_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The ban extracts astronaut autonomy and family connection, but the extraction is not as severe as pure control mechanisms (ε > 0.60) because mission control genuinely solves coordination problems. The extraction is blended with real operational coordination benefits. Suppression (0.65): Moderate-high. Astronauts have zero alternatives — they cannot exit without abandoning their career and mission. The suppression is structural: orbital mechanics and contractual obligation eliminate exit options. However, at the policy level, NASA does face pressure from crew advocates and commercial operators, creating some institutional escape routes (though high cost). Theater ratio (0.58): Moderate. The security justification has become increasingly performative as cybersecurity has matured, yet security reviews continue to be conducted and the ban is maintained. The rising trend (0.42 → 0.58) reflects increasing gap between stated rationale and functional justification. The constraint is maintained through ritual more than through genuine security necessity, but it is not yet pure theater (which would require theater_ratio > 0.70).
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a severe perspectival gap between the isolated astronaut and mission control. Astronauts perceive a snare (trapped, extractive, no exit). Mission control perceives a rope (coordinating, beneficial, low extraction). The gap arises because the astronaut's experience is purely cost (isolation, control) while mission control's experience is purely benefit (unified communication, authority, predictability). The open communications movement and security apparatus occupy intermediate positions: they see the ban as degrading (scaffold and piton) because they perceive the original justification as increasingly hollow. The analytical observer's mountain classification is a false summit — it naturalizes a policy choice as a physical law, obscuring the contingent institutional arrangement beneath.
 *
 * DIRECTIONALITY LOGIC:
 *   Astronauts: Full victim status (d ≈ 0.95) because they bear costs without agency. Trapped exit options compound the extraction — no alternative employment within the same role. Mission control: Full beneficiary status (d ≈ 0.05) because they capture all coordination benefits. Arbitrage exit options (regulatory authority, capability to revise policy) make the constraint optional for them. Open communications movement: d ≈ 0.45 (moderate victim) because they advocate for crew but have organizational agency. Constrained exit (policy change is possible but costly) reflects their mixed position. Security apparatus: d ≈ 0.20 (moderate beneficiary) because they benefit from control but increasingly perceive the benefit as performative. Arbitrage exit is theoretically available (they could recommend policy change) but organizationally constrained by institutional culture.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: This constraint demonstrates the mandatrophy by showing that labeling it as pure 'coordination' (rope) would naturalize extraction (snare), while labeling it as pure 'extraction' (snare) would obscure genuine coordination benefits. The tangled rope classification captures both: the ban solves real coordination problems (unified communication, bandwidth management, emergency response) while simultaneously extracting astronaut autonomy and family connection. The extraction is not incidental to coordination — it is the mechanism through which coordination is achieved. Mission control could implement monitored personal devices (as commercial operators do) and maintain coordination, but the current arrangement gives NASA greater control. The psychological cost to astronauts is not a tragedy of the coordination mechanism — it is the extracted value that makes the mechanism attractive to the institution. Recognizing this as tangled rope (not rope, not snare) enables policy analysis: the question becomes 'how much coordination benefit genuinely requires suppressing astronaut autonomy?' rather than 'is this coordination or extraction?' The answer, from commercial spaceflight experience, is 'less than the current ban achieves.' This resolves mandatrophy by converting it to a design question: what is the optimal balance point between coordination and autonomy, and does the current institutional arrangement achieve it or exceed the minimum necessary suppression?
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bandwidth_sufficiency,
    'How much crew communication bandwidth is genuinely consumed by mission-critical operations vs protocol overhead and redundancy?',
    'Engineering analysis of actual ISS communication logs; comparison of bandwidth utilization patterns; assessment of whether monitored personal devices could coexist within current allocation',
    'If mission-critical traffic < 60% of available bandwidth: the ban is policy choice, not technical necessity. If > 80%: the constraint may be structurally justified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(bandwidth_sufficiency, empirical, 'Actual bandwidth requirements for mission-critical vs overhead operations').

omega_variable(
    psychological_vs_security_tradeoff,
    'Does the psychological cost of isolation (crew morale, family connection, mental health) outweigh the security risk of monitored personal devices?',
    'Longitudinal crew mental health assessments; comparison with commercial space missions permitting personal communication; expert psychological/security panel review',
    'If psychological cost > security benefit: the ban is extractive (prioritizes control over crew welfare). If security risk > psychological cost: the ban is justified coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(psychological_vs_security_tradeoff, preference, 'Comparative cost of crew isolation vs security risk of personal devices').

omega_variable(
    commercial_viability_evidence,
    'Have commercial spaceflight operators (SpaceX, Axiom, Blue Origin) successfully implemented personal device policies with equivalent or superior security outcomes?',
    'Review of SpaceX Crew Dragon, Axiom Space Station, and private mission security protocols; incident reports; cybersecurity audits',
    'If yes with equivalent security: the ban is definitively a policy choice, not necessity. If no significant evidence: the ban may reflect genuine technical constraints NASA has solved differently than commercial operators.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(commercial_viability_evidence, empirical, 'Whether commercial operators have solved the personal device + security problem').

omega_variable(
    extraction_vs_coordination_intention,
    'Was the original ban motivated primarily by security/coordination (genuine collective benefit) or by control/extraction (institutional oversight preferences)?',
    'Historical documentation review; archived NASA memos, rationales, meeting minutes; expert analysis of temporal alignment with genuine security threats vs institutional culture',
    'If coordination-motivated: the tangled rope classification is accurate. If control-motivated: the snare/piton classification is more accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_intention, empirical, 'Original institutional motivation for the ban').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(iss_ped_ban, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(iss_ped_tr_t0, iss_ped_ban, theater_ratio, 0, 0.42).
narrative_ontology:measurement(iss_ped_tr_t10, iss_ped_ban, theater_ratio, 10, 0.52).
narrative_ontology:measurement(iss_ped_tr_t20, iss_ped_ban, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(iss_ped_be_t0, iss_ped_ban, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(iss_ped_be_t10, iss_ped_ban, base_extractiveness, 10, 0.36).
narrative_ontology:measurement(iss_ped_be_t20, iss_ped_ban, base_extractiveness, 20, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(iss_ped_ban, enforcement_mechanism).
narrative_ontology:affects_constraint(iss_ped_ban, crew_mental_health_degradation).
narrative_ontology:affects_constraint(iss_ped_ban, family_communication_deprivation).

% DUAL FORMULATION NOTE:
% The ISS personal device ban is a coordination mechanism (mission control's perspective) upstream of crew morale constraints (astronaut's perspective). These are not the same constraint viewed differently — they are distinct constraints with causal dependency. The ban affects the crew's capacity to maintain family connections, which in turn affects psychological resilience in isolation. The upstream constraint (the ban as coordination/extraction) has lower ε (0.38) reflecting blended coordination; the downstream constraints (morale degradation, communication deprivation) have higher ε reflecting pure extraction of psychological well-being.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(iss_ped_ban, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
