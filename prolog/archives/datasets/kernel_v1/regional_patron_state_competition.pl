% ============================================================================
% CONSTRAINT STORY: regional_patron_state_competition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_regional_patron_state_competition, []).

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
 *   constraint_id: regional_patron_state_competition
 *   human_readable: Regional Patron State Competition
 *   domain: geopolitical/economic
 *
 * SUMMARY:
 *   Regional patron state competition creates a structural constraint where
 *   geopolitical rivalry between great powers (patron states) transforms
 *   medium-sized and smaller states (client states) into contested
 *   territories. The constraint operates through conditional benefits —
 *   security guarantees, economic aid, diplomatic recognition, market
 *   integration — that create asymmetric dependency relationships. Patron
 *   states compete to expand their regional spheres of influence; client
 *   states navigate between competing patrons to secure resources and
 *   security. This constraint exhibits the full spectrum of DR
 *   classifications depending on the observer's structural position. From the
 *   client population perspective, the constraint appears as pure extraction
 *   (snare) — trapped within geopolitical competition with no independent
 *   exit. From the client government perspective, it is a hybrid
 *   coordination-extraction mechanism (tangled rope) — genuine security and
 *   development benefits offset by political conditionality and reduced
 *   autonomy. From the patron state perspective, it is coordination (rope) —
 *   supporting allies extends influence at manageable cost. From regional
 *   institutional perspectives, it appears as degraded theater (piton) —
 *   formal coordination mechanisms persist despite patron pressure that
 *   undermines their effectiveness. From the non-aligned coalition
 *   perspective, it is a temporary coordination problem with structural exit
 *   paths (scaffold) — alternative patronage mechanisms and development
 *   partnerships create a sunset. From the civilizational realist
 *   perspective, it appears as immutable structural law (mountain) — weak
 *   states must align with great powers in anarchic international systems.
 *   The engine's false-summit detection will identify this last
 *   classification as naturalization: patron states and client elites are
 *   identifiable beneficiaries whose existence contradicts the claim that
 *   hierarchy is inherent to international structure.
 *
 * KEY AGENTS:
 *   - Patron State Core (institutional/arbitrage): Primary beneficiary — expands regional sphere, captures strategic positioning and resource access, maintains security architecture
 *   - Competing Patron State (powerful/constrained): Secondary beneficiary and rival — drives competition dynamics; both patrons benefit from client alignments
 *   - Client State Government (moderate/constrained): Victim and tactical agent — accepts political conditionality and autonomy restrictions in exchange for security and development benefits
 *   - Client State Population (powerless/trapped): Primary victim — dependent on patron-provided security and markets with no independent exit options
 *   - Regional Institutions (institutional/arbitrage): Performative theater maintainers — formal coordination mechanisms persist as theater masking bilateral patron-client transactions
 *   - Non-Aligned Coalition (organized/constrained): Counter-structure builders — organized movement to diversify partnerships and reduce bilateral patron dependency
 *   - Structural Realist Observer (analytical/analytical): Risks naturalizing contingent institutional arrangements as immutable features of anarchic international system
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(regional_patron_state_competition, 0.58).
domain_priors:suppression_score(regional_patron_state_competition, 0.68).
domain_priors:theater_ratio(regional_patron_state_competition, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(regional_patron_state_competition, extractiveness, 0.58).
narrative_ontology:constraint_metric(regional_patron_state_competition, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(regional_patron_state_competition, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(regional_patron_state_competition, tangled_rope).
narrative_ontology:human_readable(regional_patron_state_competition, "Regional Patron State Competition").
narrative_ontology:topic_domain(regional_patron_state_competition, "geopolitical/economic").

domain_priors:requires_active_enforcement(regional_patron_state_competition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(regional_patron_state_competition, patron_state_core).
narrative_ontology:constraint_beneficiary(regional_patron_state_competition, client_state_elites).
narrative_ontology:constraint_victim(regional_patron_state_competition, client_state_populations).
narrative_ontology:constraint_victim(regional_patron_state_competition, regional_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CLIENT STATE POPULATION (SNARE) — Trapped within contested geopolitical space. Dependent on patron-provided security, market access, and development aid with no independent exit. Caught between competing patronage claims; defection or neutrality risks economic deprivation or security vulnerability. Suppression through security dependency and economic integration prevents exit.
constraint_indexing:constraint_classification(regional_patron_state_competition, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: CLIENT STATE GOVERNMENT (TANGLED ROPE) — Constrained by security needs and economic development requirements but retains tactical agency through balancing strategies. Genuine coordination function: patron provides security umbrella and market integration. Asymmetric extraction: patron captures strategic positioning and resource access; client must accept political conditionality and reduced autonomy. Active enforcement through aid conditionality and implicit security guarantees.
constraint_indexing:constraint_classification(regional_patron_state_competition, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PATRON STATE CORE (ROPE) — Benefits from regional sphere expansion and client market integration. Experiences constraint as pure coordination: supporting client allies extends influence, access, and strategic positioning. Net beneficiary with exit options (can redirect patronage elsewhere). Arbitrage capacity allows selection among potential clients.
constraint_indexing:constraint_classification(regional_patron_state_competition, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COMPETING PATRON STATE (TANGLED ROPE) — Locked in ongoing competition for regional dominance. Coordination function: both patrons provide genuine security and development benefits that clients require. Asymmetric extraction: competition drives aid escalation and creates conditions for client elite capture. Enforcement through competitive matching of patron offers and implicit security commitments.
constraint_indexing:constraint_classification(regional_patron_state_competition, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: REGIONAL INSTITUTIONS (PITON) — Regional organizations (ASEAN, African Union, MERCOSUR) maintain formal neutral coordination functions but experience substantial theater ratio. The institutional frameworks for conflict resolution and economic integration persist despite patron state pressure that undermines their actual effectiveness. Formalized procedures (summit meetings, treaty mechanisms) are performative — real decisions are made through patron-client bilateral channels. Theater has risen as patrons have weaponized institutional participation.
constraint_indexing:constraint_classification(regional_patron_state_competition, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: NON-ALIGNED COALITION (SCAFFOLD) — Organized movement (India, South Africa, smaller states seeking strategic autonomy) to construct alternative patronage pathways and reduce bilateral dependency. Sees patron competition as temporary phase where distributed partnerships and South-South cooperation create exit routes. Has sunset logic: as client states build autonomous development capacity and diversify partnerships, bilateral patron dependency erodes. Constrained by donor concentration and security asymmetries but building structural alternatives.
constraint_indexing:constraint_classification(regional_patron_state_competition, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: STRUCTURAL REALIST OBSERVER (MOUNTAIN) — From civilizational scale, patron-client hierarchies appear inherent to anarchic international system: weak states must align with great powers for security; this is an immutable feature of international structure. However, engine false-summit detection will identify beneficiaries (patron states, client elites) whose existence contradicts the naturalization. The 'immutable anarchy' framing obscures contingent institutional choices about how great powers compete.
constraint_indexing:constraint_classification(regional_patron_state_competition, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(regional_patron_state_competition_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(regional_patron_state_competition, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(regional_patron_state_competition, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(regional_patron_state_competition, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(regional_patron_state_competition, TR),
    TR >= 0.70.

:- end_tests(regional_patron_state_competition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint exhibits substantial extraction — patron states capture strategic positioning, resource access, and policy influence over clients. But extraction is not maximized because genuine coordination benefits exist: patrons provide real security guarantees and market integration that clients need. The coordination benefit prevents classification as pure snare. The value reflects that the extraction mechanism is embedded within a functional coordination framework. Rising trajectory (0.35 → 0.58) indicates increasing extraction as competition intensifies: patron states escalate conditionality and elite capture mechanisms over time. Suppression (0.68): High. Multiple layers suppress exit options: security dependency (client cannot defend against regional threats without patron), economic dependency (patron controls trade preferences and development aid), diplomatic isolation (client that defects faces regional pressure), implicit military coercion (patron security guarantees can be withdrawn). Suppression has risen over the interval (0.55 → 0.68) as patron competition has intensified and client states have become more deeply integrated into patron-controlled supply chains and security architectures. Theater ratio (0.62): Moderate-high. Regional institutions (ASEAN, African Union, MERCOSUR) maintain formal coordination functions and decision procedures that are substantially performative. Real decisions are made through bilateral patron-client channels; institutional processes are theater that obscures this reality. Theater has risen over the interval (0.42 → 0.62) as patrons have weaponized institutional participation — sending high-level delegations to summit meetings, using institutional forums to broadcast their patronage narratives, while simultaneously pressuring clients bilaterally to defect from institutional consensus. The rising theater indicates that formal institutional processes are becoming increasingly decoupled from actual decision-making authority.
 *
 * PERSPECTIVAL GAP:
 *   The patron state and client government classifications diverge sharply on the same structural arrangement. Patron sees rope (pure coordination); client government sees tangled rope (mixed). This gap reveals the directionality asymmetry: the same transaction is experienced as coordination by the beneficiary and as extraction by the victim. The client population perspective (snare) is qualitatively different: the population has no voice in the negotiation and bears suppression costs without receiving direct benefits. The regional institution perspective (piton) reveals that formal mechanisms are theater masking bilateral transactions. The scaffold perspective reveals that alternative structures are being built at the margins. The mountain perspective is a false summit — naturalizing what is actually a choice about how patron states organize competition.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality structure reveals why beneficiaries and victims experience it differently. Patron states (beneficiary, arbitrage exit) experience low effective extraction because they can redirect patronage and capture value. Client governments (mixed status, constrained exit) experience moderate extraction because they gain security benefits but lose autonomy. Client populations (victim, trapped exit) experience high extraction because they bear suppression costs without receiving direct benefits. The asymmetry in exit options (arbitrage vs. constrained vs. trapped) produces the perspectival gap: the same institutional arrangement is coordination from the beneficiary view, extraction from the victim view. This is the structural definition of asymmetric extraction: one party chooses to participate (patron), another party accepts participation to survive (client government), a third party has participation imposed without choice (client population).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by demonstrating how the same structural data produces different classifications from different perspectives. No single type is 'correct' — the constraint genuinely exhibits coordination function (rope) from the beneficiary perspective and extraction function (snare) from the trapped victim perspective. The engine's claimed_type (tangled rope) reflects the primary agents' perspective — the client government that experiences both coordination benefits and extraction costs. This is the accurate classification for the central structural relationship. The false-summit mountain classification reveals the analytical risk: framing weak-state alignment as 'natural' or 'immutable' rather than as a contingent institutional arrangement that benefits identifiable actors (patron states and client elites). The piton classification reveals the secondary institutional risk: regional coordination mechanisms becoming theatrical cover for bilateral transactions. The scaffold classification reveals the emerging counter-structure: non-aligned coalitions building alternative patronage pathways to reduce bilateral dependency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    client_elite_capture_mechanism,
    'Do patron states intentionally cultivate client state elite factions, or does elite capture emerge as unintended consequence of aid flows and security partnerships?',
    'Intelligence documentation, leaked communications, tracked funding flows to client political factions, interview evidence from client state officials regarding patron pressure for specific elite appointments',
    'If intentional: patron states are orchestrating factional control (higher extraction, pure snare). If unintended: extraction mechanism is structural incentive misalignment (tangled rope classification strengthened). Changes assessment of systemic vs. agentic extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(client_elite_capture_mechanism, empirical, 'Whether patron elite capture is intentional strategy or structural side effect').

omega_variable(
    exit_option_feasibility,
    'Can a client state genuinely pursue strategic autonomy without patron security guarantees, or is exit functionally impossible given regional security environment?',
    'Case study analysis of states that have attempted non-alignment or patron switching: Costa Rica (security via US umbrella but regionalized), Finland (NATO accession after decades of constraint), Vietnam (post-Cold War balancing). Measurement of security costs, economic penalties, and diplomatic isolation incurred by exit attempts.',
    'If exit is feasible: client state powerlessness is overstated; reclassify from trapped to constrained. If exit is functionally impossible: client populations are trapped by structural security dependency (mountain-level constraint). Affects whether constraint is snare or tangled_rope from client perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_option_feasibility, empirical, 'Whether strategic autonomy is feasible for client states').

omega_variable(
    patron_competition_intensity_threshold,
    'At what level of patron competition intensity does the coordination benefit (security provision) fall below the extraction cost (political conditionality, elite capture)?',
    'Cross-regional comparison of security provision levels vs. political autonomy restrictions; measurement of aid flows vs. policy concessions; analysis of client state welfare outcomes under high-competition vs. low-competition patron regimes',
    'If threshold is low: most client states are in net-extraction regime (snare). If threshold is high: tangled_rope framing is accurate for majority of cases. Determines whether constraint is inherently extractive or contingent on competition intensity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(patron_competition_intensity_threshold, empirical, 'Patron competition intensity threshold for coordination vs. extraction trade-off').

omega_variable(
    regional_institution_functionality,
    'Do regional institutions provide genuine coordination benefits independent of patron state pressure, or are they purely theater masking bilateral patron-client transactions?',
    'Process tracing of major regional disputes: do institutional mechanisms produce outcomes independent of patron preferences? Measurement of institutional decision autonomy from bilateral patron pressure. Analysis of when institutional rules are enforced vs. when bilateral arrangements supersede them.',
    'If institutions are functional: scaffold perspective (sunset through institutional strengthening) is plausible. If purely theatrical: piton classification confirmed; exit path through institutional strengthening is aspirational rather than structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_institution_functionality, empirical, 'Whether regional institutions provide independent coordination benefits').

omega_variable(
    alternative_patronage_viability,
    'Can South-South alternatives (BRICS, Shanghai Cooperation Organization, non-aligned coalitions) genuinely reduce bilateral patron dependency, or do they reproduce the same hierarchical patron-client structure at reduced scale?',
    'Structural analysis of alternative patronage mechanisms: do they offer lower-extraction alternatives or merely diversify patron options? Measurement of conditionality levels, aid volatility, and policy autonomy under alternative vs. traditional patrons. Long-term tracking of client state outcomes under alternative partnerships.',
    'If viable alternatives exist: scaffold sunset is real; non-aligned coalition has structural basis. If alternatives reproduce hierarchy: constraint is structural feature of weak-state positioning (mountain-level); scaffold is aspirational. Determines whether exit paths are genuine or illusory.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_patronage_viability, empirical, 'Whether alternative patronage systems can reduce extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(regional_patron_state_competition, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rpsc_tr_t0, regional_patron_state_competition, theater_ratio, 0, 0.42).
narrative_ontology:measurement(rpsc_tr_t15, regional_patron_state_competition, theater_ratio, 15, 0.55).
narrative_ontology:measurement(rpsc_tr_t30, regional_patron_state_competition, theater_ratio, 30, 0.62).

% Extraction over time
narrative_ontology:measurement(rpsc_be_t0, regional_patron_state_competition, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(rpsc_be_t15, regional_patron_state_competition, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(rpsc_be_t30, regional_patron_state_competition, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(rpsc_su_t0, regional_patron_state_competition, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(rpsc_su_t15, regional_patron_state_competition, suppression_requirement, 15, 0.62).
narrative_ontology:measurement(rpsc_su_t30, regional_patron_state_competition, suppression_requirement, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(regional_patron_state_competition, resource_allocation).
narrative_ontology:affects_constraint(regional_patron_state_competition, client_state_elite_capture).
narrative_ontology:affects_constraint(regional_patron_state_competition, regional_security_dilemma).
narrative_ontology:affects_constraint(regional_patron_state_competition, development_conditionality_extraction).

% DUAL FORMULATION NOTE:
% Regional patron state competition is the umbrella constraint within which multiple specific extraction mechanisms operate: elite capture (patron targets client government factions), security dilemmas (patron competition escalates arms races), development conditionality (patron ties aid to policy concessions). Each downstream constraint has its own epsilon reflecting the specific extraction mechanism; the patron-state competition constraint represents the overarching structural framework that coordinates them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(regional_patron_state_competition, institutional, 0.15).
constraint_indexing:directionality_override(regional_patron_state_competition, powerful, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
