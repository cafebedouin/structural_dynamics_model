% ============================================================================
% CONSTRAINT STORY: bakuhan_outer_bandwidth_degradation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bakuhan_outer_bandwidth_degradation, []).

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
 *   constraint_id: bakuhan_outer_bandwidth_degradation
 *   human_readable: Tokugawa Bakuhan System Bandwidth Atrophy
 *   domain: japanese_history/governance_decline
 *
 * SUMMARY:
 *   The Tokugawa bakuhan system represents a distinctive failure mode in
 *   constraint classification: not anchored fixity (a constraint locked
 *   immovably in place) but bandwidth atrophy (a constraint that nominally
 *   retains authority while operationally losing capacity to execute). Under
 *   the Ieyasu founding generation (1603-1651), the bakufu possessed genuine
 *   operational bandwidth: reforms were implemented, daimyo compliance was
 *   enforced, and institutional innovation was possible. By the mid-Edo
 *   period (Kanbun 1661-1673 through Tempō 1830-1844), this capacity had
 *   eroded to theater: reform proposals were issued, elaborate administrative
 *   machinery was maintained, ceremonial compliance was performed, but
 *   genuine change could not be executed. By the late Edo period (Ansei
 *   1854-1860), the gap between nominal authority and operational bandwidth
 *   had become catastrophic — the bakufu could not respond to the Western
 *   threat because the institutional mechanisms for crisis mobilization no
 *   longer functioned. The bandwidth degradation was cumulative: each
 *   non-executed reform made the next harder, as administrative capacity,
 *   political coalition-building, and institutional memory all atrophied
 *   together. The constraint's extractiveness increased from 0.18 (founding
 *   era, genuine coordination) to 0.52 (late Edo, mostly extraction masked as
 *   coordination). Theater ratio increased from 0.28 (early era, mostly
 *   functional activity) to 0.68 (late era, mostly performative). This
 *   trajectory demonstrates that bandwidth atrophy is a distinct failure mode
 *   from anchored fixity — the system didn't get stuck in place; it
 *   maintained the appearance of functioning while becoming progressively
 *   less able to execute any substantial change.
 *
 * KEY AGENTS:
 *   - Ieyasu and Founding Shoguns (institutional/arbitrage): Possessed genuine operational bandwidth to implement reforms and enforce daimyo compliance; established the initial coordination framework
 *   - Mid-Edo Reform Shoguns (institutional/constrained): Attempted reforms (Kanbun, Kyōhō, Tempō) that calcified into theater; experienced the system's inability to execute meaningful change
 *   - Reform Faction (powerless/trapped): Intellectuals and administrators who recognized the system's dysfunction but could not exit; bore the extraction of invested reform effort with no functional outcome
 *   - Established Daimyo Hierarchy (powerful/mobile): High-status daimyo benefited from coordination function; experienced bandwidth degradation as maintaining acceptable status quo, not as crisis
 *   - Bakufu Administrative Apparatus (institutional/arbitrage): Maintained elaborate ceremonial machinery as theater of governance; their institutional positions depended on system persistence regardless of functional capacity
 *   - Bakumatsu Reform Movement (organized/mobile): Imperial restoration advocates, reform daimyo, and military innovators who recognized the system required replacement; mobile enough to switch loyalty when Western threat made old system's inadequacy undeniable
 *   - Western Powers (institutional/analytical): External observers who exposed the gap between nominal bakufu authority and operational capacity through unequal treaty demands
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bakuhan_outer_bandwidth_degradation, 0.52).
domain_priors:suppression_score(bakuhan_outer_bandwidth_degradation, 0.48).
domain_priors:theater_ratio(bakuhan_outer_bandwidth_degradation, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bakuhan_outer_bandwidth_degradation, extractiveness, 0.52).
narrative_ontology:constraint_metric(bakuhan_outer_bandwidth_degradation, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(bakuhan_outer_bandwidth_degradation, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bakuhan_outer_bandwidth_degradation, tangled_rope).
narrative_ontology:human_readable(bakuhan_outer_bandwidth_degradation, "Tokugawa Bakuhan System Bandwidth Atrophy").
narrative_ontology:topic_domain(bakuhan_outer_bandwidth_degradation, "japanese_history/governance_decline").

domain_priors:requires_active_enforcement(bakuhan_outer_bandwidth_degradation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bakuhan_outer_bandwidth_degradation, shogunal_core).
narrative_ontology:constraint_beneficiary(bakuhan_outer_bandwidth_degradation, established_daimyo_hierarchy).
narrative_ontology:constraint_victim(bakuhan_outer_bandwidth_degradation, reform_capacity).
narrative_ontology:constraint_victim(bakuhan_outer_bandwidth_degradation, crisis_response_capability).
narrative_ontology:constraint_victim(bakuhan_outer_bandwidth_degradation, administrative_flexibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE REFORM FACTION (SNARE) — Mid-period reformers (Kanbun, Kyōhō, Tempō periods) saw the bakuhan system's inability to adapt as a trap: their proposals were consistently calcified into ceremonial compliance, administrative theater that satisfied no actual functional need. Unable to exit institutional service without abandoning reform goals entirely, trapped between institutional loyalty and recognition that the system could not execute reform. Maximum experienced extraction — effort invested in reform design produced no operational change.
constraint_indexing:constraint_classification(bakuhan_outer_bandwidth_degradation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ESTABLISHED DAIMYO HIERARCHY (ROPE) — High-status tozama and fudai daimyo benefited from the bakuhan system's coordination function: stable allocation of domains, predictable career progression, shared maintenance of the status system. The system's bandwidth degradation was experienced as coordination maintenance, not extraction — as institutional effort required to sustain collective order. Mobile enough (could withdraw political support) to make the arrangement genuine coordination rather than coercion.
constraint_indexing:constraint_classification(bakuhan_outer_bandwidth_degradation, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE SHOGUNAL CORE (TANGLED ROPE) — The bakufu itself experienced the bandwidth constraint as an extraction mechanism it could not escape: maintaining the bakuhan coordination required ever-increasing ritual performance (Edo ceremonies, ceremonial castle maintenance, elaborate court protocols) while operational capacity declined. Constrained by the historical logic of the system itself — couldn't exit without abandoning the shogunate entirely. Extraction comes from the coordination requirement (maintaining daimyo compliance) through an increasingly dysfunctional mechanism (ritual theater rather than effective governance).
constraint_indexing:constraint_classification(bakuhan_outer_bandwidth_degradation, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: BAKUMATSU-ERA REORGANIZATION MOVEMENT (SCAFFOLD) — The organized movement for imperial restoration and structural reform (1853-1868) saw the bakuhan system as a temporary coordination failure requiring replacement, not repair. External pressure (Western ships, unequal treaties) provided the sunset mechanism — forced crisis made the old system's inadequacy undeniable and created political space for systemic change. Organized actors (reform daimyo, intellectuals, military innovators) had mobile exit options: switching loyalty to imperial restoration was politically viable by the 1860s. The system's theater ratio being high (0.68) enabled the movement to argue convincingly that bakufu authority was performative, not real.
constraint_indexing:constraint_classification(bakuhan_outer_bandwidth_degradation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: THE LATE-EDO ADMINISTRATIVE APPARATUS (PITON) — The elaborate administrative machinery of the late Edo period — the bugyō system, the inspection hierarchies, the ceremonial protocols — was substantially degraded from its original Ieyasu-era function. Theater ratio of 0.68 reflects that much of this apparatus was performative: maintaining the appearance of daimyo oversight while lacking genuine enforcement capacity, ritual confirmation of submission while lacking actual operational compliance verification. Inertial maintenance through institutional roles rather than functional necessity. Persisted because alternatives hadn't fully consolidated (until Meiji restoration), not because it worked.
constraint_indexing:constraint_classification(bakuhan_outer_bandwidth_degradation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: THE ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the bakuhan system's bandwidth decay appears as an inherent structural property of feudal coordination: systems organized around personal loyalty and ceremonial hierarchy inevitably lose operational capacity as they mature, because ritual performance replaces real problem-solving. This perspective naturalizes what is actually a contingent institutional arrangement. The engine's false summit detection will identify this as naturalization of a political choice (to prioritize status maintenance over administrative capacity) rather than a law of social organization.
constraint_indexing:constraint_classification(bakuhan_outer_bandwidth_degradation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bakuhan_outer_bandwidth_degradation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bakuhan_outer_bandwidth_degradation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bakuhan_outer_bandwidth_degradation, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(bakuhan_outer_bandwidth_degradation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(bakuhan_outer_bandwidth_degradation, TR),
    TR >= 0.70.

:- end_tests(bakuhan_outer_bandwidth_degradation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high and rising. In the founding era (value 0.18), the bakuhan system was primarily coordination — allocating domains, establishing status hierarchy, managing succession. By the late Edo period (value 0.52), the system functioned as extraction: maintaining the appearance of daimyo oversight while lacking genuine enforcement capacity, extracting compliance through ritual performance rather than effective governance, demanding administrative resources for theater rather than functionality. The intermediate value (0.35 at mid-Edo) represents the transition point where theater began replacing function. This rise is not driven by increased coercion from above but by the shogunal core's increasing inability to manage coordination except through ritual — as functional capacity atrophied, more elaborate performance was required to maintain the same compliance. Suppression (0.48): Moderate, stable across the interval. Daimyo faced significant barriers to refusing bakufu directives (legal sanctions, domain confiscation, loss of status) but these barriers were not total — daimyo could and did withdraw political support (sankin-kōtai disruption, refusal to fund bakufu projects). The constancy suggests that daimyo resistance was structural (inherent to the power relationship) rather than increasing over time. Theater ratio (0.68): High and rising. Founding-era bakufu governance was substantially functional — edicts were executed, reforms were implemented, daimyo compliance was verified through active administration. By late Edo, the same administrative apparatus was substantially performative — the elaborate inspection system verified ritual submission rather than actual compliance; the ceremonial structures maintained the appearance of integration rather than achieving it. The theater ratio rise reflects Goodhart drift: as the system's actual coordinating capacity declined, the bakufu invested more resources in the appearance of coordination to maintain compliance through deference to ritual rather than through fear of enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence: the same structural phenomenon is experienced as coordination (daimyo perspective), extraction (reform faction perspective), mixed coordination-extraction (bakufu perspective), and a temporary failure requiring replacement (bakumatsu movement perspective). The foundational gap is between powerful/mobile actors (daimyo) who could exit but benefited from the system's coordination function, and powerless/trapped actors (reform faction) who were committed to improving the system but could not escape it or achieve change. The bakufu's perspective is internally contradictory: they maintain nominal authority to coordinate daimyo relations while experiencing their own bandwidth constraints as extraction they cannot escape. The piton perspective reveals that the administrative apparatus itself was degraded — the bakufu's own maintenance of this apparatus was increasingly performative. The analytical observer risks naturalizing this as inevitable structural decay in feudal systems, when it is actually a contingent outcome of prioritizing status maintenance over administrative capacity. The Bakumatsu movement's scaffold perspective shows that alternatives were structurally possible — once Western pressure created the sunset mechanism, organized actors could reorganize the system entirely.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from the agent's structural relationship to the bandwidth extraction mechanism. Daimyo (powerful/mobile/beneficiary) experience low d: they benefit from the coordination function and can exit if terms become unfavorable, so the shogun cannot extract maximum power. Their f(d) value is low, making χ low or negative from their perspective — they experience the system as roughly fair coordination. Reform faction (powerless/trapped/victim) experience high d: they are committed to the institution but cannot escape it, and their reform efforts produce no functional outcome. Their f(d) value is high, making χ high — they experience maximum extraction (effort invested with no return). Bakufu (institutional/constrained/both beneficiary and victim) experience moderate d: they maintain authority over daimyo (beneficiary position) but cannot exercise operational capacity (victim position). Their f(d) value is moderate, reflecting their mixed position — they extract nominal compliance while being extracted from by the system's own inertia. The piton classification derives from high theater rather than from high experienced extraction — the administrative apparatus is degraded but maintains institutional positions through inertia. The analytical mountain risks deriving d from 'inevitability of feudal decay' rather than from actual structural relationships — the false summit detection should identify this.
 *
 * MANDATROPHY ANALYSIS:
 *   BANDWIDTH ATROPHY AS DISTINCT FROM ANCHORED FIXITY: This constraint resolves mandatrophy by distinguishing two failure modes that superficially appear identical but have different structural signatures. Anchored fixity (a constraint locked immovably in place, like an ossified caste system) keeps the same measurement values constant across time. Bandwidth atrophy (a constraint that nominally retains authority while operationally losing capacity) shows rising theater_ratio and rising extractiveness as the gap between appearance and function grows. The bakuhan system's measurements (theater rising from 0.28 to 0.68, extractiveness rising from 0.18 to 0.52) demonstrate atrophy, not fixity. The analytical observer's mountain classification risks naturalizing this as inevitable structural decay, when examination of the measurement trajectory shows it is contingent — the system functioned at founding, degraded during the middle period due to specific institutional choices (prioritizing status maintenance over administrative capacity), and became catastrophically non-functional by late Edo when the Western threat exposed the gap. The constraint is a tangled_rope from the bakufu perspective because the coordination function (daimyo hierarchy management) persists even as extraction (via theater and bandwidth degradation) rises. The scaffold classification from the bakumatsu perspective shows that alternatives were possible once the sunset mechanism (Western pressure) made the system's inadequacy undeniable. The mandatrophy is resolved by recognizing that this is a *governance failure mode* story, not a natural law story — the bandwidth atrophy is a choice-dependent outcome, not an inevitable property of feudal systems.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bandwidth_vs_authority_slippage,
    'At what point did nominal shogunal authority (power to issue edicts) and effective operational bandwidth (ability to execute reform or crisis response) become fully decoupled?',
    'Analysis of decree issuance vs. observable compliance/implementation across Kanbun, Kyōhō, Tempō, and Ansei periods. Measurement of time-lag between edict and effective administrative change. Comparison of reform proposal adoption rate.',
    'If decoupling occurred early (pre-1700): bakuhan system was a snare even from bakufu perspective for most of its history. If late (post-1800): reformability hypothesis is more viable, and late reforms'' failure is evidence of threshold crossing rather than inevitable decay.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(bandwidth_vs_authority_slippage, empirical, 'Timing of authority vs bandwidth decoupling').

omega_variable(
    reform_calcification_mechanism,
    'Was the Kanbun, Kyōhō, and Tempō reform calcification driven by daimyo resistance to change, shogunal inability to enforce change, or mutual investment in stability-through-theater?',
    'Analysis of daimyo responses to specific reform proposals — did they resist openly, comply performatively, or engage in genuine implementation? Examination of bakufu enforcement mechanisms and their evolution over time.',
    'If daimyo resistance: suppression value should increase (daimyo actively preventing reform). If shogunal inability: bandwidth atrophy is the primary mechanism. If mutual theater investment: both parties were complicit in maintaining the constraint — shifts classification toward tangled_rope from daimyo perspective as well.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_calcification_mechanism, empirical, 'Mechanism of reform calcification').

omega_variable(
    capacity_erosion_irreversibility,
    'Could the bakuhan system have recovered functional capacity at any point in the Edo period, or was the bandwidth degradation irreversible once administrative erosion reached critical mass?',
    'Counterfactual analysis of alternative reform sequences; examination of whether mid-period shoguns had structural capacity to execute broader reforms if they had attempted them; assessment of whether institutional memory loss was the binding constraint.',
    'If recoverable: reform failures are contingent political choices, not structural necessity — earlier intervention could have prevented late-period crisis. If irreversible: bandwidth atrophy becomes a deterministic feature of the system''s trajectory — extractiveness values should be adjusted upward to reflect inescapability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(capacity_erosion_irreversibility, conceptual, 'Whether bandwidth degradation was irreversible').

omega_variable(
    false_summit_natural_law_risk,
    'Is the bakuhan system''s bandwidth atrophy a natural structural property of feudal governance, or a contingent institutional arrangement that benefited specific power holders?',
    'Comparative analysis of other feudal systems (European feudalism, Chinese tributary systems, Japanese post-Meiji regional governance) for similar bandwidth degradation patterns. Identification of whether degradation is universal or context-specific.',
    'If universal natural law: mountain classification is justified, and bandwidth atrophy is inevitable in feudal systems. If contingent: beneficiaries can be identified (those who benefited from status maintenance over reform capacity), and FSM reclassification to tangled_rope is appropriate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law_risk, conceptual, 'Whether bandwidth atrophy is natural law or contingent institutional arrangement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bakuhan_outer_bandwidth_degradation, 0, 180).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bakuhan_theater_founding, bakuhan_outer_bandwidth_degradation, theater_ratio, 0, 0.28).
narrative_ontology:measurement(bakuhan_theater_mid_edo, bakuhan_outer_bandwidth_degradation, theater_ratio, 100, 0.52).
narrative_ontology:measurement(bakuhan_theater_late_edo, bakuhan_outer_bandwidth_degradation, theater_ratio, 180, 0.68).

% Extraction over time
narrative_ontology:measurement(bakuhan_extractiveness_founding, bakuhan_outer_bandwidth_degradation, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(bakuhan_extractiveness_mid_edo, bakuhan_outer_bandwidth_degradation, base_extractiveness, 100, 0.35).
narrative_ontology:measurement(bakuhan_extractiveness_late_edo, bakuhan_outer_bandwidth_degradation, base_extractiveness, 180, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bakuhan_outer_bandwidth_degradation, enforcement_mechanism).
narrative_ontology:affects_constraint(bakuhan_outer_bandwidth_degradation, sankin_kotai_ritual_theater).
narrative_ontology:affects_constraint(bakuhan_outer_bandwidth_degradation, han_domain_allocation_stability).

% DUAL FORMULATION NOTE:
% The bakuhan bandwidth degradation is upstream of specific institutional mechanisms (sankin-kōtai enforcement, domain allocation disputes) but represents a distinct structural constraint on the system's ability to adapt. Each downstream constraint exhibits the effects of reduced bakufu bandwidth while maintaining its own local extractiveness values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bakuhan_outer_bandwidth_degradation, organized, 0.32).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
