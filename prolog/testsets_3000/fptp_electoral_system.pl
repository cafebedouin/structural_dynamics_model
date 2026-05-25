% ============================================================================
% CONSTRAINT STORY: fptp_electoral_system
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fptp_electoral_system, []).

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
 *   constraint_id: fptp_electoral_system
 *   human_readable: The First-Past-the-Post (Plurality) Electoral System
 *   domain: political/institutional
 *
 * SUMMARY:
 *   The First-Past-the-Post electoral system is a structural constraint that
 *   exhibits all six DR types from different vantage points. At its origin
 *   (interval start), FPTP functioned primarily as coordination mechanism: it
 *   simplified voter choice, enabled local accountability, and clarified
 *   winner determination. Over time (interval progression), its extractive
 *   properties have become more pronounced as electorates have become more
 *   ideologically fragmented. Modern FPTP simultaneously enables major party
 *   coordination while systematically suppressing minor party representation.
 *   This creates a mixed constraint that combines genuine coordination
 *   function (simplified voting, clear local accountability) with substantial
 *   extraction (mathematical spoiler effect, geographic distortion of
 *   proportionality). The constraint's theater ratio has risen because much
 *   modern electoral discourse centers on defending familiar FPTP procedures
 *   rather than on defending their representational outcomes.
 *
 * KEY AGENTS:
 *   - Major Parties: Primary beneficiary (institutional/arbitrage) — experience FPTP as coordination mechanism that systematically suppresses competitors; can exit via supporting alternative systems
 *   - Minor Parties: Primary victim (moderate/constrained) — systematically suppressed by spoiler effect; constrained but not trapped; can attempt fusion voting or cross-endorsement
 *   - Voters Preferring Minor Parties: Severe victim (powerless/trapped) — mathematically coerced into strategic voting; no exit within single election cycle
 *   - Geographically Concentrated Interests: Beneficiary (powerful/mobile) — FPTP advantages groups clustered in districts; can form stable local coalitions
 *   - Geographically Dispersed Interests: Victim (powerless/trapped) — minorities scattered across districts receive no seats despite aggregate vote share
 *   - Electoral Representation Fidelity: Victim (powerless/trapped) — abstract collective good; systematic distortion between vote share and seat share; unmeasurable direct exit
 *   - Electoral Reform Coalition: Organized agent (organized/constrained) — building alternative systems with institutional sunset logic
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional choice as mathematical law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fptp_electoral_system, 0.58).
domain_priors:suppression_score(fptp_electoral_system, 0.62).
domain_priors:theater_ratio(fptp_electoral_system, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fptp_electoral_system, extractiveness, 0.58).
narrative_ontology:constraint_metric(fptp_electoral_system, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(fptp_electoral_system, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fptp_electoral_system, tangled_rope).
narrative_ontology:human_readable(fptp_electoral_system, "The First-Past-the-Post (Plurality) Electoral System").
narrative_ontology:topic_domain(fptp_electoral_system, "political/institutional").

domain_priors:requires_active_enforcement(fptp_electoral_system).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fptp_electoral_system, major_parties).
narrative_ontology:constraint_beneficiary(fptp_electoral_system, geographically_concentrated_interests).
narrative_ontology:constraint_victim(fptp_electoral_system, minor_parties).
narrative_ontology:constraint_victim(fptp_electoral_system, geographically_dispersed_interests).
narrative_ontology:constraint_victim(fptp_electoral_system, electoral_representation_fidelity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THIRD-PARTY VOTER (SNARE) — Structurally trapped by mathematical properties of FPTP. Voting for preferred candidate risks 'wasting' the vote and enabling worse outcome (spoiler effect). Strategic voting under duress becomes mandatory. No exit: cannot change local district structure or aggregate vote rules without national coordination. Experiences maximum extraction of ballot intention.
constraint_indexing:constraint_classification(fptp_electoral_system, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MINOR PARTY ORGANIZER (TANGLED_ROPE) — Constrained but not fully trapped. FPTP suppresses their vote share (15% votes = <5% seats), but the system also provides coordination function: district-level campaigns are relatively simple to organize, candidates are locally known, ballot access is clear. Experiences both coordination benefit and systematic extraction. Exit exists but costly (re-platforming to major party, supporting fusion voting experiments).
constraint_indexing:constraint_classification(fptp_electoral_system, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MAJOR PARTY LEADERSHIP (ROPE) — Benefits from FPTP's mechanical suppression of competitors. The system solves a genuine coordination problem: two-party binary choice simplifies voter decision-making and candidate recruitment. Major parties experience FPTP as coordination mechanism that happens to concentrate power in their favor. Can exit via supporting alternative systems but faces institutional inertia.
constraint_indexing:constraint_classification(fptp_electoral_system, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ELECTORAL REFORM COALITION (SCAFFOLD) — Organized agents (proportional representation advocates, ranked-choice voting proponents, local democracy labs) view FPTP as a solvable temporary coordination failure with clear institutional exit path: legal rule change to alternative voting method. Many jurisdictions (Maine, Alaska, New Zealand) have already transitioned or are transitioning away from pure FPTP. Sunset clause: as alternative systems prove viable and political barriers decline, FPTP's suppression mechanism loses institutional support.
constraint_indexing:constraint_classification(fptp_electoral_system, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: COLONIAL ELECTORAL LEGACY (PITON) — From civilizational view, FPTP is a degraded institutional artifact: it was designed for relatively homogeneous two-party systems in mid-20th century context. Its primary function (simplifying voter choice, enabling local accountability) persists but is substantially theatrical in modern context. The system is maintained through institutional inertia, habit, and resistance to complexity rather than because it optimally solves modern electoral coordination. Theater ratio reflects that much FPTP discourse centers on defending familiar procedures rather than defending their outcomes.
constraint_indexing:constraint_classification(fptp_electoral_system, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: MATHEMATICAL CONSTRAINT VIEW (MOUNTAIN) — From universal analytical perspective, FPTP appears as consequence of a mathematical law: any single-round, single-winner voting system with more than two candidates must allow some voters' preferences to be non-decisive (Arrow's impossibility theorem analogue). The claim is that FPTP is not a contingent institutional choice but a natural boundary of voting mathematics. HOWEVER: this perspective naturalizes a choice point (single-round, single-winner) that is itself contingent. The mathematical constraint is narrower than the institutional one.
constraint_indexing:constraint_classification(fptp_electoral_system, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fptp_electoral_system_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fptp_electoral_system, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fptp_electoral_system, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(fptp_electoral_system, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(fptp_electoral_system, TR),
    TR >= 0.70.

:- end_tests(fptp_electoral_system_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. FPTP creates measurable extraction of minor party vote share into major party seats. The system suppresses third-party representation even when they receive 15-20% national vote share (resulting in <5% seat share). However, the extraction is not as severe as pure snare mechanisms (which approach 0.90+) because the coordination function is real: two-choice ballot simplification genuinely reduces cognitive load and enables local accountability. The historical rise from 0.35 to 0.58 reflects increasing ideological fragmentation and rise of minor party movements (Greens, populists, independence parties), making FPTP's suppression more visible. Suppression (0.62): High. FPTP creates multiple barriers to minor party success: spoiler effect mathematically punishes vote-splitting, geographic representation advantage goes to spatially concentrated groups, and two-party equilibrium creates credibility barrier for third-party entry. But suppression is not total (0.90+) because voter loyalty and candidate recruitment are partly independent of FPTP mechanics. Theater ratio (0.45): Moderate. FPTP discourse contains substantial theatrical elements (defending procedure for procedural familiarity rather than outcomes), but the voting mechanism itself is mechanically simple and outcomes are algorithmically clear. Theater has risen historically as representational failures (geographic distortion, minority suppression) have become more visible, forcing defensive institutional rhetoric.
 *
 * PERSPECTIVAL GAP:
 *   Major parties see FPTP as coordination (Rope) — it genuinely simplifies their candidate recruitment and voter messaging. Third-party voters see extraction (Snare) — they are mathematically coerced into strategic voting. Minor party organizers see mixed extraction and coordination (Tangled Rope) — the system constrains them but also provides simple, locally-focused campaign structure. Reform coalition sees a temporary problem with institutional exit (Scaffold) — alternative voting methods are proven viable, sunset is real. The colonial legacy perspective sees degraded ritual (Piton) — FPTP is maintained through institutional habit rather than functional necessity in modern context. The mathematical perspective risks seeing immutable law (Mountain) — voting mathematics might require single-round, single-winner constraints — but this naturalizes a design choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is derived from structural position. Major party institutional actors experience low d (beneficiary with arbitrage exit) — they are structurally favored and can shift to alternative systems if politically expedient. Third-party voters experience high d (victim with trapped exit) — they cannot change voting rules within a cycle and cannot strategically exit spoiler effect. Minor party organizers experience mid-high d (victim with constrained exit) — they can attempt fusion voting or migration to major party, but with significant cost. Reform coalition experiences mid d (organized with constrained exit) — they have institutional power and exit strategy (legal rule change) but face major party resistance. The engine derives these directionalities from beneficiary/victim declarations and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   FPTP resolves the mandatrophy through clear perspectival differentiation. It is NOT purely extractive (snare) because it provides genuine coordination benefit: binary choice simplifies voting, district-level races enable accountability, outcome determination is mechanically clear. It is NOT purely coordinative (rope) because it systematically suppresses minority representation and creates spoiler effect trap for voters. The tangled_rope classification captures both: major parties coordinate through a system that happens to extract from minorities; voters experience it as mixed benefit (simplified choice) and harm (coerced strategy). The scaffold perspective reveals that the constraint has institutional exit path — electoral system change has occurred in New Zealand, Maine, Alaska, and is under active consideration in many jurisdictions. The piton perspective reveals theatrical maintenance: much defender rhetoric focuses on 'tried and tested' and 'simple' rather than on outcomes. The mountain perspective is false: FPTP is not a law of nature but a contingent institutional choice — mathematical voting constraints exist, but FPTP is one particular solution to them, not the inevitable one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    spoiler_effect_severity,
    'How much of FPTP''s suppression of minor parties is attributable to mechanical vote-splitting (spoiler effect) versus genuine voter preference for major parties?',
    'Jurisdictional comparison: vote-share vs seat-share analysis across FPTP jurisdictions; exit polling on strategic voting rates; simulation of ranked-choice outcomes in FPTP districts',
    'If spoiler dominates: FPTP is pure extraction mechanism (higher snare classification). If preference dominates: FPTP is legitimate coordination (higher rope classification from minor party perspective).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(spoiler_effect_severity, empirical, 'Attribution of minor party suppression to mathematical spoiler effect versus genuine voter preference').

omega_variable(
    accountability_preservation_capacity,
    'Do alternative voting systems (proportional representation, ranked choice, open list) preserve or degrade local district accountability that FPTP enables?',
    'Comparative institutional analysis across electoral systems; constituent service rates; MP-constituent contact frequency; correlation between geographic representation and policy responsiveness',
    'If alternatives preserve accountability: scaffold perspective confirmed, sunset is robust. If degraded: FPTP coordination function is less easily replaced, tangled_rope rather than scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accountability_preservation_capacity, empirical, 'Whether alternative voting systems maintain local accountability').

omega_variable(
    two_party_equilibrium_stability,
    'Is the two-party equilibrium under FPTP a stable attractor (mathematically inevitable) or a contingent historical outcome that could be disrupted by coordination?',
    'Game-theoretic modeling of voter choice under FPTP with varying candidate entry; historical analysis of third-party surges and their suppression; experimental economics with FPTP voting',
    'If stable attractor: FPTP constraint is closer to mountain (immutable aggregate property). If contingent: it is more clearly institutional choice (snare/tangled_rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(two_party_equilibrium_stability, conceptual, 'Whether FPTP two-party equilibrium is mathematically inevitable or contingent').

omega_variable(
    suppression_mechanism_attribution,
    'Is electoral suppression of minor parties driven by mechanical FPTP rules or by independent institutional factors (ballot access, campaign finance, debate rules)?',
    'Jurisdictional analysis of FPTP vs proportional systems with identical campaign finance and debate rules; counterfactual reconstruction of FPTP outcomes with relaxed ballot access',
    'If mechanical FPTP is dominant driver: suppression is intrinsic (snare). If institutional factors dominate: suppression could be reduced without changing voting rule (tangled_rope becomes more rope-like).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_attribution, empirical, 'Attribution of electoral suppression to mechanical rules versus institutional context').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fptp_electoral_system, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fptp_tr_t0, fptp_electoral_system, theater_ratio, 0, 0.25).
narrative_ontology:measurement(fptp_tr_t50, fptp_electoral_system, theater_ratio, 50, 0.38).
narrative_ontology:measurement(fptp_tr_t100, fptp_electoral_system, theater_ratio, 100, 0.45).

% Extraction over time
narrative_ontology:measurement(fptp_be_t0, fptp_electoral_system, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(fptp_be_t50, fptp_electoral_system, base_extractiveness, 50, 0.52).
narrative_ontology:measurement(fptp_be_t100, fptp_electoral_system, base_extractiveness, 100, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fptp_electoral_system, enforcement_mechanism).
narrative_ontology:affects_constraint(fptp_electoral_system, gerrymandering_district_design).
narrative_ontology:affects_constraint(fptp_electoral_system, two_party_political_polarization).
narrative_ontology:affects_constraint(fptp_electoral_system, campaign_finance_concentration).

% DUAL FORMULATION NOTE:
% FPTP is structurally linked to gerrymandering (district design becomes critical under FPTP winner-take-all), two-party polarization (FPTP mathematically incentivizes binary coalitions), and campaign finance concentration (major parties can acquire disproportionate resources due to fundraising advantages from FPTP position). These constraints form a family where FPTP is upstream: alternative voting systems reduce but do not eliminate the pressure toward these downstream effects.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fptp_electoral_system, powerful, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
