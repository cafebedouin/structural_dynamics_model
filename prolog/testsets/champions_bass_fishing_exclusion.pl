% ============================================================================
% CONSTRAINT STORY: champions_bass_fishing_exclusion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_champions_bass_fishing_exclusion, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: champions_bass_fishing_exclusion
 *   human_readable: Champions Bass Fishing Tournament Exclusionary Practices
 *   domain: economic/recreational_sports
 *
 * SUMMARY:
 *   Champions Bass Fishing tournaments exemplify a hybrid economic constraint
 *   that combines genuine coordination mechanisms (standardized tournament
 *   formats, venue scheduling, professional rules enforcement) with
 *   asymmetric extraction through barrier construction (entry fees, equipment
 *   requirements, multi-tier qualification systems). The constraint creates a
 *   two-tier system: elite professional anglers with established sponsorships
 *   and capital access experience the tournaments as coordination
 *   infrastructure that enables their careers; amateur and aspiring anglers
 *   from low-income or rural backgrounds experience the same structure as
 *   systematic exclusion. The constraint's extractiveness (0.52) reflects
 *   moderate extraction — less coercive than predatory lending or labor
 *   trafficking, but more systematic than pure coordination. Suppression
 *   (0.58) is substantial: alternative pathways to professional fishing exist
 *   (independent guide services, local tournaments, media careers) but are
 *   materially constrained relative to the tournament pathway. Theater ratio
 *   (0.48) indicates that the constraint maintains moderate performative
 *   content: televised tournaments and celebrity angler personas drive media
 *   engagement, but the selection mechanism (who advances) remains
 *   substantively tied to capital access rather than skill alone.
 *
 * KEY AGENTS:
 *   - Tournament Organizers: Primary beneficiary (institutional/arbitrage) — capture entry fees, sponsorship allocations, media rights; have exit options to alternative professional fishing structures
 *   - Elite Professional Anglers: Secondary beneficiary (powerful/arbitrage) — leverage tournament platform for sponsorships and prize money; already-established access makes barriers immaterial
 *   - Amateur/Aspiring Anglers: Primary victim (powerless/trapped) — face systematic barriers (capital, geographic, informational); tournament pathway is primary route to professional status; no viable alternatives
 *   - Rural/Low-Income Fishing Communities: Secondary victim (moderate/constrained) — lack capital for tournament entry; less access to sponsorship networks; some can access regional qualifiers but advancement is suppressed
 *   - Grassroots Fishing Equity Coalitions: Organized secondary actor (organized/constrained) — building alternative low-cost pathways; constrained by limited funding but mobilized by equity goals
 *   - Analytical Observer: Systemic perspective (analytical/analytical) — recognizes both coordination and extraction functions as structurally entangled
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(champions_bass_fishing_exclusion, 0.52).
domain_priors:suppression_score(champions_bass_fishing_exclusion, 0.58).
domain_priors:theater_ratio(champions_bass_fishing_exclusion, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(champions_bass_fishing_exclusion, extractiveness, 0.52).
narrative_ontology:constraint_metric(champions_bass_fishing_exclusion, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(champions_bass_fishing_exclusion, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(champions_bass_fishing_exclusion, tangled_rope).
narrative_ontology:human_readable(champions_bass_fishing_exclusion, "Champions Bass Fishing Tournament Exclusionary Practices").
narrative_ontology:topic_domain(champions_bass_fishing_exclusion, "economic/recreational_sports").

domain_priors:requires_active_enforcement(champions_bass_fishing_exclusion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(champions_bass_fishing_exclusion, tournament_organizers).
narrative_ontology:constraint_beneficiary(champions_bass_fishing_exclusion, elite_professional_anglers).
narrative_ontology:constraint_victim(champions_bass_fishing_exclusion, amateur_aspiring_anglers).
narrative_ontology:constraint_victim(champions_bass_fishing_exclusion, rural_low_income_fishing_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: AMATEUR ANGLER (SNARE) — Entry fees, equipment requirements, and tournament qualification tiers create multiple barriers to entry. Anglers from low-income or rural backgrounds face trapped exit: the tournament pathway is the primary route to professional fishing, but access requires capital they lack. Suppression is high — alternative pathways (sponsorship, informal competitions) are limited. Extraction is experienced as maximal from this structural position.
constraint_indexing:constraint_classification(champions_bass_fishing_exclusion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SEMI-PROFESSIONAL REGIONAL ANGLER (TANGLED ROPE) — Can access some tournament pathways through regional qualifying events and sponsorships, but advancement to national championship tiers requires sustained capital investment and geographic mobility. Benefits from tournament infrastructure (coordination function: venue, scheduling, rules standardization) but faces extraction through qualification tiers that advantage already-established competitors. Exit is constrained — they can fish independently but professional income requires tournament participation.
constraint_indexing:constraint_classification(champions_bass_fishing_exclusion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: TOURNAMENT ORGANIZERS / ELITE COALITION (ROPE) — Beneficiaries who experience the constraint as pure coordination: fee structures, qualification tiers, and sponsorship allocation mechanisms solve the collective action problem of organizing high-stakes competitions. These actors have arbitrage options (they can host private tournaments, negotiate sponsorships globally, shift to alternative professional fishing leagues). The constraint yields positive extraction toward this group — entry fees and sponsorship arrangements flow to organizers and established professionals.
constraint_indexing:constraint_classification(champions_bass_fishing_exclusion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: GRASSROOTS FISHING EQUITY COALITION (SCAFFOLD) — Organized advocacy groups and regional fishing clubs are building alternative entry pathways: pay-as-you-go local tournaments, youth fishing programs with scholarship funding, and open-access tournament tiers. These represent temporary scaffolding intended to lower barriers while professional tournaments retain their elite structure. Success depends on sustained funding and coalition maintenance; sunset occurs if alternative pathways become self-sustaining and reduce demand for exclusionary professional tournaments.
constraint_indexing:constraint_classification(champions_bass_fishing_exclusion, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PROFESSIONAL BASS FISHING LEAGUE LEGACY (PITON) — The institutional structure of established professional tournaments (entry tiers, media partnerships, sponsorship allocation) persists largely through inertia. Its primary function (identifying elite talent) has become secondary to rent extraction through fees and merchandise. Theater ratio is moderate (0.48) — the tournament spectacle (live broadcasts, dramatic final rounds, celebrity angler personas) maintains public engagement, but the underlying selection mechanism has atrophied. The structure is maintained because alternative professional fishing leagues have not fully displaced it, and changing the model would disrupt sponsor contracts.
constraint_indexing:constraint_classification(champions_bass_fishing_exclusion, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational scope, the constraint embeds both genuine coordination (standardized tournament formats enable cross-regional competition and sponsorship flow) and asymmetric extraction (capital and geographic barriers exclude talent pools systematically by socioeconomic status and region). The engine's analysis reveals neither pure natural law nor pure extraction — rather, a hybrid system where coordination mechanisms (competition rules, prize distribution) are entangled with extraction mechanisms (fee structures, qualification gates). This is the baseline constraint classification.
constraint_indexing:constraint_classification(champions_bass_fishing_exclusion, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(champions_bass_fishing_exclusion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(champions_bass_fishing_exclusion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(champions_bass_fishing_exclusion, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(champions_bass_fishing_exclusion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(champions_bass_fishing_exclusion, TR),
    TR >= 0.70.

:- end_tests(champions_bass_fishing_exclusion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): The constraint extracts from amateur/aspiring anglers through entry fees (often $500-$2000 per tournament), equipment requirements (specialized boats, electronics, tackle worth $50,000+), and time costs of travel to regional qualifiers. However, extractiveness is not maximal (not 0.70+) because some legitimate coordination benefits exist — the tournament system does standardize competition and create measurable rankings. The value reflects that extraction is substantial but intertwined with coordination function. Suppression (0.58): High barriers include capital requirements, geographic concentration of tournaments (forcing travel), informational asymmetry (sponsorship networks favor already-established competitors), and publication bias toward elite angler narratives. Rural and low-income anglers face trapped-exit conditions — they cannot exit professional fishing via tournament participation without substantial capital. Suppression is not total (0.70+) because some grassroots alternatives exist and some anglers do overcome barriers through exceptional talent or sponsorship. Theater (0.48): Moderate. The tournament spectacle (live broadcasts, dramatic final-day competitions, celebrity angler personas) drives media engagement and sponsorship revenue but does not fully substitute for the selection mechanism — tournaments still require participants to actually catch fish. Theater has increased over the interval as media production values have increased and celebrity marketing has expanded, but the core function (skill-based ranking) remains substantive.
 *
 * PERSPECTIVAL GAP:
 *   The constraint creates a perspectival gap between the institutional beneficiaries' experience (Rope — coordination infrastructure) and the powerless victims' experience (Snare — exclusionary barrier). This gap is characteristic of Tangled Rope: the same rules and mechanisms appear as enabling coordination to those with capital and as systematic exclusion to those without. The gap would not exist if barriers were truly negligible (would be pure Rope) or if the tournaments provided no coordination benefit (would be pure Snare). The Tangled Rope classification preserves both aspects: genuine coordination function exists AND asymmetric extraction exists. The perspectival gap also reveals the extraction mechanism: it is not coercive force but rather capital concentration. Grassroots alternatives (Scaffold perspective) show that the sunset logic is partially viable — alternative pathways are emerging — but their adoption depends on whether they can achieve sufficient scale and legitimacy to reduce dependence on professional tournaments.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from structural position and exit options. Tournament organizers and elite anglers (beneficiaries with arbitrage exit) derive low d (around 0.15) — they experience near-zero effective extraction because they control the constraint's allocation mechanisms and can exit to alternatives. Amateur/aspiring anglers (victims with trapped exit) derive high d (around 0.85-0.95) — they experience maximal extraction because the tournament pathway is their primary route to professional status and they cannot exit without abandoning professional fishing ambitions. Rural/low-income communities (victims with constrained exit) derive moderate-high d (around 0.65-0.75) — they have some alternatives (guide services, local tournaments) but meaningful exits are materially constrained. Grassroots coalitions (organized secondary actors with constrained exit) derive moderate d (around 0.50-0.55) — they are partly beneficiaries (building alternative infrastructure) and partly victims (constrained by limited funding). The directionality distribution across perspectives reflects the fundamental asymmetry: the constraint extracts from those who depend on it and benefits those who control it.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY QUESTION: Is this a coordination mechanism (tournament system organizing national-scale fishing competitions) being misidentified as pure extraction, or is it an extraction mechanism (capital gatekeeping) being legitimized as coordination? The mandatrophy is resolved by the Tangled Rope classification, which specifies that BOTH functions are present and structurally intertwined. The coordinate function (standardized tournaments, venue scheduling, professional rules) is real and benefits all participants by creating measurable rankings and sponsorship pathways. The extraction function (capital barriers, geographic concentration, elite network effects) is also real and benefits organizers and established professionals by creating supply constraints that justify premium fees and sponsorship concentration. The resolution mechanism: empirical investigation of whether capital access predicts tournament advancement as strongly as fishing skill. If merit selection is high (fishing skill dominates), the Rope classification is vindicated and the constraint is primarily coordination with secondary extraction. If merit selection is low (capital access and existing sponsorships dominate), the Snare classification is vindicated and the constraint is primarily extraction disguised as meritocratic competition. The Tangled Rope classification asserts that both factors are simultaneously present and neither dominates completely — the constraint is neither pure coordination nor pure extraction but a genuine hybrid.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    merit_selection_sufficiency,
    'Do tournament qualification tiers and entry fees genuinely select for fishing skill, or do they select for participant capital and geographic access?',
    'Empirical analysis: correlation between tournament placement and angler demographics (income, geography, sponsorship status); comparison of skill distributions in fee-based vs free-entry tournaments; tracking of tournament winners'' socioeconomic backgrounds over time',
    'If merit selection is high: qualification tiers are legitimate coordination mechanisms (Rope from more perspectives). If merit selection is low: tiers are pure extraction mechanisms disguised as meritocracy (Snare from more perspectives).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(merit_selection_sufficiency, empirical, 'Whether tournament tiers select for skill or for capital').

omega_variable(
    alternative_pathway_viability,
    'Can grassroots, low-cost tournament networks achieve sufficient scale and legitimacy to reduce dependence on professional tournament structures?',
    '5-year projection: participation rates in free/low-cost tournaments vs CBF tournaments; sponsorship funding trends for alternative pathways; angler career outcome comparisons (alternative pathway winners vs CBF qualifiers)',
    'If viable: scaffold sunset is real — alternative structures are becoming self-sustaining. If unviable: grassroots pathways remain supplementary, and professional tournament exclusion remains primary extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_pathway_viability, empirical, 'Viability of alternative low-cost tournament pathways').

omega_variable(
    sponsorship_gatekeeping_necessity,
    'Is the concentration of sponsorships among professional tournaments structurally necessary for fishing industry economics, or is it a choice that artificially inflates entry barriers?',
    'Analysis of sponsorship allocation: what proportion of fishing industry advertising budget flows to professional tournaments vs regional events; hypothetical modeling of sponsorship distribution under alternative tournament structures',
    'If necessary: sponsorship concentration is a coordination outcome (Rope logic). If contingent choice: it is an extraction mechanism (Snare logic).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sponsorship_gatekeeping_necessity, conceptual, 'Whether sponsorship gatekeeping is structurally necessary').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(champions_bass_fishing_exclusion, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cbf_tr_t0, champions_bass_fishing_exclusion, theater_ratio, 0, 0.32).
narrative_ontology:measurement(cbf_tr_t5, champions_bass_fishing_exclusion, theater_ratio, 5, 0.4).
narrative_ontology:measurement(cbf_tr_t10, champions_bass_fishing_exclusion, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(cbf_be_t0, champions_bass_fishing_exclusion, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(cbf_be_t5, champions_bass_fishing_exclusion, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(cbf_be_t10, champions_bass_fishing_exclusion, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(champions_bass_fishing_exclusion, resource_allocation).
narrative_ontology:affects_constraint(champions_bass_fishing_exclusion, professional_fishing_sponsorship_gatekeeping).
narrative_ontology:affects_constraint(champions_bass_fishing_exclusion, recreational_fishing_equipment_commodification).

% DUAL FORMULATION NOTE:
% The Champions Bass Fishing exclusion constraint is downstream of broader professional sports gatekeeping mechanisms but represents a distinct structural problem specific to fishing. Upstream constraints include sponsorship concentration in outdoor recreation industries; downstream constraints include the ripple effects of professional exclusion on rural fishing communities and guide service markets.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
