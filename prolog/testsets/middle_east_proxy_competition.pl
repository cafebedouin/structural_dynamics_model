% ============================================================================
% CONSTRAINT STORY: middle_east_proxy_competition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_middle_east_proxy_competition, []).

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
 *   constraint_id: middle_east_proxy_competition
 *   human_readable: Middle East Proxy Competition and Regional Entrapment
 *   domain: geopolitical/security
 *
 * SUMMARY:
 *   Middle East proxy competition represents a structural mechanism through
 *   which external great powers maintain regional domination while
 *   maintaining plausible deniability of direct colonialism. Client states
 *   are drawn into proxy relationships by security dilemmas (rivals are
 *   sponsored by opposing patrons, so accepting patronage becomes mandatory
 *   for survival), creating a trap where exit incurs existential risk. The
 *   constraint exhibits the full morphology of a snare: high extractiveness
 *   (regional states sacrifice sovereignty and resources to patron
 *   interests), high suppression (military necessity and absence of
 *   alternatives), and increasingly high theater (interstate institutions
 *   declare non-interference while members orchestrate proxy networks). The
 *   constraint has degraded over the 20-year measurement interval:
 *   extractiveness increased from 0.42 to 0.68 as patron competition
 *   intensified, and theater increased from 0.48 to 0.68 as institutional
 *   facades grew more performative relative to actual constraint function.
 *
 * KEY AGENTS:
 *   - External Great Powers: Primary beneficiaries (institutional/arbitrage) — extract strategic advantage, military basing, resource influence, and balance-of-power control without direct military commitment
 *   - Client State Governments: Primary victims (powerless/trapped) — subordinate foreign policy to patron requirements; sacrifice sovereignty and resources
 *   - Regional Civilian Populations: Secondary victims (powerless/trapped) — bear full cost of proxy conflict through casualties, displacement, infrastructure destruction
 *   - Regional Proxy Forces: Organized tertiary actors (organized/constrained) — coordinate collective defense against rival proxies while extracting territorial control and resource rents; trapped by mutual deterrence
 *   - Regional Interstate Institutions: Performative actors (institutional/constrained) — maintain facade of non-interference while member states openly sponsor proxies; theater increases as functional constraint decreases
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — views entire system as structural snare trapping region in subordination
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(middle_east_proxy_competition, 0.68).
domain_priors:suppression_score(middle_east_proxy_competition, 0.75).
domain_priors:theater_ratio(middle_east_proxy_competition, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(middle_east_proxy_competition, extractiveness, 0.68).
narrative_ontology:constraint_metric(middle_east_proxy_competition, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(middle_east_proxy_competition, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(middle_east_proxy_competition, snare).
narrative_ontology:human_readable(middle_east_proxy_competition, "Middle East Proxy Competition and Regional Entrapment").
narrative_ontology:topic_domain(middle_east_proxy_competition, "geopolitical/security").

domain_priors:requires_active_enforcement(middle_east_proxy_competition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(middle_east_proxy_competition, external_great_powers).
narrative_ontology:constraint_beneficiary(middle_east_proxy_competition, regional_military_industrial_complex).
narrative_ontology:constraint_victim(middle_east_proxy_competition, client_state_governments).
narrative_ontology:constraint_victim(middle_east_proxy_competition, civilian_populations).
narrative_ontology:constraint_victim(middle_east_proxy_competition, regional_state_sovereignty).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CLIENT STATE GOVERNMENT (SNARE) — Structurally trapped by military dependence, security threats from rival proxies, and lack of autonomous defense capacity. Cannot exit proxy relationships without facing immediate existential security threat. Maximum suppression through military necessity and absence of alternatives. Pure extraction flow toward great powers; state sovereignty is subordinated to patron requirements.
constraint_indexing:constraint_classification(middle_east_proxy_competition, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: CIVILIAN POPULATIONS (SNARE) — Bear full cost of proxy conflict (displacement, casualties, infrastructure destruction) with zero decision-making power. Trapped by geography and war. Suppression is absolute — coercion enforced through military violence. No beneficiary function exists for civilian victims; pure extraction of human capital and resources.
constraint_indexing:constraint_classification(middle_east_proxy_competition, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: REGIONAL PROXY FORCES (TANGLED ROPE) — Organized actors that both coordinate collective security (defensive against rival proxies) and extract rents (control territory, extort protection payments, capture state resources). Mixed extraction and coordination. Exit is constrained by mutual deterrence logic — standing down enables rival proxy dominance. Real agency and genuine coordination benefit alongside asymmetric extraction.
constraint_indexing:constraint_classification(middle_east_proxy_competition, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: EXTERNAL GREAT POWERS (TANGLED ROPE) — Coordinate regional stability and balance-of-power (genuine coordination function) while extracting strategic advantages, military basing rights, and economic leverage. Effective extraction through proxy mechanism without direct military commitment. High agency and exit options (can withdraw); moderate effective extraction because arbitrage options exist.
constraint_indexing:constraint_classification(middle_east_proxy_competition, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGIONAL INTERSTATE SYSTEM (PITON) — The formal architecture of state-to-state relations (UN, Arab League, Gulf Cooperation Council) is substantially performative in managing proxy competition. Interstate institutions declare principles of non-interference while members openly sponsor proxies. Theater ratio (0.68) reflects that institutional meetings, agreements, and statements persist despite minimal actual constraint on proxy activity. The system has atrophied — maintained through inertia and face-saving diplomacy rather than functional effectiveness.
constraint_indexing:constraint_classification(middle_east_proxy_competition, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — Viewing the entire regional system from civilizational/global scope reveals that proxy competition is a snare trapping the Middle East in perpetual conflict subordination to external power competition. The structural logic (prevent peer state dominance → sponsor proxies → lock in regional dependency → extract strategic advantage) is maximally extractive and suppressive. No agent at regional level has sufficient power to exit. The frame is snare.
constraint_indexing:constraint_classification(middle_east_proxy_competition, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(middle_east_proxy_competition_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(middle_east_proxy_competition, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(middle_east_proxy_competition, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(middle_east_proxy_competition, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(middle_east_proxy_competition, TR),
    TR >= 0.70.

:- end_tests(middle_east_proxy_competition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. External powers extract strategic advantage and resource influence without bearing direct military/economic cost. Client states sacrifice sovereign decision-making, resource autonomy, and internal stability. The value reflects sustained extraction over 20 years with increasing intensity — patron competition has deepened proxy commitment as each side fears losing regional footing. Suppression (0.75): Very high. Suppression operates through multiple mechanisms: (1) Military necessity — security dilemma means unilateral proxy abandonment invites rival dominance and state collapse. (2) Economic dependency — military aid and rentier state revenues flow through patron relationships. (3) Information asymmetry — external patrons control narrative about which regional actors are 'moderate' or 'extremist,' constraining client state negotiating space. (4) Institutional isolation — regional states that resist proxy logic face UN veto, sanctions, or isolation. Theater ratio (0.68): High and increasing. Interstate institutions (Arab League, GCC, UN regional mechanisms) produce summit statements, agreements, and declarations against interference, yet member states actively sponsor rival proxies. The theater has increased over time as institutional meetings have become more frequent while actual constraint effectiveness has declined — institutions persist through inertia and face-saving diplomacy.
 *
 * PERSPECTIVAL GAP:
 *   Client state governments experience constrained rather than trapped exit in the immediate/biographical timeframe — they can theoretically negotiate with patrons, balance between competing external powers, or invoke non-aligned positioning. However, at the biographical horizon, the constraint hardens: regional states that attempted non-alignment (e.g., Iraq under early Ba'athism, attempts at GCC-led neutrality) faced pressure, isolation, or security shocks that forced recommitment to proxy relationships. The gap between the institutional/arbitrage perspective (patrons see rope — balanced coordination with strategic advantage) and the powerless/trapped perspective (client states see snare — pure extraction) reveals the asymmetric exit capacity. The piton perspective (regional interstate system) shows that institutional constraint mechanisms have atrophied — the theater persists (summits, declarations) but the functional effect has declined to near-zero, indicating that the constraint is now maintained by raw asymmetry rather than by institutional cooperation.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for external great powers derives from beneficiary status + arbitrage exit options: low d (approximately 0.15-0.25), producing low/negative effective extraction chi. These are the designers and beneficiaries of the system. Directionality for client states derives from victim status + trapped exit options: high d (approximately 0.90-0.95), producing high effective extraction chi via sigmoid f(d). Client states experience the constraint as maximally extractive. Directionality for regional proxy forces derives from organized power + constrained exit (mutual deterrence prevents unilateral stand-down): moderate d (approximately 0.55-0.65), producing moderate chi reflecting both coordination benefit (collective defense against rival proxies) and extraction cost (patronage imposes constraints and extractive demands). Directionality for civilian populations derives from powerless status + trapped exit: d approximately 0.95, maximum f(d), experienced as pure snare.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint classified as snare at extractiveness 0.68 would face mandatrophy risk if the structure could be reframed as either pure coordination (rope) or inevitable natural law (mountain). The mandatrophy is resolved through perspectival differentiation: (1) The patron/great-power perspective legitimately sees rope — the balance-of-power coordination is real and mutual. (2) The regional perspective legitimately sees snare — the proxy mechanism implements the patron coordination on the backs of client states with asymmetric extraction. (3) The analytical perspective correctly identifies that the rope from the patron view is realized through snare mechanisms at the regional level — the overall system is snare-at-regional-scale even though it is rope-at-patron-scale. The mandatrophy is not a classification error but a perspectival boundary crossing. The system can only be rope for all participants if regional actors have genuine voice in the balance-of-power coordination — they do not. Therefore, the analytical view at global/civilizational scope correctly identifies the overall mechanism as snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    patron_commitment_duration,
    'Will external great powers sustain proxy competition indefinitely, or are there endogenous limits to commitment?',
    'Historical tracking of patron withdrawal patterns; analysis of sunk costs vs forward-looking strategic value; demographic/economic changes in patron societies affecting appetite for regional involvement',
    'If indefinite: the snare is truly inescapable without external shock. If commitment degrades: regional actors may regain autonomy as patron interest declines, but withdrawal could trigger acute state-failure crises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(patron_commitment_duration, empirical, 'Duration limits of external patron commitment to proxy competition').

omega_variable(
    regional_defection_cascade,
    'Can regional states defect from proxy competition collectively, or does unilateral exit create vulnerability to rivals?',
    'Game-theoretic analysis of coordination payoffs; historical cases of regional arms race de-escalation; feasibility of verified mutual proxy withdrawal agreements',
    'If collective defection is possible: the snare may be game-theoretic rather than structural (coordination problem solvable by agreement). If individual defection triggers predation: snare is structural (asymmetric exit costs).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_defection_cascade, empirical, 'Feasibility of collective defection from proxy competition').

omega_variable(
    sectarian_identity_lock,
    'Are regional sectarian identities (Sunni-Shia divide, ethnic-national fractures) authentic political cleavages or constructed narratives that serve proxy competition logic?',
    'Comparative historical analysis of pre-proxy-era sectarian relations; tracking of sectarian rhetoric intensity relative to proxy patron messaging; ethnographic and survey data on identity salience independent of conflict',
    'If sectarian identity is primary: proxy competition is a consequence of deep identity conflict (weaker snare framing). If identity is constructed/amplified by proxy dynamics: the snare manufactures its own legitimacy through narrative (stronger snare framing, instantiates identity_locked exit for regional populations).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sectarian_identity_lock, conceptual, 'Whether sectarian identity is authentic cleavage or proxy-conflict-constructed narrative').

omega_variable(
    economic_dependency_reversibility,
    'Can regional economies diversify away from military-security spending and rentier state models reinforced by proxy competition?',
    'Analysis of comparative economic diversification in regions with lower proxy competition; feasibility studies for non-rentier growth models in Middle East political-economy context; tracking of actual diversification attempts and their sustainability',
    'If reversible: the snare has economic-structural exit points that are politically constrained (deepens identity_lock and constrained-exit framing). If irreversible: the snare is locked by economic structure as well as security structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_dependency_reversibility, empirical, 'Reversibility of rentier-state economic dependency on proxy competition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(middle_east_proxy_competition, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(midd_tr_t0, middle_east_proxy_competition, theater_ratio, 0, 0.48).
narrative_ontology:measurement(midd_tr_t10, middle_east_proxy_competition, theater_ratio, 10, 0.58).
narrative_ontology:measurement(midd_tr_t20, middle_east_proxy_competition, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(midd_be_t0, middle_east_proxy_competition, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(midd_be_t10, middle_east_proxy_competition, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(midd_be_t20, middle_east_proxy_competition, base_extractiveness, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(middle_east_proxy_competition, enforcement_mechanism).
narrative_ontology:affects_constraint(middle_east_proxy_competition, regional_state_sovereignty_loss).
narrative_ontology:affects_constraint(middle_east_proxy_competition, sectarian_conflict_amplification).
narrative_ontology:affects_constraint(middle_east_proxy_competition, resource_curse_rentier_state).

% DUAL FORMULATION NOTE:
% Proxy competition is downstream of the great-power balance-of-power logic (the patron-coordination constraint) but represents a structurally distinct constraint at the regional level. The upstream constraint is rope (legitimate great-power coordination); the downstream implementation creates a snare for regional actors. These are linked via network.affects_constraints because regional entrapment depends on maintaining external patron competition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(middle_east_proxy_competition, organized, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
