% ============================================================================
% CONSTRAINT STORY: suppression_of_civil_society_assembly
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_suppression_of_civil_society_assembly, []).

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
 *   constraint_id: suppression_of_civil_society_assembly
 *   human_readable: Suppression of Civil Society Assembly
 *   domain: political/governance/civic_rights
 *
 * SUMMARY:
 *   Suppression of civil society assembly is a structural constraint that
 *   prevents collective coordination of political voice through coercion and
 *   surveillance. The constraint operates at the national scale through legal
 *   prohibitions (permit requirements, public order statutes), physical force
 *   (police dispersal, arrest), and psychological suppression (surveillance,
 *   informant networks, threat of violence). It extracts political efficacy
 *   from dissenting populations while concentrating power in state hands. The
 *   constraint is enforced through a machinery of specialized institutions
 *   (police, security services, courts) and operates through both visible
 *   coercion and invisible surveillance. The extractiveness has increased
 *   over the interval (0.42 → 0.68) as surveillance technology and permit
 *   denial mechanisms have expanded. The theater ratio has similarly risen
 *   (0.35 → 0.62) as constitutional frameworks maintain the appearance of
 *   assembly rights while de facto suppression expands through emergency
 *   powers and national security doctrine.
 *
 * KEY AGENTS:
 *   - Dissenting Populations: Primary victim (powerless/trapped) — bear full suppression cost; no exit option
 *   - Civil Society Organizations: Secondary victim (moderate/constrained) — face legal liability, arrests, permit denial; constrained exit via organizational shutdown
 *   - State Apparatus: Primary beneficiary (institutional/arbitrage) — captures concentrated power and ability to prevent opposing coordination
 *   - Middle-Tier Enforcement Agencies: Complicit actor (institutional/constrained) — benefit from hierarchy but constrained by orders to suppress
 *   - Constitutional Framework: Degraded institution (institutional/arbitrage) — maintains legal theater while de facto suppression operates in parallel
 *   - Analytical Observer: Cross-national view (analytical/analytical) — sees consistent snare structure across geographic and regime contexts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(suppression_of_civil_society_assembly, 0.68).
domain_priors:suppression_score(suppression_of_civil_society_assembly, 0.78).
domain_priors:theater_ratio(suppression_of_civil_society_assembly, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(suppression_of_civil_society_assembly, extractiveness, 0.68).
narrative_ontology:constraint_metric(suppression_of_civil_society_assembly, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(suppression_of_civil_society_assembly, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(suppression_of_civil_society_assembly, snare).
narrative_ontology:human_readable(suppression_of_civil_society_assembly, "Suppression of Civil Society Assembly").
narrative_ontology:topic_domain(suppression_of_civil_society_assembly, "political/governance/civic_rights").

domain_priors:requires_active_enforcement(suppression_of_civil_society_assembly).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(suppression_of_civil_society_assembly, state_apparatus).
narrative_ontology:constraint_beneficiary(suppression_of_civil_society_assembly, incumbent_power_structures).
narrative_ontology:constraint_victim(suppression_of_civil_society_assembly, civil_society_organizations).
narrative_ontology:constraint_victim(suppression_of_civil_society_assembly, collective_political_voice).
narrative_ontology:constraint_victim(suppression_of_civil_society_assembly, dissenting_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISSENTING POPULATION (SNARE) — Trapped within national jurisdiction with no exit option for expressing collective grievances. Subject to arrest, surveillance, dispersal, and violence for attempting assembly. Maximum extraction: the constraint removes the right to coordinate, leaving isolated individuals bearing full cost of suppression.
constraint_indexing:constraint_classification(suppression_of_civil_society_assembly, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CIVIL SOCIETY ORGANIZATION (SNARE) — Constrained by legal liability, founder/member arrests, permit denial, and funding freezes. High costs to exit (organizational dissolution, loss of institutional capacity). Primary extraction: loss of platform and political efficacy. Secondary suppression: infiltration, informant networks, and legal harassment reduce capacity to organize even nominally permitted activities.
constraint_indexing:constraint_classification(suppression_of_civil_society_assembly, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE APPARATUS (ROPE) — Experiences the constraint as coordination mechanism: suppression aggregates state power, prevents opposition coordination, and centralizes decision-making authority. Net beneficiary — extraction flows toward this actor. State experiences constraint as solution to collective action problem (preventing coordinated challenges to state authority).
constraint_indexing:constraint_classification(suppression_of_civil_society_assembly, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: COMPLICIT MIDDLE-TIER APPARATUS (TANGLED ROPE) — Police departments, local administrators, and security services are both enforcers and victims. They benefit from centralized authority and resource allocation (rope function) but are also constrained by orders to suppress, ethical dilemmas, and institutional fragility. Mixed classification reflects genuine coordination role (maintaining state hierarchy) alongside asymmetric extraction (forced participation in repression).
constraint_indexing:constraint_classification(suppression_of_civil_society_assembly, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: FORMAL CONSTITUTIONAL FRAMEWORK (PITON) — Written constitutional guarantees of assembly rights exist in most suppressive regimes, but are performatively overridden by emergency powers, national security doctrines, and public order statutes. The formal legal framework persists through institutional inertia while actual suppression operates through parallel de facto mechanisms. Theater ratio high: constitutional theater of rights exists alongside de facto suppression regime.
constraint_indexing:constraint_classification(suppression_of_civil_society_assembly, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From cross-national analysis, suppression of assembly is consistently classified as snare: high extraction, high suppression, reliance on coercion and surveillance for maintenance, minimal coordination benefit to victims. This classification is consistent across institutional contexts (authoritarian, hybrid, democratic backsliding) and geographic regions.
constraint_indexing:constraint_classification(suppression_of_civil_society_assembly, snare,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(suppression_of_civil_society_assembly_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(suppression_of_civil_society_assembly, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(suppression_of_civil_society_assembly, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(suppression_of_civil_society_assembly, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(suppression_of_civil_society_assembly, TR),
    TR >= 0.70.

:- end_tests(suppression_of_civil_society_assembly_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Suppression removes the fundamental right to collective political coordination. The extraction is not partial or negotiable — either assembly is permitted or it is not. The value reflects the near-total removal of one mode of political voice. Suppression (0.78): Very high. Barriers to exit are material (arrest, violence, surveillance), structural (geographic jurisdiction, legal liability), and psychological (fear, internalized self-censorship). Suppression is intentionally layered: legal prohibitions create formal justification; enforcement creates physical costs; surveillance creates information asymmetry that makes safe coordination impossible. Theater ratio (0.62): Moderate-high. Constitutional guarantees of assembly remain performatively in place in most regimes while de facto suppression operates through emergency powers and statutory exceptions. As the regime matures, the ratio rises — the gap between formal rights and actual enforcement widens. Measurements show extractiveness rising more slowly than theater, suggesting that as regimes consolidate, they rely less on visible coercion and more on invisible surveillance and psychological suppression.
 *
 * PERSPECTIVAL GAP:
 *   The primary gap is between the state apparatus (experiences rope: centralized coordination of authority) and the dissenting population (experiences snare: pure extraction with no benefit). The state sees the constraint as solving the collective action problem of maintaining hierarchy; the dissenting population sees it as preventing their collective action problem (mobilizing political voice). A secondary gap appears between the formal constitutional framework (claims rope: coordination mechanism for protecting rights) and the de facto suppression apparatus (executes snare: extraction through coercion). The middle-tier enforcement agencies experience tangled rope: they coordinate state power (rope function) while being constrained to participate in repression (snare extraction). These perspectival gaps reveal that the constraint's primary function (from state view) is coordination of state power, but its effect on victims is pure extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit options. The state apparatus has arbitrage exit options (can choose enforcement level) and benefits from suppression, yielding low d (beneficiary position). The dissenting population has trapped exit options (cannot leave jurisdiction) and bears victim costs, yielding high d (full target position). The civil society organization has constrained exit (can dissolve but at high cost) and bears victim costs, yielding high d but modulated by the organizational power level (moderate rather than powerless). The middle-tier apparatus has constrained exit (cannot refuse orders without legal consequences) and is both beneficiary (shares state power concentration) and victim (forced participation in repression), yielding mid-range d. The analytical observer has analytical exit (can change frame) and no structural position in suppression, yielding derived d reflecting the aggregate victim burden.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that suppression of assembly is a snare from the analytical perspective but appears as rope from the state beneficiary perspective. The state's experience of suppression as coordination (rope) naturalizes what is extraction (snare) from the victim perspective. The formal constitutional guarantee of assembly rights creates false rope framing — it appears as a coordination mechanism for protecting political voice, but it operates as scaffolding for snare enforcement (rights are guaranteed but enforcement is suppressed). The middle-tier institutional perspective shows tangled rope because enforcement agencies genuinely coordinate state function while being extracted from (forced participation in repression). The piton classification of the constitutional framework reveals that formal rights persist through institutional inertia while actual suppression operates through de facto mechanisms. The constraint's theater (constitutional rights language) masks its function (extraction of political voice). Mandatrophy resolution requires understanding that the state's rope experience is real (they are solving a coordination problem) but structurally depends on the snare extraction from victims. The constraint cannot function as rope without functioning as snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    violence_escalation_threshold,
    'At what frequency/intensity of state violence does suppression transition from institutional coercion to state terror, and does the transition change the constraint''s structural type?',
    'Comparative analysis of suppression regimes by incident frequency, severity (arrests vs. lethal force), and population exposure. Correlation with victim organization rates and collective action attempts.',
    'If escalation threshold is exceeded: suppression may lose all coordination function and reclassify as pure extraction snare. If below threshold: some regimes may show tangled rope characteristics (state legitimacy mixed with suppression).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(violence_escalation_threshold, empirical, 'Violence escalation threshold triggering transition to state terror').

omega_variable(
    underground_coordination_capacity,
    'Can dissenting populations organize effectively through underground/clandestine networks, and if so, does this change the ''trapped'' exit classification to ''constrained''?',
    'Historical analysis of clandestine organizing success rates; study of arrested underground networks; comparison of above-ground vs underground movement effectiveness and sustainability.',
    'If clandestine organization is viable: exit_options shift from trapped to constrained for some subpopulations (those with network access). Perspectives would reclassify from snare to tangled_rope or constrained/mobile rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(underground_coordination_capacity, empirical, 'Viability of underground coordination networks').

omega_variable(
    state_apparatus_fragmentation,
    'When enforcement agencies (police, military, local government) refuse or selectively enforce suppression orders, does the constraint lose coercive enforcement capacity and degrade to piton?',
    'Analysis of regime transitions where security force defection coincided with suppression failure; study of selective enforcement patterns and their relationship to constraint collapse.',
    'If state apparatus fragmentation is primary failure mechanism: suppression is contingent on monopoly enforcement, not on structural inevitability. This suggests the snare classification is accurate but the regime is unstable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_apparatus_fragmentation, empirical, 'State apparatus fragmentation as suppression mechanism failure').

omega_variable(
    international_spillover_dynamics,
    'Do international legal mechanisms (UN human rights bodies, ICC, foreign sanctions) effectively constrain suppression, or do they merely create theater without reducing enforcement capacity?',
    'Analysis of correlation between international condemnation/sanctions and actual reduction in suppression incidents. Study of regime behavior before vs after international attention.',
    'If international mechanisms are effective: they reduce scope of viable suppression (or raise costs), potentially moving suppression toward piton (theater) or scaffold (sunset if sanctions create exit pressure).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(international_spillover_dynamics, empirical, 'International mechanism effectiveness in constraining suppression').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(suppression_of_civil_society_assembly, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(supp_civil_tr_t0, suppression_of_civil_society_assembly, theater_ratio, 0, 0.35).
narrative_ontology:measurement(supp_civil_tr_t10, suppression_of_civil_society_assembly, theater_ratio, 10, 0.5).
narrative_ontology:measurement(supp_civil_tr_t20, suppression_of_civil_society_assembly, theater_ratio, 20, 0.62).
narrative_ontology:measurement(supp_civil_tr_t5, suppression_of_civil_society_assembly, theater_ratio, 5, 0.42).

% Extraction over time
narrative_ontology:measurement(supp_civil_be_t0, suppression_of_civil_society_assembly, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(supp_civil_be_t10, suppression_of_civil_society_assembly, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(supp_civil_be_t20, suppression_of_civil_society_assembly, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(supp_civil_be_t5, suppression_of_civil_society_assembly, base_extractiveness, 5, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(suppression_of_civil_society_assembly, enforcement_mechanism).
narrative_ontology:affects_constraint(suppression_of_civil_society_assembly, permit_denial_systems).
narrative_ontology:affects_constraint(suppression_of_civil_society_assembly, surveillance_infrastructure).
narrative_ontology:affects_constraint(suppression_of_civil_society_assembly, protest_criminalization).

% DUAL FORMULATION NOTE:
% Suppression of civil society assembly decomposes into three related constraints: permit denial systems (ε=0.55, tangled rope — genuine coordination of public order mixed with asymmetric gate-keeping), surveillance infrastructure (ε=0.72, snare — pure extraction with no coordination benefit), and protest criminalization (ε=0.65, snare — legal prohibition with enforcement apparatus). Each has distinct ε values reflecting whether coordination function is present. All three are upstream of the aggregate suppression constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(suppression_of_civil_society_assembly, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
