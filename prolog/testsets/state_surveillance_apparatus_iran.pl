% ============================================================================
% CONSTRAINT STORY: state_surveillance_apparatus_iran
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_surveillance_apparatus_iran, []).

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
 *   constraint_id: state_surveillance_apparatus_iran
 *   human_readable: Iranian State Surveillance Apparatus and Citizen Control
 *   domain: political/security
 *
 * SUMMARY:
 *   Iran's state surveillance apparatus represents a nearly pure extraction
 *   mechanism disguised as national security coordination. Beginning with the
 *   1979 revolution and crystallized through decades of regime consolidation,
 *   the system combines digital monitoring, informant networks, militarized
 *   street-level enforcement, and periodic theatrical public executions to
 *   maintain comprehensive control over the population. The apparatus is
 *   beneficiary to the Revolutionary Guards and state executive (who use it
 *   for regime survival and factional control) and predator to Iranian
 *   citizens, political opposition, religious minorities, and women
 *   activists. The constraint exhibits the full range of DR types across
 *   perspectives: a pure snare from the powerless citizen's view, a
 *   coordination mechanism from the state's view, a degraded ritual from the
 *   middle class's view, and a refined extraction from the activist's
 *   constrained perspective. The theater component (0.58) reflects that while
 *   the apparatus is functional—it does suppress dissent—a significant
 *   portion of its activity is performative: public executions serve
 *   deterrent effect rather than purely utilitarian control, and periodic
 *   arrest waves oscillate with periods of semi-tolerated dissent. The
 *   measured extractiveness (0.68) reflects the asymmetry: substantial
 *   population burden (self-censorship, psychological fear, risk of arrest),
 *   concentrated benefits to the apparatus, minimal coordination benefit to
 *   the general population.
 *
 * KEY AGENTS:
 *   - Iranian Citizens: Primary victims (powerless/trapped) — bear full cost through self-censorship, psychological suppression, and legal risk
 *   - Political Opposition and Activists: Primary targets (moderate/constrained) — face surveillance, arrest, torture, and execution; emigration is dangerous but theoretically possible
 *   - Women Activists and Religious Minorities: Secondary victims (powerless/trapped) — face compounded surveillance and enforcement
 *   - Revolutionary Guards (IRGC): Primary beneficiary (powerful/arbitrage) — capture organizational power, resource allocation, and information monopoly
 *   - State Executive (Supreme Leader, Cabinet): Secondary beneficiary (institutional/arbitrage) — uses apparatus for regime survival and factional control
 *   - Iranian Middle Class and Urban Professionals: Tertiary agents (moderate/constrained) — experience oscillating surveillance; sufficient productivity required that perfect suppression is economically irrational
 *   - Analytical Observer: Global perspective (analytical/analytical) — classifies the apparatus as a pure snare with minimal coordination function beyond regime survival
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_surveillance_apparatus_iran, 0.68).
domain_priors:suppression_score(state_surveillance_apparatus_iran, 0.82).
domain_priors:theater_ratio(state_surveillance_apparatus_iran, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_surveillance_apparatus_iran, extractiveness, 0.68).
narrative_ontology:constraint_metric(state_surveillance_apparatus_iran, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(state_surveillance_apparatus_iran, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_surveillance_apparatus_iran, snare).
narrative_ontology:human_readable(state_surveillance_apparatus_iran, "Iranian State Surveillance Apparatus and Citizen Control").
narrative_ontology:topic_domain(state_surveillance_apparatus_iran, "political/security").

domain_priors:requires_active_enforcement(state_surveillance_apparatus_iran).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_surveillance_apparatus_iran, security_apparatus).
narrative_ontology:constraint_beneficiary(state_surveillance_apparatus_iran, state_executive).
narrative_ontology:constraint_victim(state_surveillance_apparatus_iran, iranian_citizens).
narrative_ontology:constraint_victim(state_surveillance_apparatus_iran, political_opposition).
narrative_ontology:constraint_victim(state_surveillance_apparatus_iran, religious_minorities).
narrative_ontology:constraint_victim(state_surveillance_apparatus_iran, women_activists).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IRANIAN CITIZENS (SNARE) — Trapped within the national territory with no legal exit mechanism for dissent or privacy. Digital surveillance, informant networks, and street-level checkpoints create comprehensive suppression. Citizens bear full extraction cost through self-censorship, psychological fear, and elimination of autonomous social coordination. Maximum experienced extraction.
constraint_indexing:constraint_classification(state_surveillance_apparatus_iran, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: POLITICAL OPPOSITION AND ACTIVISTS (SNARE) — Structurally constrained exit (emigration costly and dangerous; family remains hostage). Face targeted surveillance, arrest, torture, and execution. Minimal coordination benefit — the apparatus exists to suppress their coordination, not enable it. High-grade snare: extraction without coordination function.
constraint_indexing:constraint_classification(state_surveillance_apparatus_iran, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REVOLUTIONARY GUARDS AND SECURITY APPARATUS (TANGLED ROPE) — Benefits from surveillance through resource allocation, organizational power, and information monopoly. Simultaneously coordinates internal hierarchy and faces pressure from external intelligence agencies. Genuine coordination function (managing factional competition within apparatus) mixed with asymmetric extraction from the population. Net beneficiary.
constraint_indexing:constraint_classification(state_surveillance_apparatus_iran, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE EXECUTIVE LEADERSHIP (ROPE) — Experiences surveillance apparatus primarily as coordination mechanism: consolidates power, prevents palace coups, and enables policy enforcement. While extraction occurs, leadership perceives the apparatus as solving the collective action problem of regime survival. Low suppression from leadership perspective — they designed and control the system. Rope classification reflects that the state benefits from coordination, not pure predation.
constraint_indexing:constraint_classification(state_surveillance_apparatus_iran, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: IRANIAN MIDDLE CLASS AND URBAN PROFESSIONALS (PITON) — Moderate baseline surveillance (phone monitoring, social media tracking) with oscillating enforcement. Theater-heavy: periodic public arrests and executions performed for deterrent effect, with long periods of semi-tolerated dissent. The constraint is degraded from maximal snare — economically productive people cannot be surveilled perfectly without destroying productivity. Maintains itself through theatrical enforcement (public trials, hanging demonstrations) rather than constant suppression.
constraint_indexing:constraint_classification(state_surveillance_apparatus_iran, piton,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From a civilizational/global perspective, the Iranian surveillance apparatus is a pure extractive structure with minimal coordination function beyond regime survival. The apparatus does not enable voluntary coordination among citizens; it suppresses it. International pressure for reform, refugee flows, and diaspora political organization reveal the constraint's non-negotiable asymmetry. Classification as snare is robust across observational standpoints.
constraint_indexing:constraint_classification(state_surveillance_apparatus_iran, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_surveillance_apparatus_iran_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(state_surveillance_apparatus_iran, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(state_surveillance_apparatus_iran, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_surveillance_apparatus_iran, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(state_surveillance_apparatus_iran, TR),
    TR >= 0.70.

:- end_tests(state_surveillance_apparatus_iran_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): Moderately high, reflecting substantial asymmetry between extraction to the apparatus and benefits to the population. Not maximal (0.85+) because some coordination function exists for regime survival, which is a legitimate (if not democratically legitimate) collective action problem. The value captures that the population pays a high cost while the apparatus captures the benefit. Suppression (0.82): Very high. The apparatus combines comprehensive digital surveillance, informant networks, street-level military enforcement, and periodic violent public executions. Exit options are severely constrained: emigration is expensive and dangerous; internal dissent risks arrest or death. Suppression creates near-total behavioral modification in the target population. Theater ratio (0.58): Moderate-high, reflecting that while the apparatus is functionally suppressive, a significant portion of visible activity (public trials, televised confessions, hanging demonstrations) is performative. The middle-class piton perspective observes that perfect constant surveillance of economically productive populations is not sustainable; the system maintains itself through theatrical enforcement (high-visibility punishments) interspersed with semi-tolerated dissent. The trajectory over 20 years shows increasing extractiveness (0.52 → 0.68) as digital capacity expands and informant networks mature, and increasing theater (0.45 → 0.58) as public executions become more ritualized and their deterrent effect becomes the primary mechanism for maintaining control among the semi-tolerated middle class.
 *
 * PERSPECTIVAL GAP:
 *   The deepest gap lies between the state executive's rope perception and the citizen's snare perception. From the state's view, the apparatus solves collective action problems: How do we prevent palace coups? How do we enforce policy against factional competitors? How do we maintain regime stability? The apparatus coordinates answers to these questions. From the citizen's view, the apparatus solves only the state's problems, not theirs. There is no coordination benefit to the citizen; the apparatus suppresses the citizen's voluntary coordination (protests, unions, political organizing) while coordinating only the state's interests. This is the definitional asymmetry of a snare: a structure that coordinates some agents while extracting from others. The piton perception (degraded ritual) from the middle class is a crucial diagnostic. It reveals that the apparatus cannot maintain constant surveillance of economically productive people without destroying the economy. The apparatus therefore degrades: theatrical enforcement replaces comprehensive suppression, and periods of semi-tolerated dissent alternate with crackdowns. This oscillation is itself a sign that the structure is approaching sustainability limits — it cannot stay at maximum snare indefinitely, so it settles into a rhythm that generates sufficient theater to maintain psychological suppression while tolerating some level of dissent.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) reflect the agent's structural position in the extraction flow. The Revolutionary Guards as beneficiaries with full control (arbitrage exit) have low d (~0.10-0.15), experiencing negative effective extraction (the apparatus subsidizes them). The state executive as secondary beneficiary (institutional/arbitrage) has similar low d. Citizens as victims with no exit (trapped) have maximum d (~0.95), experiencing maximum f(d) ≈ 1.42 and high chi even when epsilon is moderate. Activists as constrained victims (moderate power but trapped by family/resource dependencies) have high d (~0.70-0.85), experiencing strong f(d) ≈ 1.00-1.15. The middle class as tertiary agents with constrained mobility but some economic leverage (moderate power, constrained exit) have moderate d (~0.55-0.65). The scope modifier σ(S) = 1.0 (national scope) does not amplify or dampen chi. The composition: chi = ε × f(d) × σ(S) yields effective extractiveness of approximately 0.96-1.06 for powerless trapped citizens, 0.68-0.77 for constrained activists, 0.44-0.55 for middle-class constrained agents, and approximately -0.13 for beneficiary guards. The analytical observer at d ≈ 0.72 experiences chi ≈ 0.49 (can analyze but cannot directly escape the constraint's spatial scope).
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved by observing that this constraint does NOT resolve as multiple types depending on framing. From EVERY perspective except the beneficiary's, it classifies as snare. The state and guards alone perceive it as coordination (rope). This is not an artifact of measurement basis or observational standpoint — it is a true structural asymmetry. The constraint's legitimacy crisis flows directly from this gap: the apparatus is perceived as just by those who benefit and as purely extractive by those who bear the cost. If the apparatus actually performed a genuine coordination function for the population (epidemic control, foreign security, infrastructure coordination), we would expect perspectives to diverge less sharply. The fact that all victim and neutral perspectives converge on snare, while only beneficiaries see coordination, is diagnostic evidence that the apparatus is indeed extractive. The mandatrophy is resolved not by finding a 'true type' but by recognizing that the perspectival gap itself proves the assertion: the apparatus lacks legitimate coordination function beyond regime survival. If it had genuine coordination function, the middle class would perceive rope or tangled rope, not piton. If it were truly immutable natural law, all perspectives would classify as mountain. The convergence of snare across all non-beneficiary perspectives is the signature of an extractive structure approaching its sustainability limit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    informant_network_sustainability,
    'What percentage of informant network reports are genuine intelligence vs. manufactured/coerced denunciations? Does the degradation of information quality enable or disable the surveillance apparatus?',
    'Defector testimony analysis; comparison of informant-sourced arrests against subsequent evidence quality; examination of recantation rates and false accusation patterns in court records',
    'If informant quality degrades sharply: extractiveness should increase (more false arrests, higher suppression cost to maintain coverage). If system maintains effectiveness: suggests either superior coordination or unrecognized filtering mechanisms. Either way, structural extractiveness may be higher than measured (false reports are a cost to the population, not a reduction in apparatus function).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(informant_network_sustainability, empirical, 'Quality and sustainability of informant network intelligence').

omega_variable(
    digital_vs_analog_suppression_balance,
    'Does digital surveillance (phone tracking, social media monitoring) reduce or increase the extraction cost to maintain control compared to analog methods (informants, street checkpoints, interrogation)?',
    'Resource allocation analysis: personnel requirements for digital monitoring vs traditional surveillance; correlation between digital capacity expansion and reduction in personnel-intensive street-level enforcement; cost-per-suppression metrics over time',
    'If digital reduces cost: extractiveness may decline as efficiency improves (bottleneck shifts from suppression to technological reach). If digital adds cost without reducing traditional methods: extractiveness increases (cumulative suppression mechanisms). Classification may shift from snare toward piton if theatrical component increases while actual enforcement burden declines.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(digital_vs_analog_suppression_balance, empirical, 'Efficiency trade-off between digital and analog surveillance methods').

omega_variable(
    regime_fragmentation_and_apparatus_coherence,
    'Are multiple security agencies (IRGC, Bassij, Ministry of Intelligence, police) coordinating or competing? Does internal institutional competition reduce or increase external suppression?',
    'Pattern analysis of contradictory enforcement actions; detention practices variance across agencies; turf war incidents; coordination cost indicators (overlapping surveillance, duplicate informant networks)',
    'If highly fragmented: effective suppression may decline despite nominal apparatus size (competitors undermine each other). Classification could degrade from snare toward tangled rope if substantial extraction goes to internal competition rather than regime control. If highly coordinated: snare classification is robust; fragmentation disappears and single unified extraction emerges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regime_fragmentation_and_apparatus_coherence, empirical, 'Degree of coordination vs competition among security agencies').

omega_variable(
    exile_network_coordination_capacity,
    'Does the Iranian diaspora''s political organization constitute a genuine alternative coordination mechanism, or does distance and diaspora fragmentation prevent effective counter-surveillance organizing?',
    'Analysis of diaspora-coordinated action success rates; comparison of exile broadcasting impact on domestic protest coordination; measurement of information flow capacity from exile networks back into Iran',
    'If diaspora networks are effective: suppression classification may be overstated (trapped victims have an external coordination channel). Classification could shift toward constrained. If diaspora is fragmented and ineffective: trapped classification is confirmed; suppression value is accurate. If diaspora networks are effective but disconnected from domestic audiences: suppression remains high despite external coordination (information cannot reach population).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exile_network_coordination_capacity, empirical, 'Whether diaspora networks provide effective alternative coordination').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_surveillance_apparatus_iran, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(surveillance_iran_tr_t0, state_surveillance_apparatus_iran, theater_ratio, 0, 0.45).
narrative_ontology:measurement(surveillance_iran_tr_t10, state_surveillance_apparatus_iran, theater_ratio, 10, 0.52).
narrative_ontology:measurement(surveillance_iran_tr_t20, state_surveillance_apparatus_iran, theater_ratio, 20, 0.58).
narrative_ontology:measurement(surveillance_iran_tr_t5, state_surveillance_apparatus_iran, theater_ratio, 5, 0.48).

% Extraction over time
narrative_ontology:measurement(surveillance_iran_be_t0, state_surveillance_apparatus_iran, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(surveillance_iran_be_t10, state_surveillance_apparatus_iran, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(surveillance_iran_be_t20, state_surveillance_apparatus_iran, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(surveillance_iran_be_t5, state_surveillance_apparatus_iran, base_extractiveness, 5, 0.56).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_surveillance_apparatus_iran, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(state_surveillance_apparatus_iran, 0.18).
narrative_ontology:affects_constraint(state_surveillance_apparatus_iran, iranian_civil_society_coordination).
narrative_ontology:affects_constraint(state_surveillance_apparatus_iran, digital_dissent_suppression).
narrative_ontology:affects_constraint(state_surveillance_apparatus_iran, family_structure_atomization).

% DUAL FORMULATION NOTE:
% The state surveillance apparatus constrains multiple downstream structures. Digital dissent suppression operates as a snare specific to internet-mediated coordination (higher extractiveness due to transparency of digital footprint). Civil society coordination is suppressed by the same apparatus but represents a distinct constraint (measuring the residual coordination capacity of trusted networks). Family structure atomization is a downstream consequence (informants within families; kinship networks as surveillance risk). Each story has its own epsilon and perspectives; all three are influenced by the parent apparatus constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(state_surveillance_apparatus_iran, moderate, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
