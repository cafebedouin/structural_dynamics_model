% ============================================================================
% CONSTRAINT STORY: family_estrangement_ratio
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_estrangement_ratio, []).

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
 *   constraint_id: family_estrangement_ratio
 *   human_readable: The "Family is Forever" Dogma
 *   domain: social/cultural_constraint
 *
 * SUMMARY:
 *   The 'family is forever' dogma operates as a constraint on exit options
 *   for individuals in harmful family relationships. The dogma combines a
 *   legitimate coordination function — family kinship has historically
 *   enabled mutual aid, cultural transmission, and belonging — with
 *   systematic suppression of the alternative of clean estrangement.
 *   Individuals who attempt to exit harmful relationships face overwhelming
 *   social punishment: accusations of selfishness, guilt narratives about
 *   duty and obligation, economic pressure (inheritance, housing, shared
 *   property), social isolation from extended networks, and religious or
 *   cultural condemnation. The constraint has intensified in recent decades
 *   as economic independence and geographic mobility have reduced the
 *   functional coordination role of family obligation, while entertainment,
 *   media, and therapeutic narratives about 'family is everything' have
 *   increased the theatrical enforcement. Trauma survivors and abuse victims
 *   bear the full extraction cost — they must choose between psychological
 *   survival (estrangement) and social legitimacy (continued contact).
 *   Abusive family members benefit from the dogma's enforcement of
 *   reconciliation narratives and mandatory contact. The extended family
 *   system maintains the dogma through performative rituals and guilt
 *   maintenance. The emerging estrangement rights movement represents a
 *   scaffold perspective — new structures (support communities,
 *   trauma-informed therapy, healthier kinship alternatives) are building
 *   exits that will eventually make the dogma obsolete.
 *
 * KEY AGENTS:
 *   - Estranged Individuals: Primary victims (powerless/trapped) — bear psychological and social costs of estrangement while facing intense pressure to reconcile
 *   - Abuse Survivors: Secondary victims (powerless/trapped) — experience maximum extraction when forced to maintain contact with harm sources
 *   - Adult Children Seeking Boundaries: Mixed agents (moderate/constrained) — experience both coordination benefits and extraction through family obligation
 *   - Abusive Family Members: Primary beneficiaries (institutional/arbitrage) — leverage the dogma to maintain access to victims and avoid accountability
 *   - Extended Family System: Secondary beneficiary (institutional/constrained) — enforces dogma through social rituals and guilt maintenance
 *   - Estrangement Support Communities: Organized agents (organized/mobile) — building alternative structures and sunset pathways
 *   - Cultural Gatekeepers: Enforcement agents (institutional/arbitrage) — religious leaders, therapists, media figures who reinforce the dogma through narrative authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_estrangement_ratio, 0.58).
domain_priors:suppression_score(family_estrangement_ratio, 0.72).
domain_priors:theater_ratio(family_estrangement_ratio, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_estrangement_ratio, extractiveness, 0.58).
narrative_ontology:constraint_metric(family_estrangement_ratio, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(family_estrangement_ratio, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_estrangement_ratio, tangled_rope).
narrative_ontology:human_readable(family_estrangement_ratio, "The \"Family is Forever\" Dogma").
narrative_ontology:topic_domain(family_estrangement_ratio, "social/cultural_constraint").

domain_priors:requires_active_enforcement(family_estrangement_ratio).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_estrangement_ratio, abusive_family_members).
narrative_ontology:constraint_beneficiary(family_estrangement_ratio, extended_family_stakeholders).
narrative_ontology:constraint_beneficiary(family_estrangement_ratio, cultural_gatekeepers).
narrative_ontology:constraint_victim(family_estrangement_ratio, estranged_individuals).
narrative_ontology:constraint_victim(family_estrangement_ratio, trauma_survivors).
narrative_ontology:constraint_victim(family_estrangement_ratio, escaping_abuse_victims).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ESTRANGED INDIVIDUAL (SNARE) — Trapped by overwhelming social pressure, guilt narratives, and emotional manipulation. The dogma suppresses alternatives to reconciliation, making clean exit impossible. No institutional support for estrangement as a legitimate choice. Maximum experienced extraction — the individual must maintain psychological contact with harmful relatives despite severe cost, or face systematic social punishment, isolation, and identity erosion.
constraint_indexing:constraint_classification(family_estrangement_ratio, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ADULT CHILD SEEKING BOUNDARIES (TANGLED ROPE) — Constrained by family economic ties (inheritance, housing, caregiving obligations), social networks, and cultural identity embedded in family relationships. Also benefits from some aspects of family structure (mutual aid, cultural continuity, social validation). Experiences both coordination function (family provides mutual support and belonging) and asymmetric extraction (obligatory contact with harmful members, suppressed agency to exit).
constraint_indexing:constraint_classification(family_estrangement_ratio, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ABUSIVE FAMILY MEMBER (ROPE) — Benefits from the dogma's enforcement of contact and reconciliation narratives. Can leverage family obligation to access victims, extract emotional labor, and avoid accountability. Experiences the constraint as pure coordination — the dogma solves their problem of maintaining connection to others despite harmful behavior. High arbitrage options: can exit by denying abuse, reframing behavior, or leveraging family mythology.
constraint_indexing:constraint_classification(family_estrangement_ratio, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EXTENDED FAMILY SYSTEM (PITON) — Maintains the dogma through performative enforcement: holiday gatherings, 'family is forever' rhetoric, public narratives of reconciliation and duty. The functional coordination role (mutual support, collective identity) has degraded as geographic mobility and economic independence have separated family units. The dogma persists through inertia and theatrical enforcement despite reduced coordination function. Theater ratio reflects that much family obligation now consists of performative rituals (obligatory contact, guilt maintenance, reconciliation scripts) rather than material mutual aid.
constraint_indexing:constraint_classification(family_estrangement_ratio, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ESTRANGEMENT RIGHTS COALITION (SCAFFOLD) — Organized agents (therapists, estrangement support communities, progressive family therapy, trauma-informed advocacy) see the dogma as a temporary coordination failure being actively replaced by healthier frameworks. Sunset clause: as psychological understanding of abuse, trauma, and healthy boundaries spreads, the dogma loses normative force. Support networks for estrangement are building parallel social structures that legitimize clean exit. Estimated sunset: 20-30 years for norms to shift in dominant culture.
constraint_indexing:constraint_classification(family_estrangement_ratio, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: CIVILIZATIONAL ANALYTICAL OBSERVER (MOUNTAIN CLAIM) — From an ultra-long time horizon, kinship bonds might appear immutable and inherent to human social organization. The 'family is forever' dogma could be naturalized as reflecting deep evolutionary constraints on family bonding. However, this is a false summit: the constraint is contingent institutional enforcement, not a law of nature. The structural data contradicts the mountain classification — the dogma requires active suppression of alternatives, theatrical maintenance, and beneficiary enforcement. True mountains require neither.
constraint_indexing:constraint_classification(family_estrangement_ratio, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_estrangement_ratio_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(family_estrangement_ratio, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(family_estrangement_ratio, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(family_estrangement_ratio, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(family_estrangement_ratio, TR),
    TR >= 0.70.

:- end_tests(family_estrangement_ratio_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The dogma suppresses a key alternative (clean exit) and forces continued engagement with harmful relationships, creating asymmetric costs for victims. However, it is not a pure snare because the family structure does provide some genuine coordination benefits (mutual aid, cultural continuity, belonging) that legitimate family bonds. The mixed nature reflects that most people have some family members they benefit from maintaining contact with; the constraint's extraction comes from the forced maintenance of contact with harmful members. Suppression (0.72): High. Powerful social, religious, cultural, and economic mechanisms enforce continued family contact. Social punishment for estrangement includes isolation, condemnation, economic consequences, and identity stigma. Therapeutic and psychological help for estrangement is only recently emerging; for most of history, there was no institutional support for exit. Theater ratio (0.68): Moderately high and increasing. As functional family coordination has declined (due to economic independence, geographic mobility, state-provided services), the dogma increasingly relies on performative rituals: obligatory holiday gatherings, guilt-maintenance narratives, public reconciliation displays, and 'family is forever' rhetoric. The theater has increased over the interval as the material coordination function has decreased.
 *
 * PERSPECTIVAL GAP:
 *   Why does the abusive family member see Rope while the victim sees Snare? Because they occupy opposite structural positions in the extraction flow. The abuser benefits from the dogma's enforcement of contact (positive direction); the victim bears the cost of mandatory engagement (negative direction). This gap is the source of deadlock: the beneficiary feels they are coordinating (solving the problem of maintaining connection); the victim feels they are being extracted from (forced to enable harm). No amount of therapeutic discussion bridges this gap because it is structural, not conceptual.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural position relative to extraction flow. Estranged individuals (powerless/trapped) have high d values (0.85+): they are victims with no exit, experiencing maximum chi. Abusive family members (institutional/arbitrage) have low d values (0.10-0.20): they are beneficiaries with high exit options, experiencing negative or minimal chi. Adult children seeking boundaries (moderate/constrained) have mid-range d values (0.50-0.60): they occupy mixed positions with limited but real exit options. The extended family system (institutional/constrained) occupies a mid-range (0.45-0.55): they enforce the dogma but lack perfect arbitrage to escape if called out. The engine derives these d values from beneficiary/victim declarations and exit options. The key insight: the dogma persists not because beneficiaries internalized its moral case, but because it's easier to enforce through social pressure than to acknowledge that families can be harmful.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint resolves the potential confusion between pure extraction (Snare) and coordination with asymmetric benefit (Tangled Rope) by clarifying the structural logic. The 'family is forever' dogma IS a tangled rope from the system perspective — family kinship genuinely solves coordination problems for mutual aid and cultural continuity. But FROM THE VICTIM'S PERSPECTIVE (trapped, powerless, no exit), it functions as pure snare extraction. Both readings are correct from their respective viewpoints. The mandatrophy is resolved by recognizing that the dogma fulfills a real coordination function AND simultaneously extracts from those seeking to exit harmful relationships. It is not mislabeled extraction pretending to be coordination; it is actual coordination mechanisms that are weaponized against victims. The key distinguishing feature: if the dogma were purely extractive, removing it would harm no one. But removing it without building alternative mutual-aid structures would eliminate genuine coordination benefits. This is why the scaffold perspective (building chosen family and community care alternatives) is structural rather than merely aspirational — the sunset depends on replacing real coordination function, not just dismantling harmful enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    abuse_severity_threshold,
    'At what severity level of family harm does the ethical case for estrangement overcome social pressure to reconcile?',
    'Clinical outcome tracking: long-term psychological health correlations between estrangement vs reconciliation attempts across abuse severity categories',
    'If threshold is low (verbal abuse sufficient): estrangement becomes widely recognized as legitimate, snare classification hardens. If threshold is high (only severe physical danger): dogma retains authority over moderate harm cases, extending extraction window.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(abuse_severity_threshold, empirical, 'Abuse severity threshold at which estrangement is ethically justified').

omega_variable(
    social_penalty_enforcement,
    'How much of the estrangement stigma is enforced through direct punishment (economic, social exclusion) vs internalized guilt narratives?',
    'Comparative analysis of estrangement outcomes in low-enforcement cultures (Scandinavian countries, urban US) vs high-enforcement cultures (collectivist societies, religious communities)',
    'If mostly external punishment: organizational and policy interventions can reduce suppression rapidly. If mostly internal guilt: requires generational cultural shift; scaffold timeline extends.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_penalty_enforcement, empirical, 'Mechanism of estrangement penalty enforcement').

omega_variable(
    alternative_kinship_structures,
    'Do chosen family, fictive kinship, and community care structures provide equivalent psychological and material support to biological family?',
    'Longitudinal psychological health, economic stability, and social isolation metrics comparing estranged individuals with strong chosen families vs those with weak both',
    'If equivalence established: scaffold perspective confirmed — alternative structures can fully replace dogma function. If biological family provides irreducible coordination: dogma retains partial legitimacy for non-abusive cases.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_kinship_structures, empirical, 'Whether chosen family can replace biological family functions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_estrangement_ratio, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fam_est_tr_t0, family_estrangement_ratio, theater_ratio, 0, 0.45).
narrative_ontology:measurement(fam_est_tr_t25, family_estrangement_ratio, theater_ratio, 25, 0.58).
narrative_ontology:measurement(fam_est_tr_t50, family_estrangement_ratio, theater_ratio, 50, 0.68).

% Extraction over time
narrative_ontology:measurement(fam_est_be_t0, family_estrangement_ratio, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(fam_est_be_t25, family_estrangement_ratio, base_extractiveness, 25, 0.45).
narrative_ontology:measurement(fam_est_be_t50, family_estrangement_ratio, base_extractiveness, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_estrangement_ratio, resource_allocation).
narrative_ontology:affects_constraint(family_estrangement_ratio, inheritance_obligation_asymmetry).
narrative_ontology:affects_constraint(family_estrangement_ratio, caregiving_duty_extraction).
narrative_ontology:affects_constraint(family_estrangement_ratio, cultural_identity_gatekeeping).

% DUAL FORMULATION NOTE:
% The 'family is forever' dogma is the umbrella constraint; it affects specific downstream constraints like inheritance obligation, caregiving duty, and cultural identity gatekeeping. Each downstream constraint operates through the dogma's suppression of estrangement as a legitimate option. The network links show how dismantling the dogma would change the classification of all three downstream constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(family_estrangement_ratio, powerless, 0.88).
constraint_indexing:directionality_override(family_estrangement_ratio, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
