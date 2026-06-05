% ============================================================================
% CONSTRAINT STORY: governance__autocratic_rule
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_governance__autocratic_rule, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: governance__autocratic_rule
 *   human_readable: Autocratic Rule: Unmediated Ruler Will
 *   domain: political/legal/governance
 *
 * SUMMARY:
 *   Autocratic rule — governance by the unmediated will of a single ruler
 *   whose commands are law because of who issues them, not because of any
 *   prior authorization — is one reading of the fundamental governance kernel
 *   contested across multiple political and legal traditions. This constraint
 *   story instantiates only the autocratic_rule reading: it models governance
 *   where the ruler is not bound by any prior text, custom, or divine law,
 *   but rather is the source of all law, legitimacy, and authority. The
 *   structural consequence is maximal suppression of institutional
 *   alternatives (constitutional constraints, customary limits, assemblies,
 *   councils) and maximal extraction concentrated in the ruler and inner
 *   circle. The subject population is trapped with no institutional exit;
 *   alternative governing forms are actively prevented. The constraint
 *   exhibits the classic snare signature: high extractiveness (0.85), high
 *   suppression (0.92), concentrated beneficiary (ruler and apparatus),
 *   dispersed victims (general population plus suppressed institutional
 *   forms). The theater ratio (0.55) reflects that autocratic rule requires
 *   performative elements — ceremonial obedience, courtly ritual, and often
 *   religious endorsement — but these are secondary to the core mechanism of
 *   enforcement. The measurement trajectory shows rising suppression
 *   requirement as the ruler ages or as institutional pressure mounts, and
 *   slight declining extractiveness in later periods, suggesting either
 *   degradation into warlordism or increasing inefficiency of the extraction
 *   apparatus. The false summit perspective (analytical observer treating
 *   autocracy as a natural law of governance) is flagged as a misleading
 *   naturalization: the constraint is maintained by active suppression of
 *   alternatives, not by immutable structural limits.
 *
 * KEY AGENTS:
 *   - Ruler and Inner Circle: Primary beneficiary (institutional/arbitrage) — capture unmediated authority, extractive revenue, appointments, and protection. Perspective: rope (pure coordination of obedience)
 *   - General Population: Primary victim (powerless/trapped) — no mechanism to contest, amend, or refuse commands; no exit from the polity. Perspective: snare (maximum extraction and suppression)
 *   - Institutional Alternatives: Victim (organized/constrained to trapped) — constitutional structures, councils, customary law, and mediating institutions are actively suppressed; agents of these forms face exile or execution. Perspective: snare (prevented from existing)
 *   - Legitimating Apparatus: Institutional actor (institutional/constrained) — court rituals, religious endorsement, ceremonial display maintain the fiction that unmediated rule is natural or ordained. Perspective: piton (degraded, performative, maintained by inertia)
 *   - International State System: External pressure (organized/mobile) — surrounding states with higher institutional capacity create structural pressure toward constitutionalization, decentralization, or formalization of rules. Perspective: scaffold (temporary form being superseded by external competition)
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing constructed constraint as inherent to power itself. Perspective: mountain (false summit, flagged for review)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(governance__autocratic_rule, 0.85).
domain_priors:suppression_score(governance__autocratic_rule, 0.92).
domain_priors:theater_ratio(governance__autocratic_rule, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(governance__autocratic_rule, extractiveness, 0.85).
narrative_ontology:constraint_metric(governance__autocratic_rule, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(governance__autocratic_rule, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(governance__autocratic_rule, snare).
narrative_ontology:human_readable(governance__autocratic_rule, "Autocratic Rule: Unmediated Ruler Will").
narrative_ontology:topic_domain(governance__autocratic_rule, "political/legal/governance").

domain_priors:requires_active_enforcement(governance__autocratic_rule).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(governance__autocratic_rule, '2a0475e3-0975-416c-b872-5e58a2ae59fb').
narrative_ontology:cs_kernel_codification('2a0475e3-0975-416c-b872-5e58a2ae59fb', implicit).
narrative_ontology:cs_authority_grounding('2a0475e3-0975-416c-b872-5e58a2ae59fb', extraction).
narrative_ontology:cs_reading_relation('2a0475e3-0975-416c-b872-5e58a2ae59fb', governance__constitutional_government, forecloses).
narrative_ontology:cs_reading_relation('2a0475e3-0975-416c-b872-5e58a2ae59fb', governance__customary_rule, coexists_with).
narrative_ontology:cs_reading_relation('2a0475e3-0975-416c-b872-5e58a2ae59fb', governance__direct_democracy, forecloses).
narrative_ontology:cs_reading_relation('2a0475e3-0975-416c-b872-5e58a2ae59fb', governance__theocratic_rule, coexists_with).
narrative_ontology:cs_axiom('2a0475e3-0975-416c-b872-5e58a2ae59fb', foundational, unmediated_personal_authority).
narrative_ontology:cs_axiom_status(unmediated_personal_authority, holdable).
narrative_ontology:cs_axiom_grounding('2a0475e3-0975-416c-b872-5e58a2ae59fb', unmediated_personal_authority, instrumental).
narrative_ontology:cs_axiom('2a0475e3-0975-416c-b872-5e58a2ae59fb', secondary, suppression_of_institutional_alternatives).
narrative_ontology:cs_axiom_status(suppression_of_institutional_alternatives, holdable).
narrative_ontology:cs_axiom_grounding('2a0475e3-0975-416c-b872-5e58a2ae59fb', suppression_of_institutional_alternatives, instrumental).
narrative_ontology:cs_reference_frame('2a0475e3-0975-416c-b872-5e58a2ae59fb', unified_personal_sovereignty).
narrative_ontology:cs_drift_state('2a0475e3-0975-416c-b872-5e58a2ae59fb', contemporary_institutional_pressure_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2a0475e3-0975-416c-b872-5e58a2ae59fb', '').
narrative_ontology:cs_kernel_id(governance__autocratic_rule, governance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(governance__autocratic_rule, ruler_and_inner_circle).
narrative_ontology:constraint_victim(governance__autocratic_rule, general_population).
narrative_ontology:constraint_victim(governance__autocratic_rule, institutional_alternatives).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBJECT POPULATION (SNARE) — Trapped within the polity with no exit and no institutional redress. Commands are law solely because of the ruler's will; subjects have no mechanism to contest, amend, or refuse. Maximum suppression: alternative governing arrangements are not merely disfavored but actively prevented. Maximum extraction: the ruler's arbitrary decisions can appropriate property, labor, life itself.
constraint_indexing:constraint_classification(governance__autocratic_rule, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INSTITUTIONAL ALTERNATIVES (SNARE) — Customary law, written constitutions, assemblies, councils, or other mediating institutions that could limit or distribute ruling authority are actively suppressed. Their agents face exile, execution, or permanent political marginalization. These institutions cannot exit the system without being destroyed — they are trapped within it as targets of extraction. The constraint's core function is to prevent their emergence.
constraint_indexing:constraint_classification(governance__autocratic_rule, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RULER AND INNER CIRCLE (ROPE) — The constraint solves a pure coordination problem for this actor: how to concentrate and wield power without institutional constraints. Subjects' obedience coordinates on the ruler's will. The inner circle benefits from proximity to unmediated power — access to extractive revenue, appointments, and protection. This perspective experiences the constraint as functional coordination.
constraint_indexing:constraint_classification(governance__autocratic_rule, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LEGITIMATING APPARATUS (PITON) — Court rituals, religious endorsement, ceremonial display, and theatrical submission by subordinate rulers or assemblies maintain the fiction that unmediated rule is natural or divinely ordained. The theater persists through institutional inertia even when actual rule has become fragmented (warlordism, plural sovereigns with overlapping jurisdictions). The apparatus sees its own function as degraded — performing legitimation for a rule that no longer commands unified enforcement.
constraint_indexing:constraint_classification(governance__autocratic_rule, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERNATIONAL STATE SYSTEM (SCAFFOLD) — From the perspective of external state actors, autocratic rule becomes unsustainable when surrounded by states with higher institutional capacity and resource extraction efficiency. Pressure toward constitutionalization, decentralization, or at least formalization of rules emerges. This perspective treats autocracy as a temporary form on the path toward institutional consolidation — a sunset clause enforced by external structural competition.
constraint_indexing:constraint_classification(governance__autocratic_rule, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURALIZATION VIEW (MOUNTAIN) — From a civilizational scope, the autocratic form might be treated as a natural law of human political organization: the concentration of power is inevitable; the ruler's will must be unmediated to be effective; constraints breed paralysis. This perspective naturalizes the constraint as inherent to governance itself. However, the structural data contradicts the mountain classification — identifiable beneficiaries exist, suppression is active and asymmetric, and alternatives are systematically prevented. The engine will flag this as a false summit, revealing that the 'inherent to power' framing naturalizes what is actually a constructed and maintained constraint.
constraint_indexing:constraint_classification(governance__autocratic_rule, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(governance__autocratic_rule_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(governance__autocratic_rule, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(governance__autocratic_rule, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(governance__autocratic_rule, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(governance__autocratic_rule, TR),
    TR >= 0.70.

:- end_tests(governance__autocratic_rule_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.85): Very high. The ruler's commands appropriate property, labor, and life without institutional limit or recourse. The extraction is not merely high but arbitrary — the ruler may change property rights, confiscate holdings, or conscript labor at will. The initial value (0.78) reflects that enforcement may be incomplete in some regions; the peak value (0.87 at t=6) reflects consolidation of control and rising extraction efficiency; the slight decline (0.83 at t=10) may reflect either degradation as the ruler ages or increasing inefficiency as enforcement apparatus becomes bloated. Suppression (0.92): Very high and stable. Alternative governing arrangements are not merely disfavored but actively prevented. Institutions that could mediate, distribute, or limit authority are crushed, exiled, or coopted. The measurement trajectory (0.88 → 0.93 → 0.90) shows rising enforcement cost as suppression requirement increases during the peak of the ruler's power and slight decline as capacity erodes. Theater ratio (0.55): Moderate. Autocratic rule requires performative elements — ceremonial obedience, courtly ritual, religious endorsement, and the spectacle of power — but these are secondary to enforcement. The ruler's will alone is the operative mechanism; the theater provides legitimation but is not the main function. The rising trajectory (0.48 → 0.62) suggests the theatrical component increases as the constraint ages or as enforcement begins to decline — the apparatus compensates for weakening actual authority by increasing performative display.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates stark perspectival divergence across positions. The ruler and inner circle experience rope (pure coordination of obedience, beneficiary status, institutional perspective with arbitrage exit) — the constraint solves their problem of concentrating power. The general population experiences snare (trapped, powerless, no exit, no institutional recourse, maximum extraction) — the same constraint is their prison. Institutional alternatives (customary law, councils, constitutions) experience snare at the structural level (prevented from existing, agents face suppression) — the constraint's core function is to prevent their emergence. The legitimating apparatus experiences piton (performative, theatrical, maintained by inertia, degraded function) — they maintain the fiction of legitimate authority for a rule that no longer commands unified enforcement. The international state system experiences scaffold (temporary form being superseded by external structural competition) — the constraint has a sunset clause enforced by diffusion of institutional alternatives globally. The analytical observer at civilizational scope risks seeing mountain (naturalizing autocracy as inherent to governance) but this is flagged as a false summit: the constraint requires active suppression, not natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is the agent's structural position relative to the constraint's extraction flow. The ruler (beneficiary, institutional, arbitrage) has d ≈ 0.10 — full beneficiary with exit option (could relocate, abdicate, shift power to inner circle). The general population (victim, powerless, trapped) has d ≈ 1.00 — full target with no exit. Institutional alternatives (victim, organized, constrained-to-trapped depending on whether they can organize underground) have d ≈ 0.92 — targets of active suppression. The legitimating apparatus (beneficiary, institutional, constrained) has d ≈ 0.30 — benefits from proximity to power but constrained by dependence on the ruler's will (the apparatus can be purged). The international state system (external observer, organized, mobile) has d ≈ 0.50 — symmetric position (their structural incentives are to pressure constitutional change, but their capacity to do so is limited). The analytical observer has canonical d ≈ 0.73 (analytical power atom). The chi formula applies to each perspective: χ = ε × f(d) × σ(S). For the general population at national scope: χ = 0.85 × f(1.00) × 1.0 ≈ 0.85 × 1.42 × 1.0 ≈ 1.21 (effective extraction exceeds base by a factor reflecting powerlessness). For the ruler: χ = 0.85 × f(0.10) × 1.0 ≈ 0.85 × (-0.01) × 1.0 ≈ -0.01 (negative, indicating subsidy/benefit).
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED MANDATROPHY: This constraint is unambiguously high-extraction (ε=0.85) with beneficiary/victim asymmetry that rules out pure coordination (rope). The mandatrophy — the paradoxical classification as both coordination and extraction — is resolved by perspectival differentiation. From the beneficiary's view (rope), the constraint coordinates obedience. From the victim's view (snare), it is pure extraction. From the apparatus's view (piton), it is performative. From the external observer's view (scaffold), it is temporary. All six types are correct within their respective observational frames. The constraint is not 'really' any one type; rather, the presheaf of perspectives over the governance kernel is the appropriate model. The mandatrophy is an artifact of trying to force a single type onto a radically asymmetric relationship. The indexed classification system correctly captures this by producing different types from different contexts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ruler_legitimacy_grounding,
    'What grounds the claim that the ruler''s unmediated will is legitimate authority — personal charisma, hereditary right, divine selection, superior competence, conquest, or pure force?',
    'Historical analysis of succession crises, rebellions, and legitimacy contestation. If authority transfers smoothly, some grounding mechanism exists; if each succession triggers civil war, pure force is the operative mechanism.',
    'If grounding is hereditary/customary: the constraint is coexistent with customary_rule reading (different axes of the same kernel). If grounding is theocratic: coexistent with theocratic_rule. If grounding is pure force: the constraint is a snare with minimal legitimating apparatus (theater_ratio should be lower). If grounding is charisma: the constraint is unstable, degrading toward piton as charisma erodes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ruler_legitimacy_grounding, empirical, 'What grounds the ruler''s claimed authority to issue unmediated commands').

omega_variable(
    enforcement_capacity_limits,
    'Does the ruler actually enforce commands uniformly across the polity, or does enforcement degrade at geographic/social distance, creating de facto plural sovereigns with overlapping jurisdictions?',
    'Mapping of effective enforcement: which commands are obeyed uniformly, which are routinely defied, which are delegated to regional strongmen whose compliance is negotiated rather than unmediated. Presence of warlordism, banditry, or customary law in geographic periphery indicates enforcement collapse.',
    'If uniform enforcement: snare classification holds across the polity. If enforcement degrades to plural sovereigns: the constraint becomes piton in periphery (theater of submission without actual rule) and tangled_rope in intermediate zones (negotiated authority). The measurement of extractiveness should track enforcement geography.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_capacity_limits, empirical, 'Whether the ruler''s commands are actually enforced uniformly or degrade into plural sovereigns').

omega_variable(
    inner_circle_stability,
    'Does the inner circle benefit from the constraint''s stability, or do they themselves live in fear of the ruler''s arbitrary will?',
    'Historical patterns of purge, succession instability, and guard rotation. Long tenure of inner-circle members suggests genuine coordination benefit. High turnover and violent succession suggests the constraint extracts from the inner circle too (they are trapped victims despite apparent proximity to power).',
    'If inner circle is secure: rope perspective holds — genuine coordination benefit. If inner circle is unstable: rope perspective collapses, and all institutional actors become moderate-to-powerless victims experiencing tangled_rope or snare. The extractiveness value may need adjustment upward if even beneficiaries cannot retain their gains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inner_circle_stability, empirical, 'Whether the inner circle is beneficiary or victim of the ruler''s arbitrary will').

omega_variable(
    reading_contest_empirical_test,
    'Is the ruling authority grounded in the ruler''s personal will (autocratic_rule), or in inherited custom the ruler acknowledges as binding (customary_rule), or delegated from divine order the ruler interprets (theocratic_rule)?',
    'Explicit texts and actions: does the ruler cite precedent, custom, or sacred law as limiting their authority, or do they claim authority derives from their will alone? Do they present themselves as bound by rules, or as the source of all rules? How do succession disputes resolve — by appeal to law, tradition, or conquest?',
    'If customary grounding: the constraint is a reading coexistent with customary_rule, not foreclosing it. If theocratic grounding: coexistent with theocratic_rule. If purely voluntary (ruler could invoke custom but explicitly rejects it): foreclosing coexistent readings and instantiating this reading clearly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_empirical_test, empirical, 'Whether ruling authority is grounded in personal will vs. custom vs. divine delegation').

omega_variable(
    constitutional_pressure_mounting,
    'Are institutional pressures toward constitutionalization, decentralization, or formalization of rules increasing over time, or is autocratic suppression maintaining the constraint indefinitely?',
    'Temporal measurement of: (a) frequency of attempts to constrain authority institutionally, (b) effectiveness of suppression against such attempts, (c) diffusion of literacy, commerce, and communication technology that enable coordination of institutional alternatives. Rising attempt frequency + declining suppression effectiveness = structural pressure toward sunset.',
    'If pressure is mounting: the scaffold perspective is structurally real — the constraint''s sunset is enforced by external structural competition. If pressure is suppressed: the constraint may be stable indefinitely (snare remains terminal). The measurement trajectory will show this.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_pressure_mounting, empirical, 'Whether institutional pressure toward constitutionalization is rising or being suppressed').

omega_variable(
    kernel_reading_contest,
    'Is this constraint instantiating the autocratic_rule reading, or are multiple readings of the governance kernel coexisting (e.g., the ruler invokes custom as limiting, theocratic scholars interpret sacred law as constraining)?',
    'Documentary and rhetorical analysis: what justification does the ruling authority itself provide for its legitimacy? What do challengers invoke as counter-authority? Are there competing readings within the same polity?',
    'If pure autocratic_rule: coexistence relations with other readings are accurate as specified in cs_structure. If multiple readings coexist: the constraint may need decomposition — separate stories for the autocratic framing vs. the customary or theocratic framing, linked via network.affects_constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether this is a pure autocratic_rule reading or coexistent with other readings of the governance kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(governance__autocratic_rule, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(autocrat_tr_t0, governance__autocratic_rule, theater_ratio, 0, 0.48).
narrative_ontology:measurement(autocrat_tr_t2, governance__autocratic_rule, theater_ratio, 2, 0.52).
narrative_ontology:measurement(autocrat_tr_t4, governance__autocratic_rule, theater_ratio, 4, 0.55).
narrative_ontology:measurement(autocrat_tr_t6, governance__autocratic_rule, theater_ratio, 6, 0.58).
narrative_ontology:measurement(autocrat_tr_t8, governance__autocratic_rule, theater_ratio, 8, 0.61).
narrative_ontology:measurement(autocrat_tr_t10, governance__autocratic_rule, theater_ratio, 10, 0.62).

% Extraction over time
narrative_ontology:measurement(autocrat_be_t0, governance__autocratic_rule, base_extractiveness, 0, 0.78).
narrative_ontology:measurement(autocrat_be_t2, governance__autocratic_rule, base_extractiveness, 2, 0.81).
narrative_ontology:measurement(autocrat_be_t4, governance__autocratic_rule, base_extractiveness, 4, 0.85).
narrative_ontology:measurement(autocrat_be_t6, governance__autocratic_rule, base_extractiveness, 6, 0.87).
narrative_ontology:measurement(autocrat_be_t8, governance__autocratic_rule, base_extractiveness, 8, 0.85).
narrative_ontology:measurement(autocrat_be_t10, governance__autocratic_rule, base_extractiveness, 10, 0.83).

% Suppression requirement over time
narrative_ontology:measurement(autocrat_su_t0, governance__autocratic_rule, suppression_requirement, 0, 0.88).
narrative_ontology:measurement(autocrat_su_t2, governance__autocratic_rule, suppression_requirement, 2, 0.9).
narrative_ontology:measurement(autocrat_su_t4, governance__autocratic_rule, suppression_requirement, 4, 0.92).
narrative_ontology:measurement(autocrat_su_t6, governance__autocratic_rule, suppression_requirement, 6, 0.93).
narrative_ontology:measurement(autocrat_su_t8, governance__autocratic_rule, suppression_requirement, 8, 0.92).
narrative_ontology:measurement(autocrat_su_t10, governance__autocratic_rule, suppression_requirement, 10, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(governance__autocratic_rule, enforcement_mechanism).
narrative_ontology:affects_constraint(governance__autocratic_rule, governance__customary_rule).
narrative_ontology:affects_constraint(governance__autocratic_rule, governance__constitutional_government).
narrative_ontology:affects_constraint(governance__autocratic_rule, governance__direct_democracy).
narrative_ontology:affects_constraint(governance__autocratic_rule, governance__theocratic_rule).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested governance kernel. Sibling readings (constitutional_government, customary_rule, direct_democracy, theocratic_rule) instantiate competing answers about the source of governing authority. The coexistence/foreclosure relations are defined in cs_structure.reading_relations. If empirical investigation (omega: ruler_legitimacy_grounding) reveals the ruler actually grounds authority in custom or theocratic law, this story may require decomposition: separate constraint stories for the actual grounding, linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
