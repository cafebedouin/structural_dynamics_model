% ============================================================================
% CONSTRAINT STORY: royal_status_hierarchy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_royal_status_hierarchy, []).

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
 *   constraint_id: royal_status_hierarchy
 *   human_readable: Royal Status Hierarchy as Coordination-Extraction Hybrid
 *   domain: political/social/institutional
 *
 * SUMMARY:
 *   Royal status hierarchies represent one of humanity's most persistent
 *   institutional constraints. From absolute monarchy through constitutional
 *   monarchy to symbolic monarchy, the hierarchy evolves across different
 *   structural forms while its core mechanism — differentiating access to
 *   power, resources, and legitimacy based on bloodline — remains
 *   recognizable. This constraint exhibits the full spectrum of DR
 *   classifications because different agents occupy radically different
 *   structural positions: peasants are trapped in a snare; excluded heirs
 *   occupy the tangled boundary between coordination and extraction; the
 *   monarchy experiences coordination; reform movements see a temporary
 *   scaffold amenable to constitutional sunset; post-reform monarchies
 *   maintain a piton (ceremonial residue); and the civilizational analyst
 *   risks naturalizing a contingent institutional form as inevitable. The
 *   constraint's extractiveness (0.58) reflects genuine coordination
 *   functions alongside asymmetric power concentration. Theater ratio (0.65)
 *   captures the increasing performative dimension as real power transfers to
 *   democratic institutions while ceremonial authority persists. Suppression
 *   (0.72) reflects both external legal barriers and internalized cultural
 *   acceptance of status hierarchy.
 *
 * KEY AGENTS:
 *   - Peasant/Commoner Population: Primary victim (powerless/trapped) — bears labor obligations, taxation, movement restrictions; zero structural exit options
 *   - Excluded Noble Branches: Secondary victim (powerful/constrained) — high status but subordinated to primary heir; constrained by succession rules and dynastic obligation
 *   - Royal Family/Monarchy Institution: Primary beneficiary (institutional/arbitrage) — concentrates power, legitimacy, and resources; experiences hierarchy as pure coordination; has exit options but exercises them to preserve privilege
 *   - Constitutional Reform Movement: Organized agent (organized/constrained) — perceives hierarchy as temporary problem with democratic solution; has organizing capacity and sees sunset via constitutional reform
 *   - Post-Reform Ceremonial Monarchy: Institutional actor (institutional/arbitrage) — maintains symbolic role after power transfer; persists through cultural attachment and institutional inertia (piton)
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional form as inevitable social law; tempted to see status hierarchy as immutable feature of human organization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(royal_status_hierarchy, 0.58).
domain_priors:suppression_score(royal_status_hierarchy, 0.72).
domain_priors:theater_ratio(royal_status_hierarchy, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(royal_status_hierarchy, extractiveness, 0.58).
narrative_ontology:constraint_metric(royal_status_hierarchy, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(royal_status_hierarchy, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(royal_status_hierarchy, tangled_rope).
narrative_ontology:human_readable(royal_status_hierarchy, "Royal Status Hierarchy as Coordination-Extraction Hybrid").
narrative_ontology:topic_domain(royal_status_hierarchy, "political/social/institutional").

domain_priors:requires_active_enforcement(royal_status_hierarchy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(royal_status_hierarchy, royal_family_members).
narrative_ontology:constraint_beneficiary(royal_status_hierarchy, nobility_system_beneficiaries).
narrative_ontology:constraint_beneficiary(royal_status_hierarchy, institutional_power_consolidators).
narrative_ontology:constraint_victim(royal_status_hierarchy, commoner_populations).
narrative_ontology:constraint_victim(royal_status_hierarchy, excluded_heir_branches).
narrative_ontology:constraint_victim(royal_status_hierarchy, peasant_labor_base).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PEASANT SUBJECT (SNARE) — Structurally trapped by feudal obligation, legal status, and resource dependency. Bears full extraction cost through labor obligations, taxation, and restriction of movement. Zero degrees of freedom for exit. Cannot leave the hierarchy without forfeiting survival means. High suppression derives from both external legal barriers and internalized acceptance of the natural order. Maximum experienced extraction from position of complete structural immobility.
constraint_indexing:constraint_classification(royal_status_hierarchy, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: EXCLUDED NOBLE BRANCH (TANGLED ROPE) — High-status individuals with significant power but constrained exit options. Bound by succession rules, marriage alliances, and dynastic obligation. The hierarchy coordinates military loyalty and territorial administration (genuine coordination function) while extracting through inheritance restrictions and political subordination. Experiences both coordination benefits (security, status protection) and asymmetric extraction (power concentrated in primary heir). Cannot easily leave the system without losing noble status and associated privileges.
constraint_indexing:constraint_classification(royal_status_hierarchy, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MONARCHY INSTITUTION (ROPE) — The royal family benefits from the hierarchy's coordination function: it organizes political authority, provides succession clarity, and legitimates institutional continuity. The monarchy experiences the constraint as pure coordination with minimal perceived extraction cost. Has arbitrage options (could restructure the system, adopt alternative governance) but chooses to maintain hierarchy because it concentrates power and benefits. Net beneficiary position with exit optionality exercised strategically to preserve privilege.
constraint_indexing:constraint_classification(royal_status_hierarchy, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CONSTITUTIONAL REFORM MOVEMENT (SCAFFOLD) — Organized agents (parliament, reform advocates, civil society) perceive the absolute royal hierarchy as a temporary coordination failure amenable to constitutional sunset. The hierarchy becomes transitional: from absolute monarchy toward constitutional monarchy toward potential republicanism. Significant suppression during the transition period (revolutionary risk, institutional resistance) but declining over the reform horizon. Theater ratio remains high during the transition (ceremonial monarchy maintains symbolic role while power transfers), declining as genuine democratic coordination replaces performative royal authority. The movement has agency through organizing capacity and sees a clear exit path via constitutional reform.
constraint_indexing:constraint_classification(royal_status_hierarchy, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: SYMBOLIC MONARCHY (PITON) — In constitutional monarchies that have completed the reform arc, the royal hierarchy persists primarily as ceremonial performance. The genuine coordination functions (succession clarity, institutional continuity) have been transferred to democratic institutions (parliament, constitutional law). The monarchy remains through institutional inertia and cultural attachment rather than functional necessity. Theater ratio approaches 0.9 (almost entirely symbolic): royal duties are performative; actual power flows through democratic channels; the hierarchy persists because alternatives haven't fully displaced the cultural role. Classification reflects degraded function masked by ceremonial theater, not active extraction.
constraint_indexing:constraint_classification(royal_status_hierarchy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From an ultra-civilizational perspective, status hierarchies are inevitable features of human social organization. Every complex society requires some status ordering, authority differentiation, and succession mechanism. From this view, the royal hierarchy is not a contingent institutional arrangement but a natural expression of universal social coordination requirements. The hierarchy is invariant across cultures and epochs — every civilization produces both monarchy and alternatives, suggesting a fundamental human constraint. However, the structural data reveals this as false naturalization: alternative coordination mechanisms (democratic election, merit-based succession, rotating authority) demonstrate that status hierarchy is contingent, not immutable. The mountain classification fails on accessibility_collapse and resistance metrics.
constraint_indexing:constraint_classification(royal_status_hierarchy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(royal_status_hierarchy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(royal_status_hierarchy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(royal_status_hierarchy, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(royal_status_hierarchy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(royal_status_hierarchy, TR),
    TR >= 0.70.

:- end_tests(royal_status_hierarchy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The royal hierarchy extracts through labor obligations, taxation, and restriction of mobility. But significant coordination functions are genuine: succession clarity prevents civil war, institutional continuity enables long-term planning, cultural legitimacy stabilizes governance. The extracted value is not pure redistribution — some goes to public goods (infrastructure, defense). Extraction is highest in absolute monarchy (0.72 initial) and declines as constitutional reform transfers power to democratic institutions (0.58 final). Suppression (0.72): High. Commoners face legal barriers (feudal status restrictions, sumptuary laws), economic dependency (landlessness, resource control by nobility), and physical enforcement (military, police). Internalized acceptance of the 'natural order' adds psychological suppression. But suppression is not total — peasant revolts occur, mobility margins exist, alternative life paths (clergy, merchant) exist for exceptional individuals. Theater ratio (0.65): Moderate-high, increasing over the interval. Absolute monarchy's theater is lower (genuine power consolidation) because the hierarchy performs real coordination and extraction simultaneously. As reform progresses, the monarchy's actual power declines (transferred to parliament) while ceremonial authority persists, raising theater ratio. Constitutional monarchies maintain high theater (royal ceremonies perform legitimacy while parliament exercises power).
 *
 * PERSPECTIVAL GAP:
 *   The gap between peasant and monarchy perspectives is maximal: the same constraint classifies as snare (extraction, suppression, zero agency) from one position and rope (pure coordination, net benefit, high agency) from another. This is not a difference in how they interpret identical facts — it is a difference in their actual structural relationship to the constraint. The peasant truly bears extraction costs; the monarchy truly benefits. The constraint's structure is asymmetric. The analytical observer's false mountain reveals that we risk naturalizing this asymmetry: calling it 'inevitable' rather than 'contingent institutional arrangement.' The constitutional reform scaffold shows that the hierarchy is changeable — reform movements have successfully transitioned absolute monarchies to constitutional forms, reducing extraction and increasing commoner agency. The piton perspective (ceremonial monarchy) shows that even after power is transferred, institutional inertia preserves the form for cultural reasons. The hierarchy persists not because it is necessary, but because the cultural attachment (theater) keeps it in place.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values derive from each agent's position in the extraction flow and exit capacity. Peasants are full targets (d ≈ 0.95 due to trapped status + victim declaration) experiencing maximum f(d) ≈ 1.42. Excluded heirs are partial targets (d ≈ 0.60 due to powerful status but constrained exit + victim declaration), experiencing moderate f(d) ≈ 0.75. The monarchy is a net beneficiary (d ≈ 0.10 due to institutional power + arbitrage exit options + beneficiary declaration), experiencing negative or near-zero f(d). The reform coalition has moderate target status (d ≈ 0.50 due to organized power + constrained exit + mixed beneficiary/victim relationship), experiencing f(d) ≈ 0.65. These directionality values feed the chi formula: χ = ε × f(d) × σ(S). For peasants at national scope (σ=1.0): χ ≈ 0.58 × 1.42 × 1.0 ≈ 0.82 (high extraction). For monarchy: χ ≈ 0.58 × -0.12 × 1.0 ≈ -0.07 (negative, indicating net benefit). The peasant snare classification emerges from χ ≥ 0.66 + high suppression; the monarchy rope classification emerges from low χ + high perceived coordination.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy resolution: This constraint resolves the classification ambiguity by showing that tangled rope is the correct synthesizing type. The constraint is NOT pure extraction (snare) because genuine coordination functions exist (succession clarity, institutional continuity, legitimacy framework). It is NOT pure coordination (rope) because asymmetric extraction is structural and fundamental to how the hierarchy operates — the monarchy's benefits are purchased by commoner subordination. It is NOT a mountain (natural law) because alternative coordination mechanisms exist and historical transitions demonstrate contingency. The tangled rope classification unifies the data: the hierarchy does coordinate (settlement of succession disputes, institutional stability) AND extracts asymmetrically (differential access to power, resources, mobility). Active enforcement (military, law, cultural reinforcement) is required to maintain the asymmetry. The coordination function would disappear if extraction ceased — they are not separable. This is the defining characteristic of tangled rope: genuine coordination fused with asymmetric extraction, neither reducible to the other, both required by the structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_internalization_mechanism,
    'Is suppression of commoner exit primarily structural (legal barriers, economic dependency) or internalized (belief in divine right, cultural internalization of natural order)?',
    'Post-revolution behavioral analysis: if peasants maintain deference to monarchy after legal barriers are removed, suppression is partly internalized. If deference dissolves when barriers fall, suppression was primarily structural.',
    'If internalized: effective suppression persists even after institutional hierarchy dissolves; exit costs remain high due to identity lock. If structural: dissolution of the hierarchy immediately reduces suppression. Affects classification of post-reform peasant position from snare toward constrained or mobile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_internalization_mechanism, empirical, 'Internalization vs. structural mechanism in commoner suppression').

omega_variable(
    coordination_function_necessity,
    'How much of the royal hierarchy''s coordination function (succession clarity, institutional continuity, legitimate authority) is genuinely supplied by the hierarchy versus merely claimed by it?',
    'Cross-cultural institutional comparison; analysis of successor clarity and institutional stability in republics vs monarchies; measurement of coordination problems that emerge when hierarchies are removed without replacement mechanisms.',
    'If genuine: tangled rope classification confirmed — hierarchy supplies real coordination alongside extraction. If mostly theatrical: snare classification more appropriate — extraction is primary, coordination claim is cover story. Affects entire presheaf of classifications from institutional perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_function_necessity, empirical, 'Whether royal hierarchy supplies genuine coordination function').

omega_variable(
    constitutional_transition_timeline,
    'What is the characteristic timeline and mechanism by which absolute royal hierarchies transition to constitutional or democratic governance?',
    'Historical analysis of constitutional transitions (Britain 1689-1928, Sweden, Belgium, Spain); identification of common pathways and transition duration; determination of whether scaffold classification applies across multiple cases.',
    'If transitions follow predictable patterns with identifiable sunset: scaffold perspective is structural across monarchies. If transitions are chaotic, contingent, or reversible: scaffold is aspirational rather than descriptive. Affects whether constitutional reform movement is truly organized with agency or merely expressing hope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_transition_timeline, empirical, 'Characteristic timeline and mechanism of constitutional monarchy transitions').

omega_variable(
    status_hierarchy_alternative_exhaustion,
    'Are all alternative status hierarchy forms (democratic, meritocratic, rotating, anonymous) merely different expressions of the same universal status-ordering principle, or do they represent genuinely distinct coordination mechanisms?',
    'Structural analysis of how status is determined, assigned, and enforced in alternative systems; identification of invariant requirements vs contingent implementation choices.',
    'If all hierarchies are isomorphic: mountain classification gains strength — status ordering is natural law. If alternatives are genuinely distinct: the specific form (royal) is contingent, not inevitable; mountain classification fails. Determines whether analytical observer''s view naturalizes or correctly identifies universal constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(status_hierarchy_alternative_exhaustion, conceptual, 'Whether royal hierarchy is necessary form of universal status ordering').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(royal_status_hierarchy, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rsh_tr_t0, royal_status_hierarchy, theater_ratio, 0, 0.45).
narrative_ontology:measurement(rsh_tr_t2, royal_status_hierarchy, theater_ratio, 2, 0.55).
narrative_ontology:measurement(rsh_tr_t4, royal_status_hierarchy, theater_ratio, 4, 0.65).

% Extraction over time
narrative_ontology:measurement(rsh_be_t0, royal_status_hierarchy, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(rsh_be_t2, royal_status_hierarchy, base_extractiveness, 2, 0.68).
narrative_ontology:measurement(rsh_be_t4, royal_status_hierarchy, base_extractiveness, 4, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(royal_status_hierarchy, identity_coordination).
narrative_ontology:affects_constraint(royal_status_hierarchy, feudal_land_tenure).
narrative_ontology:affects_constraint(royal_status_hierarchy, succession_dispute_resolution).
narrative_ontology:affects_constraint(royal_status_hierarchy, legitimacy_of_authority).

% DUAL FORMULATION NOTE:
% Royal status hierarchy is a parent constraint that coordinates multiple institutional subsystems. The feudal land tenure system and succession dispute resolution mechanisms are downstream constraints that depend on the hierarchy's existence. Legitimacy of authority is an upstream constraint that the hierarchy claims to solve. These stories should be linked via network.affects_constraints to show how the hierarchy integrates multiple coordination problems into a single extractive structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(royal_status_hierarchy, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
