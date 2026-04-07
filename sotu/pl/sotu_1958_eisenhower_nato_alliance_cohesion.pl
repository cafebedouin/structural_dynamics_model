% ============================================================================
% CONSTRAINT STORY: sotu_1958_eisenhower_nato_alliance_cohesion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1958_eisenhower_nato_alliance_cohesion, []).

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
 *   constraint_id: sotu_1958_eisenhower_nato_alliance_cohesion
 *   human_readable: NATO Alliance Cohesion and Integrated Security Coordination
 *   domain: foreign_policy/geopolitics/alliance_structure
 *
 * SUMMARY:
 *   NATO represents a critical structural arrangement in Cold War
 *   geopolitics: a voluntary alliance of democratic nations unified by
 *   collective security commitment and ideological alignment against
 *   Soviet-dominated Eastern bloc. Eisenhower's 1958 framing emphasizes NATO
 *   as a multiplication of U.S. security power through aggregation of allied
 *   military, economic, and spiritual (ideological) resources. The constraint
 *   operates at multiple levels simultaneously: as genuine coordination
 *   mechanism solving the collective action problem of defending against a
 *   superior Soviet military threat; as extraction mechanism concentrating
 *   leadership authority in the U.S.; as performance of democratic unity
 *   masking asymmetric burden-sharing; and as institutional structure that
 *   persists long after its primary justification (Soviet containment) has
 *   dissolved. The constraint's evolution from 1958 through the post-Cold War
 *   period demonstrates lifecycle drift: from high coordination value with
 *   moderate extraction (1958-1989), toward increasing theater and reduced
 *   coordination necessity (1990-2026). This story models how a constraint
 *   can be simultaneously all six DR types from different structural
 *   perspectives, revealing how alliance membership legitimation rhetoric
 *   naturalizes what may be hegemonic extraction disguised as coordination.
 *
 * KEY AGENTS:
 *   - United States: Primary beneficiary (institutional/arbitrage) — gains legitimacy for security leadership, burden-sharing leverage, ideological validation, military base network, and multiplication of power projection
 *   - Small/Medium NATO Members (Europe): Secondary victims (moderate/constrained) — gain genuine security coordination benefits but pay extraction costs through military budget commitments, loss of independent foreign policy, and subordination to U.S. strategic dominance
 *   - Soviet Union / Warsaw Pact: Contrasting extractive system (institutional/trapped) — demonstrates alternative alliance structure based on coercive dominance rather than voluntary ideological alignment
 *   - Non-Aligned Nations: Geopolitical victims (powerless/trapped) — experience Cold War binary pressure and extraction through exclusion from Western economic order and security guarantees
 *   - Liberal Democratic Order (civilizational actor): Primary beneficiary (institutional/arbitrage) — receives institutional legitimation through NATO as proof of superior organizational model vs. Soviet authoritarianism
 *   - NATO Institutional Structure: Self-perpetuating actor (institutional/arbitrage) — benefits from its own existence; develops self-preservation interests independent of original security mission
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1958_eisenhower_nato_alliance_cohesion, 0.52).
domain_priors:suppression_score(sotu_1958_eisenhower_nato_alliance_cohesion, 0.48).
domain_priors:theater_ratio(sotu_1958_eisenhower_nato_alliance_cohesion, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1958_eisenhower_nato_alliance_cohesion, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_1958_eisenhower_nato_alliance_cohesion, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(sotu_1958_eisenhower_nato_alliance_cohesion, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1958_eisenhower_nato_alliance_cohesion, tangled_rope).
narrative_ontology:human_readable(sotu_1958_eisenhower_nato_alliance_cohesion, "NATO Alliance Cohesion and Integrated Security Coordination").
narrative_ontology:topic_domain(sotu_1958_eisenhower_nato_alliance_cohesion, "foreign_policy/geopolitics/alliance_structure").

domain_priors:requires_active_enforcement(sotu_1958_eisenhower_nato_alliance_cohesion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1958_eisenhower_nato_alliance_cohesion, united_states).
narrative_ontology:constraint_beneficiary(sotu_1958_eisenhower_nato_alliance_cohesion, nato_member_states).
narrative_ontology:constraint_beneficiary(sotu_1958_eisenhower_nato_alliance_cohesion, liberal_democratic_order).
narrative_ontology:constraint_victim(sotu_1958_eisenhower_nato_alliance_cohesion, member_state_sovereignty).
narrative_ontology:constraint_victim(sotu_1958_eisenhower_nato_alliance_cohesion, non_aligned_nations).
narrative_ontology:constraint_victim(sotu_1958_eisenhower_nato_alliance_cohesion, soviet_sphere_friction).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SOVIET-ALIGNED SATELLITE STATE (SNARE) — Trapped within Soviet sphere without genuine consent; experiences Warsaw Pact membership as pure coercive extraction masquerading as coordination. Maximum suppression, no alternative exit. Serves as contrast case: demonstrates that NATO's voluntary membership structure and ideological alignment creates genuine coordination benefits unavailable in Soviet dominance model.
constraint_indexing:constraint_classification(sotu_1958_eisenhower_nato_alliance_cohesion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: NON-ALIGNED NATION (SNARE) — Structurally trapped between Cold War blocs; NATO expansion and U.S. security umbrella create geopolitical pressure to align without full membership benefits. Cannot exit Cold War binary pressure; extraction through coercive alignment or exclusion from Western economic order. High suppression, minimal coordination benefit.
constraint_indexing:constraint_classification(sotu_1958_eisenhower_nato_alliance_cohesion, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: SMALL NATO MEMBER STATE (TANGLED ROPE) — Gains genuine security benefit from collective defense commitment but pays cost through military coordination burden, budget allocation to NATO commitments, and loss of independent foreign policy flexibility. Exit is legally possible but politically/militarily constrained: without NATO, vulnerable to Soviet pressure. Genuine coordination (collective security) with asymmetric extraction (burden-sharing disproportionate to power).
constraint_indexing:constraint_classification(sotu_1958_eisenhower_nato_alliance_cohesion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 4: UNITED STATES (ROPE) — Net beneficiary experiencing NATO as pure coordination mechanism. Receives legitimation of U.S. security leadership, burden-sharing among prosperous allies, ideological alignment reducing Cold War friction, and multiplication of U.S. power projection through allied bases and capabilities. Can arbitrage U.S. position: shift resources between NATO, Pacific alliances, or bilateral arrangements. Experiences constraint as coordination with clear benefits and negotiable costs.
constraint_indexing:constraint_classification(sotu_1958_eisenhower_nato_alliance_cohesion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: EUROPEAN GREAT POWER / FRANCE (TANGLED ROPE) — Powerful enough to negotiate and extract concessions (French force de frappe, NATO command structure disputes) but constrained by geopolitical reality of Soviet threat and relative decline vs. U.S. power. Gains security coordination benefit but chafes at U.S. hegemony within the alliance. Mobile exit option (partial withdrawal, independent deterrent) creates leverage. Sees constraint as mixed genuine coordination with structured subordination to U.S. strategic dominance.
constraint_indexing:constraint_classification(sotu_1958_eisenhower_nato_alliance_cohesion, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 6: NATO INSTITUTIONAL FRAMEWORK (SCAFFOLD) — From the vantage of the Cold War's end (1989 onward), the NATO alliance appears as a temporary coordination structure with declining justification. The original security threat (Soviet Union) dissolved; the extraction mechanism (mandatory Cold War alignment) lost its rationale. NATO's continuation into the post-Cold War era reveals the constraint as increasingly theatrical — institutionalized by inertia rather than geopolitical necessity. Theater ratio rises as alliance searches for new missions (out-of-area interventions, Eastern expansion) to justify continued existence.
constraint_indexing:constraint_classification(sotu_1958_eisenhower_nato_alliance_cohesion, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: LIBERAL DEMOCRATIC INSTITUTIONAL ORDER (PITON) — NATO persists as a degraded institutional form after its primary function (Soviet containment) has atrophied. The alliance maintains itself through institutional self-preservation rhetoric ('burden-sharing,' 'rules-based order,' 'democratic values') rather than genuine security necessity. Theater ratio very high: NATO summits, burden-sharing controversies, and Article 5 invocation (after 9/11) become performative maintenance of a constraint that no longer has structural justification. The constraint persists through institutional inertia and the vested interests of military bureaucracies, not because the original coordination problem requires it.
constraint_indexing:constraint_classification(sotu_1958_eisenhower_nato_alliance_cohesion, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / STRUCTURAL REALISM (MOUNTAIN) — From a civilizational/universal perspective, NATO reflects an immutable constraint: in an anarchic international system without supranational authority, states must band together for security. The need for alliance coordination is a structural feature of international relations, not a contingent institutional choice. Hegemonic stability theory suggests that U.S. leadership and burden-sharing are natural emergents from power asymmetries. However, this perspective risks naturalizing what is actually a contingent post-WWII institutional choice. The engine will flag this as a false summit: the 'structural necessity' framing naturalizes political arrangements (membership, burden-sharing formulas, command hierarchy) that are actually negotiated and contestable.
constraint_indexing:constraint_classification(sotu_1958_eisenhower_nato_alliance_cohesion, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1958_eisenhower_nato_alliance_cohesion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1958_eisenhower_nato_alliance_cohesion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1958_eisenhower_nato_alliance_cohesion, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1958_eisenhower_nato_alliance_cohesion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1958_eisenhower_nato_alliance_cohesion, TR),
    TR >= 0.70.

:- end_tests(sotu_1958_eisenhower_nato_alliance_cohesion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. NATO exhibits genuine coordination benefits (collective security against a credible military threat) combined with asymmetric extraction (U.S. bears ~70% of defense expenditure but captures disproportionate leadership authority, geopolitical leverage, and ideological validation). The constraint aggregates resources from member states in service of U.S.-led security architecture. The extraction is not maximal because member states receive substantial security benefits and retain legal sovereignty. Suppression (0.48): Moderate. Exit barriers exist but are not absolute. Legal withdrawal is possible (NATO has no mechanism to prevent exit) but carries high political and military costs: a member exiting loses collective defense guarantee and faces Soviet pressure. Smaller states experience higher suppression (exit more costly) than larger states. Theater ratio (0.58): Moderate-high. Alliance legitimation rhetoric emphasizes democratic values and shared civilization, which functions as performance of alignment. Military command structures, burden-sharing negotiations, and summit declarations serve partly to maintain organizational cohesion rather than to solve purely technical security problems. Theater has risen from 1958 (0.35) to 2026 (0.68) as the original security mission (Soviet containment) has become less salient and NATO searches for new justifications. This trajectory indicates potential Piton degradation — constraint persists through institutional inertia after primary function atrophied.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The U.S. (institutional/arbitrage) experiences NATO as a coordination mechanism — solving security collective action problem while amplifying U.S. influence — with clear benefits and negotiable costs. Small member states (moderate/constrained) experience genuine security coordination benefit but alongside extraction of military resources and subordination of independent foreign policy. Non-aligned nations (powerless/trapped) experience coercive alignment pressure and exclusion from Western economic order without membership benefits. The Soviet-aligned satellite (powerless/trapped) experiences pure coercive domination that lacks even the voluntary ideological alignment NATO claims. The European great power (powerful/mobile) negotiates extraction of concessions (independent deterrent, command structure disputes) while remaining aligned. The NATO institution itself (institutional/arbitrage) increasingly appears to be self-perpetuating (Piton) rather than functionally necessary. The analytical observer (analytical/analytical) risks seeing NATO as a Mountain — an immutable structural feature of anarchy — when it is actually a contingent post-WWII institutional choice that beneficiaries have rationalized as natural law. This perspectival separation is the engine diagnostic: multiple legitimate readings of the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from each agent's position in the extraction flow and exit capacity. United States (d ≈ 0.15): Net beneficiary with arbitrage options; can shift resources to Pacific theater or bilateral arrangements; experiences negative effective extraction (benefits exceed costs). Small NATO member (d ≈ 0.70): Moderate victim status with constrained exit; must maintain NATO alignment for security; can exit but at high military/political cost. Non-aligned state (d ≈ 0.92): Victim without membership; trapped in Cold War binary pressure; can neither join (geopolitical or ideological barriers) nor exit (coercive alignment pressure). Soviet satellite (d ≈ 0.95): Maximal victim status; trapped without meaningful exit option; coercive dominance with minimal coordination benefit. France (d ≈ 0.55): Powerful enough to negotiate (mobile exit option) but structurally dependent on NATO for security; can extract concessions through threat of partial withdrawal. The derivation chain operates correctly: beneficiary status + arbitrage exit → low d; victim status + trapped exit → high d; mixed status + constrained exit → middle d. These d values feed the sigmoid f(d) to produce effective extractiveness chi for each perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that all six types are legitimate perspectival readings reflecting different structural positions. There is no single 'correct' classification — the manifold of classifications IS the answer. The U.S. sees Rope (pure coordination); small members see Tangled Rope (mixed); non-aligned states see Snare (pure extraction); the institution itself sees Piton (degraded); European powers see constrained Tangled Rope with negotiating leverage; the analytical observer sees Mountain but risks false summit naturalization. The true insight is not 'which type is right?' but 'what does the perspectival gap reveal about extraction mechanisms?' The gap shows that NATO's legitimating rhetoric (democratic values, shared civilization, voluntary alignment) functions to obscure asymmetric extraction (U.S. leadership authority, burden-sharing disproportionality, strategic dominance). The constraint persists partly because its extractive structure is hidden behind coordination framing. Post-Cold War, as the genuine security necessity declines, the theater ratio rises — NATO searches for new missions (counterterrorism, Eastern expansion, humanitarian intervention) to justify institutional continuation. This lifecycle drift from coordination-dominant to theater-dominant suggests nascent Piton degradation. The mandatrophy is resolved by recognizing that the 'true' classification depends on time horizon and agent perspective: at civilizational scale and from a powerless non-aligned agent, the constraint appears as pure extraction; at immediate scale and from the U.S., it appears as pure coordination; from the institution itself at generational scale, it appears as increasingly theatrical maintenance of a defunct constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alliance_cohesion_mechanism_contested,
    'Does NATO cohesion derive from genuine shared security interests (coordination) or from U.S. power and hegemonic enforcement (extraction)?',
    'Historical analysis of alliance behavior during periods of U.S. decline in relative power (1968-1975, post-2008 financial crisis); measurement of cohesion during conflicts where U.S. interests diverge from allied interests (Suez 1956, Vietnam, Iraq 2003); comparison of exit costs for members with vs. without independent nuclear deterrents',
    'If genuine shared interests: classification shifts toward Rope from multiple perspectives. If hegemonic enforcement: classification shifts toward Snare for smaller members, reflecting extraction masked by coordination framing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alliance_cohesion_mechanism_contested, empirical, 'Whether NATO cohesion is genuine shared interest or hegemonic enforcement').

omega_variable(
    burden_sharing_asymmetry_sustainability,
    'Is the disproportionate U.S. military spending within NATO (roughly 70% of alliance defense expenditure) a sustainable coordination cost or an unsustainable extraction from the U.S. to allies?',
    'Long-term comparison of NATO defense burden-sharing vs. benefits accrual; measurement of U.S. GDP devoted to alliance commitments vs. economic gains from allied markets and bases; identification of counterfactual: what would U.S. security posture be without NATO?',
    'If sustainable: U.S. gain (arbitrage), Rope classification confirmed. If unsustainable: U.S. is extracting security from allies at cost to itself, suggesting Piton (degraded mechanism that persists despite imbalance) or constraint reclassification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(burden_sharing_asymmetry_sustainability, empirical, 'Sustainability of U.S. burden-sharing disproportionality').

omega_variable(
    false_summit_natural_law_ambiguity,
    'Is NATO an immutable structural feature of international anarchy (Mountain) or a contingent institutional choice that beneficiaries have rationalized as natural law?',
    'Comparison with non-Western alliance structures (ASEAN, African Union) that achieve coordination without hierarchical hegemonic structure; analysis of alternative institutional designs proposed during NATO''s formation; investigation of whether ''structural realism'' explaining NATO necessity also explains Soviet sphere necessity',
    'If contingent institutional choice: Mountain classification is a false summit revealing naturalization of U.S. hegemonic structure as inevitability. If genuine structural necessity: Mountain holds and alternative structures are empirically inferior.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law_ambiguity, conceptual, 'Whether NATO necessity is structural law or naturalized choice').

omega_variable(
    post_cold_war_theater_accumulation,
    'After 1989 Soviet collapse, does NATO theater ratio increase through institutional self-preservation missions (counterterrorism, out-of-area operations, Eastern expansion) without corresponding security necessity?',
    'Time series analysis of NATO mission statements and operations: 1958-1989 (containment focus) vs. 1990-2026 (proliferating missions); measurement of actual threat from non-state actors vs. Soviet threat; assessment of whether NATO''s new missions generate genuine coordination benefits or primarily serve institutional preservation',
    'If theater increases significantly post-1989: Piton classification (degraded former constraint) is correct; NATO persists through institutional inertia. If NATO generates genuine new coordination benefits: Rope or Tangled Rope persists post-Cold War.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(post_cold_war_theater_accumulation, empirical, 'Whether NATO theater increases post-1989 through institutional self-preservation').

omega_variable(
    soviet_sphere_comparison_validity,
    'Is the comparison between voluntary NATO membership and coercive Warsaw Pact dominance structurally sound, or does it obscure how alliance membership constrains smaller states regardless of formal voluntariness?',
    'Analysis of exit capacity for NATO members: legal vs. practical (Hungary 1956, attempted Czech withdrawal 1968, France 1966 partial withdrawal); comparison of exit costs for NATO vs. Warsaw Pact members; investigation of coercive mechanisms within NATO (economic pressure, military pressure, diplomatic isolation)',
    'If NATO exit is genuinely costless: moral distinction from Soviet sphere holds; Rope/Tangled Rope for small members is correct. If exit is effectively coercive: distinction is rhetorical rather than structural; small-member classification shifts toward Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(soviet_sphere_comparison_validity, empirical, 'Whether NATO voluntariness is genuine or coercive exit prevention').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1958_eisenhower_nato_alliance_cohesion, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nato_cohesion_tr_t0, sotu_1958_eisenhower_nato_alliance_cohesion, theater_ratio, 0, 0.35).
narrative_ontology:measurement(nato_cohesion_tr_t5, sotu_1958_eisenhower_nato_alliance_cohesion, theater_ratio, 5, 0.48).
narrative_ontology:measurement(nato_cohesion_tr_t10, sotu_1958_eisenhower_nato_alliance_cohesion, theater_ratio, 10, 0.58).
narrative_ontology:measurement(nato_cohesion_tr_t15, sotu_1958_eisenhower_nato_alliance_cohesion, theater_ratio, 15, 0.68).

% Extraction over time
narrative_ontology:measurement(nato_cohesion_be_t0, sotu_1958_eisenhower_nato_alliance_cohesion, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(nato_cohesion_be_t5, sotu_1958_eisenhower_nato_alliance_cohesion, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(nato_cohesion_be_t10, sotu_1958_eisenhower_nato_alliance_cohesion, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(nato_cohesion_be_t15, sotu_1958_eisenhower_nato_alliance_cohesion, base_extractiveness, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1958_eisenhower_nato_alliance_cohesion, enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_1958_eisenhower_nato_alliance_cohesion, cold_war_bloc_competition).
narrative_ontology:affects_constraint(sotu_1958_eisenhower_nato_alliance_cohesion, nuclear_deterrence_stability).
narrative_ontology:affects_constraint(sotu_1958_eisenhower_nato_alliance_cohesion, european_integration_trajectory).
narrative_ontology:affects_constraint(sotu_1958_eisenhower_nato_alliance_cohesion, transatlantic_burden_sharing).

% DUAL FORMULATION NOTE:
% NATO alliance cohesion is upstream of multiple structural constraints in Cold War geopolitics. The alliance structure enables nuclear deterrence stability (mutual defense commitment reduces Soviet incentive for preemptive strike) and constrains European integration (subordinates European autonomy to U.S.-led security framework). The post-Cold War degradation of NATO's original security mission creates downstream effects in European autonomy, burden-sharing disputes, and institutional search for justification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1958_eisenhower_nato_alliance_cohesion, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
