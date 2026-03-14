% ============================================================================
% CONSTRAINT STORY: cooperative_member_retention
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cooperative_member_retention, []).

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
 *   constraint_id: cooperative_member_retention
 *   human_readable: Cooperative Member Retention Constraint
 *   domain: economic/organizational
 *
 * SUMMARY:
 *   Cooperative member retention represents a structural constraint where the
 *   mechanism for maintaining collective commitment exhibits simultaneous
 *   coordination and extraction functions. Cooperatives depend on stable
 *   membership to sustain collective capital, risk pooling, and democratic
 *   governance. However, the enforcement mechanisms that prevent exit
 *   (capital lockup, equity restrictions, social enforcement) also create
 *   barriers that concentrate benefits on established members while imposing
 *   costs on newcomers and marginal participants. The constraint exhibits all
 *   six DR types across different observer positions, revealing how the same
 *   structural phenomenon appears as coordination necessity, extractive trap,
 *   temporary scaffold, degraded ritual, or hybrid depending on observer
 *   power and exit capacity. The extractiveness has increased over the
 *   measured interval (0.22 → 0.38) as digital alternatives have reduced
 *   functional exit costs while institutional retention mechanisms have
 *   remained constant, increasing the discrepancy between structural lock-in
 *   and functional necessity. Theater ratio has also increased (0.15 → 0.35)
 *   as governance enforcement increasingly appears performative rather than
 *   necessary to coordination function.
 *
 * KEY AGENTS:
 *   - Trapped New Members: Primary victims (powerless/trapped) — face high exit costs, social ostracism, capital loss; minimal perceived coordination benefit
 *   - Active Core Members: Secondary victims and beneficiaries (moderate/constrained) — experience both coordination benefits and labor asymmetry extraction
 *   - Established Membership Coalition: Primary beneficiaries (institutional/arbitrage) — accumulated equity, governance control; experience constraint as pure coordination
 *   - Cooperative Federation Networks: Organized layer (organized/constrained) — building digital infrastructure and inter-cooperative alternatives that provide sunset mechanism
 *   - Legacy Governance Structures: Institutional maintenance (institutional/arbitrage) — enforce retention mechanisms through tradition and structural momentum
 *   - Analytical Observer: Civilizational position (analytical/analytical) — perceives genuine hybrid structure requiring both coordination and extraction functions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cooperative_member_retention, 0.38).
domain_priors:suppression_score(cooperative_member_retention, 0.42).
domain_priors:theater_ratio(cooperative_member_retention, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cooperative_member_retention, extractiveness, 0.38).
narrative_ontology:constraint_metric(cooperative_member_retention, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(cooperative_member_retention, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cooperative_member_retention, tangled_rope).
narrative_ontology:human_readable(cooperative_member_retention, "Cooperative Member Retention Constraint").
narrative_ontology:topic_domain(cooperative_member_retention, "economic/organizational").

domain_priors:requires_active_enforcement(cooperative_member_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cooperative_member_retention, established_member_base).
narrative_ontology:constraint_beneficiary(cooperative_member_retention, cooperative_governance_structure).
narrative_ontology:constraint_victim(cooperative_member_retention, potential_new_members).
narrative_ontology:constraint_victim(cooperative_member_retention, departing_members).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED NEW MEMBER (SNARE) — A member drawn into a cooperative by initial benefits but facing high exit costs: social ostracism, loss of access to cooperative resources, reputation damage within tight-knit membership. Exit barriers include sunk capital contributions, loss of accumulated equity share, and psychological identification with group. Minimal coordination benefit perceived; extraction appears maximal.
constraint_indexing:constraint_classification(cooperative_member_retention, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: CONFLICTED ACTIVE MEMBER (TANGLED ROPE) — Member experiences both genuine coordination benefits (shared purchasing power, collective decision-making, risk pooling) and asymmetric extraction (labor burden concentrated on active minority, governance structures that favor long-term incumbents, capital lock-in). Exit is possible but costly: loses accumulated equity, social bonds, and cooperative benefits. Mixed experience reflects real hybrid structure.
constraint_indexing:constraint_classification(cooperative_member_retention, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: ESTABLISHED MEMBERSHIP COALITION (ROPE) — Long-term members with accumulated equity and governance influence see the constraint as pure coordination: retaining member commitment preserves the collective's scale and capital base. Exit options abundant (market alternatives, geographic mobility) but unnecessary — arbitrage options are available within the structure. Extraction appears minimal from this perspective; constraint functions as binding mechanism for mutual benefit.
constraint_indexing:constraint_classification(cooperative_member_retention, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: COOPERATIVE FEDERATION (SCAFFOLD) — Regional or national cooperative federation networks see member retention enforcement as a temporary coordination mechanism with structural sunset. As digital platforms reduce transaction costs and inter-cooperative networks mature, individual cooperative boundaries become less critical to member value capture. Retention mechanisms are strategically enforced but understood to be transitional — expect shift toward looser federated models. High visibility of sunset logic.
constraint_indexing:constraint_classification(cooperative_member_retention, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGACY COOPERATIVE GOVERNANCE (PITON) — Traditional board structures and membership committees enforce retention mechanisms (capital lockup, exit fees, governance restrictions) that were functionally necessary in pre-digital cooperative environments. These mechanisms persist through institutional inertia and cultural tradition despite reduced functional need. Theater ratio reflects that enforcement is increasingly performative — member retention is driven more by habit and social cohesion than by structural necessity. The governance apparatus maintains the constraint not because it works optimally but because alternative structures haven't fully replaced it.
constraint_indexing:constraint_classification(cooperative_member_retention, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scope, the constraint exhibits genuine hybrid function. Retention mechanisms coordinate member commitment (necessary for collective capital and risk pooling) while extracting labor asymmetry and capital immobility from newer/marginal members. Both functions are structurally real. The constraint's persistence reflects this genuine coordination necessity, not pure extraction or false naturalness. Mandatrophy resolved: the mechanism serves dual coordination-extraction function simultaneously.
constraint_indexing:constraint_classification(cooperative_member_retention, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cooperative_member_retention_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cooperative_member_retention, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cooperative_member_retention, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(cooperative_member_retention, TR),
    TR >= 0.70.

:- end_tests(cooperative_member_retention_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. Member retention mechanisms extract labor asymmetry, limit capital mobility, and create information asymmetries that favor incumbents. But extraction is not severe — genuine coordination benefits (purchasing power, risk pooling, democratic input) exist and are real. The intermediate value reflects true hybrid structure. Suppression (0.42): Moderate-high. Barriers to exit include sunk capital contributions, loss of accumulated equity share, social ostracism within tight-knit membership, and loss of specialized cooperative benefits. But suppression is not total — market alternatives exist, geographic mobility is possible, and some cooperatives have reduced exit costs. The moderate-high value captures both real barriers and available workarounds. Theater ratio (0.35): Moderate. Governance enforcement mechanisms (board approval of exits, capital lockup periods, equity restrictions) are partly functional (managing collective capital) and partly performative (maintaining incumbent control). The ratio has increased as digital alternatives have reduced functional necessity while institutional enforcement persists. Claimed type (Tangled Rope): Correct classification requires demonstrating both genuine coordination function (member commitment is necessary for collective scale) and asymmetric extraction (exit costs and labor burdens fall disproportionately on newer members and passive participants). Both are empirically present.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. The trapped new member sees pure extraction (Snare) because they perceive exit costs but no coordination benefit — they joined for promised benefits, now face high barriers. The active member sees mixed coordination and extraction (Tangled Rope) because they both contribute to and benefit from collective function but experience labor burden asymmetry. The established member sees pure coordination (Rope) because they have accumulated equity, low effective exit costs through arbitrage, and perceive retention as mutual benefit preservation. The federation sees temporary coordination (Scaffold) because they understand member-level retention barriers as functionally obsolete given emerging inter-cooperative networks and digital platforms. The legacy governance sees degraded ritual (Piton) because enforcement mechanisms are maintained through tradition despite reduced functional necessity. The analytical observer sees genuine hybrid (Tangled Rope) because both coordination and extraction functions are structurally real and necessary to account for observed member retention patterns and dissatisfaction. The perspectival gap reveals that the constraint's true nature depends on observer power and exit capacity — and that the mandatrophy resolution lies in accepting the genuine coexistence of both functions rather than attempting to classify as pure coordination or pure extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values vary by agent structural position within the constraint. Trapped new members (powerless + trapped exit) experience d ≈ 0.95, producing high f(d) and high experienced extraction. Active members (moderate power + constrained exit) experience d ≈ 0.60, producing moderate f(d) and mixed extraction. Established members (institutional power + arbitrage exit) experience d ≈ 0.10, producing low f(d) and minimal extraction. The federation and analytical observers experience d ≈ 0.72 (observer position with constrained/analytical exit), perceiving extraction at the structural level but not experiencing it as personal extraction. These varying d values are derived from: (1) beneficiary/victim declarations (established members are beneficiaries; new and departing members are victims); (2) power level (constraint-relative); (3) exit options (arbitrage vs trapped vs constrained). No directionality overrides are needed — the derivation chain produces appropriate differentiation.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The constraint exhibits genuine coexistence of coordination and extraction functions at multiple structural levels. Mandatrophy is resolved by recognizing that the constraint must simultaneously: (1) coordinate member commitment to sustain collective capital and function (requiring retention mechanisms), and (2) asymmetrically extract from newer/marginal members to concentrate benefits on established members (using those same retention mechanisms as extraction tools). The two functions are structurally entangled — you cannot remove the extraction without eliminating the coordination capacity, and the coordination cannot function without creating asymmetric burdens. This is not a case of mislabeled pure extraction; it is a case of genuinely hybrid mechanism. The mandatrophy classification (Tangled Rope) is confirmed by: (a) presence of beneficiaries (established members); (b) presence of victims (new and departing members); (c) active enforcement required to maintain both functions; (d) perspectival gap showing that different observers with different power/exit relationships perceive different classification types. The constraint is not a hidden snare (pure extraction) nor a hidden rope (pure coordination) — it is legitimately tangled. The analytical resolution moves from 'which is the real nature?' to 'how are these two functions structurally coupled and what trade-offs exist between them?'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exit_cost_mechanism_ambiguity,
    'Are high exit costs structural necessities for cooperative function or extractive gatekeeping mechanisms?',
    'Cross-cooperative comparison: high-exit-cost cooperatives vs low-exit-cost federated networks; measurement of member retention rates, satisfaction, and capital efficiency across models',
    'If structural necessity: tangled_rope classification confirmed; suppression ≥ 0.40 represents coordination cost. If extractive gatekeeping: snare from broader perspective; suppression reduces to 0.25–0.35.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_cost_mechanism_ambiguity, empirical, 'Whether exit costs are structural necessity or extractive gatekeeping').

omega_variable(
    labor_asymmetry_inevitability,
    'Is concentration of labor burden on active minority an inevitable coordination consequence or a choice-dependent extraction pattern?',
    'Comparative analysis of cooperatives with distributed labor rotation vs delegated management; measurement of participation rates, burnout metrics, and succession stability',
    'If inevitable: extractiveness is a necessary cost of democratic coordination. If choice-dependent: extractiveness can be reduced through structural design, revising classification toward pure rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_asymmetry_inevitability, empirical, 'Whether labor asymmetry is inevitable or choice-dependent').

omega_variable(
    member_identity_fusion_depth,
    'To what degree is member retention driven by identity fusion (person sees self primarily as member) vs material incentive (person rationally calculates benefits)?',
    'Ethnographic study; exit interviews documenting stated reasons for departure; measurement of identity-locked vs constrained exit cost perception',
    'If primarily identity-fused: suppression mechanism is partly internalized; identity_locked exit option appropriate for some members; effective suppression higher than structural metrics suggest. If primarily material: trapped and constrained exit options account for observed retention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(member_identity_fusion_depth, empirical, 'Depth of member identity fusion with cooperative').

omega_variable(
    digital_platform_substitution_timeline,
    'How quickly will digital platforms and inter-cooperative networks reduce the functional necessity of individual member retention enforcement?',
    'Tracking adoption of cooperative e-commerce platforms, federated networks, and digital governance tools; measurement of member switching costs over time',
    'If substitution occurs in 5–10 years: scaffold classification confirmed, sunset mechanism is real. If longer: scaffold is aspirational; current retention structure may persist indefinitely as piton.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(digital_platform_substitution_timeline, empirical, 'Timeline for platform-driven substitution of retention enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cooperative_member_retention, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coop_ret_tr_t0, cooperative_member_retention, theater_ratio, 0, 0.15).
narrative_ontology:measurement(coop_ret_tr_t10, cooperative_member_retention, theater_ratio, 10, 0.25).
narrative_ontology:measurement(coop_ret_tr_t20, cooperative_member_retention, theater_ratio, 20, 0.35).

% Extraction over time
narrative_ontology:measurement(coop_ret_be_t0, cooperative_member_retention, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(coop_ret_be_t10, cooperative_member_retention, base_extractiveness, 10, 0.3).
narrative_ontology:measurement(coop_ret_be_t20, cooperative_member_retention, base_extractiveness, 20, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cooperative_member_retention, resource_allocation).
narrative_ontology:affects_constraint(cooperative_member_retention, cooperative_capital_formation).
narrative_ontology:affects_constraint(cooperative_member_retention, democratic_governance_scaling).

% DUAL FORMULATION NOTE:
% Member retention is downstream of capital formation mechanisms (how members accumulate and lose equity) and governance scaling (how collectives maintain decision-making as size increases). These constraints have their own extractiveness values reflecting capital structure and voting dilution dynamics; member retention constraint captures the enforcement side of these upstream structural constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
