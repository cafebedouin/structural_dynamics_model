% ============================================================================
% CONSTRAINT STORY: unrwa_eviction_order
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unrwa_eviction_order, []).

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
 *   constraint_id: unrwa_eviction_order
 *   human_readable: Israeli Land Authority's Eviction Order for UNRWA HQ in East Jerusalem
 *   domain: geopolitical/legal
 *
 * SUMMARY:
 *   The Israeli Land Authority's eviction order for UNRWA's East Jerusalem
 *   headquarters exemplifies a tangled_rope constraint operating at the
 *   intersection of state sovereignty, international humanitarian law, and
 *   geopolitical power asymmetry. The order simultaneously serves an
 *   institutional coordination function (Israeli consolidation of territorial
 *   control and state property regime enforcement) and exercises asymmetric
 *   extraction (coercive displacement of an international humanitarian
 *   institution). The constraint exhibits high suppression (0.72) because
 *   meaningful exit options are systematically constrained: UNRWA cannot
 *   appeal to higher legal authority (suppressed by state power asymmetry),
 *   cannot ignore the order without operational disruption, and cannot easily
 *   relocate without catastrophic service delivery consequences. The theater
 *   ratio (0.68) reflects the prevalence of legal formalism (official
 *   contracts, formal notices, administrative procedures) masking the
 *   underlying power dynamic. The extractiveness value (0.58) reflects that
 *   while extraction is severe for refugee populations and UNRWA operations,
 *   it is not total — the constraint depends on sustained coercive pressure
 *   and international acceptance, making it potentially unstable.
 *
 * KEY AGENTS:
 *   - Israeli Land Authority: Primary beneficiary (institutional/arbitrage) — consolidates state control over contested territory, exercises property regime enforcement, eliminates international presence in disputed jurisdiction
 *   - Israeli State Apparatus: Coordinating beneficiary (institutional/arbitrage) — coordinates across legal, security, administrative institutions to enforce eviction
 *   - Palestinian Refugee Populations: Primary victim (powerless/trapped) — face service disruption with no exit options, no alternative institutional support
 *   - UNRWA Operational Command: Secondary victim (moderate/constrained) — constrained by humanitarian mandate and international legal status; exit costs are catastrophic even if technically possible
 *   - International Humanitarian System: Organized victim (organized/constrained) — sees both coordination problem (humanitarian gap) and extraction (pressure to abandon or legitimize)
 *   - Diplomatic Actors (UN organs, donor states, international coalitions): Organized responders (organized/constrained) — constrained by geopolitical fragmentation; can protest but lack enforcement mechanisms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unrwa_eviction_order, 0.58).
domain_priors:suppression_score(unrwa_eviction_order, 0.72).
domain_priors:theater_ratio(unrwa_eviction_order, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unrwa_eviction_order, extractiveness, 0.58).
narrative_ontology:constraint_metric(unrwa_eviction_order, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(unrwa_eviction_order, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unrwa_eviction_order, tangled_rope).
narrative_ontology:human_readable(unrwa_eviction_order, "Israeli Land Authority's Eviction Order for UNRWA HQ in East Jerusalem").
narrative_ontology:topic_domain(unrwa_eviction_order, "geopolitical/legal").

domain_priors:requires_active_enforcement(unrwa_eviction_order).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unrwa_eviction_order, israeli_land_authority).
narrative_ontology:constraint_beneficiary(unrwa_eviction_order, israeli_state_apparatus).
narrative_ontology:constraint_victim(unrwa_eviction_order, unrwa_operational_capacity).
narrative_ontology:constraint_victim(unrwa_eviction_order, palestinian_refugee_populations).
narrative_ontology:constraint_victim(unrwa_eviction_order, humanitarian_access).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PALESTINIAN REFUGEE POPULATIONS (SNARE) — Trapped within the constraint structure with no exit options. UNRWA's operational disruption directly threatens access to essential services (healthcare, education, social welfare). Cannot exit the jurisdiction, cannot establish alternative infrastructure, cannot appeal to higher authority. Extraction is maximal and coercive.
constraint_indexing:constraint_classification(unrwa_eviction_order, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: UNRWA OPERATIONAL COMMAND (SNARE) — Constrained by international legal status and humanitarian mandate. Can theoretically exit through relocation, but exit costs are catastrophic: losing institutional continuity, affecting service delivery across 5 countries, facing operational fragmentation. Faces coercive pressure through legal enforcement mechanisms. High suppression of alternatives (relocation is technically possible but practically devastating).
constraint_indexing:constraint_classification(unrwa_eviction_order, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: ISRAELI LAND AUTHORITY & STATE APPARATUS (ROPE) — Experiences the eviction order as a coordination mechanism: consolidating state territorial control, asserting sovereignty over contested land, enforcing property regime. Benefits from coordination among state institutions (legal, security, administrative). Low experienced extraction because the constraint aligns with institutional interests and provides arbitrage options (alternative allocations of the same territory).
constraint_indexing:constraint_classification(unrwa_eviction_order, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INTERNATIONAL HUMANITARIAN & UN SYSTEM (TANGLED ROPE) — Organized actors (UN organs, humanitarian coalitions, donor governments) see both coordination and extraction. The constraint creates coordination problems (humanitarian vacuum if UNRWA collapses) but also extracts from the international system by forcing choice between legitimizing eviction or sustaining parallel institutions. Constrained exit options due to geopolitical fragmentation and competing state interests. Active enforcement is required; international pressure is partially suppressed by asymmetric power relations.
constraint_indexing:constraint_classification(unrwa_eviction_order, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL LEGAL FRAMEWORK (PITON) — Formal treaty provisions (UNRWA mandate, UN status agreements, humanitarian law principles) persist as institutional ritual despite degraded enforcement capacity. The constraint reveals that the international legal system's ability to protect UN institutions in disputed territories is largely performative. Legal remedies exist on paper but lack enforcement mechanisms. Theater ratio high: extensive legal proceedings, formal appeals, documented violations — but the underlying structural power asymmetry remains unmoved.
constraint_indexing:constraint_classification(unrwa_eviction_order, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (STRUCTURAL VIEW) — From a civilizational analytical perspective, the constraint exemplifies the collision between territorial sovereignty claims and international humanitarian obligations. The eviction order simultaneously serves coordination (state consolidation) and extraction (forcing humanitarian system to accept humanitarian vacuum or exit). This is a textbook tangled_rope: genuine coordination function within the Israeli institutional framework paired with asymmetric extraction from the humanitarian system.
constraint_indexing:constraint_classification(unrwa_eviction_order, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unrwa_eviction_order_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(unrwa_eviction_order, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(unrwa_eviction_order, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(unrwa_eviction_order, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(unrwa_eviction_order, TR),
    TR >= 0.70.

:- end_tests(unrwa_eviction_order_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts significant costs from UNRWA and refugee populations through service disruption and operational fragmentation. However, extraction is not maximal (0.70+) because the mechanism depends on legal formalism and sustained enforcement rather than deep institutional capture. The Israeli state must continuously assert the constraint; it does not self-enforce through internalized compliance. Suppression (0.72): High. Multiple exit routes are systematically suppressed: UNRWA cannot appeal to higher authority (international law enforcement is asymmetric), cannot maintain current operations without eviction (coercive force), cannot exit without humanitarian consequences (constrained exit), cannot organize resistance effectively (power asymmetry). The suppression is enforced through legal channels, security apparatus, and geopolitical isolation. Theater ratio (0.68): Moderate-high. The constraint operates through formal legal mechanisms (contract violations, administrative notices, formal court proceedings) that create performative legitimacy. However, the underlying reality — territorial consolidation and coercive displacement — is direct and substantive, not purely performative. The theater masks the power dynamic but does not constitute the entire constraint.
 *
 * PERSPECTIVAL GAP:
 *   The constraint manifests as six distinct types depending on observer position. For trapped Palestinian refugees (powerless/trapped), it is a pure Snare — coercive displacement with zero exit options and zero benefits. For UNRWA (moderate/constrained), it is a Snare with partial agency — constrained exit options and some technical compliance capacity, but catastrophic consequences. For the Israeli state (institutional/arbitrage), it is a Rope — coordination mechanism that consolidates state control while providing multiple arbitrage options for alternative territory allocations. For international humanitarian organizations (organized/constrained), it is a Tangled Rope — the system produces both coordination problems (humanitarian vacuum) and extraction (pressure to choose between legitimacy and service continuity). For the international legal framework (institutional/arbitrage), it is a Piton — formal treaty obligations persist as institutional ritual but lack enforcement capacity, revealing degraded structural function. For the analytical observer (analytical/analytical), it is a Tangled Rope — the constraint has genuine coordination function within Israeli state institutions paired with asymmetric extraction from the humanitarian system and refugee populations.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from each agent's structural position relative to extraction flow. Israeli institutional actors (land authority, state apparatus) occupy beneficiary positions with arbitrage exit options — they experience low or negative effective extraction (d ≈ 0.05-0.15) because the constraint aligns with their interests and provides alternative options. UNRWA occupies a moderate victim position with constrained exit (d ≈ 0.55-0.65) — the constraint extracts significant costs but not maximum because UNRWA has some technical capacity to comply or relocate, even at high cost. Palestinian refugees occupy full victim positions with trapped exit (d ≈ 0.95) — maximum extracted costs with zero exit options. International humanitarian actors occupy organized victim positions with constrained exit (d ≈ 0.60-0.70) — they can theoretically withdraw support or pressure Israel, but geopolitical fragmentation constrains these options. The analytical observer (d ≈ 0.72) sees the full structure: beneficiary side and victim side, coordination function and extraction mechanism, institutional coherence and humanitarian violation.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint's dual nature (coordination + extraction) is not a measurement artifact but a structural reality. From the Israeli institutional perspective, the eviction order IS coordination — it solves the institutional problem of state consolidation over disputed territory. From the humanitarian perspective, it IS extraction — it coercively displaces an international institution without substantive alternative provision. Both readings are correct because the constraint operates across incompatible institutional domains with asymmetric power. The constraint is not 'really' a Rope misclassified as a Snare, or vice versa. It is a Tangled Rope because it simultaneously solves coordination problems for one institutional framework (Israeli state) while extracting from another (international humanitarian system). The resolution: classify from explicit perspective. Analytical perspective classifies as Tangled Rope (the system-level view where both functions are visible). Beneficiary perspective classifies as Rope (coordination). Victim perspective classifies as Snare (pure extraction). All three are true.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legal_status_jurisdiction,
    'Does Israeli sovereignty or international treaty obligation take precedence in determining the validity of the eviction order?',
    'International Court of Justice advisory opinion; explicit clarification of UN status agreements and their enforceability over national law; precedent from comparable UN facility disputes',
    'If Israeli sovereignty prevails: eviction is legally valid, constraint is pure institutional consolidation. If international obligation prevails: eviction is a treaty violation, constraint is coercive overreach of state authority.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legal_status_jurisdiction, conceptual, 'Jurisdictional conflict between Israeli sovereignty and international treaty').

omega_variable(
    contract_violation_substance,
    'Are the alleged contract violations substantive breaches or pretextual justifications for political eviction?',
    'Independent technical audit of UNRWA facility compliance with cited contract terms; comparison with enforcement patterns for equivalent violations by other tenants; investigation of contract amendment history and bilateral communications',
    'If substantive: eviction has legal merit, constraint moves toward legitimate property enforcement (Rope). If pretextual: eviction is purely coercive, constraint classifies as Snare from all perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contract_violation_substance, empirical, 'Whether contract violations are substantive or pretextual').

omega_variable(
    humanitarian_continuity_feasibility,
    'Can UNRWA maintain service continuity to Palestinian refugee populations from alternative locations, or does loss of the Jerusalem HQ create an operational vacuum?',
    'Simulation modeling of UNRWA operations from dispersed locations; assessment of communication redundancy, service delivery architecture, donor coordination capacity without centralized HQ; comparison to other humanitarian organizations operating without fixed headquarters',
    'If continuity is feasible: extraction is recoverable, constraint is temporary (Scaffold). If continuity breaks down: extraction is catastrophic, constraint is pure Snare from refugee perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(humanitarian_continuity_feasibility, empirical, 'Whether UNRWA can maintain humanitarian services without East Jerusalem HQ').

omega_variable(
    geopolitical_precedent_cascade,
    'Does the eviction order establish a precedent for other host states to evict UN agencies, degrading the international humanitarian system''s structural integrity?',
    'Monitoring of subsequent eviction threats or attempts against UN agencies in other states; analysis of state rhetoric justifying similar actions; institutional response by UN General Assembly and Security Council',
    'If precedent cascade occurs: constraint weakens the entire international humanitarian order, system-level Snare. If precedent is isolated: constraint is localized institutional conflict, regional Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geopolitical_precedent_cascade, empirical, 'Whether eviction establishes precedent for UN agency expulsion globally').

omega_variable(
    suppression_mechanism_sustainability,
    'Can the Israeli state sustain coercive suppression of international pushback over the 30-day enforcement window and beyond?',
    'Assessment of diplomatic costs (donor funding impacts, international standing damage), legal costs (ongoing litigation, ICJ proceedings), operational costs (security requirements for forced eviction); comparison to historical cases of state coercion against international institutions',
    'If suppression is sustainable: constraint remains Snare/Tangled Rope. If suppression faces escalating costs: constraint becomes unstable, may collapse or transform into negotiated settlement (Scaffold with sunset).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_sustainability, empirical, 'Sustainability of Israeli coercive capacity against international response').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unrwa_eviction_order, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unrwa_tr_t0, unrwa_eviction_order, theater_ratio, 0, 0.55).
narrative_ontology:measurement(unrwa_tr_t15, unrwa_eviction_order, theater_ratio, 15, 0.62).
narrative_ontology:measurement(unrwa_tr_t30, unrwa_eviction_order, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(unrwa_be_t0, unrwa_eviction_order, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(unrwa_be_t15, unrwa_eviction_order, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(unrwa_be_t30, unrwa_eviction_order, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unrwa_eviction_order, enforcement_mechanism).
narrative_ontology:affects_constraint(unrwa_eviction_order, palestinian_territorial_fragmentation).
narrative_ontology:affects_constraint(unrwa_eviction_order, unrwa_mandate_erosion).
narrative_ontology:affects_constraint(unrwa_eviction_order, israeli_settlement_expansion_legality).

% DUAL FORMULATION NOTE:
% The eviction order is upstream of broader constraints on Palestinian territorial coherence and UNRWA institutional viability. Each downstream constraint has its own extractiveness reflecting specific institutional or legal dimensions; the eviction order serves as a structural mechanism enabling those downstream constraints by weakening UNRWA's operational capacity and institutional presence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unrwa_eviction_order, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
