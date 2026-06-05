% ============================================================================
% CONSTRAINT STORY: market_as_natural_default__dual_operation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_as_natural_default__dual_operation_reading, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: market_as_natural_default__dual_operation_reading
 *   human_readable: Market Naturalization as Dual-Operation Closure
 *   domain: political_economy/institutional_theory/ideology_studies
 *
 * SUMMARY:
 *   Market naturalization in political economy operates through a specific
 *   dual mechanism: institutional-level state redesign defending market
 *   allocation coexists with ideological-level presentation of 'the market'
 *   as self-evident, non-constructed, and inevitable. This constraint reading
 *   instantiates the claim that BOTH operations are simultaneously active—the
 *   engineering is not forgotten (lapsed_closure_reading) nor is it purely
 *   current without ideological cover (engineered_closure_reading). Instead,
 *   institutional beneficiaries continuously maintain market-defending state
 *   structures while the ideological apparatus simultaneously renders that
 *   institutional work invisible. The dual operation creates a structural
 *   trap for agents seeking institutional change: they encounter active state
 *   defense when inside legislative or regulatory arenas, but when they
 *   attempt public mobilization around that defense, the ideological
 *   naturalization makes the institutional work disappear from public
 *   discourse. This is different from mere cognitive failure—it is a
 *   structural feature of how the constraint operates. Progressive
 *   challengers face asymmetric costs: institutional defense is continuous,
 *   well-funded, and embedded in state machinery; challenging that machinery
 *   requires first breaking through ideological naturalization to make the
 *   institutional work visible. The constraint exhibits all six
 *   classification types from different observer positions, with the dual
 *   operation explaining why the perspectives diverge.
 *
 * KEY AGENTS:
 *   - Market-Defending State Apparatus: Institutional beneficiary (institutional/arbitrage) — continuously redesigns tax, labor, financial regulation to maintain market allocation; experiences work as coordination, not extraction
 *   - Capital Concentration Holders: Powerful beneficiary (powerful/mobile) — benefits from both market coordination function AND from ideological naturalization that prevents institutional challenge
 *   - Progressive Reform Coalition: Moderate victim (moderate/constrained) — structurally disadvantaged by dual operation; can see institutional work inside legislative arenas, cannot maintain visibility in public discourse
 *   - Decommodification Advocates: Primary victim (powerless/trapped) — trapped in cognitive double-bind; cannot mobilize against a mechanism that appears non-existent at ideological level while operating covertly at institutional level
 *   - Economics Discipline: Institutional actor (institutional/arbitrage) — maintains performative naturalization in pedagogy while conducting research documenting market engineering; internal contradiction visible only to insiders
 *   - Analytical Observer: Civilizational position (analytical/analytical) — risks naturalizing the dual operation itself as inevitable rather than recognizing it as a specific institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_as_natural_default__dual_operation_reading, 0.58).
domain_priors:suppression_score(market_as_natural_default__dual_operation_reading, 0.68).
domain_priors:theater_ratio(market_as_natural_default__dual_operation_reading, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_as_natural_default__dual_operation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(market_as_natural_default__dual_operation_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(market_as_natural_default__dual_operation_reading, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_as_natural_default__dual_operation_reading, tangled_rope).
narrative_ontology:human_readable(market_as_natural_default__dual_operation_reading, "Market Naturalization as Dual-Operation Closure").
narrative_ontology:topic_domain(market_as_natural_default__dual_operation_reading, "political_economy/institutional_theory/ideology_studies").

domain_priors:requires_active_enforcement(market_as_natural_default__dual_operation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_as_natural_default__dual_operation_reading, '976940bc-e72c-464b-ae21-fc7ec7376e15').
narrative_ontology:cs_kernel_codification('976940bc-e72c-464b-ae21-fc7ec7376e15', formalized).
narrative_ontology:cs_authority_grounding('976940bc-e72c-464b-ae21-fc7ec7376e15', extraction).
narrative_ontology:cs_interpretation_layer_present('976940bc-e72c-464b-ae21-fc7ec7376e15').
narrative_ontology:cs_reading_relation('976940bc-e72c-464b-ae21-fc7ec7376e15', market_as_natural_default__engineered_closure_reading, coexists_with).
narrative_ontology:cs_reading_relation('976940bc-e72c-464b-ae21-fc7ec7376e15', market_as_natural_default__lapsed_closure_reading, influences).
narrative_ontology:cs_axiom('976940bc-e72c-464b-ae21-fc7ec7376e15', foundational, institutional_engineering_currently_active).
narrative_ontology:cs_axiom_status(institutional_engineering_currently_active, holdable).
narrative_ontology:cs_axiom_grounding('976940bc-e72c-464b-ae21-fc7ec7376e15', institutional_engineering_currently_active, empirically_contingent).
narrative_ontology:cs_axiom('976940bc-e72c-464b-ae21-fc7ec7376e15', foundational, ideological_naturalization_obscures_institutional_work).
narrative_ontology:cs_axiom_status(ideological_naturalization_obscures_institutional_work, holdable).
narrative_ontology:cs_axiom_grounding('976940bc-e72c-464b-ae21-fc7ec7376e15', ideological_naturalization_obscures_institutional_work, instrumental).
narrative_ontology:cs_reference_frame('976940bc-e72c-464b-ae21-fc7ec7376e15', market_construction_requires_active_institutional_defense).
narrative_ontology:cs_drift_state('976940bc-e72c-464b-ae21-fc7ec7376e15', contemporary_financialized_capitalism, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('976940bc-e72c-464b-ae21-fc7ec7376e15', '2026-02-26T14:32:15Z').
narrative_ontology:cs_kernel_id(market_as_natural_default__dual_operation_reading, market_as_natural_default).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_as_natural_default__dual_operation_reading, institutional_market_defenders).
narrative_ontology:constraint_beneficiary(market_as_natural_default__dual_operation_reading, capital_concentration_holders).
narrative_ontology:constraint_victim(market_as_natural_default__dual_operation_reading, progressive_institutional_challengers).
narrative_ontology:constraint_victim(market_as_natural_default__dual_operation_reading, decommodification_advocates).
narrative_ontology:constraint_victim(market_as_natural_default__dual_operation_reading, epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DECOMMODIFICATION ADVOCATE (SNARE) — Trapped in a cognitive/structural double bind. At the institutional level, encounters active state redesign defending market allocation (engineered closure). At the ideological level, the defense is invisible — 'the market' appears self-evident, not actively maintained. Cannot mobilize against a structure that denies it is a structure. Maximum extraction: the constraint's dual form prevents the powerless agent from naming the mechanism binding them.
constraint_indexing:constraint_classification(market_as_natural_default__dual_operation_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PROGRESSIVE REFORM COALITION (TANGLED ROPE) — Constrained by the dual-operation structure. Can see institutional work (tax policy, labor law redesign) when positioned inside legislative arenas. Cannot maintain visibility of that institutional work when operating in public-facing ideology. Faces asymmetric costs: institutional defense is continuous and well-funded; institutional challenge requires building counter-narrative despite ideological naturalization. Mixed experience: some coordination function (legislative contestation) alongside significant extraction (structural disadvantage in naming the contest).
constraint_indexing:constraint_classification(market_as_natural_default__dual_operation_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MARKET-DEFENDING STATE APPARATUS (ROPE) — Institutional actor experiencing the constraint as pure coordination: the state continuously redesigns tax, labor, and financial regulation to defend market allocation. This is not experienced as extraction but as rational institutional work solving the collective action problem of market stability. Net beneficiary: the state apparatus gains legitimacy and material capacity through its market-defending function. Experiences the constraint as coordination because the institutional machinery continuously functions, and the machinery sees itself as solving problems.
constraint_indexing:constraint_classification(market_as_natural_default__dual_operation_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CAPITAL CONCENTRATION BENEFICIARY (TANGLED ROPE) — Powerful actor with genuine mobility (can relocate capital, arbitrage between jurisdictions) but benefits from the dual-operation closure. Experiences a genuine coordination function: market institutions coordinate capital flows and property rights allocation. Simultaneously experiences extraction advantage: ideological naturalization prevents institutional challengers from organizing effective opposition. Mobile exit option but uses it strategically WITHIN the market framework, not to exit the constraint itself. The constraint produces genuine coordination (for this agent) with asymmetric extraction (against challengers).
constraint_indexing:constraint_classification(market_as_natural_default__dual_operation_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: ECONOMICS DISCIPLINE (PITON) — The discipline maintains a degraded function: teaching and defending market-naturalization ideology while simultaneously conducting research showing the engineered nature of market institutions (behavioral economics, institutional economics, complexity economics). The disciplinary structure persists through institutional inertia despite internal contradiction. Theater ratio high: the discipline performs the naturalization ritual in pedagogy while its cutting-edge research documents the engineering. The piton classification derives from the internal theatrical maintenance of a function (market defense) that the discipline's own research has partially dismantled.
constraint_indexing:constraint_classification(market_as_natural_default__dual_operation_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN FALSE-SUMMIT VIEW) — From a civilizational frame, this perspective reads market naturalization as inevitable: humans naturally organize allocation through exchange; markets emerge spontaneously wherever scarcity exists; alternative coordination mechanisms require continuous enforcement so fail in scaled contexts. This view naturalizes the constraint as an immutable property of human economic organization. However, the structural data (declared beneficiaries, measurable institutional maintenance costs, documented state redesign) indicates this is a false summit — naturalizing what is actually the engineered closure dual operation. The engine's false-summit detector will flag this perspective.
constraint_indexing:constraint_classification(market_as_natural_default__dual_operation_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_as_natural_default__dual_operation_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(market_as_natural_default__dual_operation_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(market_as_natural_default__dual_operation_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(market_as_natural_default__dual_operation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(market_as_natural_default__dual_operation_reading, TR),
    TR >= 0.70.

:- end_tests(market_as_natural_default__dual_operation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts from decommodification advocates and progressive challengers through the structural inability to name and mobilize against it. However, extractiveness is not maximal (0.72+) because the constraint requires continuous institutional work to maintain—the engineering is not passive. As institutional maintenance costs rise (Perspective 2's escalating resource requirements for institutional defense), the constraint becomes more visibly extractive. The rising trajectory (0.38 → 0.58 over t=10) reflects increasing institutional maintenance costs as market naturalization faces empirical challenges (climate policy, financialization instability, inequality visibility). Suppression (0.68): High. The constraint operates through suppression of alternative institutional framings—market-as-natural prevents visibility of market-as-constructed alternatives. This suppression is both material (institutional barriers to non-market allocation) and cognitive (ideological barriers to perceiving institutions as engineered). Suppression is increasing (0.55 → 0.68) as institutional defenders invest more heavily in ideological maintenance in response to visibility threats (climate, inequality, pandemic disruptions). Theater ratio (0.64): Moderate-high. The ideological naturalization is substantially performative—it requires continuous work to maintain the appearance of inevitability, yet empirical evidence contradicts it. The theater is increasing (0.48 → 0.64) as the gap between naturalization claims and institutional reality widens.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exemplifies how the same structural mechanism produces radically different classifications across observer positions. The market-defending state sees coordination (Rope)—legitimate institutional work. Capital holders see genuine coordination with extraction advantage (Tangled Rope). Progressive coalitions see mixed coordination with structural disadvantage (Tangled Rope but with asymmetric experience). Decommodification advocates see pure extraction (Snare)—they are trapped in a mechanism they cannot name. The economics discipline sees its own degraded function (Piton)—pedagogically defending market naturalization while research documents market engineering. The civilizational analytical observer risks seeing natural law (Mountain false summit)—naturalizing the dual operation as inevitable. The perspectival gap is NOT due to disagreement about facts; all perspectives could see the same institutional mechanisms if the ideological naturalization were broken. The gap persists because the constraint's operation depends on maintaining asymmetric visibility—institutional actors see the engineering because they conduct it; challengers do not see the engineering because the ideology hides it.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is derived from the agent's structural position within the dual operation. Market-defending state actors experience low d (beneficiary with arbitrage options) → low chi despite high ε, because they benefit from both coordination and extraction. Progressive challengers experience high d (victim with constrained exit) → high chi despite moderate power, because the dual operation structurally disadvantages them. Decommodification advocates experience maximum d (powerless victim with no exit) → maximum chi, because they cannot escape the mechanism and cannot even see it clearly. Capital holders experience low-moderate d (beneficiary with mobile options) because they can exit within the market framework, though they benefit from the ideological naturalization preventing broader exit options. The dual operation's specific feature is that d values are masked by the ideological layer—state actors do not perceive themselves as beneficiaries (they see themselves as managing inevitable forces), and victims do not perceive the mechanism extracting from them (they perceive only a natural constraint, not an institutional one).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves mandatrophy by showing that the constraint's ε value depends on whether the dual operation remains hidden or becomes visible. At ε=0.58, the constraint is a genuine Tangled Rope: market institutions coordinate allocation AND extract advantage through naturalization. If institutional engineering becomes fully visible, extractiveness would rise toward 0.72+ (Snare territory) because the extraction mechanism would no longer be obscured. If ideological naturalization breaks completely and the engineering is revealed as contingent rather than inevitable, the constraint would shift from Tangled Rope toward either genuine Rope (if decommodification succeeds in institutional redesign) or collapse entirely (if alternative institutional forms prove viably coordinative). The mandatrophy is not 'which type is correct?' but 'which layer of the dual operation is currently visible?' At the current state (both layers partially active, ideological layer still dominant in public discourse), Tangled Rope is the accurate classification. As visibility conditions change, so does the type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dual_operation_visibility_threshold,
    'At what scale of institutional maintenance cost does the ideological naturalization become untenable? What ratio of visible engineering to invisible ideology triggers crisis?',
    'Historical comparison of moments when market naturalization has visibly broken: post-2008 financial crisis, COVID disruption, climate policy failures. Measurement of public discourse shift from ''markets are natural'' to ''markets are constructed'' following institutional failures.',
    'If threshold low: dual operation is fragile, vulnerable to empirical challenge. If threshold high: the operation is robust even under evidence of engineering. Determines whether this reading is sustainable long-term.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_operation_visibility_threshold, empirical, 'Visibility threshold for breaking market-naturalization ideology').

omega_variable(
    institutional_maintenance_cost_measurement,
    'How do we measure and attribute institutional maintenance costs to market-defending state work versus genuine coordination overhead?',
    'Forensic institutional accounting: trace state spending on tax compliance infrastructure, financial regulation enforcement, property rights adjudication, intellectual property protection. Compare to parallel spending in genuinely market-independent domains (public health coordination, scientific knowledge commons). Determine what portion is market-specific maintenance versus general administration.',
    'If maintenance costs are clearly attributable: the engineered nature of the constraint becomes visible. If costs are distributed/opaque: the naturalization persists. Affects whether decommodification advocates can mobilize around concrete institutional targets.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_maintenance_cost_measurement, empirical, 'How to measure state spending on market maintenance').

omega_variable(
    covert_operation_sustainability,
    'Can the dual operation (engineered at institutional level, naturalized at ideological level) be sustained indefinitely, or does scale/complexity eventually force the engineering into visibility?',
    'Long-term organizational analysis: study whether other dual-operation constraints (authoritarian legitimacy through natural-law framing, patriarchal family structures, colonial institutional inheritance) have historically remained covert or eventually required explicit defense when scale increased.',
    'If sustainable: the reading describes a stable structural form. If unsustainable: the dual operation is a transitional state, and either institutional engineering or ideological breakdown becomes inevitable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(covert_operation_sustainability, conceptual, 'Sustainability of covert dual-operation closure').

omega_variable(
    progressive_challenge_structural_disadvantage,
    'Is the structural disadvantage to progressive challengers (inability to name the engineered mechanism) inherent to the dual-operation form, or contingent on specific ideological saturation?',
    'Comparison across reform movements: labor movements that explicitly rejected naturalization framing versus those that accepted market-as-inevitable premises. Measurement of mobilization capacity correlated with ideological framing (naming engineering vs accepting naturalization).',
    'If inherent: reform strategies must first target the ideological naturalization (cognitive reframing) before institutional change becomes possible. If contingent: institutional mechanisms can be challenged directly once coalition awareness increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(progressive_challenge_structural_disadvantage, empirical, 'Whether structural disadvantage is inherent to dual operation').

omega_variable(
    reading_contest_foreclosure_question,
    'Does THIS reading (dual operation) logically foreclose the lapsed_closure_reading, or do they coexist as different framings of the same mechanisms?',
    'Conceptual analysis: lapsed_closure reads market naturalization as sedimented ideology—original construction forgotten. Dual_operation reads the same phenomenon as engineered closure hidden by lapsed ideology. These are compatible if: (a) original construction happened, (b) it was forgotten, but (c) institutional engineers continue the work implicitly. They are incompatible if: one party claims the engineering is currently active, and the other claims it has entirely atrophied (lapsed). Empirical evidence: do state actors consciously defend market structures, or do they follow inherited routines without intention?',
    'If foreclosed: this reading''s core premise (current active engineering) contradicts the lapsed reading''s core premise (engineering has atrophied). If coexisting: both readings remain live—different parties genuinely perceive the market differently.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_foreclosure_question, conceptual, 'Logical relationship between dual_operation and lapsed_closure readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_as_natural_default__dual_operation_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mktnat_theater_t0, market_as_natural_default__dual_operation_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(mktnat_theater_t5, market_as_natural_default__dual_operation_reading, theater_ratio, 5, 0.58).
narrative_ontology:measurement(mktnat_theater_t10, market_as_natural_default__dual_operation_reading, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(mktnat_extract_t0, market_as_natural_default__dual_operation_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(mktnat_extract_t5, market_as_natural_default__dual_operation_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(mktnat_extract_t10, market_as_natural_default__dual_operation_reading, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(mktnat_suppress_t0, market_as_natural_default__dual_operation_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(mktnat_suppress_t5, market_as_natural_default__dual_operation_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(mktnat_suppress_t10, market_as_natural_default__dual_operation_reading, suppression_requirement, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_as_natural_default__dual_operation_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(market_as_natural_default__dual_operation_reading, 0.18).
narrative_ontology:affects_constraint(market_as_natural_default__dual_operation_reading, market_as_natural_default__engineered_closure_reading).
narrative_ontology:affects_constraint(market_as_natural_default__dual_operation_reading, market_as_natural_default__lapsed_closure_reading).
narrative_ontology:affects_constraint(market_as_natural_default__dual_operation_reading, labor_commodification_extraction).
narrative_ontology:affects_constraint(market_as_natural_default__dual_operation_reading, financial_stability_management).
narrative_ontology:affects_constraint(market_as_natural_default__dual_operation_reading, neoliberal_state_form).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a three-way kernel contest over market naturalization. The dual_operation_reading (this file) claims simultaneous engineered closure (institutional) and ideological naturalization (conceptual). The sibling readings—engineered_closure_reading and lapsed_closure_reading—are separate constraint stories with their own ε values and perspectives. All three stories share the kernel 'market_as_natural_default' but instantiate different claims about which layer (engineering or ideology) is currently operative. The dual_operation_reading has ε=0.58 (Tangled Rope); expect sibling readings to have materially different ε values reflecting different dominant mechanisms (engineered_closure at ε≈0.72, lapsed_closure at ε≈0.35). Link all three via network.affects_constraints. The perspectival gap within this single reading is explained by the dual-layer operation; the gap between readings is explained by different empirical claims about which layer dominates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(market_as_natural_default__dual_operation_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
