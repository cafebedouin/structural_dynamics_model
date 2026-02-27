% ============================================================================
% CONSTRAINT STORY: royal_navy_middle_east_withdrawal
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_royal_navy_middle_east_withdrawal, []).

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
 *   constraint_id: royal_navy_middle_east_withdrawal
 *   human_readable: End of Permanent Royal Navy Presence in the Gulf
 *   domain: political/security/geopolitics
 *
 * SUMMARY:
 *   The UK's withdrawal of its last permanent combat ship from the Middle
 *   East in 2026 marks the end of a continuous 46-year naval presence in the
 *   Gulf (1980-2026). This constraint exemplifies how a single structural
 *   phenomenon — the reallocation of military capacity driven by shifting
 *   geopolitical priorities — generates radically different classifications
 *   depending on the observer's structural position. For the UK Ministry of
 *   Defence, the withdrawal solves a coordination problem (reallocating
 *   scarce naval capacity toward Indo-Pacific peer competition with China).
 *   For allied Gulf states, it creates an extraction problem (dependence
 *   shifts from British naval presence to market-based private security and
 *   less reliable regional capacity). For Gulf shipping, it increases
 *   vulnerability and cost (trapped in the constraint with constrained
 *   alternatives). For regional naval coalitions, it is a planned transition
 *   (scaffold with sunset). For the institutional British naval
 *   establishment, it represents a degraded ritual — the permanent presence
 *   became increasingly performative after the Cold War, maintained through
 *   institutional inertia rather than strategic necessity (piton). From
 *   civilizational scope, the withdrawal could be naturalized as inevitable
 *   great power transition, but this risks false naturalization — the
 *   constraint is contingent on budget choices, not immutable geopolitical
 *   law. The extractiveness has increased from 0.28 (early 1980s, when
 *   presence had genuine tactical necessity against Soviet expansion) to 0.52
 *   (2026, when presence is maintained partly through institutional inertia
 *   and political commitment despite competing strategic priorities). The
 *   theater ratio has increased from 0.35 (Cold War functional necessity) to
 *   0.58 (post-Cold War performative maintenance).
 *
 * KEY AGENTS:
 *   - UK Ministry of Defence: Primary beneficiary (institutional/arbitrage) — captures flexibility to reallocate naval capacity to Indo-Pacific; arbitrage option enables strategic pivot
 *   - Gulf Shipping and Commerce: Primary victim (powerless/trapped) — cannot exit exposure to maritime security gaps; bears increased insurance and security costs
 *   - Allied Gulf States (Saudi Arabia, UAE, Bahrain): Secondary victim (moderate/constrained) — lose British naval partnership; constrained in their ability to independently replace capability; forced to reallocate defense resources
 *   - US Regional Coalition: Secondary victim (organized/constrained) — loses British burden-sharing partner; constrained by own commitments; faces increased coordination complexity
 *   - Regional Naval Coalition (Saudi, UAE, India): Transitional beneficiary (organized/constrained) — opportunity to expand own naval role, but constrained by capital costs and capacity maturation timeline
 *   - Private Maritime Security Industry: Beneficiary (institutional/arbitrage) — increased contract opportunities as naval gap opens; arbitrage position as market alternative
 *   - Analytical Observer: Risk of naturalizing contingent institutional choice (analytical/analytical) — temptation to frame as inevitable great power decline
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(royal_navy_middle_east_withdrawal, 0.52).
domain_priors:suppression_score(royal_navy_middle_east_withdrawal, 0.68).
domain_priors:theater_ratio(royal_navy_middle_east_withdrawal, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(royal_navy_middle_east_withdrawal, extractiveness, 0.52).
narrative_ontology:constraint_metric(royal_navy_middle_east_withdrawal, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(royal_navy_middle_east_withdrawal, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(royal_navy_middle_east_withdrawal, tangled_rope).
narrative_ontology:human_readable(royal_navy_middle_east_withdrawal, "End of Permanent Royal Navy Presence in the Gulf").
narrative_ontology:topic_domain(royal_navy_middle_east_withdrawal, "political/security/geopolitics").

domain_priors:requires_active_enforcement(royal_navy_middle_east_withdrawal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(royal_navy_middle_east_withdrawal, uk_defense_budget_reallocation).
narrative_ontology:constraint_beneficiary(royal_navy_middle_east_withdrawal, indo_pacific_strategic_focus).
narrative_ontology:constraint_beneficiary(royal_navy_middle_east_withdrawal, regional_naval_hegemons).
narrative_ontology:constraint_victim(royal_navy_middle_east_withdrawal, british_maritime_interests_gulf).
narrative_ontology:constraint_victim(royal_navy_middle_east_withdrawal, us_regional_coalition_stability).
narrative_ontology:constraint_victim(royal_navy_middle_east_withdrawal, gulf_shipping_security_coordination).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GULF SHIPPING AND COMMERCE (SNARE) — Small merchant vessels, regional shipping lines, and trade infrastructure dependent on stable maritime security cannot exit the constraint. The withdrawal of the dominant naval power leaves them vulnerable to piracy, regional conflicts, and blockade risk. Their extraction increases as security provision shifts to market-based private alternatives (insurance, private security firms) with higher costs. Trapped with no alternatives.
constraint_indexing:constraint_classification(royal_navy_middle_east_withdrawal, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: ALLIED GULF STATES (TANGLED ROPE) — Saudi Arabia, UAE, Bahrain benefit from UK security coordination and intelligence sharing (rope function) but are constrained by dependence on British naval presence as one pillar of regional stability against Iran. The withdrawal forces reallocation of their own naval capacity and increases reliance on US security guarantees. Mixed coordination and asymmetric extraction — they benefit from the historical alliance but bear costs of reconfiguring regional security architecture.
constraint_indexing:constraint_classification(royal_navy_middle_east_withdrawal, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: UK MINISTRY OF DEFENCE (ROPE) — Net beneficiary of the withdrawal. Reallocation of naval capacity to Indo-Pacific enables UK strategic pivot toward China and India. The constraint itself solves a collective action problem: UK budget constraints force choices between regional commitments; the withdrawal coordinates this reallocation. MoD experiences low effective extraction despite the Gulf costs — the arbitrage value of redeploying to Indo-Pacific and reclaiming naval capacity from sustained Gulf presence exceeds the withdrawal costs.
constraint_indexing:constraint_classification(royal_navy_middle_east_withdrawal, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGIONAL NAVAL COALITION (SCAFFOLD) — The constraint has a built-in sunset: as regional navies (Saudi Arabia, UAE, Indian Navy) expand their capacity and coordination mechanisms mature, the British naval gap becomes less critical. This is explicitly framed as transitional — the UK withdrawal is part of a planned shift toward regional states providing their own security. Theater is moderate (0.58) because the transition relies partly on aspirational capacity that has not yet materialized. The coalition is organized but constrained by capital costs and political will.
constraint_indexing:constraint_classification(royal_navy_middle_east_withdrawal, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: BRITISH POSTCOLONIAL NAVAL PRESENCE MYTH (PITON) — The permanent Gulf presence from 1980-2026 became increasingly performative after the Cold War. The constraint's institutional inertia reflected Britain's historical great-power identity more than current strategic necessity. The withdrawal reveals the piton: the permanent presence was largely theatrical maintenance of geopolitical status rather than a functional security mechanism. Theater_ratio (0.58) reflects this degradation — significant portion of the deployment's justification shifted from tactical necessity to symbolic British presence.
constraint_indexing:constraint_classification(royal_navy_middle_east_withdrawal, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: US REGIONAL COALITION STABILITY (TANGLED ROPE) — The US benefits from UK presence as part of stabilizing Gulf coalition and burden-sharing (coordination function). The UK withdrawal increases US force requirements and coordination complexity. The US is constrained by its own strategic commitments and cannot fully exit the Gulf, but the coalition's collective capacity diminishes. The US sees the withdrawal as both a loss of coordination (ally reducing presence) and an extraction mechanism (carrying additional burden). This is the inverse of the UK's arbitrage perspective.
constraint_indexing:constraint_classification(royal_navy_middle_east_withdrawal, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / GEOPOLITICAL INEVITABILITY (MOUNTAIN) — From civilizational scope, some contraction of British imperial-era security commitments appears inevitable as UK relative power declines and focus shifts to peer competition with China. The constraint could be naturalized as an immutable feature of great power transition. However, the structural data shows this is false naturalization: the withdrawal is driven by contingent budget choices, strategic reframing, and political decisions, not by physical or logical necessity. Risk of false summit classification.
constraint_indexing:constraint_classification(royal_navy_middle_east_withdrawal, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(royal_navy_middle_east_withdrawal_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(royal_navy_middle_east_withdrawal, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(royal_navy_middle_east_withdrawal, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(royal_navy_middle_east_withdrawal, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(royal_navy_middle_east_withdrawal, TR),
    TR >= 0.70.

:- end_tests(royal_navy_middle_east_withdrawal_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high and rising. The early permanent presence (1980-1991) had genuine tactical necessity against Soviet expansion and regional conflicts, with extractiveness ~0.28. Post-Cold War, the presence became maintained partly through institutional inertia and symbolic British great-power identity, raising extractiveness through the 1990s-2010s. By 2026, the presence extraction reaches 0.52 — shipping bears higher costs, allied states bear dependence, and the UK bears opportunity costs of capacity not deployed to Indo-Pacific. The withdrawal itself is the constraint: it forces reallocation of security burden to higher-cost alternatives and less reliable regional capacity. Suppression (0.68): High. Allied Gulf states cannot easily exit dependence on maritime stability. Shipping cannot withdraw from the Gulf. Regional states are suppressed by limited capacity to independently provide security. The private security industry emerges to suppress regulatory alternatives (governments cannot easily reassert direct security provision). Theater ratio (0.58): Moderate-high and increasing. The permanent presence increasingly shifted from tactical function to symbolic affirmation of British maritime power and regional engagement. Post-Cold War maintenance reflected institutional inertia — the deployments continued partly because discontinuing them would symbolically affirm British decline. The theater increased as strategic rationales shifted from concrete (Soviet containment, Iran-Iraq War stability) to abstract (maintaining presence, great-power status).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a perspectival gap between beneficiary, victim, and transitional perspectives. The UK MoD sees the withdrawal as solving a coordination problem (arbitrage away from a constrained commitment). Allied Gulf states and shipping see it as an extraction problem (dependence on less capable alternatives). The regional coalition sees it as a transitional scaffold (their growing capacity will fill the gap). The British naval establishment sees it as degraded theater (piton) — institutional performance without underlying function. The US sees it as a loss of coordination (ally reducing presence) and a forced absorption of burden (tangled rope — mixed coordination loss and extraction increase). The analytical observer risks a false mountain classification by naturalizing what is a contingent institutional choice as inevitable great-power decline. The perspectival unity across these contradictions is the constraint_id: it is the structural requirement to reallocate capacity, create alternatives, and absorb costs in the absence of a dominant naval power in the Gulf.
 *
 * DIRECTIONALITY LOGIC:
 *   The UK MoD as institutional beneficiary with arbitrage options experiences low effective extraction (they benefit from freed capacity and strategic flexibility). The constraint's directionality flows FROM constrained actors (shipping, allied states) TOWARD the beneficiary (UK budget relief and strategic reallocation). The US as an organized actor with constrained exit experiences higher effective extraction (loses British partner, forced to absorb additional burden). The private security industry as institutional beneficiary with arbitrage options experiences negative extraction (they profit from the gap). Regional naval coalitions experience asymmetric extraction — they gain opportunity but are constrained by capital and timeline. The analytical observer risks deriving d from a false naturalization (inevitable decline = high d), when the actual structural choice is made by contingent budget and strategy decisions (lower d for those actual decision-makers).
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: The constraint avoids mandatrophy through explicit acknowledgment of both the coordination function (UK/US partnership stability in the Gulf, regional naval coalition-building) and the asymmetric extraction (victims bear increased costs, beneficiaries gain strategic flexibility). The tangled_rope classification is validated by the presence of active enforcement (naval deployments, alliance coordination) combined with both beneficiary groups (UK MoD, private security) and victim groups (shipping, allied states) and the satisfying of both coordination and extraction criteria. The key mandatrophy resolution is recognizing that the constraint is not a pure extraction mechanism disguised as coordination, nor a pure coordination mechanism hidden by extraction language. It genuinely involves both: the UK is solving a real coordination problem (reallocating capacity toward peer competition) while simultaneously imposing asymmetric costs on those dependent on the old arrangement. The scaffold perspective confirms the sunset is real and structural (regional capacity building is a verifiable transition pathway), preventing misclassification as a permanent snare. The piton perspective identifies institutional inertia in the historical presence (not the withdrawal), confirming that the earlier constraint was partly theatrical. The mandatrophy emerges when recognizing that 'permanent presence' itself was the constraint (extractiveness 0.28-0.52 over 46 years), and 'withdrawal' is the mechanism to resolve it — but the resolution creates new constraints (security gap, private substitution) rather than eliminating constraint entirely. This is the typical mandatrophy pattern: solving one tangled rope creates new constraints in adjacent domains.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regional_naval_capacity_gap,
    'Will Saudi, UAE, and Indian naval expansion fill the security gap at equivalent capability and speed following UK withdrawal?',
    'Tracking of regional naval procurement, doctrine development, and inter-naval coordination exercises over 5-10 year horizon; comparison of coverage capacity before and after transition period',
    'If filled within 5 years: scaffold sunset confirmed, constraint transitions to temporary coordination problem. If gap persists >10 years: constraint remains a snare for shipping and a tangled rope for allied states; extraction increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_naval_capacity_gap, empirical, 'Whether regional navies fill the security gap').

omega_variable(
    iran_opportunism_scenario,
    'Does Iran interpret the UK withdrawal as an opening for increased regional naval assertiveness, triggering escalation spiral?',
    'Monitoring Iranian naval activity, Strait of Hormuz incidents, and regional military exercises in 2-5 year window post-withdrawal; cross-reference with intelligence assessments of Iranian strategic calculus',
    'If opportunism occurs: constraint reclassifies to higher-extractiveness snare (shipping bears greater cost); scaffold sunset is delayed by conflict. If Iran maintains status quo: scaffold timeline holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(iran_opportunism_scenario, empirical, 'Whether Iran exploits the withdrawal for regional assertiveness').

omega_variable(
    uk_strategic_reallocation_credibility,
    'Does the UK actually invest the reallocated naval capacity into Indo-Pacific presence, or do budget pressures divert funds elsewhere?',
    'Tracking of naval procurement decisions, deployment schedules, and actual ship days in Indo-Pacific vs Gulf post-withdrawal over 5-year horizon',
    'If reallocated: UK arbitrage benefit is real, piton perspective is correct (capacity freed for meaningful use). If diverted: piton perspective becomes a trap — capacity is released but not used; UK surrenders influence in both regions. Extractiveness interpretation changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(uk_strategic_reallocation_credibility, empirical, 'Whether UK actually redeployable capacity to Indo-Pacific').

omega_variable(
    private_security_substitution_cost,
    'What is the net cost change for shipping security when UK naval presence is replaced by private maritime security contracting and regional alternatives?',
    'Insurance cost analysis, private security firm billing records, total cost of ownership for shipping pre- and post-withdrawal, including increased insurance premiums and security service costs',
    'If private substitution is more expensive: extraction increases for shipping (snare victim bears higher costs). If costs are neutral or lower: constraint transitions to lower-extraction tangled rope. This determines victim impact directly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(private_security_substitution_cost, empirical, 'Net cost change for shipping security alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(royal_navy_middle_east_withdrawal, 1980, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rnmew_tr_t0, royal_navy_middle_east_withdrawal, theater_ratio, 0, 0.35).
narrative_ontology:measurement(rnmew_tr_t15, royal_navy_middle_east_withdrawal, theater_ratio, 15, 0.48).
narrative_ontology:measurement(rnmew_tr_t30, royal_navy_middle_east_withdrawal, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(rnmew_be_t0, royal_navy_middle_east_withdrawal, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(rnmew_be_t15, royal_navy_middle_east_withdrawal, base_extractiveness, 15, 0.42).
narrative_ontology:measurement(rnmew_be_t30, royal_navy_middle_east_withdrawal, base_extractiveness, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(royal_navy_middle_east_withdrawal, global_infrastructure).
narrative_ontology:affects_constraint(royal_navy_middle_east_withdrawal, strait_of_hormuz_chokepoint_stability).
narrative_ontology:affects_constraint(royal_navy_middle_east_withdrawal, gulf_shipping_insurance_market).
narrative_ontology:affects_constraint(royal_navy_middle_east_withdrawal, us_middle_east_security_commitments).
narrative_ontology:affects_constraint(royal_navy_middle_east_withdrawal, indo_pacific_uk_naval_expansion).

% DUAL FORMULATION NOTE:
% The royal_navy_middle_east_withdrawal constraint represents the institutional reallocation decision (extractiveness 0.52) and should be decomposed from the structural consequence constraints it affects. The Strait of Hormuz chokepoint and shipping insurance constraints have their own extractiveness values reflecting physical/market constraints; the withdrawal is a policy constraint that shifts burden between these domains. The affects_constraints edges model the contamination propagation — reducing naval capacity in one region increases extraction pressure in adjacent constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(royal_navy_middle_east_withdrawal, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
