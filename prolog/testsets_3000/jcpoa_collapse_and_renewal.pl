% ============================================================================
% CONSTRAINT STORY: jcpoa_collapse_and_renewal
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jcpoa_collapse_and_renewal, []).

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
 *   constraint_id: jcpoa_collapse_and_renewal
 *   human_readable: JCPOA Collapse and Renewal: Coordination Failure with Asymmetric Extraction
 *   domain: international_relations/nuclear_policy/geopolitics
 *
 * SUMMARY:
 *   The Joint Comprehensive Plan of Action (JCPOA) and its subsequent
 *   collapse and renewal attempts represent a structural constraint that
 *   simultaneously coordinates international nuclear non-proliferation
 *   verification and extracts asymmetrically from Iran through sanctions
 *   architecture and unilateral hegemonic authority. The constraint exhibits
 *   tangled_rope characteristics: both genuine coordination benefits (IAEA
 *   verification, transparency, reduced proliferation risk) and extractive
 *   mechanisms (sanctions conditionality, US unilateral withdrawal authority,
 *   Iranian civilian economy targeting) operate simultaneously. The collapse
 *   in 2018 under US withdrawal and the subsequent renewal attempts in
 *   2021-2024 reveal the underlying asymmetry — one powerful actor (US
 *   executive branch) can unilaterally exit while weaker actors (Iran,
 *   signatory states) cannot. Theater_ratio increased from 0.42 (initial
 *   agreement with genuine verification mechanisms) to 0.73 at peak renewal
 *   negotiations (largely performative 'talks about talks'), then declined
 *   slightly to 0.68 as post-2024 arrangements stabilized into hybrid
 *   structure. Extractiveness increased monotonically from 0.38 to 0.62
 *   during collapse period, reflecting removal of coordination benefits and
 *   pure extraction through sanctions, then stabilized at 0.58 as partial
 *   renewal occurred. The constraint family includes upstream claims about
 *   Iranian nuclear capability (bgs_eigenvector_thermalization analogue —
 *   contested empirical status) and downstream constraints on sanctions
 *   governance and multilateral enforcement.
 *
 * KEY AGENTS:
 *   - Iranian Civilian Economy: Primary victim (powerless/trapped/national) — bears full cost of sanctions and agreement collapse cycles; no exit option from sanctions architecture or unilateral withdrawal decisions
 *   - United States Executive Branch: Primary beneficiary and extractor (powerful/arbitrage/global) — unilateral withdrawal authority and sanctions reposition authority; can arbitrage between agreement compliance and coercion
 *   - European Signatories: Secondary victim (moderate/constrained/continental) — genuine coordination interest in nuclear non-proliferation but structurally dependent on US security umbrella and unable to override US decisions
 *   - IAEA Technical Verification: Coordinating actor (institutional/arbitrage/global) — genuine coordination mechanism with technical authority; experiences constraint as rope (pure coordination function)
 *   - Multilateral Governance Coalition: Organized coalition (organized/constrained/continental) — attempts to scaffold renewal as interim step toward more durable framework; sees sunset logic in current JCPOA iterations
 *   - Nuclear Non-Proliferation Regime: Institutional framework (institutional/constrained/global) — degraded by JCPOA collapse; performs commitment through renewal theater while actual enforcement capacity remains fragmented
 *   - Analytical Observer: Civilizational view (analytical/analytical/global) — sees full structure: genuine coordination need (preventing weaponization) coupled with hegemonic extraction (asymmetric sanctions and withdrawal authority)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jcpoa_collapse_and_renewal, 0.58).
domain_priors:suppression_score(jcpoa_collapse_and_renewal, 0.72).
domain_priors:theater_ratio(jcpoa_collapse_and_renewal, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jcpoa_collapse_and_renewal, extractiveness, 0.58).
narrative_ontology:constraint_metric(jcpoa_collapse_and_renewal, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(jcpoa_collapse_and_renewal, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jcpoa_collapse_and_renewal, tangled_rope).
narrative_ontology:human_readable(jcpoa_collapse_and_renewal, "JCPOA Collapse and Renewal: Coordination Failure with Asymmetric Extraction").
narrative_ontology:topic_domain(jcpoa_collapse_and_renewal, "international_relations/nuclear_policy/geopolitics").

domain_priors:requires_active_enforcement(jcpoa_collapse_and_renewal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jcpoa_collapse_and_renewal, united_states_executive_branch).
narrative_ontology:constraint_beneficiary(jcpoa_collapse_and_renewal, regional_security_competitors).
narrative_ontology:constraint_beneficiary(jcpoa_collapse_and_renewal, sanctions_dependent_economies).
narrative_ontology:constraint_victim(jcpoa_collapse_and_renewal, iran_civilian_economy).
narrative_ontology:constraint_victim(jcpoa_collapse_and_renewal, agreement_signatory_states).
narrative_ontology:constraint_victim(jcpoa_collapse_and_renewal, global_nuclear_governance_framework).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IRANIAN CIVILIAN ECONOMY (SNARE) — Trapped by sanctions architecture with no exit option. Bears full cost of agreement collapse and renewal cycles. Cannot negotiate independently; subject to external power's unilateral withdrawal and reimposition decisions. Maximum experienced extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(jcpoa_collapse_and_renewal, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: EUROPEAN SIGNATORIES (TANGLED ROPE) — Constrained by both strategic dependence on US security umbrella and commitment to multilateral agreements. Experience genuine coordination benefit (nuclear non-proliferation) alongside asymmetric extraction (unable to override US withdrawal decisions, forced to choose between agreement and US relations). Active enforcement required to maintain both commitments.
constraint_indexing:constraint_classification(jcpoa_collapse_and_renewal, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: IAEA TECHNICAL VERIFICATION (ROPE) — Pure coordination mechanism for inspections and compliance monitoring. Benefits from clear verification mandate and international legitimacy. Can arbitrage between signatories through technical authority. No significant extraction experienced — constraint functions as intended coordination tool.
constraint_indexing:constraint_classification(jcpoa_collapse_and_renewal, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: US EXECUTIVE BRANCH (SNARE) — Primary extractor. Can arbitrage between agreement compliance and sanctions reimposition. Experiences agreement as constraining without binding: can withdraw unilaterally, reimpose sanctions, negotiate terms. Extraction mechanism: leverage over signatories through credible withdrawal threat and economic coercion. High chi from powerful position + arbitrage exit + beneficiary status.
constraint_indexing:constraint_classification(jcpoa_collapse_and_renewal, snare,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: MULTILATERAL GOVERNANCE COALITION (SCAFFOLD) — Organized actors (EU, UN Security Council permanent members, regional states) see renewal as temporary structural fix with sunset logic: JCPOA can be renewed only if enforcement capacity increases and unilateral withdrawal consequences are constrained. Current renewal attempts are scaffolding toward more durable nuclear governance framework. Theater_ratio reflects performative 'negotiations' that are actually choreography for domestic audiences.
constraint_indexing:constraint_classification(jcpoa_collapse_and_renewal, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: NUCLEAR NON-PROLIFERATION REGIME (PITON) — Degraded institutional framework. NPT verification mechanisms persist through inertia; JCPOA collapse revealed the regime's inability to prevent weaponization pathways or constrain great-power unilateral withdrawal. Theater_ratio high: continued diplomatic engagement and renewal cycles perform commitment to non-proliferation while actual coercive capacity (sanctions, enforcement) remains fragmented and dependent on hegemonic consensus. The regime maintains legitimacy through ritual despite structural ineffectiveness.
constraint_indexing:constraint_classification(jcpoa_collapse_and_renewal, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — The JCPOA simultaneously coordinates nuclear non-proliferation verification (genuine coordination function generating benefits for all signatories) and extracts asymmetrically from Iran through sanctions conditionality and unilateral withdrawal vulnerability. The constraint is not a mountain (not inevitable) but also not pure extraction (coordination is real). The analytical view sees the full structure: genuine coordination need coupled with hegemonic extraction mechanism exploiting that coordination dependence.
constraint_indexing:constraint_classification(jcpoa_collapse_and_renewal, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jcpoa_collapse_and_renewal_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(jcpoa_collapse_and_renewal, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(jcpoa_collapse_and_renewal, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(jcpoa_collapse_and_renewal, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(jcpoa_collapse_and_renewal, TR),
    TR >= 0.70.

:- end_tests(jcpoa_collapse_and_renewal_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts substantially from Iran through sanctions that target civilian economy, but not at snare-level because (a) genuine nuclear non-proliferation verification benefits exist (coordination function is real), (b) other signatories gain benefit from reduced proliferation risk, and (c) renewal attempts have restored some verification access. The extractiveness is concentrated on Iran and diffused across signatories — not pure extraction from all parties. Suppression (0.72): High. Extremely limited alternatives to JCPOA framework for achieving nuclear verification. Iran has no exit from sanctions except through compliance. Signatories cannot exit without abandoning non-proliferation goals. US can exit unilaterally but faces legitimacy costs — suppression is structural (no good alternatives exist). Theater_ratio (0.68): Moderate-high. JCPOA verification mechanisms (IAEA inspections, declared nuclear facilities) are genuine coordination, but surrounding negotiation theater has increased significantly. Renewal attempts (2021-2024) are substantially choreography for domestic audiences in multiple countries — performance of commitment to non-proliferation while underlying extraction mechanisms (US unilateral authority, sanctions architecture) remain unchanged. Claimed type (tangled_rope): Required because both genuine coordination (nuclear non-proliferation verification generating benefits for all) and asymmetric extraction (sanctions conditionality, unilateral withdrawal authority concentrated on US/against Iran) operate simultaneously.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how a single structural phenomenon appears as radically different types from different positions. For Iran (powerless/trapped), the constraint is snare — pure extraction via sanctions with minimal coordination benefit perceived at biographical horizon. For US (powerful/arbitrage), the constraint is rope — coordination mechanism (non-proliferation) with net beneficial leverage. For European signatories (moderate/constrained), the constraint is tangled_rope — genuine coordination benefit (security from proliferation) mixed with asymmetric extraction (cannot override US decisions). For IAEA (institutional/arbitrage), the constraint is rope — pure technical coordination with no extraction. For the multilateral coalition (organized/constrained), the constraint is scaffold — temporary coordination with sunset logic (renewal as interim toward better framework). For the NPT regime (institutional/constrained/civilizational), the constraint is piton — degraded framework performing commitment through renewal theater while actual enforcement capacity remains fragmented. For the analytical observer (civilizational), the constraint is tangled_rope — both coordination and extraction are structurally real, neither can be dismissed. The perspectival gap reveals that 'what is the JCPOA' depends on where you are positioned in the extraction flow.
 *
 * DIRECTIONALITY LOGIC:
 *   Iranian perspective (powerless/trapped): Iran appears as victim in base_properties; structurally dependent on agreement compliance for sanctions relief; no independent exit option; trapped exit options implies maximum d and high f(d) yielding high chi — Iran experiences maximum effective extraction. US perspective (powerful/arbitrage): US appears as beneficiary; unilateral withdrawal authority gives arbitrage option (can exit at any time); beneficiary status implies low d and negative f(d) — US experiences negative extraction (extraction runs toward them). European perspective (moderate/constrained): Europeans appear as both beneficiary (non-proliferation coordination) and victim (dependent on US security umbrella); constrained exit options (can exit agreement but at high cost to security posture) imply moderate d and moderate f(d). IAEA perspective (institutional/arbitrage): Pure technical coordination function with arbitrage authority between signatories; no beneficiary/victim distinction; standard institutional canonical d yields moderate f(d) — coordination mechanism experienced as rope. Scope modifier applied: national (Iran) σ=0.8 dampens chi; global (US, IAEA) σ=1.2 amplifies chi — increases asymmetry between Iranian and global actor perspectives. Directionality derivation confirms tangled_rope: asymmetric extraction exists (higher chi on Iran) alongside genuine coordination (IAEA verification, non-proliferation benefits).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint resolves the 'is it coordination or extraction' mandatrophy by demonstrating that coordination and extraction are not mutually exclusive at high extractiveness (ε=0.58). The JCPOA genuinely solves a coordination problem (nuclear non-proliferation verification) that all parties benefit from in principle. But it solves that coordination problem through an extractive mechanism (sanctions conditionality, asymmetric authority) that distributes costs and benefits asymmetrically. The constraint cannot be classified as pure rope (coordination only) because the asymmetric withdrawal authority and sanctions targeting are not coordination — they are coercion. The constraint cannot be classified as pure snare (extraction only) because the nuclear non-proliferation verification mechanism is genuine coordination generating real benefits. Therefore tangled_rope is the correct classification: both the coordination function (IAEA verification, transparency, proliferation reduction) and the extraction mechanism (sanctions, unilateral authority) operate simultaneously. The mandatrophy is resolved by recognizing that this is structurally correct — many international agreements coordinate and extract simultaneously. The analytical observer's task is to measure where on the 0.40-0.90 extraction range the constraint sits (0.58 in this case) and to identify which actors benefit from the coordination and which actors bear the extraction costs (clear divergence: US + signatories benefit from coordination, Iran bears extraction costs).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unilateral_withdrawal_threshold,
    'What level of Iranian nuclear activity triggers justified withdrawal vs what level is domestic political theater masquerading as nuclear concern?',
    'Comparative analysis of declared thresholds vs revealed preferences: if withdrawal occurs at activity level below stated threshold, or if threshold statements change between administrations independent of Iranian behavior, the threshold is political theater not technical limit',
    'If thresholds are genuine: JCPOA is constraining agent extractiveness (lower chi). If thresholds are theater: JCPOA extraction is nearly pure (chi approaches snare level for Iran). Changes classification from tangled_rope to snare for US perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unilateral_withdrawal_threshold, empirical, 'Whether withdrawal thresholds are technical or political').

omega_variable(
    european_autonomy_capacity,
    'Can European signatories maintain the agreement and sanctions relief apparatus independent of US participation, or are they structurally dependent on US enforcement capacity?',
    'Test case: if US reimposition occurs, do European states have independent banking, shipping, and energy infrastructure to support continued Iranian trade? Historical data on INSTEX effectiveness, secondary sanctions circumvention, corporate compliance patterns.',
    'If independent capacity exists: European constraint is rope (genuine coordination benefit despite US absence). If dependent: European constraint is tangled_rope or snare (coordinated dependence on external enforcer). Changes classification for European perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(european_autonomy_capacity, empirical, 'European independence from US sanctions enforcement').

omega_variable(
    renewal_commitment_depth,
    'Is renewed JCPOA commitment genuine structural change or performative theater for domestic audiences while underlying extraction mechanisms persist?',
    'Mechanism design test: does renewal include (a) constraints on executive withdrawal authority, (b) irreversible sanctions relief commitment, (c) binding dispute resolution? Absence of any suggests theater masquerading as renewal.',
    'If genuine structural change: scaffold perspective confirmed, sunset logic is real. If theater: scaffold is aspirational piton (degraded institution performing renewal). Changes theater_ratio and classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(renewal_commitment_depth, conceptual, 'Whether renewal involves genuine institutional change').

omega_variable(
    iran_identity_locked_exit,
    'Does Iran''s identity commitment to non-alignment block exit options that would be structurally available (alignment with US security apparatus) at the biographical time horizon?',
    'Counterfactual analysis: if Iran abandoned non-alignment identity commitment, would exit barriers dissolve? Evidence: prior non-aligned states that aligned (USSR neighbors during Cold War transitions, Gulf monarchies). If identity shift alone would enable exit, classify exit as identity_locked not trapped.',
    'If identity_locked: Iran experiences moderate d and chi (constrained by identity, not purely structural barriers). If trapped: Iran experiences maximum d and chi (structural barriers insurmountable regardless of identity). Changes f(d) computation and chi for Iranian perspectives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(iran_identity_locked_exit, conceptual, 'Whether Iran''s non-alignment identity blocks structural exit options').

omega_variable(
    sanctions_mechanism_effectiveness,
    'Do sanctions actually constrain weaponization capability or do they primarily constrain civilian economy, with weaponization pathways remaining accessible through black market and state-sector channels?',
    'Technical analysis: nuclear program advancement rate under sanctions vs without; dual-use technology acquisition patterns; state-sector procurement effectiveness. If weaponization timeline unchanged by sanctions, suppression is primarily civilian, not technical.',
    'If sanctions effective against weaponization: extraction targets threat actor (moderate suppression narrative). If sanctions effective only against civilians: extraction is pure (high suppression against powerless, minimal impact on military capacity). Changes suppression interpretation and victim classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sanctions_mechanism_effectiveness, empirical, 'Whether sanctions constrain weaponization or civilian economy').

omega_variable(
    hegemon_vs_enforcer_ambiguity,
    'Is US position as unilateral withdrawal authority a structural feature of great-power politics (inevitable given military capacity) or a contingent institutional design choice (could have imposed withdrawal constraints via treaty modification)?',
    'Comparative analysis: NPT, other multilateral agreements. Do other frameworks impose binding constraints on great powers? If yes, US JCPOA withdrawal authority was choice not inevitability. If no, hegemon authority is structural.',
    'If choice: constraint is extractive design (extraction chi is decision not nature). If structural: constraint is more rope-like (asymmetry is natural given power distribution). Changes mandatrophy interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hegemon_vs_enforcer_ambiguity, conceptual, 'Whether hegemonic withdrawal authority is structural or chosen').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jcpoa_collapse_and_renewal, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcpoa_tr_t0, jcpoa_collapse_and_renewal, theater_ratio, 0, 0.42).
narrative_ontology:measurement(jcpoa_tr_t2, jcpoa_collapse_and_renewal, theater_ratio, 2, 0.55).
narrative_ontology:measurement(jcpoa_tr_t4, jcpoa_collapse_and_renewal, theater_ratio, 4, 0.68).
narrative_ontology:measurement(jcpoa_tr_t6, jcpoa_collapse_and_renewal, theater_ratio, 6, 0.73).
narrative_ontology:measurement(jcpoa_tr_t8, jcpoa_collapse_and_renewal, theater_ratio, 8, 0.68).

% Extraction over time
narrative_ontology:measurement(jcpoa_be_t0, jcpoa_collapse_and_renewal, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(jcpoa_be_t2, jcpoa_collapse_and_renewal, base_extractiveness, 2, 0.52).
narrative_ontology:measurement(jcpoa_be_t4, jcpoa_collapse_and_renewal, base_extractiveness, 4, 0.62).
narrative_ontology:measurement(jcpoa_be_t6, jcpoa_collapse_and_renewal, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(jcpoa_be_t8, jcpoa_collapse_and_renewal, base_extractiveness, 8, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jcpoa_collapse_and_renewal, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(jcpoa_collapse_and_renewal, 0.18).
narrative_ontology:affects_constraint(jcpoa_collapse_and_renewal, iranian_nuclear_weaponization_timeline).
narrative_ontology:affects_constraint(jcpoa_collapse_and_renewal, us_hegemonic_sanctions_authority).
narrative_ontology:affects_constraint(jcpoa_collapse_and_renewal, multilateral_governance_framework_degradation).

% DUAL FORMULATION NOTE:
% The JCPOA collapse and renewal is downstream of claims about Iranian nuclear weapons intentions (which have contested empirical status) and upstream of broader constraints on US hegemonic authority and multilateral governance framework capacity. The extractiveness value (0.58) reflects the agreement's position as mixed coordination-extraction; upstream empirical claims about Iranian intentions have higher uncertainty (omegas); downstream governance constraints reflect JCPOA's failure to establish irreversible institutional arrangements.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jcpoa_collapse_and_renewal, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
