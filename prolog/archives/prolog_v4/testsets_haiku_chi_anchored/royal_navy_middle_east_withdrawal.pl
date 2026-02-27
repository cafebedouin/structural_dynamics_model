% ============================================================================
% CONSTRAINT STORY: royal_navy_middle_east_withdrawal
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
 *   domain: political/security/maritime
 *
 * SUMMARY:
 *   The withdrawal of the Royal Navy's last permanent ship from the Middle
 *   East (2024) ends 46 years of continuous British naval presence in the
 *   Gulf — a cornerstone of post-imperial strategy. The constraint operates
 *   as a snare for Gulf merchant shipping and allied states (UAE, Saudi
 *   Arabia, Bahrain, Oman), a piton for Royal Navy institutional structure,
 *   and a tangled rope for UK central government and regional competitors.
 *   The withdrawal is driven by UK budget constraints and strategic
 *   reorientation toward the Pacific ('Indo-Pacific Tilt', AUKUS), but
 *   extracts security burden from Gulf allies who lose the deterrent presence
 *   without gaining compensation. Gulf allied states cannot exit the regional
 *   security architecture; they are trapped within structural dependency on
 *   Western guarantees while simultaneously losing concrete force presence.
 *   The constraint exhibits rising theater ratio (0.35→0.64 from 1980-2024),
 *   indicating that the operational justification for presence eroded faster
 *   than institutional commitment to it. The Royal Navy's institutional
 *   identity was constructed around the Gulf presence; withdrawal forces
 *   confrontation with its own decline and reorientation.
 *
 * KEY AGENTS:
 *   - Gulf Merchant Shipping (powerless/trapped): Cannot relocate shipping; dependent on security of Strait of Hormuz; bears rising insurance and protection costs
 *   - Gulf Allied States: UAE, Bahrain, Saudi Arabia, Oman (moderate-to-powerful/trapped): Structurally dependent on Western security guarantees; unable to exit or unilaterally rebalance relationships
 *   - UK Central Government (powerful/mobile): Selectively reallocates strategic priorities; extracts flexibility by reorienting to Pacific while abandoning Gulf commitment
 *   - Royal Navy Institutional Structure (institutional/constrained): Maintains Cold War doctrine while losing budget to sustain it; theatre-heavy presence persists through inertia
 *   - Regional Competitors (institutional/arbitrage): Iran, China, Russia benefit from UK withdrawal and acquire strategic space
 *   - Royal Navy Personnel (powerless/trapped): Deployed personnel face operational strain without strategic clarity; extraction manifests as risk without proportionate mission
 *   - Maritime Commons (abstract victim): Freedom of navigation becomes contingent on alternative security arrangements; extraction occurs through governance vacuum
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(royal_navy_middle_east_withdrawal, 0.58).
domain_priors:suppression_score(royal_navy_middle_east_withdrawal, 0.68).
domain_priors:theater_ratio(royal_navy_middle_east_withdrawal, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(royal_navy_middle_east_withdrawal, extractiveness, 0.58).
narrative_ontology:constraint_metric(royal_navy_middle_east_withdrawal, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(royal_navy_middle_east_withdrawal, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(royal_navy_middle_east_withdrawal, snare).
narrative_ontology:human_readable(royal_navy_middle_east_withdrawal, "End of Permanent Royal Navy Presence in the Gulf").
narrative_ontology:topic_domain(royal_navy_middle_east_withdrawal, "political/security/maritime").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(royal_navy_middle_east_withdrawal, uk_central_government).
narrative_ontology:constraint_beneficiary(royal_navy_middle_east_withdrawal, regional_competitor_states).
narrative_ontology:constraint_victim(royal_navy_middle_east_withdrawal, gulf_merchant_shipping).
narrative_ontology:constraint_victim(royal_navy_middle_east_withdrawal, gulf_allied_states).
narrative_ontology:constraint_victim(royal_navy_middle_east_withdrawal, uk_naval_personnel).
narrative_ontology:constraint_victim(royal_navy_middle_east_withdrawal, maritime_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GULF MERCHANT SHIPPING (SNARE) — Dependent on freedom of navigation in critical chokepoint (Strait of Hormuz); cannot exit or relocate shipping routes. Faces escalating protection costs, insurance premiums, and piracy risk without deterrent presence. d≈0.92, f(d)≈1.38, σ=1.1 → χ≈0.88.
constraint_indexing:constraint_classification(royal_navy_middle_east_withdrawal, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: GULF ALLIED STATES (SNARE) — Trapped within regional security architecture dependent on Western naval presence; cannot unilaterally abandon alliance without exposure to Iranian coercion or Chinese/Russian encroachment. Suffer extraction of security dependency and reduced diplomatic autonomy. d≈0.88, f(d)≈1.32, σ=1.1 → χ≈0.83.
constraint_indexing:constraint_classification(royal_navy_middle_east_withdrawal, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: ROYAL NAVY INSTITUTIONAL STRUCTURE (PITON) — Permanent Gulf presence was a structural cornerstone of naval identity and doctrine for 46 years; withdrawal is forced by budget constraints, not by functional obsolescence. The institution maintains Cold War-era force posture assumptions while losing the resources to sustain them. Theater ratio = 0.64 reflects that much Gulf deployment rhetoric (freedom of navigation patrols, port visits, exercises) was symbolic assertion of presence rather than core operational necessity. d≈0.55, f(d)≈0.75, σ=1.2 → χ≈0.50.
constraint_indexing:constraint_classification(royal_navy_middle_east_withdrawal, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: UK CENTRAL GOVERNMENT / STRATEGIC ACTOR (TANGLED ROPE) — Withdrawal is forced by domestic budget pressure but extracts strategic flexibility: UK reorients to Pacific theatre, avoids entanglement in localized Gulf conflicts, reduces sunk costs. Government experiences this as rational reallocation, not extraction. However, the constraint extracts from allies who lose security guarantees while UK retains diplomatic influence. d≈0.35, f(d)≈0.35, σ=1.2 → χ≈0.24.
constraint_indexing:constraint_classification(royal_navy_middle_east_withdrawal, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: REGIONAL COMPETITOR STATES / INSTITUTIONAL (TANGLED ROPE) — Withdrawal benefits Iran and Chinese/Russian strategic positioning; these actors gain coordinated benefit (vacuum-filling) but also face extraction through expanded security commitments and escalation risk. d≈0.42, f(d)≈0.42, σ=1.1 → χ≈0.32.
constraint_indexing:constraint_classification(royal_navy_middle_east_withdrawal, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ROYAL NAVY PERSONNEL (SNARE) — Junior ranks deployed to Gulf face operational strain without clear strategic justification as presence becomes token. Extraction manifests as risk without proportionate mission clarity; personnel trapped in institutional deployment schedule regardless of strategic coherence. d≈0.85, f(d)≈1.25, σ=1.2 → χ≈0.82.
constraint_indexing:constraint_classification(royal_navy_middle_east_withdrawal, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN VIEW) — From civilizational scope, the withdrawal appears inevitable: empires cannot maintain permanent forward military presence after relative economic decline. This is read as a natural law of geopolitics (structural realism). However, base metrics (ε=0.58, suppression=0.68, theater=0.64) contradict mountain classification. The 'natural law' framing naturalizes what is a contingent policy choice (UK domestic austerity, strategic reorientation) rather than an immutable constraint.
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
    constraint_indexing:constraint_classification(royal_navy_middle_east_withdrawal, TypeOther, context(agent_power(institutional), _, _, _)),
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
 *   Extractiveness (0.58): Moderately high. The withdrawal extracts from Gulf allies through loss of security guarantee without compensation. UK extracts strategic flexibility (reorientation to Pacific) while externalizing security costs onto allies. The extraction is not maximal (not 0.70+) because alternative mechanisms (coalition patrols, Gulf state capacity building, Chinese/Russian presence) partially mitigate the vacuum. Suppression (0.68): High. Gulf allies are suppressed from articulating alternative arrangements — strategic dependency on Western relationship constrains their diplomatic freedom. UK suppression of internal budget debates about competing commitments is also high. Maritime commerce faces suppression through reduced visibility of threat environment post-withdrawal. Theater ratio (0.64): Moderately high and rising. The Gulf presence increasingly became symbolic assertion of British status rather than core operational necessity. As UK strategic focus shifted to Pacific, Gulf operations became theater — port visits, regional exercises, diplomatic presence — performed to maintain institutional identity. The theater ratio rising from 0.35 (1980, genuine Cold War deterrent) to 0.64 (2024, token presence) reflects Goodhart drift: institutional commitment to 'presence' replaced functional justification for 'deterrence.' Claimed type (Snare): The structural relationship is extractive. UK is extracting from Gulf allies by withdrawing security commitment without building alternatives. Gulf allies are trapped within regional architecture that assumes Western presence. Suppression prevents them from openly demanding compensation or renegotiating relationships.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates three distinct classification patterns across power levels. Powerless agents (merchant shipping, naval personnel, abstract maritime commons) perceive snare: they are trapped and cannot exit. Institutional actors (UK government, Royal Navy, regional competitors) perceive mixed snare-tangled rope or piton: for UK, the extraction is mixed (they gain strategic flexibility while allies lose security). For regional competitors, the constraint is mixed (they gain strategic advantage but face escalation risk). The analytical observer risks perceiving a mountain (inevitable decline of imperial power, natural law of geopolitics) but the base metrics reveal this as a false summit: the withdrawal is a policy choice driven by UK budget austerity and strategic reorientation, not an immutable law. The gap between the snare experience (powerless agents trapped) and the rope-or-mountain experience (powerful agents, analytical observer) reveals how institutional power determines whether a constraint feels extractive or inevitable.
 *
 * DIRECTIONALITY LOGIC:
 *   Gulf merchant shipping: Victim + trapped → d≈0.92, f(d)≈1.38. Cannot exit; bears full extraction cost. Gulf allied states: Victim + trapped → d≈0.88, f(d)≈1.32. Structurally dependent; trapped within regional security dependency. UK central government: Beneficiary + mobile → d≈0.35, f(d)≈0.35. Extracts strategic flexibility; can reorient to Pacific. Regional competitors: Beneficiary (strategically) + arbitrage → d≈0.42, f(d)≈0.42. Gain strategic space but face escalation risk. Royal Navy personnel: Victim + trapped → d≈0.85, f(d)≈1.25. Deployed without clear mission; cannot exit deployment schedule. Royal Navy institution: Victim + constrained → d≈0.55, f(d)≈0.75. Institutional identity threatened; cannot unilaterally exit piton state. Maritime commons: Abstract victim + trapped → d≈0.90, f(d)≈1.36. Governance vacuum; no agent advocates for commons.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint avoids mandatrophy through perspectival differentiation. UK central government perceives the withdrawal as rational reallocation (tangled rope: benefits from flexibility while allies bear costs). Gulf allies perceive extraction (snare: trapped, no compensation). Royal Navy institution perceives institutional decline (piton: theater-heavy presence maintained through inertia, not function). The same withdrawal event classifies differently depending on the observer's structural relationship. Mandatrophy would arise if a single 'neutral' perspective tried to classify the withdrawal as uniformly rational (rope/scaffold) or uniformly coercive (snare/mountain). The framework avoids this by requiring perspectival specificity: UK sees rope/tangled rope; Gulf allies see snare; Royal Navy sees piton; analytical observer must acknowledge the perspectival gap rather than claiming a unified classification. The high extractiveness (0.58) and suppression (0.68) confirm snare classification from the perspective of powerless agents (Gulf allies, merchant shipping); the presence of alternative mechanisms (coalition patrols, regional capacity building) prevents pure snare classification from emerging as universal. The mediation is perspectival, not metric.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gulf_deterrence_necessity,
    'Is permanent Royal Navy presence in the Gulf functionally necessary for deterring Iranian coercion, or is its primary role symbolic assertion of British status?',
    'Comparative security analysis: incidents of Iranian coercion before/after withdrawal; insurance cost trends for Gulf shipping; frequency and severity of maritime incidents in Strait of Hormuz; behavioral response patterns of Iranian Islamic Revolutionary Guard Corps',
    'If functionally necessary: withdrawal is extraction from Gulf allies (snare classification confirmed). If primarily symbolic: withdrawal is rationalization of budget constraints, and the ''snare'' framing applies mainly to allies'' perception, not to objective vulnerability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gulf_deterrence_necessity, empirical, 'Whether permanent presence provides material deterrence or symbolic assertion').

omega_variable(
    allied_state_exit_capacity,
    'Can Gulf allied states (UAE, Saudi Arabia, Bahrain) develop indigenous naval capacity or form non-Western security partnerships to reduce dependency extraction?',
    'Tracking of Gulf naval modernization programs; analysis of China-Gulf military relationships; assessment of Gulf Cooperation Council institutional capacity; monitoring of Saudi/UAE partnerships with France, Italy, India',
    'If exit capacity emerges: allied states transition from trapped to constrained or mobile exit option; snare classification weakens to tangled_rope. If capacity remains asymmetric: snare classification persists; Gulf states remain structurally dependent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(allied_state_exit_capacity, empirical, 'Whether Gulf allies can develop autonomous security capacity').

omega_variable(
    uk_strategic_reorientation_credibility,
    'Is UK''s stated pivot to Pacific theatre (AUKUS, ''Indo-Pacific Tilt'') a genuine strategic reallocation or rhetorical repositioning to rationalize budget cuts?',
    'Analysis of UK defense spending allocation trends; tracking of carrier strike group deployment patterns; assessment of UK institutional capacity for Pacific operations; comparison of stated strategy against resource commitments',
    'If genuine reorientation: UK is extracting strategic flexibility (tangled_rope beneficiary perspective valid). If rhetorical: UK is abandoning commitments without building alternatives, increasing snare severity for allies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(uk_strategic_reorientation_credibility, empirical, 'Whether UK pivot to Pacific represents genuine or rhetorical reorientation').

omega_variable(
    maritime_commons_governance_vacuum,
    'Will the withdrawal create a functional governance vacuum in maritime security, or will alternative mechanisms (international naval coalitions, flag-state enforcement, commercial security) maintain order without permanent presence?',
    'Monitoring of maritime incident rates post-withdrawal; tracking of coalition naval activity in Gulf; assessment of private maritime security market expansion; evaluation of flag-state capacity enforcement',
    'If vacuum persists: extraction from maritime commons increases (snare deepens). If alternatives emerge: extraction shifts from UK withdrawal to alternative mechanisms, but snare classification may weaken.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maritime_commons_governance_vacuum, empirical, 'Whether governance vacuum emerges or alternatives sustain maritime order').

omega_variable(
    institutional_inertia_vs_strategic_choice,
    'Is the Royal Navy''s 46-year Gulf presence a vestigial imperial remnant (piton), a genuine strategic necessity (snare for allies), or a rational forward-deployed deterrent (rope)?',
    'Institutional history analysis; archival review of naval planning documents; interviews with strategic planners; tracking of cost-benefit analyses conducted by UK Ministry of Defense; comparison of stated justifications against actual deployment patterns',
    'If vestigial (piton): withdrawal confirms long-standing institutional decay; extraction from allies reflects inertia rather than strategy. If strategic necessity: withdrawal harms allies and validates snare classification. If rational deterrent: alternatives must exist to prevent snare deepening.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_inertia_vs_strategic_choice, conceptual, 'Whether presence is vestigial institution or strategic necessity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(royal_navy_middle_east_withdrawal, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rnmew_tr_t1980, royal_navy_middle_east_withdrawal, theater_ratio, 1980, 0.35).
narrative_ontology:measurement(rnmew_tr_t2000, royal_navy_middle_east_withdrawal, theater_ratio, 2000, 0.5).
narrative_ontology:measurement(rnmew_tr_t2015, royal_navy_middle_east_withdrawal, theater_ratio, 2015, 0.62).
narrative_ontology:measurement(rnmew_tr_t2024, royal_navy_middle_east_withdrawal, theater_ratio, 2024, 0.64).

% Extraction over time
narrative_ontology:measurement(rnmew_be_t1980, royal_navy_middle_east_withdrawal, base_extractiveness, 1980, 0.3).
narrative_ontology:measurement(rnmew_be_t2000, royal_navy_middle_east_withdrawal, base_extractiveness, 2000, 0.42).
narrative_ontology:measurement(rnmew_be_t2015, royal_navy_middle_east_withdrawal, base_extractiveness, 2015, 0.52).
narrative_ontology:measurement(rnmew_be_t2024, royal_navy_middle_east_withdrawal, base_extractiveness, 2024, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(royal_navy_middle_east_withdrawal, enforcement_mechanism).
narrative_ontology:affects_constraint(royal_navy_middle_east_withdrawal, strait_of_hormuz_chokepoint).
narrative_ontology:affects_constraint(royal_navy_middle_east_withdrawal, gulf_cooperation_council_security).
narrative_ontology:affects_constraint(royal_navy_middle_east_withdrawal, chinese_belt_and_road_maritime_presence).
narrative_ontology:affects_constraint(royal_navy_middle_east_withdrawal, uk_aukus_commitment).

% DUAL FORMULATION NOTE:
% The withdrawal is a structural shift affecting multiple downstream security constraints. The immediate constraint (loss of Royal Navy presence) operates at ε=0.58; upstream constraints (UK budget austerity, strategic reorientation policy) have different ε values and should be decomposed into separate stories if analyzed from first principles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(royal_navy_middle_east_withdrawal, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
