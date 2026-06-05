% ============================================================================
% CONSTRAINT STORY: sotu_1972_nixon_burden_sharing_doctrine
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1972_nixon_burden_sharing_doctrine, []).

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
 *   constraint_id: sotu_1972_nixon_burden_sharing_doctrine
 *   human_readable: U.S. Burden-Sharing Doctrine (Nixon Doctrine / Guam Doctrine, 1972)
 *   domain: foreign_policy/military_strategy
 *
 * SUMMARY:
 *   The Nixon Doctrine (1972, formally articulated in the Guam speech)
 *   represents a structural shift in U.S. military commitment from unilateral
 *   intervention to a three-tier hierarchy: (1) direct defense where treaty
 *   or vital interests apply; (2) capacity-building assistance for allied
 *   nations; (3) non-intervention elsewhere. This constraint exhibits the
 *   full range of DR classification types depending on the observer's
 *   structural position. Treaty-bound allies see genuine coordination
 *   alongside asymmetric extraction (Tangled Rope). Non-aligned nations face
 *   maximum extraction through exclusion (Snare). The military-industrial
 *   complex sees pure coordination (Rope) — sustained procurement demand,
 *   technology licensing, training missions. The doctrine's gatekeeper
 *   function (requiring interest or treaty declaration for intervention)
 *   creates institutional lock-in through treaty infrastructure while
 *   retaining discretion in 'vital interests' application. The theater ratio
 *   has increased from 0.48 (early implementation, genuine reduction in
 *   discretionary intervention) to 0.62 (mature phase, 'vital interests'
 *   interpretation becomes performative). The underlying extractiveness has
 *   risen from 0.35 to 0.52 as the doctrine settled into institutional
 *   practice: initial constraint on interventionism has become a mechanism
 *   for channeling military resources toward preferred allies and maintaining
 *   geopolitical control through treaty gatekeeping.
 *
 * KEY AGENTS:
 *   - United States Government (institutional/arbitrage) — primary architect, retains discretion in vital-interest definition and treaty commitment application
 *   - Allied Nations within Treaty Framework (moderate/constrained) — benefit from capacity-building and security guarantees; constrained by dependence on U.S. commitment and procurement restrictions
 *   - Non-Aligned / Non-Treaty Nations (powerless/trapped) — excluded from U.S. security commitment, face heightened vulnerability to regional powers or must develop capabilities in isolation
 *   - U.S. Military-Industrial Complex (institutional/arbitrage) — benefits from sustained demand for defense exports, technology licensing, military training missions, research contracts
 *   - Soviet Union / Competing Geopolitical Powers (organized/constrained) — constrained by predictable U.S. commitment boundaries but locked into regional competition with U.S.-backed allies
 *   - Formal Alliance Infrastructure (institutional/arbitrage) — NATO, SEATO, bilateral treaties become gatekeeping mechanism; institutional preservation increases theater ratio over time
 *   - Allied Capability-Building Programs (organized/constrained) — sunset clause embedded in doctrine; as allied militaries mature, doctrine's rationale weakens
 *   - Analytical Observer (analytical/analytical) — risks naturalizing contingent policy choice (burden-sharing) as structural inevitability (U.S. military limits)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1972_nixon_burden_sharing_doctrine, 0.52).
domain_priors:suppression_score(sotu_1972_nixon_burden_sharing_doctrine, 0.48).
domain_priors:theater_ratio(sotu_1972_nixon_burden_sharing_doctrine, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1972_nixon_burden_sharing_doctrine, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_1972_nixon_burden_sharing_doctrine, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(sotu_1972_nixon_burden_sharing_doctrine, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1972_nixon_burden_sharing_doctrine, tangled_rope).
narrative_ontology:human_readable(sotu_1972_nixon_burden_sharing_doctrine, "U.S. Burden-Sharing Doctrine (Nixon Doctrine / Guam Doctrine, 1972)").
narrative_ontology:topic_domain(sotu_1972_nixon_burden_sharing_doctrine, "foreign_policy/military_strategy").

domain_priors:requires_active_enforcement(sotu_1972_nixon_burden_sharing_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1972_nixon_burden_sharing_doctrine, allied_nations_security_autonomy).
narrative_ontology:constraint_beneficiary(sotu_1972_nixon_burden_sharing_doctrine, u_s_military_industrial_complex).
narrative_ontology:constraint_beneficiary(sotu_1972_nixon_burden_sharing_doctrine, u_s_strategic_positioning).
narrative_ontology:constraint_victim(sotu_1972_nixon_burden_sharing_doctrine, non_aligned_nations).
narrative_ontology:constraint_victim(sotu_1972_nixon_burden_sharing_doctrine, u_s_military_personnel_operational_risk).
narrative_ontology:constraint_victim(sotu_1972_nixon_burden_sharing_doctrine, regional_proxy_conflicts).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: REGIONAL NATIONS OUTSIDE TREATY FRAMEWORK (SNARE) — Nations not within formal U.S. alliance structures (SEATO, NATO, bilateral treaties) face the doctrine's full extraction mechanism. When the doctrine eliminates unilateral intervention capacity, these nations become dependent on regional powers or must develop capabilities alone. They are trapped: cannot exit great-power politics, cannot secure U.S. commitment without treaty or vital-interest declaration (which is gatekept by U.S. preference), face extraction through reduced security options and heightened vulnerability to stronger regional actors.
constraint_indexing:constraint_classification(sotu_1972_nixon_burden_sharing_doctrine, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ALLIED NATIONS WITHIN TREATY FRAMEWORK (TANGLED ROPE) — Treaty-bound allies (South Korea, Japan, NATO members, Philippines) experience genuine coordination (capacity-building assistance, security partnerships, technology transfer) alongside asymmetric extraction. The coordination function is real: allies develop indigenous capabilities reducing dependency. The extraction is real: security assistance comes with political conditionality, arms purchase constraints tied to U.S. procurement, and strategic subordination to U.S. interests. Allies are constrained by the need to maintain U.S. commitment against competitor powers; cannot fully exit without strategic vulnerability.
constraint_indexing:constraint_classification(sotu_1972_nixon_burden_sharing_doctrine, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: U.S. MILITARY-INDUSTRIAL COMPLEX (ROPE) — Defense contractors and military strategists experience the doctrine as coordination of U.S. strategic interests with allied development. The mechanism is pure coordination: U.S. military capacity is redirected toward high-value commitments (Europe, East Asia, Middle East) and away from lower-priority interventions. This produces sustained demand for: weapons system exports, military training missions, technology licensing, defense contracts for advanced systems. The beneficiary has arbitrage options: if allies develop too quickly, shift capacity to allied equipment purchasing; if doctrine fails, return to direct intervention contracting. Net beneficiary experiencing coordination efficiency rather than extraction.
constraint_indexing:constraint_classification(sotu_1972_nixon_burden_sharing_doctrine, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SOVIET UNION / COMPETING GEOPOLITICAL POWERS (TANGLED ROPE) — The doctrine's institutional gatekeeper structure (requiring U.S. interest or treaty for intervention) creates a coordination mechanism for great-power boundaries: spheres of influence become more legible, intervention thresholds become more predictable, arms control negotiations can address specific commitment zones. Simultaneously, the doctrine extracts through constraint: the Soviet Union is locked into regional competition with allied nations now receiving U.S. capacity-building assistance, and the U.S. retains arbitrage in declaring vital interests. Organized geopolitical actors are constrained by the doctrine's clarity on intervention thresholds but benefit from reduced unpredictability.
constraint_indexing:constraint_classification(sotu_1972_nixon_burden_sharing_doctrine, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: FORMAL U.S. TREATY COMMITMENT INFRASTRUCTURE (PITON) — The doctrine preserves and reinforces NATO, SEATO, bilateral defense treaties, and strategic alliance networks. These institutions become the gatekeeping mechanism: commitment is now via treaty, not presidential discretion or ideological alignment. The theater ratio is high (0.62): treaty language is performed as unchanging commitment, yet the doctrine's core mechanism is selective application — the U.S. retains the right to evaluate whether 'vital interests' apply in any given crisis, making treaty interpretation performative. The theater has increased over time as treaties accumulate institutional legitimacy while the vital-interest threshold remains flexible and subject to geopolitical revision.
constraint_indexing:constraint_classification(sotu_1972_nixon_burden_sharing_doctrine, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ALLIED CAPABILITY-BUILDING PROGRAMS (SCAFFOLD) — The doctrine's explicit mechanism (capacity-building, military training, technology transfer) is temporary support with sunset logic: as allied militaries mature and develop indigenous capabilities, the doctrine's justification weakens. Military aid programs have measurable completion timelines: allied air force achieves operational independence, army develops logistics capability, navy operates without U.S. presence. The theater ratio drops over time in successful cases (South Korean self-sufficiency increased from ~40% to ~80% of defense budget over 25 years). Suppression is high during the support phase (allies are dependent) but declining as capabilities mature. The doctrine itself contains its own sunset: the successful ally no longer requires the constraint.
constraint_indexing:constraint_classification(sotu_1972_nixon_burden_sharing_doctrine, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / STRUCTURAL INEVITABILITY VIEW (MOUNTAIN) — From a geopolitical-structural perspective, the burden-sharing doctrine appears to be a necessary adaptation to finite U.S. military capacity: a superpower cannot intervene everywhere simultaneously, so allocation must follow priorities (treaty + vital interests). This framing treats the doctrine as emerging from objective constraints on military power rather than as a choice-dependent institutional mechanism. The structure appears immutable: any great power must ration its intervention capacity. However, the structural data reveals this as false summit — the doctrine is a contingent policy choice whose gatekeeper function benefits specific actors (military-industrial complex, allied governments, treaty-signatories). The 'inevitable' framing naturalizes what is actually a strategic preference.
constraint_indexing:constraint_classification(sotu_1972_nixon_burden_sharing_doctrine, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1972_nixon_burden_sharing_doctrine_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1972_nixon_burden_sharing_doctrine, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1972_nixon_burden_sharing_doctrine, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1972_nixon_burden_sharing_doctrine, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1972_nixon_burden_sharing_doctrine, TR),
    TR >= 0.70.

:- end_tests(sotu_1972_nixon_burden_sharing_doctrine_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The doctrine's core mechanism is gatekeeping military commitment via treaty + vital interests. This produces asymmetric benefits: treaty-bound allies gain security guarantees and capacity-building assistance (genuine coordination gain); non-aligned nations lose unilateral U.S. intervention option (extraction). The U.S. military-industrial complex gains through sustained procurement demand from allied rearmament. The weighted extraction is moderate because the coordination gains for allies are substantial (real capacity development, genuine security), but the extraction of non-aligned nations and the gatekeeping asymmetry push the value upward. Suppression (0.48): Moderate. Allies are suppressed through dependence on U.S. commitment and procurement restrictions, but not completely — they retain development pathways and can diversify suppliers. Non-aligned nations face higher suppression (no U.S. option). The average reflects the mixed population. Theater ratio (0.62): Moderate-high and rising. Early implementation (0.48) genuinely reduced discretionary intervention (constraint was real). Mature implementation (0.62) shows increased performative content: treaty language becomes ceremonial, vital-interest declarations become flexible, and the gatekeeper function is maintained through framing rather than structural limits. The rise reflects Goodhart drift — the doctrine's original constraint (reduce unilateral intervention) has been partially subverted through interpretation.
 *
 * PERSPECTIVAL GAP:
 *   The doctrine demonstrates a full perspectival range. Allied nations see Tangled Rope: genuine coordination (capacity-building, security partnerships) embedded in asymmetric extraction (dependence, procurement control, political conditionality). Non-allied nations see Snare: the constraint eliminates their unilateral-intervention option without providing alternative security (trapped). The military-industrial complex sees Rope: pure coordination of strategic interests with procurement demand. Competing powers see mixed Tangled Rope: predictability in commitment boundaries (coordination gain) alongside lock-in to regional competition (extraction). Treaty infrastructure sees Piton: institutions persist through ceremonial maintenance rather than functional verification. Capability-building programs see Scaffold: temporary support with sunset as allied militaries mature. The analytical observer sees Mountain (false summit): burden-sharing appears to be a necessary adaptation to U.S. military limits, but structural data reveals it as a contingent policy choice whose gatekeeper function benefits specific actors. The perspectival gap between the beneficiary view (coordination, coordination, coordination) and the victim view (snare, tangled extraction) is maximal.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective follows from power level, exit options, and beneficiary/victim status. Treaty-bound allies (moderate power, constrained exit) with beneficiary status experience low d (benefits minus costs), producing moderate χ. Non-aligned nations (powerless, trapped) with victim status experience high d (costs only), producing high χ. The U.S. military-industrial complex (institutional power, arbitrage exit) with beneficiary status experiences low d, producing low χ (coordination). Competing powers (organized, constrained) experience moderate d because they have constraints (regional competition lock-in) and some benefits (predictable boundaries). The treaty infrastructure (institutional, arbitrage) experiences low d (benefits from institutionalization). Allied capability programs (organized, constrained, beneficiary/victim mixed) experience moderate d. The analytical observer (analytical, analytical) experiences canonical d ≈ 0.73, but the false-summit detector identifies this as a perspectival limit: the observer risks naturalizing contingent policy as structural necessity. The engine's directionality derivation should produce gaps: beneficiaries cluster low, victims cluster high, the beneficiary-victim differential is a diagnostic marker of extraction asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The mandatrophy is resolved through inter-institutional perspectival differentiation. The constraint is NOT uniformly classified (not all types identical), so the mandatrophy question 'Is this coordination or extraction?' is legitimate. The answer is 'both, depending on position': allies experience coordination + extraction (Tangled Rope); non-allies experience pure extraction (Snare); the architecture experiences pure coordination (Rope). The doctrine's gatekeeper function (treaty + vital interests) is the discriminating mechanism: it genuinely coordinates great-power boundaries while extracting from those excluded. The false summit is resolved through the beneficiary presence: the doctrine has identifiable beneficiaries (treaty allies, military-industrial complex), triggering FSM evaluation. The engine should classify this as Tangled Rope at the analytical level: genuine coordination function (alliance infrastructure, capacity-building) combined with asymmetric extraction (gatekeeping, non-aligned exclusion, procurement control). The unified classification (Tangled Rope) explains the perspectival multiplicity: different positions within a Tangled Rope structure experience it as either rope, snare, or mixed depending on their directionality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vital_interests_gatekeeping_definition,
    'Is ''vital U.S. interests'' an objective structural limit or a discretionary policy threshold?',
    'Historical analysis of vital-interest declarations: consistency of application, correlation with economic/strategic metrics, evidence of policy revision in response to political change vs structural shift',
    'If objective: doctrine constrains U.S. interventionism through structural limits (Mountain). If discretionary: gatekeeper function is maintained through political framing and is reversible (Tangled Rope). The false-summit detection depends on resolving this.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vital_interests_gatekeeping_definition, conceptual, 'Whether vital interests is objective or discretionary threshold').

omega_variable(
    allied_capability_development_rate,
    'Do allied military capabilities develop at rate sufficient to justify the ''temporary support'' classification, or does dependency persist indefinitely?',
    'Longitudinal measurement of allied military self-sufficiency metrics: procurement sourcing (domestic vs U.S.), force projection range, technological capability, operational independence. Comparison across cohorts (NATO Europe vs East Asia vs Middle East).',
    'If rapid (10-25 year) maturation: scaffold sunset is real. If stagnant: scaffold is misclassified, constraint is persistent Tangled Rope. Affects whether the constraint is intrinsically temporary or structurally continuous.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(allied_capability_development_rate, empirical, 'Rate of allied military capability maturation').

omega_variable(
    extraction_flow_directionality_ambiguity,
    'Are the economic and military benefits flowing to the U.S. or to allied nations — or is the flow bidirectional and balanced?',
    'Accounting analysis: defense spending by allied nations on U.S. equipment, technology transfer fees, military base access value, cost-sharing in joint operations. Compare against U.S. military expenditure on allied defense and capacity-building assistance.',
    'If benefits flow primarily to U.S.: allies are victims, constraint is Snare from allied perspective. If flow is balanced: genuine coordination (Rope). If balanced but with gatekeeping asymmetry (allies need U.S. commitment, U.S. retains discretion): Tangled Rope confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_flow_directionality_ambiguity, empirical, 'Direction and magnitude of economic/military benefit flows').

omega_variable(
    institutional_lock_in_via_treaty,
    'Do treaty commitments create structural lock-in that persists independent of strategic value, or are treaties genuinely revocable expressions of current interest?',
    'Analysis of treaty interpretation over time: instances of treaty invocation vs non-invocation when strategic conditions would justify either; political cost and legal/diplomatic consequence of treaty revision or withdrawal; comparison between NATO (heavily invoked, defended as unbreakable) and SEATO (dissolved when no longer strategically valuable).',
    'If lock-in is real: treaties become pitons (institutional inertia, high theater). If revocable: treaties are continuous expressions of current interest (doctrine is contingent, not structural). Affects whether the gatekeeper mechanism is genuinely constraining or performative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_lock_in_via_treaty, empirical, 'Whether treaty commitments create structural lock-in').

omega_variable(
    non_aligned_nation_extraction_rate,
    'How much do non-treaty nations lose in security capacity and geopolitical influence due to the doctrine''s exclusion of unilateral intervention?',
    'Counterfactual comparison: geopolitical outcomes for non-aligned nations under the pre-1972 unilateral intervention regime vs post-1972 burden-sharing regime. Measurement of security dilemmas, regional power concentration, proxy conflict intensity.',
    'If extraction is high: Snare classification for non-treaty nations is correct. If moderate: constraint is purely distributional (some gain, some lose). Directly measures whether the doctrine creates victims outside the ally-beneficiary structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_aligned_nation_extraction_rate, empirical, 'Extraction impact on non-treaty nations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1972_nixon_burden_sharing_doctrine, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nixon_burden_tr_t0, sotu_1972_nixon_burden_sharing_doctrine, theater_ratio, 0, 0.48).
narrative_ontology:measurement(nixon_burden_tr_t5, sotu_1972_nixon_burden_sharing_doctrine, theater_ratio, 5, 0.58).
narrative_ontology:measurement(nixon_burden_tr_t10, sotu_1972_nixon_burden_sharing_doctrine, theater_ratio, 10, 0.62).

% Extraction over time
narrative_ontology:measurement(nixon_burden_be_t0, sotu_1972_nixon_burden_sharing_doctrine, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(nixon_burden_be_t5, sotu_1972_nixon_burden_sharing_doctrine, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(nixon_burden_be_t10, sotu_1972_nixon_burden_sharing_doctrine, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1972_nixon_burden_sharing_doctrine, enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_1972_nixon_burden_sharing_doctrine, cold_war_proxy_conflict_escalation).
narrative_ontology:affects_constraint(sotu_1972_nixon_burden_sharing_doctrine, allied_military_dependence_structure).
narrative_ontology:affects_constraint(sotu_1972_nixon_burden_sharing_doctrine, u_s_strategic_interest_gatekeeping).
narrative_ontology:affects_constraint(sotu_1972_nixon_burden_sharing_doctrine, non_aligned_movement_geopolitical_vulnerability).

% DUAL FORMULATION NOTE:
% The burden-sharing doctrine decomposes into at least three structurally distinct constraints: (1) allied_capacity_building_mechanism (ε ≈ 0.30, Scaffold with sunset as capabilities mature), (2) vital_interests_gatekeeping (ε ≈ 0.55, Tangled Rope — genuine commitment boundaries plus discretionary application), (3) non_aligned_nation_exclusion (ε ≈ 0.72, Snare — victims with no exit). These are linked: the vital-interests gatekeeper determines who receives capacity-building, which determines non-aligned vulnerability. All three should be modeled as separate stories in a constraint family, with network linkage. The umbrella 'burden-sharing doctrine' story presented here captures the institutional totality; domain-specific decomposition would produce higher analytic precision.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1972_nixon_burden_sharing_doctrine, institutional, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
