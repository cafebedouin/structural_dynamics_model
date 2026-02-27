% ============================================================================
% CONSTRAINT STORY: ice_raids_minnesota_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ice_raids_minnesota_2026, []).

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
 *   constraint_id: ice_raids_minnesota_2026
 *   human_readable: Large-Scale Pre-Announced ICE Raids as a Deportation Mechanism
 *   domain: political/social/immigration_enforcement
 *
 * SUMMARY:
 *   Large-scale, pre-announced ICE raids create a structural constraint that
 *   operates simultaneously as immigration enforcement policy and as a
 *   mechanism of psychological and economic extraction from immigrant
 *   communities. The constraint combines elements of coercive state authority
 *   (legitimate immigration enforcement mandate), institutional performance
 *   (media announcements, detention metrics), and community terror (affecting
 *   mixed-status families and entire neighborhoods regardless of legal
 *   status). The pre-announcement mechanism creates a temporal window for
 *   community panic before enforcement, distinguishing this constraint from
 *   unannounced enforcement. Undocumented immigrants and mixed-status
 *   families experience maximum extraction with zero exit options; advocacy
 *   coalitions experience hybrid coordination-extraction relationships; the
 *   ICE institutional apparatus experiences a coordination mechanism that
 *   enables resource planning; and the legal framework risks naturalizing
 *   what are policy choices as immutable sovereign authority. The theater
 *   ratio has increased over the measurement interval as enforcement has
 *   become increasingly symbolic and media-coordinated. Base extractiveness
 *   has increased as raids have expanded beyond stated criminal alien
 *   targeting to broader economic deportations, capturing workers and family
 *   members rather than criminals.
 *
 * KEY AGENTS:
 *   - Undocumented Immigrants: Primary victim (powerless/trapped) — zero exit options, maximum extraction, directly targeted by raid mechanism
 *   - Mixed-Status Families: Secondary victim (moderate/constrained) — forced choice between family separation or voluntary departure; economically dependent on undocumented members
 *   - Immigrant Communities (Aggregate): Tertiary victim (moderate/trapped) — collective paralysis from threat environment; economic function disruption beyond directly targeted individuals
 *   - ICE Institutional Apparatus: Primary beneficiary (institutional/arbitrage) — coordinates enforcement operations; benefits from detention capacity utilization and enforcement metrics
 *   - Anti-Immigration Constituencies: Secondary beneficiary (moderate/mobile) — gain political satisfaction from enforcement visibility and media attention; can exit/engage flexibly
 *   - Immigrant Advocacy Coalition: Institutional actor (organized/mobile) — provides coordination services (bail, legal support) and gains legitimacy/fundraising from crisis response; hybrid beneficiary-victim position
 *   - Immigration Enforcement Legal Framework: Institutional actor (institutional/arbitrage) — maintains pre-announcement mechanism through institutional inertia; sees degraded targeting (original criminal alien focus vs. civil immigration violations)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent policy choices as inherent sovereign authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ice_raids_minnesota_2026, 0.68).
domain_priors:suppression_score(ice_raids_minnesota_2026, 0.78).
domain_priors:theater_ratio(ice_raids_minnesota_2026, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ice_raids_minnesota_2026, extractiveness, 0.68).
narrative_ontology:constraint_metric(ice_raids_minnesota_2026, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(ice_raids_minnesota_2026, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ice_raids_minnesota_2026, snare).
narrative_ontology:human_readable(ice_raids_minnesota_2026, "Large-Scale Pre-Announced ICE Raids as a Deportation Mechanism").
narrative_ontology:topic_domain(ice_raids_minnesota_2026, "political/social/immigration_enforcement").

domain_priors:requires_active_enforcement(ice_raids_minnesota_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ice_raids_minnesota_2026, ice_institutional_apparatus).
narrative_ontology:constraint_beneficiary(ice_raids_minnesota_2026, anti_immigration_constituencies).
narrative_ontology:constraint_victim(ice_raids_minnesota_2026, undocumented_immigrants).
narrative_ontology:constraint_victim(ice_raids_minnesota_2026, mixed_status_families).
narrative_ontology:constraint_victim(ice_raids_minnesota_2026, immigrant_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNDOCUMENTED IMMIGRANT (SNARE) — Zero exit options. Pre-announcement creates coordinated trap: communities cannot leave (family ties, economic dependency, geographic constraint), cannot hide (raids target workplaces and residences systematically), cannot appeal (legal process is designed to accelerate deportation). d≈0.98, f(d)≈1.47, σ=1.0 → χ≈1.00. Maximum extraction from the most vulnerable agent.
constraint_indexing:constraint_classification(ice_raids_minnesota_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MIXED-STATUS FAMILY (SNARE) — Constrained exit: some family members have legal status, some do not. Pre-announcement forces impossible choices (separate voluntarily or be separated by force). Financial dependence on undocumented wage-earner creates vulnerability. Constrained mobility due to childcare, property, employment. d≈0.92, f(d)≈1.38, σ=0.9 → χ≈0.90. Severe extraction with element of false choice (appear voluntary).
constraint_indexing:constraint_classification(ice_raids_minnesota_2026, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: IMMIGRANT ADVOCACY COALITION (TANGLED ROPE) — Organized agents (legal organizations, community groups, churches) experience hybrid constraint. Coordination function: provides bail assistance, legal support, family reunification services. Extraction function: raided communities become dependent on advocacy infrastructure; organizations gain legitimacy and fundraising power from crisis response. d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.51. Mixed extraction and coordination; beneficiaries from organizing victims.
constraint_indexing:constraint_classification(ice_raids_minnesota_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: ICE INSTITUTIONAL APPARATUS (ROPE) — Benefits from pre-announcement mechanism: coordinated operations enable efficient detention capacity planning, media attention, enforcement metrics reporting. Experiences constraint as coordination mechanism for deploying resources (raids must be synchronized with detention capacity, legal processing, transportation). d≈0.08, f(d)≈-0.08, σ=1.0 → χ≈-0.05. Net beneficiary; negative extraction indicates structural advantage.
constraint_indexing:constraint_classification(ice_raids_minnesota_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: IMMIGRANT COMMUNITY AGGREGATE (SNARE) — Collective agent experiences terror mechanism: pre-announcement paralyzes entire communities; workplace absenteeism spikes; children don't attend school; healthcare access collapses (fear of encountering authorities); community economic function degrades. Suppression through psychological extraction (fear of arrest without committing crime). d≈0.96, f(d)≈1.45, σ=0.9 → χ≈0.93. Extraction not through formal process but through ambient threat environment.
constraint_indexing:constraint_classification(ice_raids_minnesota_2026, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 6: IMMIGRATION ENFORCEMENT LEGAL FRAMEWORK (PITON) — Pre-announcement raids persist as degraded mechanism. Original function: target criminal aliens (asylum/felony distinction). Current function: mass extraction of civil immigration violators (employer-based visa overstays, family reunification queue wait times). Theater ratio (0.64) reflects performative elements: public announcements serve messaging rather than operational efficiency (surprise would be operationally superior); detention queue theater (processing theaters for rapid adjudication); deportation ceremony theater (staged to demonstrate enforcement commitment). Mechanism persists through institutional inertia despite questioned effectiveness at criminal targeting.
constraint_indexing:constraint_classification(ice_raids_minnesota_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / SOVEREIGN AUTHORITY VIEW (MOUNTAIN) — Observes pre-announcement raids as immutable expression of state sovereignty over borders. From civilizational/universal perspective, some mechanism of immigration enforcement is inherent to nation-state structure itself. This perspective risks naturalizing what is actually a contingent policy choice (pre-announcement timing, raid intensity, family separation protocols are policy parameters, not laws of nature). Engine will compute false summit based on structural data: ε=0.68, suppression=0.78 contradict mountain classification.
constraint_indexing:constraint_classification(ice_raids_minnesota_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ice_raids_minnesota_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ice_raids_minnesota_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ice_raids_minnesota_2026, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ice_raids_minnesota_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ice_raids_minnesota_2026, TR),
    TR >= 0.70.

:- end_tests(ice_raids_minnesota_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The mechanism extracts in multiple dimensions: (1) direct extraction through deportation (removal of earning member from household; disruption of family structure); (2) economic extraction through workplace fear (wage suppression, voluntary departure at depressed wages, job loss from workplace raids); (3) psychological extraction through terror environment (community paralysis, health impacts, school avoidance); (4) institutional extraction (detention fees, legal processing, forced recruitment of advocacy infrastructure). The value of 0.68 reflects that extraction is severe but not total — many undocumented immigrants remain after raids, some receive legal protection, some evade enforcement. Suppression (0.78): High. Multiple suppression mechanisms: (1) no legal status = no immigration court access for asylum/protection claims in many cases; (2) pre-announcement creates coordinated trap (cannot flee, cannot hide, cannot legally appeal in meaningful timeframe); (3) workplace raids specifically suppress labor organizing and wage negotiation (workers cannot report labor violations without deportation risk); (4) knowledge of raid threat suppresses healthcare access, school attendance, community reporting to police. Suppression is not absolute (underground economy continues, some migrate to sanctuary jurisdictions) but is severe. Theater ratio (0.64): Moderate-high. Pre-announcement mechanism contains significant performative elements: (1) media announcements serve political messaging rather than operational surprise; (2) detention processing includes ceremonial adjudication (rapid removal theater); (3) public deportation ceremonies serve symbolic function of enforcement commitment; (4) enforcement metrics reporting emphasizes numbers removed rather than criminal targeting effectiveness. However, enforcement process also has real legal and operational components (detention, processing, deportation are functional, not purely performative), preventing theater ratio from reaching piton threshold (≥0.70). The upward trajectory over the measurement interval indicates increasing performative content as policy has become more politically salient.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a severe perspectival divergence across the power spectrum. Undocumented immigrants and mixed-status families see pure extraction (Snare) — they have no agency in the process and no legitimate alternative. Advocacy coalitions see mixed coordination-extraction (Tangled Rope) — they benefit from crisis response but also enable continued community vulnerability. The ICE apparatus sees coordination (Rope) — the enforcement mechanism enables efficient resource deployment and operational planning. Anti-immigration constituencies see enforcement (Rope or Mountain depending on whether they naturalize it) — the public announcements coordinate political messaging and satisfy symbolic enforcement demand. The aggregate immigrant community experiences collective Snare — not from individual targeting but from ambient threat environment that affects everyone regardless of legal status. The legal framework sees institutional persistence through inertia (Piton) — original criminal alien mandate has shifted to civil immigration enforcement, and pre-announcement mechanism persists because it satisfies political constituencies rather than operational necessity. The analytical observer risks seeing Mountain (immutable sovereign authority) but the structural data reveals this as false naturalization of policy choices.
 *
 * DIRECTIONALITY LOGIC:
 *   Undocumented immigrants: Victim + trapped → d≈0.98, f(d)≈1.47. Maximum extraction. Mixed-status families: Victim + constrained → d≈0.92, f(d)≈1.38. Severe extraction with false choice. Immigrant advocacy coalitions: Both beneficiary (from crisis response) and victim (enabling continued vulnerability) + mobile → d≈0.55, f(d)≈0.75. Hybrid position reflects organized capacity to act despite being partially captured by extraction mechanism. ICE institutional apparatus: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Net beneficiary; pre-announcement mechanism coordinates enforcement operations. Anti-immigration constituencies: Beneficiary + mobile → d≈0.20, f(d)≈0.05. Low effective extraction; constituencies can engage or disengage flexibly, benefit from political messaging. Immigrant communities aggregate: Victim + trapped → d≈0.96, f(d)≈1.45. Nearly maximum extraction through ambient threat environment affecting entire communities. Immigration enforcement legal framework: Institutional + arbitrage → d≈0.08, f(d)≈-0.08. Piton classification comes from theater ratio gate (0.64) and institutional inertia, not from high chi. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain classification is perspectival false summit.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION THROUGH INSTITUTIONAL MISSION CREEP: The mandatrophy in this constraint centers on whether pre-announcement raids represent legitimate enforcement coordination or pure extraction theater. The resolution lies in empirical measurement of the constraint's stated vs. actual function. Stated function (from policy documents and enforcement rhetoric): target criminal aliens subject to deportation due to serious crimes. Actual function (from outcome data): capture and deport civil immigration violators (overstayed visas, family reunification queue delays) who are economically productive and legally non-criminal. The shift from stated to actual function indicates institutional mission creep — the pre-announcement mechanism was justified as enabling efficient targeting of criminal deportation priorities, but has become a mass extraction mechanism capturing far beyond the original mandate. This resolution moves the classification away from legitimate enforcement (Rope/Scaffold) and toward pure extraction (Snare). The theater ratio increase over the measurement interval (0.38→0.64) reflects increasing performative content as political messaging has overtaken operational efficiency as the primary function of pre-announcement. The baseline extractiveness increase (0.48→0.68) reflects widening capture scope beyond stated criminal alien target. Combined, these indicate that the constraint is accurately classified as Snare from victim perspectives and Piton from institutional perspective — the mechanism persists through political performance and institutional inertia, not through legitimate enforcement necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrent_vs_extraction_threshold,
    'At what point does the threat environment from pre-announcement raids transition from deterrent (making deportation risk salient) to pure extraction (terrorizing entire communities regardless of legal status)?',
    'Epidemiological data on health outcomes, school attendance, emergency room visits, suicide ideation in target communities; qualitative interviews with mixed-status households; comparison with low-announcement enforcement regions',
    'If threshold < 6 months post-announcement: raids are extraction mechanism (Snare classification confirmed). If threshold > 18 months: deterrent logic has real effect and classification might shift toward Tangled Rope (coordination + extraction). If no threshold exists: psychological extraction is baseline, not threshold-dependent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrent_vs_extraction_threshold, empirical, 'Transition point between deterrent effect and pure psychological extraction').

omega_variable(
    pre_announcement_operational_necessity,
    'Is pre-announcement operationally necessary for ICE enforcement capacity (detention capacity planning, legal processing, transportation coordination) or is it primarily a messaging mechanism?',
    'Operational data: comparison of detention facility utilization rates in pre-announced vs unannounced raids; processing time per detainee; deportation completion rates; fugitive apprehension rates (do announced raids catch more targets or fewer?).',
    'If necessary: pre-announcement serves coordination function (Rope). If not necessary: pre-announcement is performative signaling (Piton degradation). If operationally harmful: pre-announcement is pure suppression theater (Snare element amplified).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pre_announcement_operational_necessity, empirical, 'Whether pre-announcement serves operational necessity or is purely performative').

omega_variable(
    family_separation_scope_measurement,
    'How many U.S. citizen children are separated from deported parents per enforcement cycle, and how does this compare to stated policy intent of targeting criminal aliens?',
    'Census data on children in deportee households; family structure tracking post-raid; longitudinal outcomes (school performance, foster system entry); comparison with policy statements about targeting criminal aliens',
    'If separation rate > 40% of raids: mechanism has shifted from criminal targeting (Mountain/Rope) to family extraction (Snare). Indicates institutional mission creep where enforcement apparatus is capturing beyond stated legal mandate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(family_separation_scope_measurement, empirical, 'Scope of family separation relative to stated policy intent').

omega_variable(
    alternative_enforcement_effectiveness,
    'Would targeted, unannounced enforcement focused on specific criminal deportation priorities be more effective at immigration control than mass pre-announced raids affecting entire communities?',
    'Comparison jurisdictions with different enforcement strategies; deportation rates per law enforcement hour; recidivism rates; criminal alien removal effectiveness; community cooperation rates with law enforcement',
    'If targeted approach is more effective: pre-announcement is pure theater/extraction (Piton/Snare). If comparable effectiveness: pre-announcement may be policy choice for messaging. If less effective: pre-announcement is necessary for stated goal but still serves extraction function (ambiguous, supports Tangled Rope classification).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_enforcement_effectiveness, empirical, 'Effectiveness comparison between announced mass raids and targeted enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ice_raids_minnesota_2026, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ice_raids_tr_t0, ice_raids_minnesota_2026, theater_ratio, 0, 0.38).
narrative_ontology:measurement(ice_raids_tr_t3, ice_raids_minnesota_2026, theater_ratio, 3, 0.52).
narrative_ontology:measurement(ice_raids_tr_t6, ice_raids_minnesota_2026, theater_ratio, 6, 0.64).

% Extraction over time
narrative_ontology:measurement(ice_raids_be_t0, ice_raids_minnesota_2026, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(ice_raids_be_t3, ice_raids_minnesota_2026, base_extractiveness, 3, 0.58).
narrative_ontology:measurement(ice_raids_be_t6, ice_raids_minnesota_2026, base_extractiveness, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ice_raids_minnesota_2026, enforcement_mechanism).
narrative_ontology:affects_constraint(ice_raids_minnesota_2026, workplace_deportation_vulnerability).
narrative_ontology:affects_constraint(ice_raids_minnesota_2026, mixed_status_family_structure).
narrative_ontology:affects_constraint(ice_raids_minnesota_2026, immigrant_community_health_outcomes).

% DUAL FORMULATION NOTE:
% Pre-announced raids represent a distinct constraint from immigration enforcement generally. Unannounced, targeted enforcement focused on criminal deportation priorities would have substantially different ε values (lower extractiveness, lower suppression, higher theater ratio due to reduced performative messaging). The pre-announcement mechanism is the specific constraint generating severe extraction from immigrant communities; the underlying immigration enforcement authority is the upstream constraint with different structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ice_raids_minnesota_2026, powerful, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
