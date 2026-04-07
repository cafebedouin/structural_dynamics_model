% ============================================================================
% CONSTRAINT STORY: sotu_2006_bush_iraqi_security_force_training_handoff
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_2006_bush_iraqi_security_force_training_handoff, []).

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
 *   constraint_id: sotu_2006_bush_iraqi_security_force_training_handoff
 *   human_readable: U.S.-Iraq Security Force Training Handoff (SOTU 2006)
 *   domain: military/geopolitical
 *
 * SUMMARY:
 *   The 2006 SOTU announcement of a staged training handoff from U.S. to
 *   Iraqi security forces institutionalizes a constraint that coordinates
 *   military capability transfer while simultaneously extracting political
 *   costs from multiple agents. The mechanism promises Iraqi sovereignty and
 *   reduced U.S. commitment, but operationally preserves U.S. strategic
 *   control through training authority, equipment standardization, and
 *   intelligence dependency. The constraint explicitly removes tactical
 *   decisions from Washington politicians (U.S. political leadership) and
 *   places them under military command authority, insulating operations from
 *   democratic oversight. Iraqi civilians, conscripts, and political
 *   leadership experience this as suppression — trapped within
 *   counterinsurgency operations nominally under their own institutions but
 *   strategically controlled by external military command. U.S. military
 *   command benefits from force multiplication and insulation from political
 *   pressure. Defense contractors benefit from sustained training and
 *   equipment contracts. The theater_ratio trajectory (0.42 → 0.65) reveals
 *   progressive degradation of the constraint's coordination function: as
 *   evidence accumulated that trained forces were compromised by sectarian
 *   loyalty and U.S. control persisted despite nominal transfer, the handoff
 *   mechanism shifted from genuine coordination to performative metrics
 *   reporting — 'trained and equipped' became substitute for 'capable of
 *   independent operations.' The constraint persists through institutional
 *   inertia, enabled by identity-lock of military advisors to the training
 *   mission and political cost externalization to Iraqi institutions.
 *
 * KEY AGENTS:
 *   - U.S. Military Command: Primary beneficiary (institutional/arbitrage) — gains force multiplication and insulation from political oversight; maintains strategic control through training authority
 *   - Iraqi Civilian Population: Primary victim (powerless/trapped) — trapped in counterterrorism operations nominally under Iraqi sovereignty but strategically controlled by external command; bears full cost of collateral damage and sectarian violence
 *   - Iraqi Security Forces Personnel: Secondary victim (moderate/constrained) — conscripted or economically coerced into training pipeline; benefits from salary and institutional development but constrained by U.S. command authority and forced loyalty
 *   - U.S. Political Leadership: Beneficiary with extraction (powerful/constrained) — benefits from reduced political costs of visible U.S. deployment; constrained by military institution's recommitment pressure; experiences piton degradation as training metrics replace actual capability reporting
 *   - Iraqi Political Leadership: Nominal beneficiary with extraction (organized/constrained) — ostensibly benefits from sovereignty; actually constrained by U.S. control of security apparatus through training and equipment authority
 *   - U.S. Military Personnel (Advisors): Mixed beneficiary-victim (moderate/identity_locked) — benefits from operational authority and career advancement; extracted through indefinite commitment and moral responsibility for force actions; identity-fused to training mission
 *   - Defense Contractors: Beneficiary (powerful/arbitrage) — sustained revenue from training contracts and equipment supply; no extraction — pure coordination benefit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_2006_bush_iraqi_security_force_training_handoff, 0.58).
domain_priors:suppression_score(sotu_2006_bush_iraqi_security_force_training_handoff, 0.65).
domain_priors:theater_ratio(sotu_2006_bush_iraqi_security_force_training_handoff, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_2006_bush_iraqi_security_force_training_handoff, extractiveness, 0.58).
narrative_ontology:constraint_metric(sotu_2006_bush_iraqi_security_force_training_handoff, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(sotu_2006_bush_iraqi_security_force_training_handoff, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_2006_bush_iraqi_security_force_training_handoff, tangled_rope).
narrative_ontology:human_readable(sotu_2006_bush_iraqi_security_force_training_handoff, "U.S.-Iraq Security Force Training Handoff (SOTU 2006)").
narrative_ontology:topic_domain(sotu_2006_bush_iraqi_security_force_training_handoff, "military/geopolitical").

domain_priors:requires_active_enforcement(sotu_2006_bush_iraqi_security_force_training_handoff).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_2006_bush_iraqi_security_force_training_handoff, united_states_military_command).
narrative_ontology:constraint_beneficiary(sotu_2006_bush_iraqi_security_force_training_handoff, iraqi_security_forces_leadership).
narrative_ontology:constraint_beneficiary(sotu_2006_bush_iraqi_security_force_training_handoff, defense_contractors).
narrative_ontology:constraint_victim(sotu_2006_bush_iraqi_security_force_training_handoff, iraqi_civilian_population).
narrative_ontology:constraint_victim(sotu_2006_bush_iraqi_security_force_training_handoff, iraqi_conscripts_and_security_force_personnel).
narrative_ontology:constraint_victim(sotu_2006_bush_iraqi_security_force_training_handoff, u_s_military_personnel).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IRAQI CIVILIAN POPULATION (SNARE) — Trapped within territorial control structures nominally transitioning to Iraqi sovereignty but functionally remaining under U.S. strategic command. Cannot exit the counterterrorism conflict; bears full cost of collateral damage, displacement, and sectarian violence enabled by untrained security forces. The constraint removes tactical decision-making from Washington politicians but places it under military command authority — civilians have no representation in either structure. Maximum experienced extraction with no coordination benefit.
constraint_indexing:constraint_classification(sotu_2006_bush_iraqi_security_force_training_handoff, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: IRAQI SECURITY FORCE PERSONNEL (TANGLED ROPE) — Constrained by training dependency on U.S. command structure and limited exit options (desertion risks execution; recruitment often coerced through economic desperation). Benefits from institutional capacity-building and salary provision. The handoff mechanism creates coordinated counterterrorism capability while simultaneously extracting loyalty through institutional subordination to U.S. strategic objectives. Forced professionalization alongside asymmetric command authority.
constraint_indexing:constraint_classification(sotu_2006_bush_iraqi_security_force_training_handoff, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: U.S. MILITARY COMMAND (ROPE) — Primary beneficiary. The constraint solves the U.S. command's core coordination problem: maintaining strategic control and counterterrorism effectiveness while reducing political costs of troop deployments. The mechanism explicitly transfers tactical decision authority to military command (not politicians), insulating operations from domestic political pressure. Net extraction flows toward this agent — they gain force multiplication through Iraqi security forces while maintaining effective control. The training handoff is their coordination solution.
constraint_indexing:constraint_classification(sotu_2006_bush_iraqi_security_force_training_handoff, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: IRAQI POLITICAL LEADERSHIP (TANGLED ROPE) — Ostensibly benefits from sovereignty transfer and security apparatus development. Constrained by U.S. strategic control of training, equipment, and command authority. The handoff mechanism nominally enables Iraqi autonomy but operationally preserves U.S. veto over major security decisions through control of logistics, intelligence, and training standards. Political leadership experiences mixed coordination (building sovereign institutions) and extraction (loss of autonomous security control).
constraint_indexing:constraint_classification(sotu_2006_bush_iraqi_security_force_training_handoff, tangled_rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: DEFENSE CONTRACTING SECTOR (ROPE) — Benefits from sustained training contracts, equipment supply agreements, and advisory services. The handoff constraint creates a long-term revenue stream through the training pipeline and equipment standardization requirements. No meaningful extraction — contractors experience the constraint as a coordination mechanism that guarantees demand and enables market expansion. Arbitrage position allows them to pivot to other markets if Iraq-specific revenue declines.
constraint_indexing:constraint_classification(sotu_2006_bush_iraqi_security_force_training_handoff, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: U.S. POLITICAL LEADERSHIP (PITON) — The constraint's original coordination function (communicating a credible exit strategy to the U.S. public during an unpopular war) has degraded into performative metrics reporting. Theater_ratio high: measuring 'trained and equipped' Iraqi security forces became a substitute for measuring actual capability or civilian protection outcomes. The handoff mechanism persists through institutional inertia despite evidence that training programs were producing forces compromised by sectarian loyalty and corruption. The constraint continued because the alternative (acknowledging indefinite commitment) was politically unacceptable.
constraint_indexing:constraint_classification(sotu_2006_bush_iraqi_security_force_training_handoff, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: U.S. MILITARY PERSONNEL (ADVISORS) (TANGLED ROPE) — Identity-locked to the training mission through professional commitment and career incentive structures. Structurally mobile (could refuse deployment) but cannot exercise that option without abandoning military identity and career. Benefits from operational authority and professional development; bears extraction through indefinite commitment, casualty exposure, and moral responsibility for trained forces' actions against civilian populations. The constraint coordinates training capability while extracting lifetime subordination to strategic mission through identity fusion with military role.
constraint_indexing:constraint_classification(sotu_2006_bush_iraqi_security_force_training_handoff, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a civilizational perspective, the security force training handoff appears as an immutable constraint of counterinsurgency doctrine: any occupation force must eventually transfer capability to local forces or face indefinite commitment. The mechanism appears as a natural law of military strategy with no coordination alternative. However, the structural data reveals this as a false summit — the 'necessity' of the handoff naturalizes what is actually a contingent institutional arrangement (U.S. commitment to regional hegemony, specific doctrine choice, political decision to avoid domestic conscription). The mountain classification obscures the extraction mechanisms by framing them as strategic inevitability.
constraint_indexing:constraint_classification(sotu_2006_bush_iraqi_security_force_training_handoff, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_2006_bush_iraqi_security_force_training_handoff_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_2006_bush_iraqi_security_force_training_handoff, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_2006_bush_iraqi_security_force_training_handoff, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_2006_bush_iraqi_security_force_training_handoff, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_2006_bush_iraqi_security_force_training_handoff, TR),
    TR >= 0.70.

:- end_tests(sotu_2006_bush_iraqi_security_force_training_handoff_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts across all victim populations — from Iraqi civilians through operational suppression, from conscripts through training pipeline coercion, from U.S. military advisors through career identity-lock, from U.S. political leadership through military institution's mission creep recommendations. Primary beneficiaries (U.S. military command, defense contractors) experience it as coordination mechanism. The rising trajectory (0.35 → 0.62) reflects progressive degradation as sectarian capture becomes apparent and the handoff's performative character becomes undeniable. Suppression (0.65): High. Victims face significant barriers to exit: Iraqi civilians cannot escape territorial control; conscripts face execution for desertion; U.S. advisors face career destruction for mission abandonment; Iraqi political leadership faces political destabilization if they reject training mechanism. Theater_ratio (0.58): Moderate-high. Initially (0.42), the constraint had genuine coordination function — training actually did transfer some capability. Progressively (→0.65), measurement shifted from operational outcomes to inputs: 'trained and equipped' became the reported metric while actual force effectiveness declined. U.S. political leadership increasingly reported training progress as substitute for reporting actual counterterrorism outcomes, indicating theater substitution (Goodhart drift). Suppression is not scaled by f(d) or scope — it is a raw structural property. Extractiveness is scaled by directionality.
 *
 * PERSPECTIVAL GAP:
 *   Iraqi security forces perspective: the same training mechanism that coordinates counterterrorism capability also extracts loyalty and sectarian commitment. They experience Tangled Rope — genuine institutional development alongside coercive subordination. U.S. military command perspective: pure coordination (Rope) — the mechanism solves their force-multiplication problem with minimal cost. Iraqi civilians perspective: pure extraction (Snare) — they experience the mechanism as suppression with no coordination benefit. U.S. political leadership perspective: Piton degradation — the coordination function (communicating exit strategy) has been replaced by performative metrics. Iraqi political leadership perspective: Tangled Rope with asymmetric terms — they nominally benefit from sovereignty but experience constraint through loss of security autonomy. U.S. military advisors perspective: identity-locked Tangled Rope — they benefit from operational authority but extracted through indefinite psychological commitment. The perspectival gap is maximized between U.S. military command (sees pure coordination, pure benefit) and Iraqi civilian population (sees pure extraction, pure cost).
 *
 * DIRECTIONALITY LOGIC:
 *   U.S. military command as primary beneficiary derives d ≈ 0.10 (institutional power + arbitrage exit → low d → negative chi, meaning extraction runs toward them, not away). Iraqi civilians as primary victims derive d ≈ 0.92 (powerless + trapped exit → high d → high chi, meaning extraction runs away from them toward beneficiaries). The asymmetry is structural: beneficiaries have exit options and institutional power; victims have neither. U.S. military advisors occupy an unusual position — structurally mobile (moderate power, could refuse deployment) but identity-locked, making their effective d-value consistent with trapped agents (d ≈ 0.85) despite moderate nominal power. This is the key diagnostic signal for identity-lock: the power atom does not match the experienced extractiveness because the binding mechanism is cognitive rather than structural. Iraqi political leadership derives d ≈ 0.55 (organized power + constrained exit → moderate d) — they nominally benefit but operationally lack autonomy.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by revealing that the 'either pure coordination or pure extraction' framing is insufficient. The handoff mechanism is genuinely coordinating something (military capability transfer) while genuinely extracting something else (political costs, military authority, civilian subordination). The mandatrophy is resolved by decomposition: the coordination function (military training) and the extraction function (political cost externalization) are structurally distinct, though operationally coupled. The high theater ratio indicates that the constraint has degraded from genuine coordination toward pure performance — metrics reporting replaces outcome accountability. The constraint's analytical observer classification (mountain/natural law) is a false summit: the 'necessity' of security force handoff naturalizes a specific doctrinal choice (maintaining regional hegemony through Iraqi proxy institutions) rather than describing an inevitable constraint of counterinsurgency. The constraint persists because abandoning it would require acknowledging either indefinite U.S. commitment (politically unacceptable to U.S. leadership) or genuine Iraqi autonomy (strategically unacceptable to U.S. military command). The mandatrophy is resolved in the gap between what the constraint nominally claims (training → sovereignty) and what it operationally delivers (training → strategic control persistence + political cost externalization).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sectarian_loyalty_institutional_capture,
    'To what extent were Iraqi security forces trained by the U.S. actually performing counterterrorism versus sectarian violence against civilian populations and political opponents?',
    'Post-2011 analysis of Iraqi security force operations: civilian casualty attribution, sectarian targeting patterns, unit composition loyalty (Shiite vs Sunni dominance correlating with force behavior), comparison of force actions under U.S. supervision vs post-U.S. withdrawal',
    'If majority sectarian: training handoff created extraction mechanism masquerading as coordination (Snare from analytical perspective). If majority counterterrorism: training genuinely coordinated shared security objective (Rope from analytical perspective). Most likely: mixed, requiring constraint decomposition into separate sectarian and counterterrorism stories.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sectarian_loyalty_institutional_capture, empirical, 'Extent of sectarian capture versus counterterrorism functionality in trained forces').

omega_variable(
    us_strategic_control_retention,
    'Did the handoff mechanism genuinely transfer operational autonomy to Iraqi security forces, or did U.S. control persist through intelligence sharing, equipment standardization, training curricula, and logistical dependency?',
    'Comparative analysis of pre/post-2011 Iraqi security force operational decisions: autonomy in target selection, budget authority, force deployment, equipment procurement; dependence on U.S. intelligence for tactical decisions; documentation of U.S. advisory presence and command authority in major operations',
    'If genuine transfer: constraint is Tangled Rope with real Iraqi agency (beneficiary + victim roles balanced). If persistent control: constraint is Snare with performative sovereignty (U.S. maintains extraction through nominally Iraqi institutions). Evidence suggests persistent control, supporting Snare classification from Iraqi political leadership perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(us_strategic_control_retention, empirical, 'Whether handoff transferred genuine operational autonomy or preserved U.S. strategic control').

omega_variable(
    training_degradation_mechanism,
    'What caused the progressive degradation of Iraqi security force effectiveness after the handoff, and was this predictable from the training mechanism itself?',
    'Analysis of force composition, training curricula, vetting procedures: were recruited personnel selected for sectarian loyalty rather than capability? Did U.S. training emphasize U.S.-style tactics incompatible with Iraqi institutional resources? Did training programs fail to address corruption and unit cohesion? Post-hoc comparison with training outcomes in other counterinsurgency contexts',
    'If degradation was preventable: training handoff extracted commitment from U.S. and Iraqi actors while failing to deliver coordination (false summit of ''training effectiveness''). If degradation was structural inevitability: constraint is a mountain of counterinsurgency doctrine. Most evidence suggests preventable degradation, indicating extraction masquerading as coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(training_degradation_mechanism, empirical, 'Causes and predictability of Iraqi security force degradation after training handoff').

omega_variable(
    political_cost_externalization,
    'Did the handoff constraint reduce political costs for the U.S. by externalizing casualty risk and decision-making authority to Iraqi institutions while preserving U.S. strategic control?',
    'Comparative analysis of U.S. domestic political narrative before/after handoff announcement; casualty trends (U.S. vs Iraqi security forces); decision authority tracking (tactical decisions attributed to Iraqi vs U.S. command); political pressure changes post-handoff',
    'If yes: constraint is extraction mechanism for U.S. political leadership masquerading as coordination (Snare from political leadership perspective, false summit of ''Iraqi autonomy''). If no: constraint coordinates genuine burden-sharing (Rope from political leadership perspective). Evidence strongly supports first interpretation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(political_cost_externalization, empirical, 'Whether handoff externalizes political costs through institutional nominalization of control').

omega_variable(
    identity_lock_mechanism_us_military,
    'To what extent did the training mission become identity-constitutive for U.S. military advisors, creating organizational and individual commitment irreversible through political decision?',
    'Institutional analysis: did advising positions become career-essential roles for senior officers? Did operational command structure become dependent on advisor authority? Did military doctrine evolve to presuppose ongoing training mission? Personnel interviews and deployment pattern analysis post-withdrawal.',
    'If strong identity lock: military institution becomes constraint beneficiary with extracted lifetime commitment (identity_locked exit persists post-withdrawal as psychological subordination to mission). If weak: constraint is purely structural extraction (normal career incentives). Evidence suggests strong identity lock — advisors continued recommending recommitment 2009-2011 despite evidence of failure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_us_military, conceptual, 'Extent of identity fusion between military advising role and institutional commitment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_2006_bush_iraqi_security_force_training_handoff, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(isft_tr_t0, sotu_2006_bush_iraqi_security_force_training_handoff, theater_ratio, 0, 0.42).
narrative_ontology:measurement(isft_tr_t2, sotu_2006_bush_iraqi_security_force_training_handoff, theater_ratio, 2, 0.5).
narrative_ontology:measurement(isft_tr_t4, sotu_2006_bush_iraqi_security_force_training_handoff, theater_ratio, 4, 0.58).
narrative_ontology:measurement(isft_tr_t6, sotu_2006_bush_iraqi_security_force_training_handoff, theater_ratio, 6, 0.65).

% Extraction over time
narrative_ontology:measurement(isft_be_t0, sotu_2006_bush_iraqi_security_force_training_handoff, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(isft_be_t2, sotu_2006_bush_iraqi_security_force_training_handoff, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(isft_be_t4, sotu_2006_bush_iraqi_security_force_training_handoff, base_extractiveness, 4, 0.58).
narrative_ontology:measurement(isft_be_t6, sotu_2006_bush_iraqi_security_force_training_handoff, base_extractiveness, 6, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_2006_bush_iraqi_security_force_training_handoff, enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_2006_bush_iraqi_security_force_training_handoff, iraq_counterinsurgency_force_composition).
narrative_ontology:affects_constraint(sotu_2006_bush_iraqi_security_force_training_handoff, us_military_advisory_presence_indefinite_commitment).

% DUAL FORMULATION NOTE:
% The training handoff decomposes into two structurally distinct constraints: (1) military coordination mechanism (genuine training capability transfer, ε ≈ 0.30, Rope from most perspectives), and (2) political cost externalization (institutional nominalization of sovereignty to avoid domestic political pressure, ε ≈ 0.70, Snare from victim perspectives). This story models the hybrid Tangled Rope that results from their coupling. The sectarian capture omega suggests possible further decomposition into sectarian violence enforcement (ε ≥ 0.65, pure Snare) versus counterterrorism coordination (ε ≤ 0.35, pure Rope), depending on empirical proportions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_2006_bush_iraqi_security_force_training_handoff, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
