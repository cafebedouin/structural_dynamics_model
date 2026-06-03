% ============================================================================
% CONSTRAINT STORY: legitimacy_of_practice_standardization__endogenous_displacement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_practice_standardization__endogenous_displacement_reading, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: legitimacy_of_practice_standardization__endogenous_displacement_reading
 *   human_readable: Legitimacy of Practice Standardization (Endogenous Displacement Reading)
 *   domain: political_history/modernization_studies/institutional_change
 *
 * SUMMARY:
 *   This constraint models the legitimacy claim underlying endogenous
 *   practice standardization — the view that practice change is legitimate
 *   when it emerges from voluntary adoption driven by perceived utility or
 *   cultural evolution. This is ONE READING of the contested kernel
 *   'legitimacy_of_practice_standardization,' distinct from two sibling
 *   readings: the exogenous_override_reading (state decree legitimizes change
 *   for collective benefit) and the dual_practice_equilibrium_reading
 *   (legitimacy is domain-partitioned, with state authority governing public
 *   domains and traditional authority governing private domains). The
 *   endogenous reading claims that voluntary adoption curves show gradual
 *   diffusion, regional variation, and elite-to-mass transmission, with
 *   resistance understood as temporary friction rather than genuine
 *   incompatibility. Historical cases that instantiate this reading include
 *   the adoption of the Gregorian calendar in Protestant regions (utility of
 *   astronomical accuracy drove adoption despite religious authority's
 *   initial opposition), standardization of dress codes in merchant networks
 *   (market efficiency incentives), and shift toward alphabetic writing
 *   systems in commercial contexts (information efficiency). The constraint's
 *   low extractiveness (0.18) and suppression (0.22) reflect the reading's
 *   theoretical commitment: in the endogenous account, no agent is
 *   systematically targeted, no coercive enforcement machinery is required,
 *   and the coordination mechanism is self-sustaining through utility
 *   recognition. The theater ratio (0.35) captures performative elements that
 *   emerge as traditional authorities attempt to maintain legitimacy during
 *   functional displacement — ceremonial preservation of old practices even
 *   as daily practice shifts.
 *
 * KEY AGENTS:
 *   - Adopting Populations: Primary beneficiary (moderate/mobile) — perceive practice change as utility-enhancing coordination; exit is available and low-cost
 *   - Elite Adopters (Merchants, Administrators, Intellectuals): Primary beneficiary (powerful/arbitrage) — benefit from new practice's utility and actively promote adoption; capture early benefits
 *   - Peripheral Populations: Secondary victim (powerless/constrained) — receive coordination benefit but bear cultural friction and social pressure; lack power to shape which practices are adopted
 *   - Merchant Class: Organized beneficiary (organized/mobile) — experience constraint as pure efficiency coordination; actively sustain adoption through networks
 *   - Traditional Authority (Religious/Cultural Elites): Institutional actor (institutional/constrained) — experience functional displacement while attempting performative conservation; constrained because voluntary adoption erodes their authority independent of state action
 *   - State Modernizer: Temporary coordinator (organized/constrained) — facilitates but does not mandate transition; sunset function that phases out as adoption becomes self-sustaining
 *   - Analytical Observer: Universal perspective (analytical/analytical) — risks naturalizing contingent utility preferences as immutable cultural evolution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.18).
domain_priors:suppression_score(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.22).
domain_priors:theater_ratio(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__endogenous_displacement_reading, rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__endogenous_displacement_reading, "Legitimacy of Practice Standardization (Endogenous Displacement Reading)").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__endogenous_displacement_reading, "political_history/modernization_studies/institutional_change").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__endogenous_displacement_reading, 'c54dfcd8-57bf-4c93-ac7d-113a10c4e401').
narrative_ontology:cs_kernel_codification('c54dfcd8-57bf-4c93-ac7d-113a10c4e401', distributed).
narrative_ontology:cs_authority_grounding('c54dfcd8-57bf-4c93-ac7d-113a10c4e401', distributed).
narrative_ontology:cs_reading_relation('c54dfcd8-57bf-4c93-ac7d-113a10c4e401', legitimacy_of_practice_standardization__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('c54dfcd8-57bf-4c93-ac7d-113a10c4e401', legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, coexists_with).
narrative_ontology:cs_axiom('c54dfcd8-57bf-4c93-ac7d-113a10c4e401', foundational, voluntary_adoption_legitimates_change).
narrative_ontology:cs_axiom_status(voluntary_adoption_legitimates_change, holdable).
narrative_ontology:cs_axiom_grounding('c54dfcd8-57bf-4c93-ac7d-113a10c4e401', voluntary_adoption_legitimates_change, instrumental).
narrative_ontology:cs_axiom('c54dfcd8-57bf-4c93-ac7d-113a10c4e401', foundational, utility_preference_is_endogenous).
narrative_ontology:cs_axiom_status(utility_preference_is_endogenous, holdable).
narrative_ontology:cs_axiom_grounding('c54dfcd8-57bf-4c93-ac7d-113a10c4e401', utility_preference_is_endogenous, empirically_contingent).
narrative_ontology:cs_reference_frame('c54dfcd8-57bf-4c93-ac7d-113a10c4e401', voluntary_diffusion_legitimacy).
narrative_ontology:cs_drift_state('c54dfcd8-57bf-4c93-ac7d-113a10c4e401', contemporary_information_asymmetry_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c54dfcd8-57bf-4c93-ac7d-113a10c4e401', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__endogenous_displacement_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, adopting_populations).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, utility_beneficiaries).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ADOPTING POPULATION (ROPE) — Individual agents perceive the practice change as a coordination mechanism they can choose to adopt or reject based on perceived utility. The constraint solves a collective action problem (calendar standardization reduces transaction costs; dress norms simplify social signaling) without coercive overhead. Exit is mobile — agents can adopt new practices or maintain old ones with modest social friction. Low extraction because the beneficiary (the adopting population itself) experiences the mechanism as voluntary coordination.
constraint_indexing:constraint_classification(legitimacy_of_practice_standardization__endogenous_displacement_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 2: ELITE ADOPTERS (ROPE) — High-status agents (merchants, administrators, intellectuals) experience the new practice as a legitimate coordination tool that enhances their arbitrage capacity (calendar standardization enables interstate commerce; standardized dress signals cosmopolitan competence). They benefit from adoption and can opt out if the practice fails to deliver utility. Low extraction because benefits flow to this agent and exit is available.
constraint_indexing:constraint_classification(legitimacy_of_practice_standardization__endogenous_displacement_reading, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: PERIPHERAL POPULATIONS (TANGLED ROPE) — Communities at the margin of the adopting region experience genuine coordination benefit (access to markets, reduced administrative friction) alongside asymmetric extraction: they bear the cultural friction of abandoning indigenous practices, face pressure to conform through employment dependency and social exclusion, yet lack power to shape which new practices are adopted or how the transition is managed. Exit is constrained — refusing the new practice incurs economic and social cost. Moderate extraction reflects the mixed coordination function (real benefit) and asymmetric pressure (burden falls on periphery).
constraint_indexing:constraint_classification(legitimacy_of_practice_standardization__endogenous_displacement_reading, tangled_rope,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: MERCHANT CLASS (ROPE) — Organized economic actors experience the constraint as pure coordination for efficiency: calendar standardization eliminates transaction costs, standardized weights and measures reduce fraud detection overhead, dress codes simplify commercial interactions across regions. These agents actively promote adoption through networks and trade associations. Low extraction because the organized class benefits directly and has exit capacity (can maintain old practices if the new ones fail utility test).
constraint_indexing:constraint_classification(legitimacy_of_practice_standardization__endogenous_displacement_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: TRADITIONAL AUTHORITY (PITON) — Religious and cultural authorities experience the endogenous practice change as a threat to their legitimacy but perceive themselves as unable to prevent voluntary adoption. The response is performative conservation: ritual maintenance of old practices in ceremonial contexts while accepting their erosion in daily life. Theater ratio is high (elaborate ritual observances serve to signal continued authority even as functional practice shifts) because the core authority mechanism has degraded — the traditional elite can no longer enforce conformity through coercive means, only through theatrical affirmation of what is already being abandoned. The piton reflects institutional inertia masking functional displacement.
constraint_indexing:constraint_classification(legitimacy_of_practice_standardization__endogenous_displacement_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: STATE MODERNIZER (SCAFFOLD) — Centralized administrative actors perceive endogenous practice adoption as a temporary coordination problem: regional fragmentation in calendars, weights, measures, or dress codes creates administrative friction. The state role is to facilitate (not mandate) transition through communication infrastructure, standards publication, and removal of legal barriers — a sunset function that decays as adoption matures and becomes self-sustaining through utility recognition. Theater is moderate (some ceremonial promotion of new standards, some coordination meetings) but functional (actual administrative implementation happens). Exit is constrained for the state (maintaining infrastructure through transition period requires investment) but the constraint itself has a sunset: once adoption reaches critical mass through utility recognition, state coordination role phases out.
constraint_indexing:constraint_classification(legitimacy_of_practice_standardization__endogenous_displacement_reading, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal, civilizational perspective, the endogenous reading naturalizes voluntary practice change as an immutable feature of cultural evolution: when practices deliver perceived utility, populations adopt them; when they do not, they persist in niches or disappear through disuse. This perspective sees no enforcement machinery, no extraction, no coercion — only the inevitable logic of utility-driven selection. However, this may be a false summit: the measurement of 'perceived utility' itself depends on whose preferences are counted, whose access to information is available, and whose alternatives are visible. The naturalizing move obscures these epistemic dependencies.
constraint_indexing:constraint_classification(legitimacy_of_practice_standardization__endogenous_displacement_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_practice_standardization__endogenous_displacement_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(legitimacy_of_practice_standardization__endogenous_displacement_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(legitimacy_of_practice_standardization__endogenous_displacement_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(legitimacy_of_practice_standardization__endogenous_displacement_reading, TR),
    TR >= 0.70.

:- end_tests(legitimacy_of_practice_standardization__endogenous_displacement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Very low. The endogenous reading claims that no agent systematically extracts from others through the practice change mechanism itself. The beneficiaries (adopting populations, elites) voluntarily embrace the new practice because it delivers perceived utility. The secondary targets (peripheral populations) receive net coordination benefit even if transition friction is real. No machinery of coercion exists in the reading's theoretical account — adoption is incentivized by information, market dynamics, and social learning, not by suppression of alternatives. The value is low but not zero because some measurement of adoption outcomes across agents shows asymmetric timing (elites adopt first, then diffuse downward through social learning and economic dependency), creating a brief window where early adopters capture disproportionate benefit. Suppression (0.22): Low. The reading predicts that resistance to new practices is temporary friction, not structural opposition. Barriers are informational (unfamiliarity) and transactional (switching costs), not coercive. The low value reflects the absence of legal penalties for maintaining old practices, absence of enforcement machinery, and availability of exit. The non-zero value captures social pressure (informal sanctioning of non-conformers) and economic pressure (employment dependency on adopting new practices in commercial contexts). Theater ratio (0.35): Moderate. As adoption progresses, traditional authorities perform ceremonial affirmations of old practices even as functional practice shifts — ritual preservation becomes theater designed to signal continuing authority despite erosion. Elite adopters stage promotional events for new practices to signal modernity and trustworthiness. The state conducts coordination meetings and ceremonial standard-adoption ceremonies. The theater is not high (functional adoption is genuine) but significant because the transition period involves considerable identity-management performance from all parties.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap between the endogenous reading and its siblings is whether voluntary utility-driven adoption is sufficient for legitimacy (endogenous reading), whether state authority's role in establishing preconditions for adoption makes the state responsible for legitimating the change (exogenous reading), or whether legitimacy can be domain-partitioned so that endogenous adoption in private/cultural domains coexists with exogenous authority in public/administrative domains (dual_practice reading). The endogenous reading sees adoption curves as evidence of voluntary diffusion; the exogenous reading sees the same curves as outcomes of state-facilitated transition within a framework of state responsibility; the dual_practice reading sees different curves for different domains reflecting different legitimacy authorities.
 *
 * DIRECTIONALITY LOGIC:
 *   The endogenous reading routes directionality through perceived utility and voluntary choice. Agents with high information access and low switching costs perceive low d (beneficiary position). Agents with low information access or high switching costs perceive elevated d (constrained position). Agents whose authority is being eroded experience degradation but not extraction (piton classification) because the mechanism is voluntary adoption, not coercive suppression. This contrasts with the exogenous reading, which routes directionality through state mandate: agents who comply because the state mandates (rather than because they perceive utility) perceive higher d and higher suppression. The measurement difference is observable: endogenous adoption shows information-sensitive diffusion (faster among high-information agents), while exogenous imposition shows enforcement-sensitive diffusion (faster where penalties for non-compliance are highest).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not exhibit mandatrophy because extractiveness is very low (0.18) and suppression is low (0.22), placing it clearly in the Rope zone without ambiguity. The mandatrophy would arise if the endogenous reading claimed both (a) genuine coordination function (voluntary adoption for utility) AND (b) significant asymmetric extraction. The reading avoids this by placing extraction costs on peripheral agents at the constrained rather than trapped exit level, and measuring the extractiveness as moderate asymmetry in timing (elites adopt first) rather than structural victimization. The potential mandatrophy is epistemic rather than metric: the reading risks naturalizing what is actually a contingent institutional arrangement (the utility of calendar standardization for merchants) as immutable cultural evolution. This is detected not through mandatrophy gates but through the analytical observer's false summit perspective — the natural law reading of voluntary practice change as inevitable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    utility_perception_endogeneity,
    'Is the ''perceived utility'' that drives adoption truly endogenous to the adopting population''s revealed preferences, or is it shaped by elite framing, market incentives, and information gatekeeping that the endogenous reading treats as background?',
    'Historical analysis of adoption narratives: whose voices are recorded as valuing the new practice? Comparison of elite, merchant, and peripheral accounts of why adoption occurred. Evidence of preference revision under asymmetric information access.',
    'If utility perception is genuinely endogenous: endogenous_displacement reading is confirmed — adoption curves show voluntary diffusion. If utility perception is shaped by elite framing without explicit coercion: the reading is partially captured — what appears voluntary is structured choice within a framed decision space. If utility is systematically misperceived due to information asymmetry: the reading collapses toward exogenous_override or dual_practice readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(utility_perception_endogeneity, empirical, 'Whether perceived utility driving adoption is truly endogenous or shaped by elite framing').

omega_variable(
    coercion_detection_threshold,
    'What level of enforcement/suppression is compatible with the ''endogenous'' label? Does eliminating legal barriers to old practices, or removing employment penalties for non-conformity, count as endogenous adoption, or does the persistence of social/economic pressure disqualify the reading?',
    'Comparative case analysis: calendar change in Sweden (state decree + religious authority cooperation vs. Protestant adoption logic) vs. Japan (elite-driven with minimal enforcement) vs. Iran (state decree + religious authority collaboration). Measurement of suppression_requirement over adoption timeline.',
    'If even modest economic/social pressure disqualifies endogenous reading: most historical cases shift to exogenous or dual_practice readings. If endogenous reading tolerates regional variation and transition-phase coexistence: reading remains plausible for cases with low structural suppression (≤0.25).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coercion_detection_threshold, empirical, 'Threshold of enforcement compatible with endogenous adoption classification').

omega_variable(
    double_life_duration_and_meaning,
    'The endogenous reading predicts ''double life'' as a brief transitional phase. But in many historical cases (e.g., Hindu-Muslim calendar coexistence in South Asia, old/new style calendar in Orthodox Christianity), dual-practice equilibrium persists for centuries without one displacing the other. Is this endogenous adoption plus stable cultural pluralism, or does persistence of dual practice indicate the exogenous_override or dual_practice_equilibrium readings are operative?',
    'Measurement of adoption diffusion curves: do they plateau at <80% (indicating stable pluralism) or approach fixation (indicating endogenous displacement)? Analysis of whether persistence is voluntary (agents choose both practices for different domains) or enforced (agents are forbidden to abandon either).',
    'If dual practice persists at stable equilibrium: dual_practice_equilibrium reading is confirmed, not endogenous displacement. If adoption plateaus due to religious/cultural identity lock: endogenous reading is disconfirmed — the constraint is identity_coordination (attachment domain), not information_standard. If state suppresses one practice to enforce single standard: exogenous_override reading is confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(double_life_duration_and_meaning, empirical, 'Whether dual-practice coexistence reflects endogenous pluralism or disconfirms the endogenous reading').

omega_variable(
    reading_observability_distinction,
    'Can the endogenous displacement reading be empirically distinguished from the exogenous override reading if the state both removes legal barriers AND provides information about the new practice? A state that ''merely coordinates'' vs. one that ''mandates'' may look identical in the historical record if the mandate is enforced through employment policy rather than explicit punishment.',
    'Archival analysis of state communications: are adoption incentives framed as ''voluntary modernization'' (endogenous) or ''administrative requirement'' (exogenous)? Evidence of what happens to refusers: are they penalized structurally (employment loss, administrative exclusion) or socially (shaming, status loss)? Comparison of stated rationale (government documents) vs. outcome (adoption curves, enforcement patterns).',
    'If the state coordinates without structural penalty for refusal: endogenous reading is supported. If structural penalties exist but are framed as side effects of new practice''s utility: the reading is phenomenologically endogenous but structurally exogenous — a case where the readings coexist through different framing, not because the kernel has been resolved.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_observability_distinction, conceptual, 'Whether endogenous and exogenous readings are empirically distinguishable or just different framings of the same intervention').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legit_endogenous_theater_t0, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(legit_endogenous_theater_t15, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 15, 0.32).
narrative_ontology:measurement(legit_endogenous_theater_t30, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 30, 0.35).

% Extraction over time
narrative_ontology:measurement(legit_endogenous_extract_t0, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(legit_endogenous_extract_t15, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 15, 0.15).
narrative_ontology:measurement(legit_endogenous_extract_t30, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 30, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_practice_standardization__endogenous_displacement_reading, information_standard).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__endogenous_displacement_reading, legitimacy_of_practice_standardization__exogenous_override_reading).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__endogenous_displacement_reading, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading (endogenous_displacement) of a three-way contested kernel about the legitimacy of practice standardization. The other two readings are instantiated in sibling constraint files: exogenous_override_reading (state decree legitimates change) and dual_practice_equilibrium_reading (domain-partitioned legitimacy). All three readings share the same base kernel but produce different classification profiles due to different premises about legitimate authority. The ε values differ because the readings make different empirical claims about suppression and extraction: endogenous reading (ε=0.18) assumes voluntary adoption with low suppression; exogenous reading assumes state mandate with potential high suppression; dual reading assumes equilibrium maintenance with moderate enforcement in public domains. Each reading is a structurally distinct constraint, not observational variations on one constraint. They are linked through network.affects_constraints to represent the contested kernel relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
