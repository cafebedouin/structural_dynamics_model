% ============================================================================
% CONSTRAINT STORY: edelman_2026_insularity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_edelman_2026_insularity, []).

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
 *   constraint_id: edelman_2026_insularity
 *   human_readable: The Insular Trust Mindset
 *   domain: social/economic/epistemic
 *
 * SUMMARY:
 *   The insular trust mindset represents a structural constraint where
 *   individuals preferentially trust and cooperate only with those sharing
 *   their values, information sources, or in-group identity. This constraint
 *   emerged at scale over the past 15 years through the combination of
 *   algorithmic information sorting, identity-market monetization (targeted
 *   advertising, partisan media ecosystems), and social-network clustering.
 *   The constraint functions simultaneously as a coordination mechanism
 *   (enabling rapid cooperation within in-groups) and as an extraction
 *   mechanism (preventing collaboration across groups, concentrating
 *   information control, locking individuals into identity-aligned
 *   information diets). The Edelman 2026 Trust Barometer reports that 64% of
 *   respondents trust people like themselves but only 36% trust people
 *   different from themselves — a gap that has widened steadily as
 *   algorithmic sorting has amplified in-group clustering and as
 *   political/commercial entities have monetized identity-based information
 *   markets. The constraint exhibits all six DR types from different
 *   structural positions: it is a snare for isolated individuals with no
 *   exit, a tangled rope for those navigating cross-group relationships, a
 *   rope for in-group gatekeepers, a piton for legacy media institutions, a
 *   scaffold for bridge-building coalitions, and a false natural law for
 *   those who naturalize in-group preference as evolved human nature.
 *
 * KEY AGENTS:
 *   - Isolated Individual: Powerless victim (powerless/trapped) — no exit from in-group information diet without social cost; full target of suppression and extraction
 *   - Cross-Group Collaborator: Moderate victim (moderate/constrained) — benefits from in-group cooperation but bears cost of out-group isolation; moderate extraction
 *   - In-Group Gatekeeper Institution: Primary beneficiary (institutional/arbitrage) — consolidates loyalty, extracts membership value, controls information flow within group
 *   - Identity Market Vendor: Secondary beneficiary (organized/mobile) — profits from fragmented information supply, targeted advertising, content monopolization
 *   - Legacy Media Institution: Tertiary actor (institutional/constrained) — degraded from bridge-builder to identity-affirmer; maintains audience through performative coverage
 *   - Bridge-Building Coalition: Organized agent (organized/constrained) — interfaith initiatives, cross-partisan dialogue, multilingual platforms building alternative pathways
 *   - Epistemic Commons: Primary victim (abstract/trapped) — contaminated by in-group-affirming misinformation; no self-correction mechanism across group boundaries
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements (algorithms, market incentives) as evolved human nature
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(edelman_2026_insularity, 0.58).
domain_priors:suppression_score(edelman_2026_insularity, 0.72).
domain_priors:theater_ratio(edelman_2026_insularity, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(edelman_2026_insularity, extractiveness, 0.58).
narrative_ontology:constraint_metric(edelman_2026_insularity, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(edelman_2026_insularity, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(edelman_2026_insularity, tangled_rope).
narrative_ontology:human_readable(edelman_2026_insularity, "The Insular Trust Mindset").
narrative_ontology:topic_domain(edelman_2026_insularity, "social/economic/epistemic").

domain_priors:requires_active_enforcement(edelman_2026_insularity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(edelman_2026_insularity, in_group_gatekeepers).
narrative_ontology:constraint_beneficiary(edelman_2026_insularity, identity_market_vendors).
narrative_ontology:constraint_beneficiary(edelman_2026_insularity, ideological_institutions).
narrative_ontology:constraint_victim(edelman_2026_insularity, cross_group_collaboration).
narrative_ontology:constraint_victim(edelman_2026_insularity, epistemic_commons).
narrative_ontology:constraint_victim(edelman_2026_insularity, individual_agency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ISOLATED INDIVIDUAL (SNARE) — Cannot access information sources outside approved in-group channels without social cost. Faces strong suppression (0.72) against cross-group engagement: social ostracism, identity-group pressure, algorithmic sorting. d≈0.93, f(d)≈1.40, σ=1.2 → χ≈0.97. Trapped exit; full target of extraction.
constraint_indexing:constraint_classification(edelman_2026_insularity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CROSS-GROUP COLLABORATOR (TANGLED ROPE) — Benefits from insular trust networks for immediate in-group cooperation and resource-sharing; simultaneously bears cost of reduced collaboration with out-groups. Constrained exit: leaving one's identity group carries reputation and access penalties. d≈0.72, f(d)≈1.15, σ=0.9 → χ≈0.60. Mixed coordination (in-group) and extraction (out-group isolation).
constraint_indexing:constraint_classification(edelman_2026_insularity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: IN-GROUP GATEKEEPER INSTITUTION (ROPE) — Benefits from insular trust by consolidating in-group loyalty and extracting membership value (donations, participation, adherence). Arbitrage exit: can adjust positions to maintain gatekeeper status. The constraint appears as a coordination mechanism for the group: shared values and source-trust enable rapid collective action. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.07. Net beneficiary; experiences low effective extraction.
constraint_indexing:constraint_classification(edelman_2026_insularity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LEGACY MEDIA INSTITUTION (PITON) — Once functioned as cross-group information bridge (Rope); now fragments into identity-aligned networks. Theater ratio (0.65) reflects performative 'balance' and identity-affirming coverage that maintains audience loyalty without genuine epistemic bridging. Constrained exit: institutional sunk costs and audience expectations lock in identity-aligned positioning. d≈0.45, f(d)≈0.47, σ=1.2 → χ≈0.36. Degraded function maintained by inertia.
constraint_indexing:constraint_classification(edelman_2026_insularity, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: BRIDGE-BUILDING COALITION (SCAFFOLD) — Organized groups (interfaith initiatives, cross-partisan dialogue programs, multilingual knowledge platforms) see insular trust as a temporary coordination failure with a sunset: dialogue infrastructure, translation mechanisms, and shared-problem focus are building pathways that bypass identity-gatekeeping. d≈0.35, f(d)≈0.35, σ=1.2 → χ≈0.24. Low effective extraction because the coalition has agency and an exit vision.
constraint_indexing:constraint_classification(edelman_2026_insularity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: IDENTITY MARKET VENDOR (TANGLED ROPE) — Commercial/political/ideological actors that profit from identity tribalism (targeted advertising, polarization influencers, in-group-affirming content creators). Benefits from coordination within in-group; extracts from cross-group isolation through monopolistic information supply. Mobile exit: can switch between identity markets. d≈0.42, f(d)≈0.43, σ=1.2 → χ≈0.30. Moderate extraction; primary extraction mechanism is monopolistic supply to fragmented demand.
constraint_indexing:constraint_classification(edelman_2026_insularity, tangled_rope,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE NATURAL LAW (MOUNTAIN) — Tempting but incorrect reading: 'Humans naturally trust those similar to themselves; this is a law of evolutionary psychology.' Base extractiveness (0.58) and suppression (0.72) contradict mountain gates (ε≤0.25, suppression≤0.05). The engine's false summit detector will flag this as naturalization of contingent institutional arrangements (algorithmic sorting, in-group gatekeeping incentives, identity market monetization). d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.67. Mountain classification is perspectival error.
constraint_indexing:constraint_classification(edelman_2026_insularity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 8: GENUINE ANALYTICAL OBSERVER (TANGLED ROPE) — Sees both the coordination function (in-group cooperation for mutual aid and shared resource defense) and the extraction mechanism (monopolistic control of information flow by gatekeepers, suppression of out-group alternatives, algorithmic amplification of in-group content). d≈0.65, f(d)≈1.00, σ=1.2 → χ≈0.70. The constraint is genuinely hybrid: breaks into Rope (in-group) and Snare (out-group isolation) when decomposed by structural relationship.
constraint_indexing:constraint_classification(edelman_2026_insularity, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(edelman_2026_insularity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(edelman_2026_insularity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(edelman_2026_insularity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(edelman_2026_insularity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(edelman_2026_insularity, TR),
    TR >= 0.70.

:- end_tests(edelman_2026_insularity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts substantial value through monopolistic information supply, suppression of cross-group alternatives, and lock-in of individuals into identity-aligned information diets. However, it is not maximal (0.66+) because in-group cooperation does generate real coordination benefits — food banks, mutual aid networks, collective action within communities all operate through in-group trust. The extraction is genuine but not the only function. Suppression (0.72): High. Powerful mechanisms prevent cross-group engagement: algorithmic sorting (feeds optimize for engagement, which favors in-group affirmation), social-network effects (leaving one's in-group identity carries reputation and access penalties), economic incentives (identity-market vendors invest heavily in in-group targeting), and institutional gatekeeping (religious/political/professional institutions reward loyalty and punish bridge-building). Theater ratio (0.65): Moderate-high. Performative elements are substantial: 'balance' coverage in media that remains identity-affirmed, diversity initiatives that maintain in-group gatekeeping, interfaith events that affirm group identity rather than bridge difference. But the constraint is not purely theatrical — real coordination happens, real information is shared (within groups), real resources are distributed (to in-group members). The theater has increased over the interval as algorithmic sorting has made performative engagement more profitable.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows maximum perspectival divergence. For an isolated individual with no network mobility, it is pure extraction (Snare). For an in-group gatekeeper institution, it is coordination that generates member value (Rope). For a bridge-building coalition, it is a temporary barrier with a structural sunset (Scaffold). For a legacy media institution, it is a degraded function maintained by audience expectations and sunk costs (Piton). For the analytical observer, it is tempting to naturalize as evolutionary human nature (Mountain), but the structural data reveals this as false — the ε=0.58 and suppression=0.72 contradict the mountain gates. The genuine analytical view sees a hybrid constraint: coordination within in-groups (Rope) plus extraction between groups (Snare), synthesized into a tangled rope by the gatekeeping institutions that benefit from both.
 *
 * DIRECTIONALITY LOGIC:
 *   Isolated individual: Victim + trapped → d≈0.93, f(d)≈1.40. Maximum extraction target. Cross-group collaborator: Victim + constrained → d≈0.72, f(d)≈1.15. Significant extraction but constrained by some access to alternative networks. In-group gatekeeper: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary. Identity market vendor: Beneficiary + mobile → d≈0.42, f(d)≈0.43. Moderate extraction reversed — benefits from fragmentation. Legacy media: Beneficiary + constrained → d≈0.45, f(d)≈0.47. Moderate extraction; locked in by institutional positioning but could exit. Bridge-building coalition: Organized + constrained → d≈0.35, f(d)≈0.35. Low extraction; has agency. Analytical observer: Analytical exit → d≈0.65, f(d)≈1.00. Moderate extraction of perspective; risks naturalizing institutional arrangements.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by disaggregating the in-group/out-group relationship: insular trust is simultaneously Rope within groups and Snare between groups. The mandatrophy question is 'Is this a coordination mechanism or an extraction mechanism?' Answer: both, indexed to the agent's structural position. For isolated individuals and the epistemic commons, it is pure Snare. For in-group gatekeepers, it is Rope. For those navigating across groups, it is Tangled Rope. The bridge-building coalition perspective shows that the constraint has a genuine sunset: as dialogue infrastructure matures and multilingual knowledge platforms scale, the necessity of in-group-only trust declines, and the extraction component can be dissociated from the coordination component. The piton perspective shows that performative elements have accumulated (media balance coverage, diversity initiatives that maintain gatekeeping, interfaith events that affirm group identity rather than bridge difference). The false natural law perspective reveals that much of the observed insularity is driven by recent algorithmic and market incentives, not by evolved human nature. This decomposition prevents the constraint from being mislabeled as either pure coordination (which ignores the massive extraction and suppression) or pure extraction (which ignores the real in-group cooperation benefits). It is genuinely hybrid — Tangled Rope — because all three components coexist: coordination within in-groups, extraction between groups, and active enforcement by gatekeeping institutions and identity-market vendors.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    in_group_cooperation_necessity,
    'Is in-group trust coordination genuinely necessary for collective action, or does it function primarily as extraction cover?',
    'Comparative analysis of in-group cooperation outcomes vs outcomes when trust bridges are enforced (shared-problem focus); measurement of free-rider rates in high-insularity vs high-bridge conditions',
    'If necessary: constraint is primarily Rope with extraction externalities. If cover: constraint is primarily Snare with performative coordination. Classification shifts from tangled_rope toward pure snare or pure rope depending on ratio.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(in_group_cooperation_necessity, empirical, 'Whether in-group cooperation is necessary or serves as extraction cover').

omega_variable(
    algorithmic_amplification_agency,
    'What fraction of insular trust is driven by algorithmic sorting (external suppression) vs individual preference (internal choice)?',
    'A/B testing of algorithmic vs neutral feed presentation; longitudinal tracking of individuals when algorithmic sorting is disabled; survey of reported trust barriers vs algorithmic exposure patterns',
    'If algorithmic >70%: suppression is primarily institutional (constraint is enforced extraction, Snare). If preference >70%: suppression is primarily individual (constraint is voluntary clustering, Rope). Mid-range suggests genuine tangled_rope with shared responsibility.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_amplification_agency, empirical, 'Whether insular trust is driven by algorithms or individual preference').

omega_variable(
    bridge_program_critical_mass,
    'What fraction of the population must actively engage in cross-group trust initiatives for the scaffold sunset to become structural (self-sustaining)?',
    'Analysis of historical bridge-building successes (post-conflict reconciliation, interfaith dialogue scaling, multilingual knowledge access); identification of threshold populations where tipping point occurs',
    'If threshold is low (<15%): scaffold sunset is realistic. If high (>40%): bridge-building coalition is likely to remain marginal and specialized. Affects timeline and confidence in Scaffold classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(bridge_program_critical_mass, empirical, 'Critical mass for bridge-building programs to become self-sustaining').

omega_variable(
    identity_market_counterfactual,
    'Would insular trust persist at current levels if information markets were structured to reward bridge-building instead of in-group affirmation?',
    'Experimental platform design with alternative incentive structures; analysis of communities with different information-market rules (public broadcast vs commercial identity-targeted); historical analysis of periods with lower identity-market monetization',
    'If suppression is primarily market-driven: constraint is fundamentally Tangled Rope with extraction by market vendors and coordinated in-group benefit. If suppression is cultural/evolved: constraint is closer to Rope with market capture as secondary phenomenon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_market_counterfactual, conceptual, 'Whether insular trust requires current market incentives or is culturally autonomous').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(edelman_2026_insularity, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(insula_tr_t0, edelman_2026_insularity, theater_ratio, 0, 0.38).
narrative_ontology:measurement(insula_tr_t5, edelman_2026_insularity, theater_ratio, 5, 0.52).
narrative_ontology:measurement(insula_tr_t10, edelman_2026_insularity, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(insula_be_t0, edelman_2026_insularity, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(insula_be_t5, edelman_2026_insularity, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(insula_be_t10, edelman_2026_insularity, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(edelman_2026_insularity, information_standard).
narrative_ontology:affects_constraint(edelman_2026_insularity, epistemic_fragmentation).
narrative_ontology:affects_constraint(edelman_2026_insularity, algorithmic_sorting_lock_in).
narrative_ontology:affects_constraint(edelman_2026_insularity, identity_market_monopoly).

% DUAL FORMULATION NOTE:
% The insular trust mindset decomposes into three structurally distinct constraints: epistemic_fragmentation (the information-siloing mechanism), algorithmic_sorting_lock_in (the suppression mechanism), and identity_market_monopoly (the extraction mechanism). Each has different ε and χ values. This story treats them as a unified phenomenon seen from multiple perspectives. The decomposed stories track the mechanisms separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(edelman_2026_insularity, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
