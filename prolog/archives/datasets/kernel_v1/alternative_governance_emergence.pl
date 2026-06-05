% ============================================================================
% CONSTRAINT STORY: alternative_governance_emergence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_alternative_governance_emergence, []).

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
 *   constraint_id: alternative_governance_emergence
 *   human_readable: Alternative Governance Emergence Constraint
 *   domain: political/institutional/governance
 *
 * SUMMARY:
 *   The alternative governance emergence constraint represents structural
 *   tension between the proliferation of non-state coordination mechanisms
 *   (mutual aid networks, open-source platforms, participatory budgeting,
 *   liquid democracy tools, decentralized autonomous organizations) and
 *   incumbent institutional responses that range from co-option to active
 *   suppression. This constraint exhibits the full range of Deferential
 *   Realism classifications from different observer positions, making it a
 *   key diagnostic exemplar. At local scale, grassroots actors experience
 *   pure extraction (Snare) — legal and political barriers suppress
 *   coordination without institutional benefit. At regional scale,
 *   alternative networks experience mixed coordination and extraction
 *   (Tangled Rope) — the mechanisms enable collective action but with
 *   asymmetric resource flows. State institutions experience the constraint
 *   primarily as coordination and legitimacy transfer (Rope) — incorporation
 *   of alternatives enhances state capacity without meaningful power
 *   distribution. Decentralized platforms experience enabling coordination
 *   with hidden extraction through network effects and institutional leverage
 *   (Rope). Organized reformers see a temporary problem with regulatory
 *   solutions (Scaffold) — new legal structures (cooperative statutes,
 *   participatory budgeting ordinances) are creating formal pathways for
 *   alternatives. International governance norms endorse participation while
 *   legitimating state monopoly (Piton) — the norm is performative. The
 *   civilizational analytical observer risks naturalizing contingent
 *   institutional arrangements as immutable limits on coordination scale
 *   (Mountain) — but structural decomposition reveals this as false summit.
 *   The constraint's theater_ratio (0.64) reflects substantial performative
 *   content: international endorsements of participatory governance coexist
 *   with minimal power transfer; legal incorporation of alternatives creates
 *   the appearance of pluralism while preserving state authority; platforms
 *   present themselves as neutral infrastructure while accumulating
 *   institutional control.
 *
 * KEY AGENTS:
 *   - Local Grassroots Networks: Primary victims (powerless/trapped) — mutual aid, community organizing, neighborhood coordination; experience suppression through licensing, permitting, legal harassment; bear full extraction costs
 *   - Regional Alternative Governance Movements: Secondary victims (moderate/constrained) — participatory budgeting initiatives, cooperative networks, local democracy experiments; experience mixed coordination and extraction; can exit through formalization or state merger but at high cost
 *   - State Institutions and Regulatory Bodies: Primary beneficiary (institutional/arbitrage) — extract legitimacy transfer, co-opt leaders, distribute governance burden downward; maintain ultimate authority while appearing inclusive; maximum optionality for incorporation, regulation, or suppression
 *   - Decentralized Governance Platforms: Secondary beneficiary (institutional/arbitrage) — accumulate institutional legitimacy and network effects from users; present as neutral infrastructure; extract control through protocol capture and token concentration
 *   - Regulatory Reform Coalition: Organized actors (organized/constrained) — civil society organizations, policy entrepreneurs, professional associations; perceive temporary institutional design problem with legislative solutions; constrained by reform timelines (10-15 years for normalization) and political feasibility
 *   - International Governance Norm Community: Institutional performative (institutional/arbitrage) — UN, OECD, World Bank, regional bodies; endorse participatory governance in declarations and frameworks; extract legitimacy for state structures through appearance of inclusivity; low functional impact on power distribution
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing state monopoly on legitimate coordination as immutable; risks missing contingent institutional arrangements that preserve hierarchy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(alternative_governance_emergence, 0.52).
domain_priors:suppression_score(alternative_governance_emergence, 0.58).
domain_priors:theater_ratio(alternative_governance_emergence, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(alternative_governance_emergence, extractiveness, 0.52).
narrative_ontology:constraint_metric(alternative_governance_emergence, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(alternative_governance_emergence, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(alternative_governance_emergence, tangled_rope).
narrative_ontology:human_readable(alternative_governance_emergence, "Alternative Governance Emergence Constraint").
narrative_ontology:topic_domain(alternative_governance_emergence, "political/institutional/governance").

domain_priors:requires_active_enforcement(alternative_governance_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(alternative_governance_emergence, incumbent_state_institutions).
narrative_ontology:constraint_beneficiary(alternative_governance_emergence, alternative_governance_platforms).
narrative_ontology:constraint_victim(alternative_governance_emergence, participatory_legitimacy).
narrative_ontology:constraint_victim(alternative_governance_emergence, coordination_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PARTICIPATORY COMMONS (SNARE) — Local actors who depend on coordination mechanisms for survival (mutual aid networks, community resource distribution) face high suppression through licensing requirements, permitting barriers, and legal harassment. No legitimate exit exists; the commons bears extraction costs without benefit. Incumbents extract legitimacy from alternative mechanisms without sharing power.
constraint_indexing:constraint_classification(alternative_governance_emergence, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: GRASSROOTS ALTERNATIVE GOVERNANCE (TANGLED ROPE) — Community organizations and networks experience mixed coordination and extraction. The constraint provides coordination function (enables information flow, resource sharing, collective decision-making) but with asymmetric burden: grassroots actors contribute labor and legitimacy; incumbents extract institutional validation and political cover. Exit is costly (loss of coordination benefits, social isolation) but possible through formalization or merger with state structures.
constraint_indexing:constraint_classification(alternative_governance_emergence, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: STATE INCORPORATION APPARATUS (ROPE) — State institutions and regulatory bodies experience the constraint primarily as coordination: legitimacy transfer from alternative mechanisms enhances state capacity, co-option of leaders distributes governance burden downward, formal recognition converts political threats into managed subsidiaries. Extraction is hidden within coordination framing. State actors have maximum arbitrage optionality — they can incorporate, regulate, or suppress.
constraint_indexing:constraint_classification(alternative_governance_emergence, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: OPEN-SOURCE GOVERNANCE PLATFORM (ROPE) — Decentralized coordination platforms (blockchain governance, liquid democracy tools, distributed decision systems) experience the constraint as enabling coordination across jurisdictions. Extraction mechanism: platforms accumulate network effects and institutional legitimacy from users while remaining technically independent. Users perceive utility; platforms perceive governance capture and leverage leverage over adopters. Arbitrage optionality: platforms can pivot toward institutional partnerships, user fees, or protocol capture.
constraint_indexing:constraint_classification(alternative_governance_emergence, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY REFORM COALITION (SCAFFOLD) — Organized reformers (civil society coalitions, policy entrepreneurs, professional associations) perceive the constraint as a temporary institutional design problem with a sunset: legislative frameworks recognizing alternative governance (participatory budgeting statutes, cooperative legal structures, non-profit governance standards) are creating formal pathways that bypass suppression while legitimating coordination. Coalition sees 10-15 year timeline for normalization across advanced democracies. Theater is moderate because reform advocates must perform legitimacy work, but the sunset is visible.
constraint_indexing:constraint_classification(alternative_governance_emergence, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: INTERNATIONAL GOVERNANCE NORM (PITON) — UN, OECD, World Bank, and regional multilateral bodies endorse participatory governance, alternative coordination, and civil society in formal declarations and development frameworks. The norm is substantially performative: actual authority remains concentrated in state structures; international endorsement provides cover for minimal implementation while legitimating incumbent power as 'inclusive.' The norm persists through inertia and institutional prestige despite low functional impact on state behavior.
constraint_indexing:constraint_classification(alternative_governance_emergence, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational/universal scale, hierarchical state monopoly on legitimate coordination appears as an immutable structural feature of complex societies: large-scale coordination requires unified authority, enforcement capacity, and legitimacy concentration. Alternative mechanisms are seen as necessarily temporary, local, or supplementary — they cannot scale to the coordination demands of mass societies. However, the structural data contradicts this naturalization: the constraint's beneficiaries (state institutions, platforms) and victims (grassroots actors, participatory legitimacy) reveal contingent institutional arrangements masquerading as laws of organization.
constraint_indexing:constraint_classification(alternative_governance_emergence, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(alternative_governance_emergence_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(alternative_governance_emergence, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(alternative_governance_emergence, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(alternative_governance_emergence, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(alternative_governance_emergence, TR),
    TR >= 0.70.

:- end_tests(alternative_governance_emergence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint involves substantial resource and authority asymmetry — state institutions and platforms benefit from legitimacy transfer and network effects while grassroots actors bear coordination costs and suppression burdens. However, the extraction is not maximal (0.70+) because genuine coordination benefits exist: alternative mechanisms do enable collective action, solve coordination problems, and create real value for participants. The beneficiaries include both extractive incumbents (state) and genuinely beneficial platforms, reducing average extraction magnitude. The measurement trajectory (0.38 → 0.52) reflects increasing extraction as state co-option mechanisms mature and platforms accumulate control. Suppression (0.58): Moderate-high. Significant barriers include legal restrictions on non-licensed coordination, permitting requirements for collective action, regulatory harassment of alternative platforms, cultural normalization of state authority, and resource scarcity that forces grassroots actors into dependence. Suppression is not maximal (0.85+) because legal pathways exist (cooperatives, nonprofits, participatory budgeting ordinances) and some jurisdictions actively enable alternatives. The measurement trajectory (0.52 → 0.58) reflects increasing enforcement intensity as incumbent institutions perceive threat from scaling alternatives and mount counter-mobilization. Theater ratio (0.64): Moderately high. Performative content includes: international governance norms that endorse participation without power transfer; state incorporation mechanisms that create appearance of pluralism while preserving authority; platform presentations as neutral infrastructure while extracting control; regulatory reform processes that signal responsiveness without enabling genuine alternatives. Theater has increased (0.48 → 0.64) as incumbent institutions developed sophisticated incorporation and legitimation strategies in response to alternative governance growth.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence from unified structural data. The powerless grassroots actor sees Snare — suppression without benefit, extraction without exit. The moderate regional movement sees Tangled Rope — genuine coordination function alongside asymmetric extraction. The state institution sees Rope — pure coordination that enhances state capacity. The platform sees Rope — network effects and growing institutional power experienced as enabling coordination. The reform coalition sees Scaffold — temporary institutional design problem with legislative sunset paths (10-15 years for normalization). The international norm community sees Piton — performative governance endorsement maintaining state authority. The civilizational observer risks seeing Mountain — state monopoly as immutable law of complex societies. The gap reveals the constraint's core mechanism: incumbents benefit from legitimacy transfer while appearing to empower alternatives; alternatives experience mixed coordination and extraction; grassroots actors experience pure extraction. Each agent perceives the constraint through the lens of their exit options and power position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values derive from agent structural position — power level, exit options, and relationship to extraction flow. Powerless grassroots actors with trapped exit options experience maximum directionality (d ≈ 0.95), facing full extraction force through the sigmoid f(d) ≈ 1.42. Moderate grassroots movements with constrained exit experience moderate-high directionality (d ≈ 0.65), producing f(d) ≈ 1.00, intermediate extraction. Institutional state apparatus with arbitrage options experiences minimal directionality (d ≈ 0.10), producing f(d) ≈ -0.05, negative experienced extraction (they are net beneficiaries). The constraint's effective extractiveness χ = ε × f(d) × σ(S) varies substantially across perspectives: for powerless local actors (σ=0.8), χ ≈ 0.52 × 1.42 × 0.8 ≈ 0.59 (snare territory); for moderate regional networks (σ=0.9), χ ≈ 0.52 × 1.00 × 0.9 ≈ 0.47 (tangled_rope); for institutional state (σ=1.0), χ ≈ 0.52 × (-0.05) × 1.0 ≈ -0.03 (rope/coordination perceived). The perspectival gap is extreme: the same constraint appears as pure extraction to trapped locals, mixed coordination to moderate networks, and legitimate coordination to institutional beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The constraint satisfies all three tangled_rope gates: (1) beneficiaries declared (incumbent_state_institutions, alternative_governance_platforms), (2) victims declared (participatory_legitimacy, coordination_commons), (3) active_enforcement required (true). The mandatrophy resolves by recognizing that the constraint simultaneously performs two structurally distinct functions: coordination function (enabling non-state actors to solve collective action problems, share resources, make decisions collectively) and extraction function (asymmetric resource flows favoring incumbents and platforms; legitimacy transfer to state without power redistribution; accumulation of institutional control by platforms without corresponding user benefit). The tangled_rope classification is accurate from the moderate agent perspective. The snare classification from powerless agents reflects that extraction dominates over coordination at that position. The rope classifications from beneficiary perspectives reflect that coordination benefits dominate their experience. The scaffold classification reflects the temporal structure — regulatory reform pathways are creating alternative institutional forms with sunset logic (10-15 year normalization timelines). The piton classification reflects the performative content of international norms. The mountain classification from the analytical observer is a false summit: state monopoly on coordination appears immutable at civilizational scale, but structural analysis reveals contingent institutional arrangements (legal barriers, resource concentration, legitimacy dynamics) that could be reorganized. The mandatrophy is resolved not by choosing one type but by recognizing that all types are legitimate perspectival readings of the constraint's dual function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scale_coordination_threshold,
    'What coordination scale threshold distinguishes genuinely scaling alternative governance from permanently local/supplementary mechanisms?',
    'Historical analysis of successful non-state coordination at state and continental scales (EU subsidiarity, transnational climate networks, global open-source governance); identification of failure modes and sustainability patterns',
    'If scaling beyond 10M agents is structurally possible: mountain classification is false summit (contingent institutional arrangement). If scaling fails at 1M agents: mountain perspective is structurally grounded in coordination physics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scale_coordination_threshold, empirical, 'Whether alternative governance can coordinate beyond local/regional scale').

omega_variable(
    legitimacy_extraction_direction,
    'Does state incorporation of alternative governance mechanisms extract legitimacy FROM grassroots actors TO state institutions, or does it genuinely redistribute power and resources downward?',
    'Longitudinal tracking of power distribution (decision-making authority, resource allocation, agenda-setting) before and after formal incorporation; comparison of outcomes for incorporated vs non-incorporated alternative networks',
    'If extraction dominates: snare/tangled_rope classification confirmed; suppression is rational. If power genuinely distributes: rope classification for participants; state extraction is overstated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_extraction_direction, empirical, 'Direction of legitimacy and power flow in state incorporation of alternatives').

omega_variable(
    platform_neutrality_sustainability,
    'Can decentralized governance platforms remain neutral infrastructure as they accumulate institutional power and user dependence, or do they inevitably extract institutional control?',
    'Case studies of platform protocol capture (Ethereum, DAO governance, liquid democracy implementations); analysis of governance token concentration, decision-making concentration, and user exit costs over time',
    'If platforms remain infrastructure: rope classification sustained; networks experience genuine coordination benefit. If platforms capture: platforms become snares for users; institutional benefits dominate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_neutrality_sustainability, empirical, 'Whether governance platforms can sustain infrastructure neutrality').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is measured suppression structural (legal barriers, enforcement capacity) or internalized (cultural normalization of state authority, delegitimization of alternatives)?',
    'Post-suppression-removal trajectories: if suppression persists after legal/enforcement barriers are removed, reclassify as partially internalized. Cognitive captivity surveys of grassroots actors regarding alternative governance legitimacy.',
    'If internalized: suppression acts as identity lock, and perspectival classifications remain stable. If structural: removal of barriers substantially changes experienced extractiveness and classifications.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').

omega_variable(
    accountability_paradox,
    'Do alternative governance mechanisms provide superior accountability and responsiveness compared to state institutions, or do they simply relocate accountability burdens to participants?',
    'Comparative measurement of decision reversibility, appeal mechanisms, participant exit costs, and outcome responsiveness across alternative networks vs state bureaucracies. Temporal analysis of decision quality and participant satisfaction.',
    'If alternatives genuinely more accountable: snare classification overstates extraction; rope classification more accurate. If accountability burden shifted to participants: extraction mechanism confirmed; snare classification from powerless perspective is correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accountability_paradox, empirical, 'Whether alternatives provide superior accountability or relocate burdens').

omega_variable(
    sunset_clause_enforceability,
    'Are the regulatory reform pathways (participatory budgeting statutes, cooperative legal structures) creating genuine durable alternatives, or are they tokenistic accommodations that preserve state monopoly on legitimate coordination?',
    'Longitudinal analysis of participatory budget implementation across cities; measurement of authority transfer (budget percentage, decision-making scope, reversibility); tracking of statute adoption and enforcement variation across jurisdictions',
    'If reforms are durable with genuine authority transfer: scaffold classification confirmed, sunset is real (10-15 years for normalization). If reforms are tokenistic: scaffold classification is aspirational; state monopoly persists and suppression remains structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_clause_enforceability, empirical, 'Whether regulatory reforms create durable alternatives or tokenistic accommodation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(alternative_governance_emergence, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(altgov_tr_t0, alternative_governance_emergence, theater_ratio, 0, 0.48).
narrative_ontology:measurement(altgov_tr_t5, alternative_governance_emergence, theater_ratio, 5, 0.56).
narrative_ontology:measurement(altgov_tr_t10, alternative_governance_emergence, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(altgov_be_t0, alternative_governance_emergence, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(altgov_be_t5, alternative_governance_emergence, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(altgov_be_t10, alternative_governance_emergence, base_extractiveness, 10, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(altgov_su_t0, alternative_governance_emergence, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(altgov_su_t5, alternative_governance_emergence, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(altgov_su_t10, alternative_governance_emergence, suppression_requirement, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(alternative_governance_emergence, resource_allocation).
narrative_ontology:affects_constraint(alternative_governance_emergence, state_legitimacy_consolidation).
narrative_ontology:affects_constraint(alternative_governance_emergence, platform_governance_capture).
narrative_ontology:affects_constraint(alternative_governance_emergence, grassroots_organizational_capacity).

% DUAL FORMULATION NOTE:
% Alternative governance emergence is a constraint family decomposed into three structurally distinct claims: (1) grassroots coordination mechanisms (ε ≈ 0.65, Snare), (2) decentralized platform governance (ε ≈ 0.48, Tangled Rope), (3) state co-option and incorporation (ε ≈ 0.42, Rope from state perspective). This story aggregates the systemwide constraint; individual stories track specific mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(alternative_governance_emergence, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
