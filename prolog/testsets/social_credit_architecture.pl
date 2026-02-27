% ============================================================================
% CONSTRAINT STORY: social_credit_architecture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_social_credit_architecture, []).

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
 *   constraint_id: social_credit_architecture
 *   human_readable: Social Credit Architecture
 *   domain: social/political/technological
 *
 * SUMMARY:
 *   Social credit architecture operationalizes 'trustworthiness' by encoding
 *   behavioral data into unified scores that determine access to credit,
 *   employment, travel, education, and social services. The constraint
 *   exhibits multiple classification perspectives depending on structural
 *   position. For trapped citizens, it is pure extraction (Snare) — total
 *   behavioral colonization with zero exit options. For the state apparatus,
 *   it is coordination infrastructure (Rope) — solving the epistemic problem
 *   of monitoring large populations. For tech vendors, it is mixed
 *   coordination and extraction (Tangled Rope) — genuine governance tools
 *   plus regulatory capture. For advocacy coalitions, it is degraded
 *   resistance theater (Piton) — organized opposition that makes the system
 *   appear contestable while extraction mechanisms persist unchanged. The
 *   false summit (Mountain) perspective reveals how technological
 *   inevitability frames can naturalize what is actually a constructed
 *   institutional arrangement with substantial policy contingency. The
 *   constraint's extractiveness has increased over the interval (0.42 → 0.68)
 *   as the system expanded from credit assessment to employment, travel, and
 *   civic participation domains. Theater ratio increased (0.35 → 0.58) as
 *   implementation shifted from pure scoring to elaborate appeal procedures
 *   and publicity around 'credit redemption' narratives, which are largely
 *   performative — they give the appearance of contestability while
 *   substantively changing few scores.
 *
 * KEY AGENTS:
 *   - Unscored Citizens: Primary victim (powerless/trapped) — entire population subject to behavioral encoding with no opt-out, cascading restrictions based on algorithmic scores
 *   - Marginalized Groups: Secondary victim (moderate/constrained) — disproportionately disadvantaged by algorithmic bias, historical disparities in training data, limited appeal mechanisms
 *   - Privacy Commons: Abstract victim — collective epistemic good of behavioral privacy degraded by normalized surveillance infrastructure
 *   - State Apparatus: Primary beneficiary (institutional/arbitrage) — achieves behavioral transparency and standardized enforcement across large population
 *   - Compliant Enterprise: Secondary beneficiary (institutional/arbitrage) — preferential access to credit, customers, contracts in exchange for compliance with behavioral reporting
 *   - Tech Implementation Sector: Hybrid actor (powerful/mobile) — benefit from sustained contracts and lock-in; bear extraction through regulatory capture and feature demands
 *   - Advocacy Coalition: Organized but constrained (organized/constrained) — perform resistance through campaigns and legal challenges; constrained by system momentum and state control
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as inevitable features of scaled governance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(social_credit_architecture, 0.68).
domain_priors:suppression_score(social_credit_architecture, 0.75).
domain_priors:theater_ratio(social_credit_architecture, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(social_credit_architecture, extractiveness, 0.68).
narrative_ontology:constraint_metric(social_credit_architecture, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(social_credit_architecture, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(social_credit_architecture, snare).
narrative_ontology:human_readable(social_credit_architecture, "Social Credit Architecture").
narrative_ontology:topic_domain(social_credit_architecture, "social/political/technological").

domain_priors:requires_active_enforcement(social_credit_architecture).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(social_credit_architecture, state_apparatus).
narrative_ontology:constraint_beneficiary(social_credit_architecture, system_administrators).
narrative_ontology:constraint_beneficiary(social_credit_architecture, compliant_enterprises).
narrative_ontology:constraint_victim(social_credit_architecture, unscored_population).
narrative_ontology:constraint_victim(social_credit_architecture, marginalized_groups).
narrative_ontology:constraint_victim(social_credit_architecture, privacy_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNSCORED CITIZEN (SNARE) — Trapped within the system's behavioral encoding. Cannot opt out of data collection or score calculation. Subject to cascading restrictions (credit denial, employment barriers, travel prohibition, education access) based on algorithmic determinations. Zero exit options. Maximum experienced extraction as the constraint colonizes all dimensions of economic and social participation.
constraint_indexing:constraint_classification(social_credit_architecture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MARGINALIZED GROUP (SNARE) — Systematically disadvantaged by scoring algorithms (historical bias in training data, algorithmic amplification of existing inequalities). Cannot appeal or meaningfully contest scores. Constrained exit options — some mobility through migration or informal economy, but at severe cost. Extraction compounds existing vulnerabilities.
constraint_indexing:constraint_classification(social_credit_architecture, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE APPARATUS (ROPE) — Primary beneficiary. The system enables coordination of governance through behavioral transparency and standardized enforcement. Experiences the constraint as a coordination mechanism: aggregating behavioral data solves the state's epistemic problem of monitoring large populations. Net extraction flow toward this agent. High arbitrage (can exit through policy revision).
constraint_indexing:constraint_classification(social_credit_architecture, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: COMPLIANT ENTERPRISE (ROPE) — Benefits from preferential access to credit, customers, and state contracts in exchange for compliance with behavioral reporting and enforcement. Sees the constraint as coordination infrastructure. Extraction flows toward this agent through regulatory advantage. High arbitrage (can exit through non-compliance, though costly).
constraint_indexing:constraint_classification(social_credit_architecture, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: TECH IMPLEMENTATION SECTOR (TANGLED ROPE) — Firms that build and maintain scoring systems. Benefit from sustained contracts and technical lock-in (difficult to switch vendors). Also bear extraction through regulatory capture: state increasingly demands features and algorithmic transparency, constraining design freedom. Mixed experience: genuine coordination (providing governance tools) plus asymmetric extraction (forced feature development). Mobile enough to exit but facing reputational and regulatory barriers.
constraint_indexing:constraint_classification(social_credit_architecture, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ADVOCACY COALITION (PITON) — Civil society organizations, privacy advocates, and international NGOs. Organized but constrained by the system's momentum and state control of data access. Perform resistance through public campaigns, legal challenges, and norm-setting, but these efforts are largely theatrical — the system persists regardless because state incentives are strong and exit costs for non-compliance are prohibitive. Theater ratio high because advocacy makes the system appear contestable while extraction mechanisms remain fundamentally unchanged.
constraint_indexing:constraint_classification(social_credit_architecture, piton,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN — FALSE SUMMIT) — From civilizational distance, one might frame social credit as an immutable feature of scaled governance: large populations require coordination mechanisms, and behavioral data is the only available signal for trustworthiness assessment. This perspective naturalizes the system as an inevitable law of social order. However, the structural data contradicts the mountain classification — active enforcement, beneficiary capture, suppression mechanisms, and policy contingency all indicate this is not a natural law but a constructed institutional arrangement. The false summit reveals how technological determinism masks extractive architectural choices.
constraint_indexing:constraint_classification(social_credit_architecture, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(social_credit_architecture_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(social_credit_architecture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(social_credit_architecture, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(social_credit_architecture, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(social_credit_architecture, TR),
    TR >= 0.70.

:- end_tests(social_credit_architecture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The state captures behavioral data and converts it into unilateral control over citizen access to credit, employment, travel, and services. Citizens have no reciprocal scoring right over the state. The system extracts behavioral compliance value (citizens modify behavior to improve scores) and economic value (restricted access creates rents captured by compliant enterprises). Extractiveness increased from 0.42 to 0.68 as the system expanded from initial credit-only scope to employment and travel domains. Suppression (0.75): High. Citizens cannot meaningfully opt out (participation is mandatory for economic participation), cannot access the algorithms (technical opacity prevents contestation), and face severe penalties for non-compliance (credit denial, employment barriers, travel prohibition). Appeal mechanisms exist but are procedurally elaborate and have low success rates — they function as theater rather than meaningful exit. Suppression increased with system maturation and expansion of enforcement domains. Theater ratio (0.58): Moderate-high. Implementation includes high-visibility 'credit redemption' narratives (publicity around individuals who improved scores, civic participation programs that claim to raise creditworthiness) that frame the system as fair and contestable. Appeal procedures are elaborate and publicized. However, underlying extraction mechanisms (algorithmic opacity, low appeal success rates, behavioral compliance pressure) remain unchanged. Theater has increased as the system has matured and faces growing public criticism.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximal perspectival divergence. Trapped citizens see pure extraction (Snare) — the constraint is a mechanism for state control of their economic participation. The state apparatus sees coordination infrastructure (Rope) — the constraint solves the legitimate problem of assessing trustworthiness at scale. Tech vendors see mixed coordination and extraction (Tangled Rope) — they provide genuine governance tools but face regulatory capture that constrains their design freedom. Marginalized groups see concentrated extraction (Snare at higher intensity than general population) due to algorithmic bias feedback loops. Advocacy coalitions see degraded resistance (Piton) — their public campaigns and legal challenges make the system appear contestable but substantively preserve extraction mechanisms. The analytical observer risks naturalizing the system as a law of social order (Mountain — false summit) rather than recognizing it as a constructed institutional arrangement with substantial policy contingency. This perspectival divergence is extreme because the beneficiaries (state, compliant enterprises) and victims (citizens, marginalized groups) occupy completely different structural positions with nearly opposite exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from the agent's structural relationship to extraction flows. Trapped citizens (powerless/trapped) experience maximum extraction — they bear full costs of scoring with zero exit options, deriving d ≈ 0.95. Marginalized groups (moderate/constrained) experience high extraction with some theoretical exit (migration, informal economy) but at prohibitive cost, deriving d ≈ 0.70. State apparatus (institutional/arbitrage) benefits from behavioral transparency and standardized enforcement, deriving d ≈ 0.10 (beneficiary with exit). Compliant enterprises (institutional/arbitrage) benefit from preferential access and regulatory advantage, deriving d ≈ 0.15. Tech sector (powerful/mobile) experiences mixed extraction (regulatory capture) and benefits (contracts), deriving d ≈ 0.55 (moderate position between beneficiary and victim). Advocacy coalition (organized/constrained) performs resistance but lacks structural power to change system, deriving d ≈ 0.45 (moderate victim without full exit). The analytical observer uses the canonical d ≈ 0.73 for analytical power.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint exemplifies mandatrophy because it is simultaneously a coordination mechanism (providing the state with tools to assess trustworthiness at scale) and pure extraction (colonizing citizen behavior and restricting economic participation). The mandate is that social credit 'improves governance efficiency' (coordination framing) and 'incentivizes prosocial behavior' (behavioral improvement framing). Mandatrophy resolves by recognizing that the coordination function is real but drastically insufficient to justify the extraction cost. The state could achieve similar governance efficiency through alternative coordination mechanisms (transparent rules-based credit assessment, participatory algorithm design, federated scoring with citizen appeal rights) that do not require behavioral colonization. The mandate for coordination does not entail the mandate for this specific extractive architecture. Therefore, classification as Snare is justified despite the genuine coordination function — the extraction mechanism is not a necessary feature of coordination but a contingent choice that serves state and enterprise interests. Mandatrophy resolved at high confidence: the system's extractiveness (0.68) and suppression (0.75) substantially exceed what would be required for the coordination function alone, indicating that extraction is a primary structural feature, not a necessary side-effect of coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_bias_feedback,
    'Does the scoring algorithm amplify existing inequalities through feedback loops, or does it merely reflect pre-existing social stratification?',
    'Longitudinal statistical analysis comparing score changes within demographic cohorts before/after algorithm updates; causality testing for algorithmic features vs historical background variables; synthetic fairness audits comparing counterfactual scores under alternative algorithms',
    'If algorithmic amplification is primary: constraint is constructed exploitation (Snare with high policy contingency). If algorithm merely reflects stratification: constraint naturalizes injustice but did not create it (shifts from Snare toward Mountain, though structural data prevents true mountain classification).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_bias_feedback, empirical, 'Whether scoring algorithm amplifies inequalities or reflects pre-existing stratification').

omega_variable(
    appeal_mechanism_effectiveness,
    'Can citizens meaningfully contest and overturn adverse credit scores through administrative or legal appeal, or are appeals largely procedural theater?',
    'Analysis of appeal success rates, reversal rates for successfully appealed scores, time-to-resolution for appeals, comparison with baseline false-positive rates in training data, interview data from appeal process participants',
    'If appeals are genuinely effective: suppression index drops (citizens have meaningful exit option), chi calculation changes, some perspectives might reclassify from Snare to Tangled Rope. If appeals are theater: confirms suppression ≥ 0.75 and validates Snare classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(appeal_mechanism_effectiveness, empirical, 'Effectiveness of appeal mechanisms for contesting credit scores').

omega_variable(
    data_accuracy_degradation,
    'Are behavioral data inputs to the scoring system sufficiently accurate and timely for fair assessment, or do they systematically encode stale, incomplete, or erroneous information?',
    'Audit of data entry errors, missing data rates, update latencies, and reconciliation failures across score components; comparison of recorded behavior vs individual attestations; analysis of how data quality varies by geography and socioeconomic status',
    'If data quality is high: suppression justified by accuracy. If quality is low: suppression is amplified by noise (citizens cannot effectively contest scores because scoring logic is opaque), increasing actual harm despite lower structural suppression. Shifts mandatrophy framing from ''system is unfair'' to ''system is broken''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(data_accuracy_degradation, empirical, 'Data quality and accuracy in behavioral scoring inputs').

omega_variable(
    state_capacity_constraint,
    'Is the social credit system technically feasible for the state apparatus to administer fairly and scalably, or does it exceed state capacity, degrading into arbitrary or corrupted enforcement?',
    'Administrative capacity audits, analysis of implementation delays and technical failures, case studies of corruption or arbitrary enforcement by local administrators, comparison of scoring consistency across regions',
    'If technically feasible: system remains a coherent Snare (extraction mechanism is stable). If capacity-constrained: enforcement becomes arbitrary and inconsistent, which paradoxically might reduce *average* extraction (because unpredictability raises exit costs unevenly) but increases *experienced* suppression for those who do bear enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_capacity_constraint, empirical, 'State administrative capacity to implement social credit fairly and scalably').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(social_credit_architecture, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(socred_tr_t0, social_credit_architecture, theater_ratio, 0, 0.35).
narrative_ontology:measurement(socred_tr_t5, social_credit_architecture, theater_ratio, 5, 0.48).
narrative_ontology:measurement(socred_tr_t10, social_credit_architecture, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(socred_be_t0, social_credit_architecture, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(socred_be_t5, social_credit_architecture, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(socred_be_t10, social_credit_architecture, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(social_credit_architecture, enforcement_mechanism).
narrative_ontology:affects_constraint(social_credit_architecture, algorithmic_transparency_bottleneck).
narrative_ontology:affects_constraint(social_credit_architecture, behavioral_surveillance_infrastructure).
narrative_ontology:affects_constraint(social_credit_architecture, credit_access_inequality).

% DUAL FORMULATION NOTE:
% Social credit architecture decomposes into three related constraints: (1) the system itself as an enforcement mechanism (this story), (2) the algorithmic opacity that prevents meaningful appeal (algorithmic_transparency_bottleneck, higher ε), and (3) the surveillance infrastructure that enables behavioral data collection (behavioral_surveillance_infrastructure, moderate ε). The system story represents the coordination-extraction hybrid; the transparency bottleneck represents the structural barrier to contestation; the surveillance infrastructure represents the technological precondition. All three are necessary for the extraction to function at current intensity. Removing any one would degrade extractiveness.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(social_credit_architecture, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
