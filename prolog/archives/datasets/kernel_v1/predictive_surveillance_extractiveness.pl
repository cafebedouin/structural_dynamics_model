% ============================================================================
% CONSTRAINT STORY: predictive_surveillance_extractiveness
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_predictive_surveillance_extractiveness, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: predictive_surveillance_extractiveness
 *   human_readable: Predictive Surveillance Extractiveness
 *   domain: technology_governance/surveillance_studies/export_control_policy
 *
 * SUMMARY:
 *   Predictive surveillance systems represent a structural transformation in
 *   state coercion: the shift from reactive monitoring (detecting dissent
 *   after expression) to pre-emptive profiling (predicting dissent before
 *   action). This constraint expands the victim set from those who have taken
 *   observable action to those algorithmically predicted to act, eliminating
 *   the behavioral threshold that historically constrained state
 *   intervention. The primary observable is the ratio of interventions based
 *   on observed behavior versus predicted behavior, and the expansion of the
 *   surveillance target population beyond active dissidents to include
 *   predicted dissidents, false positives, and the chilling effect
 *   population. The constraint exhibits rising extractiveness and suppression
 *   over the measurement interval (2015-2025) as predictive systems mature
 *   from pilot deployments to operational scale. Theater ratio remains
 *   moderate (0.45) because the systems have genuine operational function for
 *   the state security apparatus, unlike purely performative surveillance
 *   rituals. The constraint's mandatrophy is resolved through perspectival
 *   analysis: the state security apparatus experiences coordination (solving
 *   the information problem of threat identification), while the
 *   algorithmically flagged population experiences pure extraction
 *   (suppression of potential dissent with no coordination benefit). The
 *   analytical perspective classifies as snare because the constraint's
 *   primary function is extraction (regime stability as a private good for
 *   the ruling coalition) rather than coordination (public security as a
 *   collective benefit).
 *
 * KEY AGENTS:
 *   - State Security Apparatus: Primary beneficiary (institutional/arbitrage) — captures information advantage and pre-emptive control over dissent; experiences constraint as coordination
 *   - Surveillance Technology Vendors: Secondary beneficiary (institutional/arbitrage) — captures revenue from system deployment and maintenance; global arbitrage exit options
 *   - Algorithmically Flagged Citizens: Primary victim (powerless/trapped) — intervention occurs before any action; no mechanism to contest or exit predictive classification
 *   - False Positive Population: Secondary victim (moderate/constrained) — bears surveillance costs despite no actual dissent intention; can demonstrate compliance but at prohibitive cost
 *   - Chilling Effect Population: Tertiary victim (moderate/constrained) — modifies behavior to avoid algorithmic flagging; experiences mixed extraction-coordination
 *   - Export Control Coalition: Organized actors (organized/constrained) — attempts to restrict surveillance technology proliferation; faces enforcement challenges and state resistance
 *   - Analytical Observer: Civilizational view (analytical/analytical) — identifies structural shift in nature of state power from punishing observed behavior to suppressing predicted behavior
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(predictive_surveillance_extractiveness, 0.78).
domain_priors:suppression_score(predictive_surveillance_extractiveness, 0.82).
domain_priors:theater_ratio(predictive_surveillance_extractiveness, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(predictive_surveillance_extractiveness, extractiveness, 0.78).
narrative_ontology:constraint_metric(predictive_surveillance_extractiveness, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(predictive_surveillance_extractiveness, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(predictive_surveillance_extractiveness, snare).
narrative_ontology:human_readable(predictive_surveillance_extractiveness, "Predictive Surveillance Extractiveness").
narrative_ontology:topic_domain(predictive_surveillance_extractiveness, "technology_governance/surveillance_studies/export_control_policy").

domain_priors:requires_active_enforcement(predictive_surveillance_extractiveness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(predictive_surveillance_extractiveness, state_security_apparatus).
narrative_ontology:constraint_beneficiary(predictive_surveillance_extractiveness, surveillance_technology_vendors).
narrative_ontology:constraint_beneficiary(predictive_surveillance_extractiveness, regime_stability_coalition).
narrative_ontology:constraint_victim(predictive_surveillance_extractiveness, algorithmically_flagged_citizens).
narrative_ontology:constraint_victim(predictive_surveillance_extractiveness, predicted_dissidents).
narrative_ontology:constraint_victim(predictive_surveillance_extractiveness, false_positive_population).
narrative_ontology:constraint_victim(predictive_surveillance_extractiveness, chilling_effect_population).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ALGORITHMICALLY FLAGGED CITIZEN (SNARE) — Trapped by predictive classification with no mechanism to contest or exit. Intervention occurs before any action is taken, based on opaque algorithmic inference. No due process, no observable behavior threshold, no appeal. Maximum extraction: pre-emptive suppression of potential dissent extracts freedom of thought and association before any expression occurs.
constraint_indexing:constraint_classification(predictive_surveillance_extractiveness, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FALSE POSITIVE POPULATION (SNARE) — Citizens flagged by algorithmic error who bear surveillance costs despite no actual dissent intention. Constrained rather than trapped because some can demonstrate compliance through behavioral modification, but the cost of exit (proving algorithmic error to an opaque system) is prohibitive. High extraction: bears full surveillance burden with zero coordination benefit.
constraint_indexing:constraint_classification(predictive_surveillance_extractiveness, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CHILLING EFFECT POPULATION (TANGLED ROPE) — Citizens who modify behavior to avoid algorithmic flagging. Experience both extraction (self-censorship, foregone association) and coordination (predictable boundaries for permissible behavior). Constrained exit: can avoid flagging through compliance but at significant cost to autonomy. Mixed extraction-coordination: the system does communicate regime preferences, but the communication mechanism is coercive.
constraint_indexing:constraint_classification(predictive_surveillance_extractiveness, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE SECURITY APPARATUS (ROPE) — Primary beneficiary. Experiences the constraint as pure coordination: predictive surveillance solves the information problem of identifying threats before they materialize. Arbitrage exit: can adjust algorithmic parameters, exempt personnel, or abandon the system if it becomes politically costly. Net beneficiary with full control over the constraint's operation.
constraint_indexing:constraint_classification(predictive_surveillance_extractiveness, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: SURVEILLANCE TECHNOLOGY VENDORS (ROPE) — Secondary beneficiary. Captures revenue from system deployment and maintenance. Experiences the constraint as coordination: solving the technical problem of predictive classification at scale. Arbitrage exit: can pivot to other markets or clients if one regime becomes reputationally costly. Low effective extraction because benefits exceed costs.
constraint_indexing:constraint_classification(predictive_surveillance_extractiveness, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: EXPORT CONTROL COALITION (TANGLED ROPE) — International actors (Wassenaar Arrangement, EU dual-use regulations, US export controls) attempting to restrict surveillance technology proliferation. Organized but constrained: can impose some barriers but face enforcement challenges, corporate circumvention, and state resistance. Experience both coordination (shared norms against surveillance proliferation) and extraction (compliance costs, diplomatic friction, limited effectiveness). Mixed perspective: the control regime has genuine coordination function but also reveals the limits of international governance against determined state actors.
constraint_indexing:constraint_classification(predictive_surveillance_extractiveness, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From a civilizational perspective, predictive surveillance represents a structural shift in the nature of state power: the move from punishing observed behavior to suppressing predicted behavior eliminates the behavioral threshold that historically constrained state coercion. The analytical classification is snare because the constraint's primary function is extraction (suppression of dissent) with minimal coordination benefit (regime stability is a private good for the ruling coalition, not a public coordination function). The high suppression and extractiveness values, combined with the expansion of the victim set to include those who have taken no action, place this firmly in snare territory across all time horizons.
constraint_indexing:constraint_classification(predictive_surveillance_extractiveness, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(predictive_surveillance_extractiveness_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(predictive_surveillance_extractiveness, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(predictive_surveillance_extractiveness, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(predictive_surveillance_extractiveness, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(predictive_surveillance_extractiveness_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): High. The constraint extracts freedom of thought, association, and expression from the victim population before any observable behavior occurs. The extraction is severe because the behavioral threshold for state intervention has been eliminated or drastically lowered — citizens bear costs (surveillance, intervention, self-censorship) based on algorithmic prediction rather than action. The value reflects that predictive surveillance extracts not just from those who act, but from the much larger population predicted to act, including false positives. Suppression (0.82): Very high. The constraint suppresses alternatives through multiple mechanisms: (1) algorithmic flagging creates pre-emptive intervention before dissent can organize, (2) opacity of algorithmic classification prevents citizens from understanding or contesting their flagged status, (3) chilling effect suppresses expression across the broader population who modify behavior to avoid flagging, (4) lack of due process or behavioral threshold eliminates traditional legal constraints on state coercion. The suppression value is near-maximal because the constraint operates on predicted rather than observed behavior, eliminating the exit option of 'not acting.' Theater ratio (0.45): Moderate. The systems have genuine operational function for the state security apparatus (identifying and suppressing potential dissent before it materializes), unlike purely performative surveillance rituals. However, theater is non-trivial because: (1) false positive rates create interventions against non-threats, (2) some deployments are maintained for regime legitimacy signaling rather than operational effectiveness, (3) export control compliance creates performative documentation burdens. The theater ratio has risen over the interval as systems mature and false positive costs accumulate, but remains below the piton threshold (0.70) because core function persists.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a stark perspectival gap between beneficiaries and victims. The state security apparatus experiences pure coordination (rope): predictive surveillance solves the information problem of identifying threats before they materialize, with full control over system parameters and exemptions. The surveillance technology vendors experience similar coordination (rope): capturing revenue from a technical solution to a client's operational problem, with global arbitrage exit if one market becomes reputationally costly. In contrast, the algorithmically flagged citizens experience pure extraction (snare): pre-emptive suppression based on opaque algorithmic inference, with no behavioral threshold, no due process, and no exit mechanism. The false positive population experiences similar extraction (snare): bearing surveillance costs despite no actual dissent intention, with prohibitive costs to demonstrate algorithmic error. The chilling effect population experiences mixed extraction-coordination (tangled rope): the system does communicate regime preferences (coordination function), but the communication mechanism is coercive and extracts self-censorship costs. The export control coalition experiences mixed coordination-extraction (tangled rope): shared norms against proliferation provide genuine coordination, but enforcement challenges and state resistance create extraction through compliance costs and limited effectiveness. The analytical observer's snare classification reflects the constraint's structural asymmetry: the primary function is extraction (suppression of dissent to preserve regime stability as a private good for the ruling coalition) rather than coordination (public security as a collective benefit). The perspectival gap is not a disagreement about facts but a structural consequence of different positions relative to the extraction flow.
 *
 * DIRECTIONALITY LOGIC:
 *   The state security apparatus and surveillance technology vendors are primary and secondary beneficiaries with arbitrage exit options, yielding low directionality values (d ≈ 0.05-0.15) and negative or near-zero effective extraction. They experience the constraint as coordination: solving the information problem of threat identification (state) or the technical problem of predictive classification at scale (vendors). The algorithmically flagged citizens are primary victims with trapped exit options, yielding very high directionality (d ≈ 0.95) and maximum effective extraction. They cannot exit the predictive classification system and bear full suppression costs with zero coordination benefit. The false positive population and chilling effect population are secondary and tertiary victims with constrained exit options, yielding high directionality (d ≈ 0.75-0.85) and substantial effective extraction. They can modify behavior to reduce flagging probability but at significant cost to autonomy. The export control coalition is organized with constrained exit, yielding moderate directionality (d ≈ 0.55) and moderate effective extraction — they experience both coordination (shared norms against proliferation) and extraction (compliance costs, limited effectiveness). The analytical observer uses the canonical analytical directionality (d ≈ 0.72), producing high effective extraction that reflects the constraint's structural asymmetry: the system extracts from a large victim population to benefit a small ruling coalition.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint resolves the mandatrophy by demonstrating that the snare classification is robust from the analytical perspective despite the state security apparatus experiencing coordination. The key insight is that regime stability is a private good for the ruling coalition, not a public coordination function. The state security apparatus experiences coordination because predictive surveillance solves their information problem (identifying threats before they materialize), but this 'coordination' serves the private interest of regime preservation rather than a collective action problem. The constraint extracts freedom of thought, association, and expression from a large victim population (algorithmically flagged citizens, false positives, chilling effect population) to benefit a small ruling coalition (state security apparatus, surveillance vendors, regime stability coalition). The extraction is severe (ε = 0.78) and the suppression is very high (σ = 0.82) because the behavioral threshold for state intervention has been eliminated — citizens bear costs based on algorithmic prediction rather than action. The analytical perspective's snare classification captures this structural asymmetry: the constraint's primary function is extraction, not coordination, even though the beneficiaries experience it as coordination. The perspectival gap between rope (beneficiary view) and snare (victim and analytical views) is not a classification error but a structural feature of how extraction mechanisms present themselves to those who benefit from them. The mandatrophy is resolved by recognizing that 'coordination for whom?' is the critical question: coordination that serves a private good (regime stability) while extracting from a public (freedom of thought and association) is extraction, not coordination, regardless of how the beneficiaries experience it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_accuracy_threshold,
    'At what false positive rate does predictive surveillance become operationally counterproductive for the state security apparatus itself?',
    'Empirical analysis of deployed systems: ratio of interventions to actual threats materialized; resource burden of processing false positives; political cost of visible errors',
    'If false positive rate exceeds ~30%, the system may shift from snare (functional extraction) to piton (performative theater maintained for regime legitimacy despite operational failure). If false positive rate is below ~10%, the snare classification is robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_accuracy_threshold, empirical, 'False positive rate threshold for operational effectiveness').

omega_variable(
    behavioral_threshold_elimination,
    'Does predictive surveillance eliminate the behavioral threshold for state intervention, or does it merely lower the threshold to include preparatory acts and associations?',
    'Legal and operational analysis: do interventions occur based purely on algorithmic prediction with no observable behavior, or do systems require some minimal behavioral predicate (association, communication, location)?',
    'If threshold is eliminated entirely: snare classification is robust — the constraint extracts freedom of thought itself. If threshold is merely lowered: classification may shift toward tangled_rope for some victim populations who can avoid flagging through behavioral modification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(behavioral_threshold_elimination, empirical, 'Whether predictive systems eliminate or merely lower behavioral thresholds').

omega_variable(
    coordination_function_legitimacy,
    'Is regime stability a genuine public coordination good, or a private good for the ruling coalition that the security apparatus presents as coordination?',
    'Political theory analysis: does the regime provide public goods (security, infrastructure, dispute resolution) that justify stability as coordination, or does it primarily extract rents and suppress alternatives?',
    'If regime stability is a genuine public good: classification shifts toward tangled_rope (mixed coordination-extraction). If regime stability is a private good: snare classification is robust — the constraint extracts dissent to preserve elite benefits, not to solve a collective action problem.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_function_legitimacy, preference, 'Whether regime stability constitutes public coordination or private extraction').

omega_variable(
    export_control_effectiveness,
    'Do export controls on surveillance technology meaningfully constrain proliferation, or do they primarily create compliance theater while determined state actors acquire capabilities through alternative channels?',
    'Empirical tracking: correlation between export control regimes and actual deployment of predictive surveillance systems; identification of circumvention pathways (domestic development, non-signatory suppliers, dual-use technology repurposing)',
    'If export controls are effective: the organized perspective (export control coalition) shifts toward scaffold (temporary coordination with sunset as norms mature). If export controls are theater: the organized perspective remains tangled_rope or shifts toward piton (performative compliance with minimal functional constraint).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(export_control_effectiveness, empirical, 'Effectiveness of export control regimes in constraining surveillance proliferation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(predictive_surveillance_extractiveness, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pred_surv_theater_t0, predictive_surveillance_extractiveness, theater_ratio, 0, 0.35).
narrative_ontology:measurement(pred_surv_theater_t3, predictive_surveillance_extractiveness, theater_ratio, 3, 0.38).
narrative_ontology:measurement(pred_surv_theater_t6, predictive_surveillance_extractiveness, theater_ratio, 6, 0.42).
narrative_ontology:measurement(pred_surv_theater_t10, predictive_surveillance_extractiveness, theater_ratio, 10, 0.45).

% Extraction over time
narrative_ontology:measurement(pred_surv_extract_t0, predictive_surveillance_extractiveness, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(pred_surv_extract_t3, predictive_surveillance_extractiveness, base_extractiveness, 3, 0.61).
narrative_ontology:measurement(pred_surv_extract_t6, predictive_surveillance_extractiveness, base_extractiveness, 6, 0.7).
narrative_ontology:measurement(pred_surv_extract_t10, predictive_surveillance_extractiveness, base_extractiveness, 10, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(pred_surv_suppress_t0, predictive_surveillance_extractiveness, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(pred_surv_suppress_t3, predictive_surveillance_extractiveness, suppression_requirement, 3, 0.68).
narrative_ontology:measurement(pred_surv_suppress_t6, predictive_surveillance_extractiveness, suppression_requirement, 6, 0.76).
narrative_ontology:measurement(pred_surv_suppress_t10, predictive_surveillance_extractiveness, suppression_requirement, 10, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(predictive_surveillance_extractiveness, enforcement_mechanism).
narrative_ontology:affects_constraint(predictive_surveillance_extractiveness, social_credit_systems).
narrative_ontology:affects_constraint(predictive_surveillance_extractiveness, biometric_identification_mandates).
narrative_ontology:affects_constraint(predictive_surveillance_extractiveness, encrypted_communication_restrictions).
narrative_ontology:affects_constraint(predictive_surveillance_extractiveness, algorithmic_content_moderation).

% DUAL FORMULATION NOTE:
% Predictive surveillance is structurally distinct from reactive surveillance (monitoring observed behavior) and from social credit systems (scoring observed behavior for resource allocation). The ε-invariance principle applies: reactive surveillance has lower extractiveness (ε ≈ 0.45-0.55) because it operates on a behavioral threshold, while predictive surveillance has higher extractiveness (ε ≈ 0.70-0.85) because it eliminates the threshold. Social credit systems have different victim sets (those with low scores based on observed behavior) and different extraction mechanisms (resource denial rather than pre-emptive suppression). These are separate constraints linked by network effects: predictive surveillance systems often feed social credit scoring, and both rely on biometric identification infrastructure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
