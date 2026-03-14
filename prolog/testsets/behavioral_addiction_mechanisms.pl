% ============================================================================
% CONSTRAINT STORY: behavioral_addiction_mechanisms
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_behavioral_addiction_mechanisms, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: behavioral_addiction_mechanisms
 *   human_readable: Behavioral Addiction Mechanisms: Neurological Lock-In and Extraction
 *   domain: neuroscience/psychology/behavioral_health
 *
 * SUMMARY:
 *   Behavioral addiction mechanisms represent a structurally pure snare: the
 *   constraint extracts time, attention, and wellbeing from addiction-prone
 *   individuals while providing minimal coordination benefit beyond the
 *   addictive reward loop itself. The mechanism operates through engineered
 *   exploitation of neurological feedback systems — intermittent
 *   reinforcement schedules that hijack dopamine regulation — creating
 *   identity-level binding that makes exit structurally possible but
 *   psychologically inaccessible. The constraint exhibits high suppression
 *   (0.72) through both structural barriers (switching costs, social network
 *   dependency) and internalized barriers (identity fusion, cognitive
 *   capture). The theater_ratio (0.55) reflects moderate performative content
 *   in regulatory responses (warnings, age gates, self-exclusion options)
 *   that exist without functional intervention capacity. The extractiveness
 *   trajectory shows accumulation over the interval (0.35 → 0.68) as design
 *   techniques mature and user bases expand, indicating that the extraction
 *   mechanism is not stable but deliberately escalating. This constraint
 *   demonstrates why identity_locked exit options are distinct from trapped
 *   or constrained: the addicted user could materially exit (delete the app,
 *   change routines) but cannot exit from within their identity frame because
 *   their self-concept is constituted through the addictive behavior. The
 *   classification resolves the mandatrophy by showing that the design
 *   operators' rope perspective is not a legitimate alternative
 *   classification but a beneficiary's misreading of extraction as
 *   coordination — what they call 'engagement optimization' victims
 *   experience as addiction capture.
 *
 * KEY AGENTS:
 *   - Addiction-Prone Individuals: Primary victims (powerless/identity_locked) — structurally mobile but identity-fused with addictive behavior; bear maximum extraction cost
 *   - Chronic Users: Primary victims (powerless/trapped) — face high switching costs and network dependency; cannot exit without material cost
 *   - Behavior Design Operators: Primary beneficiaries (institutional/arbitrage) — architects of reinforcement schedules; capture engagement metrics and monetized attention
 *   - Platform Incentive Architects: Primary beneficiaries (institutional/arbitrage) — design the constraint to extract sustained engagement; experience as coordination
 *   - Casual Users: Secondary actors (moderate/constrained) — use the platform without addiction; experience tangled mix of coordination and extraction
 *   - Regulatory Agencies: Institutional performer (institutional/arbitrage) — maintain theater of regulation without functional intervention capacity
 *   - Addiction Recovery Ecosystem: Organized victim (organized/constrained) — organized response to constraint but underfunded and resource-constrained
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(behavioral_addiction_mechanisms, 0.68).
domain_priors:suppression_score(behavioral_addiction_mechanisms, 0.72).
domain_priors:theater_ratio(behavioral_addiction_mechanisms, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(behavioral_addiction_mechanisms, extractiveness, 0.68).
narrative_ontology:constraint_metric(behavioral_addiction_mechanisms, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(behavioral_addiction_mechanisms, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(behavioral_addiction_mechanisms, snare).
narrative_ontology:human_readable(behavioral_addiction_mechanisms, "Behavioral Addiction Mechanisms: Neurological Lock-In and Extraction").
narrative_ontology:topic_domain(behavioral_addiction_mechanisms, "neuroscience/psychology/behavioral_health").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(behavioral_addiction_mechanisms, behavior_design_operators).
narrative_ontology:constraint_beneficiary(behavioral_addiction_mechanisms, platform_incentive_architects).
narrative_ontology:constraint_victim(behavioral_addiction_mechanisms, addiction_prone_individuals).
narrative_ontology:constraint_victim(behavioral_addiction_mechanisms, chronic_users).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ADDICTED USER (SNARE) — Structurally mobile (could delete apps, change routines) but identity-fused with the behavioral pattern. The addiction is constituted through their identity: 'I am someone who games/scrolls/trades.' Exit requires abandoning not just the behavior but the identity frame, making structural mobility inaccessible from within the addiction's cognitive architecture. Maximum experienced extraction with internalized suppression.
constraint_indexing:constraint_classification(behavioral_addiction_mechanisms, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: BEHAVIOR DESIGN OPERATORS (ROPE) — Institutional actors benefit from sustained engagement. Experience the constraint as coordination: retention metrics, engagement loops, and reinforcement schedules are solving the legitimate problem of user engagement. The design operators see this as a coordination solution and experience low or negative extraction cost. Arbitrage options (migrate users to other platforms, shift engagement models) give them flexibility.
constraint_indexing:constraint_classification(behavioral_addiction_mechanisms, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: CASUAL USER (TANGLED ROPE) — Not identity-locked; can exit at high cost (social friction, status loss, reduced access to network). Constrained by social dependency and switching costs rather than neurological capture. Experiences both genuine coordination benefits (social connection, entertainment access) and extraction (time drain, attention manipulation). Mixed experience with agency.
constraint_indexing:constraint_classification(behavioral_addiction_mechanisms, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: NEUROSCIENCE VIEW / NATURAL LAW (MOUNTAIN — FALSE) — Tempting but incorrect: behavioral addiction appears to be an immutable property of dopamine-feedback mechanisms, therefore a natural law. The engine's false summit detector will flag this — the constraint is not natural law but engineered exploitation of existing neurological structures. The framing naturalizes what is actually design choice.
constraint_indexing:constraint_classification(behavioral_addiction_mechanisms, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 5: REGULATORY THEATER (PITON) — Regulatory frameworks (screen time warnings, age gates, self-exclusion options) are substantially performative — they exist on interfaces but do not meaningfully prevent addictive use. The regulation persists through institutional inertia (required by law/policy) but its primary function has atrophied. Users ignore warnings; age gates are trivially bypassed; self-exclusion is rarely invoked. The theater_ratio is elevated because compliance is maintained through checkbox compliance rather than functional intervention.
constraint_indexing:constraint_classification(behavioral_addiction_mechanisms, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ADDICTION RECOVERY ECOSYSTEM (SNARE) — Treatment and recovery infrastructure faces structural extraction: addiction prevention is underfunded relative to addiction-enabling profits. The recovery ecosystem is organized (support groups, therapy, rehab programs) but constrained by resource scarcity and institutional resistance. Sees the addiction mechanism as exploitative extraction with minimal coordination benefit. Has agency to organize but faces asymmetric power against design operators.
constraint_indexing:constraint_classification(behavioral_addiction_mechanisms, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(behavioral_addiction_mechanisms_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(behavioral_addiction_mechanisms, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(behavioral_addiction_mechanisms, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(behavioral_addiction_mechanisms, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(behavioral_addiction_mechanisms, TR),
    TR >= 0.70.

:- end_tests(behavioral_addiction_mechanisms_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts sustained engagement, attention duration, and wellbeing from addiction-prone individuals. Measurement basis: harm-inflicted observable. Alternative measurements (time stolen, data monetized) yield slightly lower values (0.50–0.65), indicating that decomposition may be warranted. The 0.68 figure reflects the wellbeing cost to victims, which is the most asymmetric extraction observable. Suppression (0.72): High. Multiple suppression mechanisms: (1) structural barriers — switching costs, network effects, social dependency; (2) internalized suppression — identity fusion makes exit cognitively inaccessible; (3) cognitive capture — beliefs about necessity and benefits maintain engagement; (4) informational barriers — most users lack clear awareness of the mechanism's extractive intent. Theater ratio (0.55): Moderate-high. Regulatory responses are substantially performative: age-gate systems are trivially bypassed, screen time warnings are ignored, self-exclusion options are rarely invoked. Regulatory theater increases over the interval as regulatory pressure grows but intervention capacity lags. The theater is not as high as (0.70+) because some platforms show genuine functionality (tighter age enforcement in some jurisdictions), preventing full piton classification of the regulatory response.
 *
 * PERSPECTIVAL GAP:
 *   The fundamental perspectival gap is between the design operators' experience of coordination-via-engagement-optimization and the victims' experience of extraction-via-neurological-hijacking. This is a gap between beneficiary and victim positions, not merely different contexts observing the same phenomenon. The analytical observer risks a false mountain classification by attributing the mechanism to immutable dopamine systems — but this naturalizes what is actually deliberate design choice. The regulatory theater perspective exposes how institutional responses can be substantially performative: compliance (posting warnings, implementing age gates) without functional intervention. The casual user's tangled_rope perspective is diagnostically important because it shows the constraint operates on a spectrum: users with lower addiction vulnerability experience genuine benefits (social connection, entertainment access) alongside extraction (attention loss, time drain). The piton classification of regulatory theater is not an indictment of regulators but an observation that oversight mechanisms have not yet scaled to match the sophistication of design operators.
 *
 * DIRECTIONALITY LOGIC:
 *   The addicted user's high directionality value (d ≈ 0.89) derives from their structural position as a victim with identity_locked exit options. The sigmoid f(d) translates this high d into maximum experienced extractiveness chi. This is not because the base extractiveness is unlimited — ε=0.68 is bounded — but because the victim's inability to exit cognitively amplifies the effective extraction they experience. The design operators' low directionality value (d ≈ 0.08) reflects their beneficiary status and arbitrage exit options: they can shift to alternative engagement models, migrate user cohorts, or adjust reinforcement schedules without material cost. The casual user's moderate d (≈ 0.55) reflects their constrained exit options and mixed benefit/cost experience. The recovery ecosystem's d (≈ 0.70) reflects their position as organized victims with some agency but constrained by resource asymmetry. Directionality overrides are not needed — the structural derivation produces coherent d values across perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED via snare classification with identity_locked binding. The mandatrophy resolves by demonstrating that the design operators' rope perspective is a beneficiary's mischaracterization, not a legitimate alternative classification. From the design operators' position, the constraint is genuinely coordination: they are solving the problem of user engagement, and engagement metrics improve. But this 'coordination' is asymmetric — the cost is externalized onto victims, particularly addiction-prone individuals. The snare classification captures this asymmetry: high extractiveness (0.68), high suppression (0.72), minimal coordination function (high theater_ratio). The identity_locked exit option for victims is the key diagnostic: if the constraint were simply a constrained choice (high costs to exit), the classification would be tangled_rope. The identity-level binding (victims cannot imagine themselves outside the addictive identity) elevates this to snare. The mandatrophy is further resolved by the recovery ecosystem perspective (perspective 6), which shows that organized counter-extraction exists but remains resource-constrained — not yet powerful enough to transform the constraint into a contested tangled_rope. The false mountain perspective (perspective 4) is flagged by the engine's false summit detector: the temptation to naturalize the mechanism as inherent to dopamine systems is revealed as misattribution when structural decomposition shows the constraint is engineered design choice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_structural_vs_internalized,
    'Is the addicted user''s inability to exit structural (material barriers make exit costly) or internalized (the identity frame makes exit unthinkable)?',
    'Longitudinal post-abstinence analysis: measure suppression persistence after the addictive stimulus is removed. If suppression persists (continued craving, identity dysphoria without the stimulus), reclassify as partially internalized.',
    'If purely structural: reclassify exit_options from identity_locked to trapped or constrained, lowering d and weakening the snare classification. If internalized: classification confirmed — the constraint travels with the agent even after stimulus removal, indicating identity-level binding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_structural_vs_internalized, empirical, 'Whether suppression is structural or internalized in behavioral addiction').

omega_variable(
    dopamine_reinforcement_schedule_design_vs_nature,
    'To what extent are intermittent reinforcement effects (variable reward schedules) consequences of evolved dopamine systems versus deliberate design choices by behavior architects?',
    'Comparative platform analysis: measure engagement metrics on identical reward schedules (fixed-ratio, variable-ratio, fixed-interval, variable-interval) across platforms with and without active reinforcement optimization. Cross-species analysis: compare behavioral addiction rates in environments with naturalistic vs engineered intermittent rewards.',
    'If primarily evolved: constraint is closer to natural law (mountain). If engineered: constraint is architectural choice (snare classification confirmed). Mixed: snare that exploits evolved vulnerabilities.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dopamine_reinforcement_schedule_design_vs_nature, empirical, 'Whether addiction mechanisms reflect evolved dopamine systems or deliberate design').

omega_variable(
    cognitive_capture_vs_neurological_binding,
    'Does the addiction mechanism operate primarily through cognitive capture (the user''s beliefs about the behavior keep them engaged) or neurological binding (dopamine dysregulation makes exit unthinkable regardless of beliefs)?',
    'Cognitive intervention trials: measure abstinence rates for groups receiving belief-challenging interventions (therapy, peer testimony) versus groups receiving only neurological support (medication, structured environment). Analyze relapse patterns.',
    'If cognitive: shorter path to recovery through reframing. If neurological: recovery requires longer intervention and more intensive support. Classification unchanged (snare) but the mechanism''s accessibility to intervention shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_capture_vs_neurological_binding, empirical, 'Whether addiction operates through cognitive or neurological mechanisms').

omega_variable(
    extractiveness_measurement_observable_ambiguity,
    'Should extractiveness be measured by time stolen, value extracted (monetized attention/data), or harm inflicted (wellbeing cost)? These observables yield different ε values.',
    'Decomposition: time-extraction constraint (ε=0.50), data-extraction constraint (ε=0.65), harm constraint (ε=0.72 — the story''s current basis). Verify that each decomposed constraint has internally consistent beneficiary/victim declarations and produces stable classification across perspectives.',
    'Current story uses harm-based ε (0.68). Time-based decomposition would be Tangled Rope. Data-based decomposition would be Snare at lower intensity. Three separate constraint stories linked via network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extractiveness_measurement_observable_ambiguity, conceptual, 'Observable-dependent extractiveness measurement ambiguity').

omega_variable(
    regulatory_theater_sunset_timeline,
    'Are regulatory theater mechanisms (warnings, age gates, self-exclusion) genuinely degraded and inert, or are they early-stage scaffolding approaching functional maturity?',
    '10-year longitudinal evaluation: do stronger regulatory mechanisms (EU Digital Services Act, screen time legislation) show effectiveness trends or continued theater? Do sophisticated enforcement mechanisms emerge that scale beyond individual choice?',
    'If degraded: piton classification confirmed. If emerging: reclassify regulatory perspective as scaffold, suggesting structural exit path. Classification shifts from pure snare to snare-with-growing-pressure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_theater_sunset_timeline, empirical, 'Regulatory theater degradation versus early-stage scaffold emergence').

omega_variable(
    recovery_ecosystem_coalition_threshold,
    'At what threshold of organized recovery infrastructure does the addiction constraint transition from pure snare to a contested tangled_rope with meaningful coordination-against-extraction?',
    'Network analysis of recovery ecosystem scale: measure funding ratios (addiction prevention vs addiction-enabling), institutional coordination strength, capacity relative to addiction incidence. Measure success rates for organized intervention.',
    'If ecosystem remains fragmented/underfunded: snare classification persists. If ecosystem reaches critical mass: may approach tangled_rope classification with meaningful counter-extraction power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recovery_ecosystem_coalition_threshold, empirical, 'Recovery ecosystem coalition threshold for constraint reclassification').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(behavioral_addiction_mechanisms, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(baddict_tr_t0, behavioral_addiction_mechanisms, theater_ratio, 0, 0.3).
narrative_ontology:measurement(baddict_tr_t3, behavioral_addiction_mechanisms, theater_ratio, 3, 0.4).
narrative_ontology:measurement(baddict_tr_t6, behavioral_addiction_mechanisms, theater_ratio, 6, 0.5).
narrative_ontology:measurement(baddict_tr_t10, behavioral_addiction_mechanisms, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(baddict_be_t0, behavioral_addiction_mechanisms, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(baddict_be_t3, behavioral_addiction_mechanisms, base_extractiveness, 3, 0.5).
narrative_ontology:measurement(baddict_be_t6, behavioral_addiction_mechanisms, base_extractiveness, 6, 0.62).
narrative_ontology:measurement(baddict_be_t10, behavioral_addiction_mechanisms, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(behavioral_addiction_mechanisms, attachment_coordination).
narrative_ontology:boltzmann_floor_override(behavioral_addiction_mechanisms, 0.12).
narrative_ontology:affects_constraint(behavioral_addiction_mechanisms, platform_engagement_metrics).
narrative_ontology:affects_constraint(behavioral_addiction_mechanisms, attention_scarcity_economics).
narrative_ontology:affects_constraint(behavioral_addiction_mechanisms, data_extraction_mechanisms).

% DUAL FORMULATION NOTE:
% Behavioral addiction mechanisms decompose into three structurally distinct constraints: (1) time-extraction (ε≈0.50, Tangled Rope) — genuine engagement coordination with embedded time asymmetry; (2) data-extraction (ε≈0.65, Snare) — user data monetization with minimal coordination benefit; (3) harm-extraction (ε≈0.68, Snare) — wellbeing damage through neurological hijacking. Current story focuses on the harm observable. Time and data variants should be authored separately and linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
