% ============================================================================
% CONSTRAINT STORY: fragile_middle_layer_collapse
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fragile_middle_layer_collapse, []).

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
 *   constraint_id: fragile_middle_layer_collapse
 *   human_readable: The Intermediary Decay: Hollowing of Middle-Layer Economic Functions
 *   domain: economic/technological/logistical
 *
 * SUMMARY:
 *   The intermediary decay represents a structural transformation in economic
 *   logistics where automation, direct-to-consumer models, and platform
 *   scaling systematically eliminate the regional distributors, local service
 *   contractors, and human moderators that previously buffered supply chains
 *   and provided local economic resilience. This constraint exhibits a
 *   perspectival pathology where the beneficiary (platform operators) and end
 *   consumers (who experience mixed benefits and hidden costs) both advocate
 *   for the elimination, while the victims (displaced intermediaries) and
 *   system resilience (which depends on hidden buffering capacity) bear
 *   compounding costs. The constraint's extractiveness has risen from 0.35 to
 *   0.58 over the interval as automation has accelerated, while theater_ratio
 *   remains moderate (0.48) because the functional coordination gains (lower
 *   costs, faster delivery) are real — but they mask the hidden extraction of
 *   system resilience and employment stability. The key structural question
 *   is whether intermediary functions are genuinely automatable or whether
 *   the apparent automation is actually displacement of functional roles into
 *   informal markets and shadow systems.
 *
 * KEY AGENTS:
 *   - Platform Operators: Primary beneficiary (institutional/arbitrage) — capture margin gains, reduce operational complexity, achieve network-effect lock-in
 *   - Regional Intermediaries: Primary victim (powerless/trapped) — face elimination with limited retraining pathways; geographic and capital constraints prevent adjacent-sector migration
 *   - End Consumers: Secondary beneficiary/victim (moderate/constrained) — benefit from lower prices and faster delivery, but constrained by network effects and hidden systemic fragility
 *   - Reskilling Institutions: Organized mediators (organized/constrained) — attempt to create scaffold with sunset logic; effectiveness is empirically contested
 *   - Supply Chain Resilience (as agent): Victim (powerless/trapped) — loses buffering capacity as intermediaries eliminated; buffering functions now ceremonial rather than substantive
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent platform economics as technological inevitability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fragile_middle_layer_collapse, 0.58).
domain_priors:suppression_score(fragile_middle_layer_collapse, 0.65).
domain_priors:theater_ratio(fragile_middle_layer_collapse, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fragile_middle_layer_collapse, extractiveness, 0.58).
narrative_ontology:constraint_metric(fragile_middle_layer_collapse, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(fragile_middle_layer_collapse, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fragile_middle_layer_collapse, tangled_rope).
narrative_ontology:human_readable(fragile_middle_layer_collapse, "The Intermediary Decay: Hollowing of Middle-Layer Economic Functions").
narrative_ontology:topic_domain(fragile_middle_layer_collapse, "economic/technological/logistical").

domain_priors:requires_active_enforcement(fragile_middle_layer_collapse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fragile_middle_layer_collapse, platform_operators).
narrative_ontology:constraint_beneficiary(fragile_middle_layer_collapse, end_consumers).
narrative_ontology:constraint_victim(fragile_middle_layer_collapse, regional_intermediaries).
narrative_ontology:constraint_victim(fragile_middle_layer_collapse, system_resilience).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPLACED INTERMEDIARY (SNARE) — Regional distributors, local service contractors, and human moderators face elimination without alternative income pathways. Trapped by capital-intensive retraining barriers and geographic concentration. The automation pathway is irreversible from their structural position. Maximum extraction from those bearing transition costs.
constraint_indexing:constraint_classification(fragile_middle_layer_collapse, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PLATFORM OPERATOR (ROPE) — Benefits from direct-to-consumer automation and elimination of margin-capturing intermediaries. Experiences the constraint as pure coordination: removing middle layers enables faster feedback loops, lower operational costs, and higher margins. Net beneficiary with full arbitrage optionality.
constraint_indexing:constraint_classification(fragile_middle_layer_collapse, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: END CONSUMER (TANGLED ROPE) — Experiences both coordination benefits (lower prices, faster delivery, direct access) and hidden extraction (loss of local service reliability, loss of human problem-solving, vulnerability to automation failures). Mobile enough to switch platforms, but constrained by network effects. Mixed structural position.
constraint_indexing:constraint_classification(fragile_middle_layer_collapse, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: RESKILLING INSTITUTION (SCAFFOLD) — Government agencies, trade unions, and educational networks see the intermediary collapse as a temporary coordination failure solvable via transition programs, apprenticeships, and upskilling mandates. High suppression during transition (workers cannot exit into adjacent sectors), but with explicit sunset logic: as new middle-layer roles (AI training, platform moderation, algorithmic auditing) mature, the suppression should decline. Sunset timeline: 5-10 years.
constraint_indexing:constraint_classification(fragile_middle_layer_collapse, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: SUPPLY CHAIN RESILIENCE (PITON) — Traditional claims about supply chain redundancy and robustness rely on the hidden buffering function of intermediaries: local warehousing, informal inventory management, human problem-solving during disruptions. As intermediaries are removed, these functions persist ceremonially (supply chain audits, resilience frameworks) but lack functional substance. The ritual survives through institutional inertia despite degraded actual resilience.
constraint_indexing:constraint_classification(fragile_middle_layer_collapse, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / TECHNOLOGICAL LOCK-IN VIEW (MOUNTAIN) — From a civilizational analytical perspective, direct-to-consumer scaling and intermediary elimination represent an irreversible technological trajectory: once automation and network effects concentrate operations, returning to distributed intermediaries becomes economically irrational. The constraint appears as an inherent property of digital-network scale economies — a natural law of how modern logistics must operate. However, structural data reveals this as false summit: the 'inevitability' naturalizes policy choices (platform governance, labor regulation, resilience requirements) that remain contingent.
constraint_indexing:constraint_classification(fragile_middle_layer_collapse, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fragile_middle_layer_collapse_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fragile_middle_layer_collapse, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fragile_middle_layer_collapse, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(fragile_middle_layer_collapse, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(fragile_middle_layer_collapse, TR),
    TR >= 0.70.

:- end_tests(fragile_middle_layer_collapse_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. The constraint extracts from displaced intermediaries and system resilience while distributing gains to platform operators and (partially) to consumers. The extraction is not maximal (Snare territory) because much of the efficiency gain is real — automation does reduce costs and delivery times. But the extraction is substantial because intermediaries bear transition costs with no compensation mechanism, and system resilience is degraded without corresponding reduction in critical-infrastructure expectations. The rising trajectory (0.35→0.58) reflects acceleration of automation and elimination of exit options for intermediaries. Suppression (0.65): High. Barriers to exit include capital-intensive retraining requirements, geographic concentration of intermediary employment, network-effect lock-in on platform dominance, and policy asymmetries favoring platform scaling over intermediary protection. However, suppression is not maximal because reskilling programs exist (even if inadequate) and some workers can migrate to new platform-dependent roles. Theater ratio (0.48): Moderate and rising (0.32→0.48). The functional coordination gains are real — platforms do achieve efficient matching between suppliers and consumers. But theater is increasing because supply chain resilience claims persist ceremonially despite functional degradation, and 'reskilling readiness' narratives substitute for actual job creation.
 *
 * PERSPECTIVAL GAP:
 *   This constraint reveals a critical failure mode: perspectives that benefit from the elimination (platform operators, partially end consumers) classify the constraint as Rope or even scaffolding-with-sunset. The beneficiary experiences pure coordination. But perspectives bearing the costs (displaced intermediaries, system resilience) see Snare — extraction with no exit. The analytical observer risks the false summit of technological inevitability, naturalizing what is actually a contingent governance choice: whether platform economics should internalize resilience costs or externalize them onto workers and infrastructure. The gap is not perspectival — it is a genuine structural tension between extractive and coordinative functions occupying the same institutional apparatus.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform operators derive d ≈ 0.05 (full beneficiary + arbitrage): they experience the constraint as pure coordination, with negative effective extraction. Regional intermediaries derive d ≈ 0.95 (full victim + trapped): they have no exit options and bear maximum extraction. Consumers derive d ≈ 0.60 (mixed + constrained): they benefit from lower prices and faster delivery, but are constrained by network effects and experience hidden costs of system fragility. The reskilling institution derives d ≈ 0.55 (moderate victim + constrained): constrained by program capacity and worker receptivity, but with explicit sunset logic and some agency to shape outcomes. The supply-chain-resilience agent derives d ≈ 1.0 (full victim, abstract): cannot organize, cannot exit, inherently powerless. The effective extraction chi varies dramatically across perspectives because f(d) is nonlinear: beneficiaries experience negative chi (the constraint subsidizes them), while trapped victims experience f(d)≈1.42, amplifying experienced extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   CRITICAL PATHOLOGY CASE: This constraint illustrates how classification can be hijacked by framing. If the question is 'Does the platform achieve efficient matching?' (YES, Rope/coordination), the constraint appears benign. If the question is 'Who bears the transition costs and can they exit?' (NO, trapped workers, Snare/extraction), the constraint is predatory. The mandatrophy is resolved by acknowledging that BOTH readings are structural: the constraint IS a genuine coordination mechanism (matching efficiency is real) AND a genuine extraction mechanism (transition costs are real and uncompensated). The Tangled Rope classification captures this: it requires BOTH beneficiaries (platform operators, consumers) AND victims (intermediaries, resilience), both coordination function (matching efficiency) AND asymmetric extraction (cost displacement). The constraint resolves mandatrophy not by choosing one view, but by insisting that both are simultaneously true. The policy challenge is whether coordination gains can be decoupled from extraction — whether platforms can achieve efficiency while internalizing transition costs, or whether extraction is structurally necessary to the model.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    critical_failure_frequency_threshold,
    'At what frequency of automation failures (supply chain disruptions, platform outages) does the loss of intermediary buffering capacity become catastrophically visible?',
    'Time-series analysis of supply chain disruption costs pre/post-automation; correlation between intermediary layer density and recovery time from shocks',
    'If threshold is crossed within 5 years: system-wide fragility becomes undeniable (perspective shifts from Rope to Snare for consumers). If threshold defers beyond 10 years: extraction persists unchallenged and becomes locked in.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(critical_failure_frequency_threshold, empirical, 'Frequency threshold for catastrophic visibility of automation failure costs').

omega_variable(
    intermediary_function_transferability,
    'Which intermediary functions (trust-brokering, local knowledge, human judgment, relationship maintenance) are genuinely non-automatable vs which require algorithmic replacement at fundamental cost parity?',
    'Cost analysis of automation attempts; measurement of service quality degradation post-elimination; documentation of functions retreating to informal/shadow-market operators',
    'If many functions are non-automatable: constraint becomes Tangled Rope at scale (persistent extraction). If most functions are automatable: constraint becomes temporary Scaffold (suppression should decline as new roles mature).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intermediary_function_transferability, empirical, 'Scope of genuinely non-automatable intermediary functions').

omega_variable(
    reskilling_program_efficacy,
    'Do reskilling and transition programs actually move displaced intermediaries into new middle-layer roles (platform moderation, algorithmic auditing, local AI training) or merely create retraining theater while employment permanently declines?',
    'Longitudinal employment tracking of reskilled cohorts; measurement of wage replacement rates; documentation of new role creation vs program throughput',
    'If efficacy is high (>60% placement in role-equivalent positions): scaffold sunset is real, suppression declines. If efficacy is low (<30%): theater-ratio climbs and constraint becomes Piton rather than temporary Scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reskilling_program_efficacy, empirical, 'Whether reskilling programs achieve genuine role-transition or theater').

omega_variable(
    platform_operator_incentive_alignment,
    'Are platform operators genuinely indifferent to intermediary layer retention (could use either model), or does the automation pathway deliver structural monopoly gains that eliminate any equilibrium where intermediaries survive?',
    'Comparative profit analysis: direct-to-consumer vs distributed-intermediary models; documentation of deliberate intermediary suppression vs natural economic selection',
    'If indifferent: constraint could be negotiated (Rope classification restored). If monopoly-aligned: constraint is locked in by structural incentives (Snare classification confirmed for intermediaries).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(platform_operator_incentive_alignment, empirical, 'Platform operator incentive alignment toward intermediary elimination').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fragile_middle_layer_collapse, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fmlc_tr_t0, fragile_middle_layer_collapse, theater_ratio, 0, 0.32).
narrative_ontology:measurement(fmlc_tr_t5, fragile_middle_layer_collapse, theater_ratio, 5, 0.4).
narrative_ontology:measurement(fmlc_tr_t10, fragile_middle_layer_collapse, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(fmlc_be_t0, fragile_middle_layer_collapse, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(fmlc_be_t5, fragile_middle_layer_collapse, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(fmlc_be_t10, fragile_middle_layer_collapse, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fragile_middle_layer_collapse, resource_allocation).
narrative_ontology:affects_constraint(fragile_middle_layer_collapse, just_in_time_fragility).
narrative_ontology:affects_constraint(fragile_middle_layer_collapse, platform_concentration_monopoly).
narrative_ontology:affects_constraint(fragile_middle_layer_collapse, labor_market_polarization).

% DUAL FORMULATION NOTE:
% The intermediary decay is upstream of several constraint families: it is a enabling condition for just-in-time supply fragility (by removing buffers), a consequence of platform concentration (which makes direct-to-consumer models profit-maximizing), and a driver of labor-market polarization (high-skill platform roles, low-skill gig roles, few mid-skill intermediate roles). Each downstream constraint inherits the extractiveness trajectory from this parent constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fragile_middle_layer_collapse, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
