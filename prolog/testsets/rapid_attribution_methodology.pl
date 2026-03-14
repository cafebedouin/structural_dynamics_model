% ============================================================================
% CONSTRAINT STORY: rapid_attribution_methodology
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rapid_attribution_methodology, []).

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
 *   constraint_id: rapid_attribution_methodology
 *   human_readable: Rapid Attribution Methodology in Crisis Response Systems
 *   domain: crisis_management/attribution/emergency_response
 *
 * SUMMARY:
 *   Rapid attribution methodology creates a structural extraction mechanism
 *   in crisis response systems. When catastrophic events occur (terrorist
 *   attacks, industrial accidents, natural disasters with unclear causation),
 *   institutional actors face intense pressure to provide rapid public
 *   explanations and attribute responsibility. Rapid attribution enables
 *   synchronized emergency response and political communication but imposes
 *   asymmetric costs on attribution accuracy and vulnerable populations who
 *   may be misattributed. The constraint exhibits the full range of DR
 *   classifications from different perspectives. For rapid response agencies
 *   and political decision makers, it functions as pure coordination (Rope) —
 *   rapid attribution solves the collective action problem of coordinating
 *   response. For attribution accuracy and misattributed populations, it is
 *   pure extraction (Snare) — they bear the costs of speed pressure with no
 *   exit. For professional investigators, it is a mixed mechanism (Tangled
 *   Rope) — they benefit from coordination of resources while being extracted
 *   through speed pressure. The theater_ratio (0.68) reflects that rapid
 *   attribution processes often involve substantial performative elements:
 *   public confidence-building claims, media management, and provisional
 *   certainty masquerading as forensic conclusion. As distributed open-source
 *   intelligence and international verification networks mature, centralized
 *   rapid attribution will lose its monopoly on speed — creating a
 *   scaffold-like sunset dynamic where alternative verification pathways
 *   build competitiveness.
 *
 * KEY AGENTS:
 *   - Rapid Response Agencies: Primary beneficiary (institutional/arbitrage) — experiences constraint as coordination mechanism enabling resource deployment and synchronized action
 *   - Political Decision Makers: Primary beneficiary (institutional/arbitrage) — benefits from narrative control and ability to demonstrate rapid decisive action
 *   - Attribution Accuracy: Primary victim (powerless/trapped) — epistemic standard cannot exit pressure for speed; bears full cost of accuracy degradation
 *   - Misattributed Populations: Primary victim (powerless/trapped) — vulnerable groups accused via rapid methodology with no recourse; reputational damage persists despite later correction
 *   - Professional Investigators: Secondary actor (moderate/constrained) — face resource pressure for speed but also benefit from coordination of investigation resources
 *   - Attribution Verification Coalition: Organized agents (organized/constrained) — international fact-checking bodies and open-source intelligence networks building parallel verification with sunset dynamics
 *   - Traditional Investigation Bureaucracies: Institutional actor (institutional/arbitrage) — maintains performative thorough investigation process while real attribution resources follow rapid methodology (piton classification)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional speed-accuracy tradeoff as immutable epistemic law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rapid_attribution_methodology, 0.58).
domain_priors:suppression_score(rapid_attribution_methodology, 0.62).
domain_priors:theater_ratio(rapid_attribution_methodology, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rapid_attribution_methodology, extractiveness, 0.58).
narrative_ontology:constraint_metric(rapid_attribution_methodology, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(rapid_attribution_methodology, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rapid_attribution_methodology, tangled_rope).
narrative_ontology:human_readable(rapid_attribution_methodology, "Rapid Attribution Methodology in Crisis Response Systems").
narrative_ontology:topic_domain(rapid_attribution_methodology, "crisis_management/attribution/emergency_response").

domain_priors:requires_active_enforcement(rapid_attribution_methodology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rapid_attribution_methodology, rapid_response_agencies).
narrative_ontology:constraint_beneficiary(rapid_attribution_methodology, political_decision_makers).
narrative_ontology:constraint_victim(rapid_attribution_methodology, attribution_accuracy).
narrative_ontology:constraint_victim(rapid_attribution_methodology, investigative_integrity).
narrative_ontology:constraint_victim(rapid_attribution_methodology, vulnerable_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ATTRIBUTION ACCURACY (SNARE) — Cannot exit the pressure for speed; bears full structural cost of premature conclusions. The epistemic standard is trapped between institutional demand for rapid answers and the irreducible time requirements for thorough investigation. No exit option, maximum extraction.
constraint_indexing:constraint_classification(rapid_attribution_methodology, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MISATTRIBUTED POPULATIONS (SNARE) — Vulnerable populations accused via rapid methodology have no recourse. Speed creates asymmetric burden of proof reversal. Once rapid attribution is publicized, reputational damage persists even after correction. Trapped with no meaningful exit.
constraint_indexing:constraint_classification(rapid_attribution_methodology, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: PROFESSIONAL INVESTIGATORS (TANGLED ROPE) — Constrained by institutional pressure for speed and resource scarcity, but also benefit from rapid methodology's resource efficiency and coordination of response. Experience both extraction (speed pressure) and coordination (shared methodology).
constraint_indexing:constraint_classification(rapid_attribution_methodology, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: RAPID RESPONSE AGENCIES (ROPE) — Primary beneficiaries. Experience the constraint as coordination solution: rapid attribution enables synchronized emergency response, resource deployment, and public communication. Net beneficiary with high exit flexibility (can slow down if needed).
constraint_indexing:constraint_classification(rapid_attribution_methodology, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: POLITICAL DECISION MAKERS (ROPE) — Beneficiaries. Rapid attribution enables rapid policy response and public narrative control. Experience constraint as coordination mechanism for political action. Can exit by demanding thorough investigation (accepting institutional costs).
constraint_indexing:constraint_classification(rapid_attribution_methodology, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ATTRIBUTION VERIFICATION COALITION (SCAFFOLD) — Organized agents (international fact-checking bodies, transparency initiatives, academic verification networks) are building parallel verification systems with built-in sunset logic. As distributed forensic capacity and open-source intelligence tools mature, centralized rapid attribution loses its monopoly on speed. Sunset: 15-25 years as decentralized verification becomes competitively fast.
constraint_indexing:constraint_classification(rapid_attribution_methodology, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: TRADITIONAL INVESTIGATION BUREAUCRACIES (PITON) — Thorough investigation processes (law enforcement, forensic analysis, international cooperation) persist through institutional inertia despite being bypassed by rapid attribution. The theater of 'serious investigation' continues as parallel process while real resource allocation follows rapid methodology. Piton classification derives from theater gate.
constraint_indexing:constraint_classification(rapid_attribution_methodology, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational perspective, rapid attribution appears as inherent trade-off: speed and attribution confidence are inversely correlated under information scarcity. This perspective sees the constraint as a mathematical/epistemic law. However, structural data contradicts mountain classification — the base_properties reveal contingent institutional arrangements (institutional incentives, resource concentration) that could be rebalanced. False summit detection applies.
constraint_indexing:constraint_classification(rapid_attribution_methodology, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rapid_attribution_methodology_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rapid_attribution_methodology, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rapid_attribution_methodology, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rapid_attribution_methodology, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(rapid_attribution_methodology, TR),
    TR >= 0.70.

:- end_tests(rapid_attribution_methodology_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint imposes significant costs on attribution accuracy and vulnerable populations while providing benefits to rapid response agencies and political decision makers. The extraction is not absolute (complete accuracy loss) because some rapid methodologies do achieve reasonable accuracy in favorable conditions. However, systematic bias toward false positives and misattribution of causation in complex scenarios reveals ongoing extraction. Measurement trajectory (0.42 → 0.58 over interval) reflects increasing institutional entrenchment of rapid attribution as default mode, displacing more careful processes. Suppression (0.62): High. Multiple mechanisms suppress exit and alternatives: institutional pressure for speed, resource concentration in rapid response agencies, media dynamics rewarding rapid explanations, reputational costs to institutions that publicly say 'we don't know yet', career incentives for demonstrating decisiveness. Vulnerable populations face suppression through lack of access to parallel verification resources and asymmetric burden of proof reversion (accused first, investigated later). Theater ratio (0.68): High. Rapid attribution processes involve substantial theatrical elements: confident public statements from partial forensic data, provisional conclusions presented as certainty, media management narratives, and parallel continuation of traditional investigation as performative bureaucratic process without resource allocation.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival differentiation. Rapid response agencies see coordination (Rope) — speed enables synchronized response. Vulnerable populations see extraction (Snare) — speed creates irreversible misattribution costs. Professional investigators see mixed (Tangled Rope) — genuine coordination of resources alongside speed pressure. The verification coalition sees temporary constraint with sunset (Scaffold) — decentralized systems will eventually compete on speed. Traditional bureaucracies see degraded ritual (Piton) — thorough investigation persists as theater. The analytical observer risks seeing natural law (Mountain) — speed-accuracy tradeoff as epistemic limit — but structural data reveals this as false summit: much of the tradeoff is contingent on institutional incentive structures.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (rapid response agencies, political decision makers) have arbitrage exit options — they can theoretically exit by accepting slower attribution without legal penalty. Derived d ≈ 0.05-0.20 (low). Victims (attribution accuracy, misattributed populations) have trapped exit options — they cannot opt out of being misattributed. Derived d ≈ 0.90-0.98 (high). Professional investigators are constrained (resource dependencies, career pressure) — intermediate d ≈ 0.55. The sigmoid f(d) maps these d values to experienced extractiveness multipliers: beneficiaries see f(d) ≈ -0.12 to 0.02 (negative or near-zero extraction); victims see f(d) ≈ 1.35-1.42 (maximal extraction amplification); investigators see f(d) ≈ 0.65 (moderate amplification). Scope modifier σ(S) applies at global scale (σ = 1.2), amplifying effective extraction across all perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   The rapid attribution constraint exhibits mandatrophy at the beneficiary level. Rapid response agencies see the constraint as pure coordination (Rope: χ = 0.58 × (-0.12) × 1.2 ≈ -0.08, classification ROPE). This misses the asymmetric extraction layer — victims experience the same mechanism as Snare (χ = 0.58 × 1.42 × 1.2 ≈ 0.99, classification SNARE). The mandatrophy resolution requires declaring that BOTH perspectives are correct: the constraint genuinely coordinates some aspects (emergency response synchronization) WHILE ALSO extracting from others (misattribution costs). This is the core structure of tangled_rope — it has a real coordination function (χ ≈ 0.58 × 0.65 × 1.2 ≈ 0.45 for moderate investigators who benefit from coordination) AND asymmetric extraction (victims experiencing near-maximal extraction). The beneficiary's Rope classification does not refute the Snare classification for victims — it confirms that different agents experience the same constraint differently because their directionality and exit options differ. No mislabeling occurs when all perspectives are measured. The mandatrophy is resolved by the perspectival framework itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    speed_confidence_tradeoff_shape,
    'What is the actual functional relationship between attribution speed and accuracy? Is it linear, sigmoid, or discontinuous?',
    'Historical analysis of rapid attributions vs. later forensic findings; correlation strength measurement; identification of speed thresholds where accuracy degrades catastrophically vs. gradually',
    'If linear or gradual sigmoid: moderate speed-accuracy tradeoff is inherent (supports mountain view). If discontinuous with steep cliff: rapid attribution system is extractive choice rather than natural constraint (supports tangled_rope view).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(speed_confidence_tradeoff_shape, empirical, 'Functional form of speed-confidence relationship in attribution').

omega_variable(
    alternative_rapid_verification_feasibility,
    'Can decentralized open-source intelligence and distributed forensic networks achieve attribution speed competitive with centralized rapid methodologies while maintaining accuracy?',
    'Longitudinal tracking of distributed OSINT speed improvements; comparison of accuracy rates between centralized rapid attribution and decentralized verification on same events; assessment of cost/skill requirements for distributed participation',
    'If feasible: scaffold sunset is real structural feature; rapid attribution is temporary coordination monopoly. If infeasible: decentralized systems are aspirational theater, and centralized rapid attribution is genuinely necessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_rapid_verification_feasibility, empirical, 'Whether distributed forensic networks can match centralized rapid attribution speed').

omega_variable(
    extraction_beneficiary_asymmetry,
    'How much of the extraction benefit to rapid response agencies and political decision makers is genuine coordination (enabling synchronized response) vs. contingent institutional benefit (political advantage from controlling narrative timing)?',
    'Comparison of outcomes (lives saved, resources deployed) when rapid attribution is used vs. delayed-but-accurate attribution in matched crisis scenarios; measurement of political/reputational benefit separate from operational benefit',
    'If coordination-dominant: beneficiaries genuinely need speed. If narrative-timing-dominant: extraction is contingent on institutional status quo and could be rebalanced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_beneficiary_asymmetry, conceptual, 'Proportion of beneficiary advantage from genuine coordination vs. political positioning').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.62) primarily structural (difficult to access forensic data, technical barriers to distributed verification) or internalized (vulnerable populations have internalized that rapid attribution is normal and just)?',
    'Post-attribution behavior tracking: do corrected populations persistently bear reputation damage despite retraction? Do institutional actors continue to use rapid methodology even after accuracy failures? Surveys of belief in methodology legitimacy.',
    'If structural: suppression could decline as verification technology improves. If internalized: suppression persists even after technical barriers are removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural or internalized in misattributed populations').

omega_variable(
    coordination_function_necessity,
    'Is the coordination function (synchronized emergency response) achievable through alternative mechanisms that don''t require premature attribution?',
    'Case studies of crises where response was coordinated without rapid attribution (e.g., coordinating aid before cause is determined); measurement of operational efficiency gains from fast attribution specifically vs. general response speed',
    'If alternatives exist: Rope classification for beneficiaries is overstated; extraction is more prominent. If coordination requires fast attribution: beneficiaries genuinely need the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_necessity, empirical, 'Whether rapid attribution is necessary for crisis response coordination').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rapid_attribution_methodology, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rapid_attr_tr_t0, rapid_attribution_methodology, theater_ratio, 0, 0.52).
narrative_ontology:measurement(rapid_attr_tr_t3, rapid_attribution_methodology, theater_ratio, 3, 0.58).
narrative_ontology:measurement(rapid_attr_tr_t6, rapid_attribution_methodology, theater_ratio, 6, 0.68).
narrative_ontology:measurement(rapid_attr_tr_t9, rapid_attribution_methodology, theater_ratio, 9, 0.73).

% Extraction over time
narrative_ontology:measurement(rapid_attr_be_t0, rapid_attribution_methodology, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(rapid_attr_be_t3, rapid_attribution_methodology, base_extractiveness, 3, 0.5).
narrative_ontology:measurement(rapid_attr_be_t6, rapid_attribution_methodology, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(rapid_attr_be_t9, rapid_attribution_methodology, base_extractiveness, 9, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rapid_attribution_methodology, enforcement_mechanism).
narrative_ontology:affects_constraint(rapid_attribution_methodology, misattribution_cascade_dynamics).
narrative_ontology:affects_constraint(rapid_attribution_methodology, narrative_timing_power_asymmetry).

% DUAL FORMULATION NOTE:
% Rapid attribution methodology decomposes into two structurally distinct constraints: (1) genuine coordination problem of synchronizing emergency response (lower ε, Rope-dominant); (2) institutional speed advantage in narrative control (higher ε, Snare-victim extraction). This story represents the hybrid composite constraint (Tangled Rope). Upstream constraints on institutional incentive structures and news cycle dynamics feed into the rapid attribution constraint's extractive mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rapid_attribution_methodology, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
