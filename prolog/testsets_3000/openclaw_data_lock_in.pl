% ============================================================================
% CONSTRAINT STORY: openclaw_data_lock_in
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_openclaw_data_lock_in, []).

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
 *   constraint_id: openclaw_data_lock_in
 *   human_readable: Data Lock-In by the OpenClaw AI Personal Assistant
 *   domain: technological/consumer_technology/platform_lock_in
 *
 * SUMMARY:
 *   OpenClaw represents a canonical modern platform lock-in constraint, where
 *   genuine product quality and user benefit are inseparable from the
 *   extraction mechanism itself. Users voluntarily invest years of behavioral
 *   data, personalization profiles, and life-integration into their
 *   hyper-personalized digital twin ('claw'), creating enormous switching
 *   costs that appear justified by the product's quality. The constraint
 *   operates through architectural choices (proprietary data formats,
 *   incompatible model structures, deliberate non-interoperability) that are
 *   technically avoidable but economically valuable to the corporation. From
 *   the user's perspective, the lock-in transitions from 'sticky product
 *   advantage' (early stages, low switching costs) to 'extractive trap'
 *   (mature product, prohibitive exit costs) as the behavioral model
 *   accumulates depth. The constraint exhibits strong mandatrophy
 *   characteristics: it simultaneously provides genuine coordination value
 *   (improved life optimization through deep personalization) and pure
 *   extraction (users cannot negotiate better terms or credibly threaten
 *   exit). The theater ratio reflects that OpenClaw maintains procedural
 *   compliance with data-portability regulations (exports data on request)
 *   while ensuring those exports are functionally useless to competitors
 *   (proprietary formats, missing behavioral models, incompatible
 *   architectures). The constraint is not deception — users understand they
 *   are trading data for service — but structural asymmetry: users cannot
 *   easily renegotiate or exit once locked in.
 *
 * KEY AGENTS:
 *   - OpenClaw Corporation: Primary beneficiary (institutional/arbitrage) — captures full value of user lock-in, accumulates behavioral data monopoly, faces minimal competitive pressure
 *   - Locked-In Users: Primary victims (powerless/trapped) — years of data investment creates prohibitive exit costs, cannot access behavioral models in portable form, cannot credibly negotiate service terms
 *   - Competing AI Providers: Secondary victims (moderate/constrained) — excluded from market share controlled by OpenClaw lock-in, cannot bootstrap equivalent personalization depth, face asymmetric competitive disadvantage
 *   - Data Sovereignty Coalition: Organized actors (organized/constrained) — privacy advocates, regulators, open-source projects working to create interoperability standards and portability enforcement; have some agency but face resource and political barriers
 *   - Legacy Privacy Regulators: Institutional actors (institutional/arbitrage) — GDPR/CCPA framework enforcers; have authority to change rules but constrained by old regulatory architectures; their procedural compliance mechanisms are degraded (piton perspective)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing lock-in as an inevitable feature of personalization technology rather than a contingent corporate design choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(openclaw_data_lock_in, 0.68).
domain_priors:suppression_score(openclaw_data_lock_in, 0.72).
domain_priors:theater_ratio(openclaw_data_lock_in, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(openclaw_data_lock_in, extractiveness, 0.68).
narrative_ontology:constraint_metric(openclaw_data_lock_in, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(openclaw_data_lock_in, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(openclaw_data_lock_in, snare).
narrative_ontology:human_readable(openclaw_data_lock_in, "Data Lock-In by the OpenClaw AI Personal Assistant").
narrative_ontology:topic_domain(openclaw_data_lock_in, "technological/consumer_technology/platform_lock_in").

domain_priors:requires_active_enforcement(openclaw_data_lock_in).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(openclaw_data_lock_in, openclaw_corporation).
narrative_ontology:constraint_victim(openclaw_data_lock_in, users_locked_in_to_ecosystem).
narrative_ontology:constraint_victim(openclaw_data_lock_in, user_autonomy_and_data_sovereignty).
narrative_ontology:constraint_victim(openclaw_data_lock_in, competing_ai_services).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOCKED-IN USER (SNARE) — Users have invested years of behavioral data, personalization profiles, and integrated life optimization into their OpenClaw claw. Exit costs are prohibitive: the digital twin cannot be ported to competitors, alternative assistants lack the contextual depth, and switching requires reconstructing the entire optimization model from scratch. Users experience maximum extraction: they cannot negotiate terms, cannot access their behavioral data in portable formats, and cannot credibly threaten exit. The constraint appears as pure rent extraction hidden behind the benefits of the optimization itself.
constraint_indexing:constraint_classification(openclaw_data_lock_in, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COMPETING AI PROVIDER (SNARE) — Alternative AI assistants face structural barriers to entry and growth. Users locked into OpenClaw claws represent inaccessible market share. Competing providers must build their own recommendation engines and behavioral models from scratch, without access to the rich longitudinal data that OpenClaw possesses. The network effects of lock-in suppress competitive pressure. Exit: difficult but not impossible — a competitor with superior features and lower switching costs could theoretically gain market share, but the practical barriers are substantial enough that most competitors cannot survive the bootstrapping phase.
constraint_indexing:constraint_classification(openclaw_data_lock_in, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: OPENCLAW CORPORATION (ROPE) — OpenClaw benefits from coordination: the system genuinely solves the user's life optimization problem, delivering real value through hyper-personalization. The corporation experiences the lock-in constraint as a coordination mechanism — the behavioral data they accumulate improves their product, users stay because the product is good, and the network effects reinforce market position. From the corporation's view, the constraint is a beneficial equilibrium. Exit: arbitrage — the corporation can always pivot to new product lines, sell data assets, or leverage the accumulated behavioral data for ancillary services. They have maximum flexibility.
constraint_indexing:constraint_classification(openclaw_data_lock_in, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DATA SOVEREIGNTY COALITION (TANGLED ROPE) — Organized actors (privacy advocates, data-portability regulators, open-source AI projects) perceive the lock-in as a solvable hybrid problem requiring both coordination standards and breaking of OpenClaw's proprietary grip. They see genuine coordination benefits in standardized data formats and interoperable AI assistants, but also recognize extraction: OpenClaw's proprietary model prevents portability and suppresses competition. The coalition has some agency — they can advocate for regulation, fund alternatives, and build open standards — but faces resource and political barriers. Exit: constrained — they cannot simply leave the ecosystem; they must work within regulatory and technical frameworks to create alternatives.
constraint_indexing:constraint_classification(openclaw_data_lock_in, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY PRIVACY REGULATION (PITON) — Existing privacy frameworks (GDPR right to data portability, CCPA data access rights) are performatively applied to OpenClaw but are substantially degraded in practice. The regulations nominally require data portability, but the practical enforcement is theater: OpenClaw exports data in formats incompatible with competitors, the export process is intentionally cumbersome, and the behavioral models themselves (the actual value) are proprietary and non-transferable. Regulators have arbitrage (they can update rules), but the institutional inertia means old frameworks persist even when their functional purpose has been hollowed out. The ritual of compliance satisfies the letter of regulation without enabling actual portability.
constraint_indexing:constraint_classification(openclaw_data_lock_in, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the constraint risks appearing as an inevitable feature of personalization technology: any system that improves recommendations through behavioral data accumulation will create lock-in effects because deep contextual models cannot be trivially ported. This naturalizes what is actually a contingent design choice (proprietary formats, incompatible model architectures, deliberate non-interoperability). The engine will detect this as a false summit: the constraint is architecturally chosen, not physically inevitable. Portability, interoperability, and open data formats are technically feasible — their absence reflects corporate strategy, not natural law.
constraint_indexing:constraint_classification(openclaw_data_lock_in, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(openclaw_data_lock_in_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(openclaw_data_lock_in, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(openclaw_data_lock_in, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(openclaw_data_lock_in, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(openclaw_data_lock_in, TR),
    TR >= 0.70.

:- end_tests(openclaw_data_lock_in_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. OpenClaw captures disproportionate value from user lock-in through proprietary model architectures and data-format incompatibility. The extractiveness increased from 0.35 (early adopter phase, low switching costs) to 0.68 (mature product, prohibitive exit costs) as the behavioral model accumulated depth. Users cannot port their profiles to competitors, cannot renegotiate service terms, and cannot access their own behavioral data in usable form. The extraction is substantial enough to meet snare thresholds. Suppression (0.72): High. Multiple barriers prevent user exit: (1) architectural lock-in through proprietary data formats and incompatible model structures, (2) network effects (better recommendations as more user data accumulates), (3) cognitive switching costs (emotional investment in the claw as a personalized life partner), (4) regulatory theater (GDPR data access exists but is non-functional). Users face real alternatives (other AI assistants) but cannot credibly switch without reconstructing their entire optimization model from scratch. Theater ratio (0.58): Moderate-high. OpenClaw's compliance with data-portability regulations is substantially performative — they export data in formats incompatible with competitors, the exports lack the behavioral models that constitute actual value, and the proprietary architectures ensure that even full data access does not enable functional switching. The theater is lower than pure regulatory theater (piton) because OpenClaw users do receive genuine product benefits; the theater consists of the gap between procedural compliance and functional portability.
 *
 * PERSPECTIVAL GAP:
 *   OpenClaw demonstrates a classic mandatrophy case where all six classification types are valid readings of the same structural data: (1) Rope from the corporation's perspective — genuine product coordination, beneficial lock-in, mutual benefit. (2) Snare from the user's perspective — extraction through proprietary lock-in, no credible exit, asymmetric power. (3) Snare from competing providers' perspective — market exclusion, asymmetric competitive disadvantage, network effects that entrench OpenClaw. (4) Tangled Rope from the coalition's perspective — coordination benefits in personalization + extraction benefits in lock-in, requiring regulation and standards to decouple. (5) Piton from the regulatory perspective — legacy rules (GDPR/CCPA) aimed at solving the problem but degraded into theater (exports exist but are non-functional). (6) Mountain from the analytical perspective (false summit) — risks naturalizing lock-in as inevitable in personalization technology. The resolution of mandatrophy is: all six are correct. The constraint is indeed a snare from the user's perspective, a rope from the corporation's perspective, and a piton from the regulatory perspective. The perspectival gap is not an error in classification — it is the core insight. Different observers experience the same structural phenomenon as different types because they occupy different positions in the extraction flow.
 *
 * DIRECTIONALITY LOGIC:
 *   OpenClaw's directionality derives from the structural relationship between user data, proprietary models, and switching costs. Users are declared victims: they bear the extraction through trapped exit (cannot port their profiles, cannot switch without catastrophic utility loss, cannot renegotiate terms). OpenClaw is declared beneficiary: it captures full value of the behavioral monopoly, has arbitrage exit (can pivot to new services, monetize data, change terms unilaterally). The derived directionality for locked-in users is d ≈ 0.92 (victim status + trapped exit + minimal power → high d → high f(d) → high experienced chi). For OpenClaw corporation, d ≈ 0.05 (beneficiary status + arbitrage exit + institutional power → low d → negative f(d) → extraction runs toward them, not from them). Competing providers have d ≈ 0.68 (victim status via market exclusion + constrained exit + moderate power → moderate-high d → moderate-high f(d)). The perspectival gap in d values (0.05 for beneficiary, 0.92 for trapped victim) translates into a massive gap in experienced extractiveness chi, which manifests as the classification difference: rope for the corporation, snare for the user.
 *
 * MANDATROPHY ANALYSIS:
 *   OpenClaw resolves mandatrophy through perspectival heterogeneity: the constraint is simultaneously rope, snare, tangled rope, and piton depending on the observer's structural position. This is not a classification error but a correct application of the indexical framework. The corporation experiences rope: genuine product coordination that creates value and justifies lock-in. The user experiences snare: extraction through proprietary architecture with no credible exit. Regulators experience piton: their rules are degraded into procedural theater that does not enable actual switching. The coalition experiences tangled rope: genuine benefits in personalization technology + genuine extraction through lock-in. The key insight: mandatrophy is resolved not by finding 'the correct' type but by recognizing that the constraint's classification is fundamentally observer-relative. What appears as beneficial coordination to the extractor appears as extractive trap to the trapped. The system is not contradictory — it is correctly capturing how lock-in feels different depending on whether you are the beneficiary or the victim. The mandatrophy threshold (extractiveness > 0.70) is met and resolved because the system explicitly declares multiple perspectives with different types, showing that the constraint's essential nature is hybrid: it genuinely coordinates (improves recommendations, personalizes life optimization) AND genuinely extracts (traps users, suppresses competition, enables rent extraction). The resolution mechanism: regulate for portability, enforce interoperability, and decouple the coordination benefits (personalization) from the extraction mechanism (proprietary lock-in) by requiring open data formats and competitive access.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    switching_cost_threshold,
    'At what ratio of switching cost to annual utility value does lock-in transition from ''sticky product advantage'' to ''extractive trap''?',
    'Longitudinal survey of users who have attempted to switch assistants; measurement of time/effort/money costs vs perceived utility loss; comparison with user willingness-to-pay for product improvements',
    'If threshold < 1.5x annual utility: lock-in is primarily a coordination side effect (Rope from user perspective). If threshold > 4.0x annual utility: lock-in is the primary extraction mechanism (Snare classification confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(switching_cost_threshold, empirical, 'Switching cost to annual utility ratio threshold').

omega_variable(
    data_portability_feasibility,
    'Can a competing AI assistant functionally replicate a user''s behavioral model and optimization profile if given portable access to OpenClaw''s data exports?',
    'Technical test: request OpenClaw data export, attempt to integrate into competitor products (Claude, Gemini, open-source alternatives), measure quality degradation of recommendations and personalization',
    'If competitor can achieve > 80% fidelity: lock-in is largely artificial (proprietary format choice). If competitor achieves < 50% fidelity: behavioral models are genuinely path-dependent and portability is technically infeasible. Changes classification implications for regulatory intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(data_portability_feasibility, empirical, 'Technical feasibility of data portability across AI assistants').

omega_variable(
    user_awareness_of_extraction,
    'Do users perceive the lock-in as extraction or as a natural feature of the product?',
    'User surveys: explicit questions about data ownership, switching costs, and whether they feel trapped; correlation with actual switching attempts and user churn rates to competitors',
    'If < 30% aware of lock-in: suppression is highly effective (Snare), users normalize the extraction. If > 70% aware: suppression is breaking down, organizing potential for coalition is higher. Affects mandatrophy resolution path.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(user_awareness_of_extraction, empirical, 'User awareness and perception of data lock-in mechanism').

omega_variable(
    regulatory_enforcement_depth,
    'Will regulators enforce genuine data portability (interoperable formats, functional model transfer) or settle for procedural compliance (data exports in opaque formats)?',
    'Monitor ongoing GDPR/CCPA enforcement actions against platform lock-in; track EU Digital Markets Act implementation and required interoperability mandates; measure whether regulatory fines change OpenClaw''s data-access practices',
    'If enforcement is procedural only: piton classification holds (regulation is theater). If enforcement requires functional portability: constraint shifts from Snare to Tangled Rope (coordinated by regulation). Affects mandatrophy path and network dynamics.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_enforcement_depth, preference, 'Depth of regulatory enforcement on data portability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(openclaw_data_lock_in, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(openclaw_tr_t0, openclaw_data_lock_in, theater_ratio, 0, 0.42).
narrative_ontology:measurement(openclaw_tr_t3, openclaw_data_lock_in, theater_ratio, 3, 0.5).
narrative_ontology:measurement(openclaw_tr_t6, openclaw_data_lock_in, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(openclaw_be_t0, openclaw_data_lock_in, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(openclaw_be_t3, openclaw_data_lock_in, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(openclaw_be_t6, openclaw_data_lock_in, base_extractiveness, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(openclaw_data_lock_in, information_standard).
narrative_ontology:affects_constraint(openclaw_data_lock_in, ai_model_moat_network_effects).
narrative_ontology:affects_constraint(openclaw_data_lock_in, behavioral_data_monopoly).
narrative_ontology:affects_constraint(openclaw_data_lock_in, digital_paternalism_asymmetry).

% DUAL FORMULATION NOTE:
% OpenClaw data lock-in is downstream of general platform lock-in mechanisms but represents a distinct structural constraint specific to AI personal assistants and behavioral data accumulation. The upstream constraint (ai_model_moat_network_effects) creates the technical conditions for lock-in; openclaw_data_lock_in is the realization of those conditions in a specific product. The downstream constraint (digital_paternalism_asymmetry) represents the governance problem created by lock-in — users become dependent on a single system's decision-making authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(openclaw_data_lock_in, moderate, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
