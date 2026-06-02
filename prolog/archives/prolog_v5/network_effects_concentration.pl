% ============================================================================
% CONSTRAINT STORY: network_effects_concentration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_network_effects_concentration, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: network_effects_concentration
 *   human_readable: Network Effects Concentration in Digital Platforms
 *   domain: economics/technology/digital_platforms
 *
 * SUMMARY:
 *   Network effects create a structural coordination problem: users benefit
 *   from joining larger networks, and platforms benefit from network density.
 *   This genuine coordination function coexists with an extractive overlay:
 *   proprietary architecture prevents interoperability, converting
 *   coordination benefits into platform-operator rents. The constraint
 *   exemplifies Tangled Rope classification because both elements are real.
 *   The platform provides genuine value through network coordination (users
 *   do want larger networks), but this value is captured asymmetrically
 *   because the platform has suppressed alternative architectures. The
 *   measurement trajectory shows extractiveness increasing over time (0.25 →
 *   0.58) as the network matures and lock-in strengthens, while theater_ratio
 *   remains relatively stable and low. This contrasts with other Tangled Rope
 *   constraints (verification bottleneck) where theater rises as functional
 *   value declines. Here, theater stays low because the platform maintains a
 *   genuinely functional coordination service — the extraction is not hidden
 *   behind performative theater but explicit in terms changes and API
 *   restrictions.
 *
 * KEY AGENTS:
 *   - Platform Operator: Primary beneficiary (institutional/arbitrage) — captures network value through scale, user data, and vendor lock-in; can exit individual markets while retaining core business
 *   - New Entrant Competitors: Primary victim (powerless/trapped) — face insurmountable network effects barrier; cannot compete despite product superiority; trapped at global scope
 *   - Consumers: Primary victim (powerless/trapped) — locked into platform despite network effects becoming less valuable; cannot coordinate exit without others exiting simultaneously
 *   - Complementary Service Providers: Secondary victim (moderate/constrained) — depend on platform for user access but face unilateral extraction of terms through API changes and revenue share modifications
 *   - Interoperability Coalition: Organized agent (organized/constrained) — regulators, open-source communities, and protocol advocates with power to negotiate but facing suppression from both network effects and platform control
 *   - Early Adopters: Secondary beneficiary (moderate/mobile) — captured early network value but increasingly constrained as platform extracts from later cohorts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(network_effects_concentration, 0.58).
domain_priors:suppression_score(network_effects_concentration, 0.68).
domain_priors:theater_ratio(network_effects_concentration, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(network_effects_concentration, extractiveness, 0.58).
narrative_ontology:constraint_metric(network_effects_concentration, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(network_effects_concentration, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(network_effects_concentration, tangled_rope).
narrative_ontology:human_readable(network_effects_concentration, "Network Effects Concentration in Digital Platforms").
narrative_ontology:topic_domain(network_effects_concentration, "economics/technology/digital_platforms").

domain_priors:requires_active_enforcement(network_effects_concentration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(network_effects_concentration, platform_operator).
narrative_ontology:constraint_beneficiary(network_effects_concentration, early_adopters).
narrative_ontology:constraint_victim(network_effects_concentration, new_entrant_competitors).
narrative_ontology:constraint_victim(network_effects_concentration, consumer_choice_diversity).
narrative_ontology:constraint_victim(network_effects_concentration, open_protocol_development).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSUMER LOCK-IN (SNARE) — Individual users cannot exit despite degrading service because network effects create switching costs that exceed individual utility. All friends, colleagues, and professional contacts are on the dominant platform. Exit requires coordinating millions of users simultaneously — impossible. Maximum extraction from this structural position: platform captures attention, data, and behavioral surplus while suppressing alternatives.
constraint_indexing:constraint_classification(network_effects_concentration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NEW ENTRANT COMPETITOR (SNARE) — Cannot compete despite better product design because network effects create winner-take-most dynamics. Users will not switch to superior alternative without network presence. Entrant faces cold-start problem with no path to critical mass. Extraction mechanism: network effects act as a moat that suppresses competitive entry regardless of relative merit. Suppression is structural — the entrant's exit option (pivot to different market) is always available but the market they actually want is sealed.
constraint_indexing:constraint_classification(network_effects_concentration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: COMPLEMENTARY SERVICE PROVIDER (TANGLED ROPE) — Depends on platform access for user acquisition (genuine coordination function: the platform aggregates users at scale). But platform operator can extract terms unilaterally: API restrictions, revenue share changes, sudden deplatforming. High switching costs within each region; moderate mobility across geographic markets. Extraction is real but the relationship also enables services that could not exist without platform scale. This is coordination with asymmetric control.
constraint_indexing:constraint_classification(network_effects_concentration, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PLATFORM OPERATOR (ROPE) — Experiences network effects as a coordination mechanism: connecting more users creates value for all existing users (and for the operator). The operator can arbitrage across jurisdictions, switch between monetization models, and exit individual markets without losing core business. The constraint itself (network effects) is the operator's primary economic asset. Experiences the constraint as pure coordination with externally captured value.
constraint_indexing:constraint_classification(network_effects_concentration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INTEROPERABILITY COALITION (TANGLED ROPE) — Organized agents (regulators, open-source communities, protocol advocates) see network effects as a coordination problem with an extractive overlay. Genuine coordination benefit exists: users want network density. But platform operator has extracted exclusionary control through API lock-in, data portability restrictions, and litigation against interoperability. Coalition has power to negotiate but faces suppression from network effects themselves (if we interoperate, does the network become less dense?). Constrained rather than trapped because coalition has agency and exit paths (regulation, forking, protocol development).
constraint_indexing:constraint_classification(network_effects_concentration, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From first principles, network effects are a mathematical property of communication networks: value scales with user count. This creates unavoidable winner-take-most dynamics — a fundamental constraint of network topology, not institutional choice. However, this mountain classification is REJECTED by the structural data: network effects alone do not determine concentration. OpenStandards (SMTP, HTTP, DNS) have universal network effects but remain open and competitive. The difference is architecture: closed platform vs open protocol. The constraint is not network effects per se but PROPRIETARY network effects. This is institutional, not natural.
constraint_indexing:constraint_classification(network_effects_concentration, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(network_effects_concentration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(network_effects_concentration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(network_effects_concentration, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(network_effects_concentration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(network_effects_concentration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Network effects create genuine coordination value, but proprietary architecture extracts a substantial rent on top of legitimate network externalities. The platform operator can demand unilateral terms changes because users cannot coordinate a switch. The 0.58 value reflects that the extraction is significant but not absolute — the platform still provides real coordination service. Suppression (0.68): High. Multiple overlapping suppression mechanisms: (1) cold-start problem makes entry mathematically impossible for competitors, (2) switching costs are deliberately engineered through API lock-in and data silos, (3) litigation against interoperability attempts, (4) acquisition of potential competitors. Theater ratio (0.45): Low-moderate. The platform maintains genuine functional coordination service, so theater is not high. But increasing amounts of platform activity are extractive (data monetization, attention capture, algorithmic manipulation) rather than coordinative. Theater rises slowly as functional value plateaus and extractive mechanisms intensify. Claimed type (Tangled Rope): Requires both genuine coordination function AND asymmetric extraction. Network effects provide the coordination; proprietary architecture provides the extraction. Both are structural and real.
 *
 * PERSPECTIVAL GAP:
 *   The gap between platform operator (Rope) and new entrant (Snare) is maximum — the same constraint appears as coordination to one and extraction to the other. This gap is diagnostic of Tangled Rope at the analytical level: the constraint genuinely coordinates (network effects are real) AND genuinely extracts (proprietary lock-in is real). The gap reveals that the platform's 'this is just how networks work' framing naturalizes what is actually a choice. The interoperability coalition's Tangled Rope classification uses constrained exit rather than trapped, which is correct — they have institutional power and exit paths (regulation, forking, protocol development) that consumers and new entrants lack. This differentiation is why the coalition sees the constraint as resolvable (Scaffold thinking) rather than absolute (Snare thinking).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each agent is derived from their position relative to extraction flow and their exit capacity. Platform operators are beneficiaries with arbitrage-level exit (can exit individual markets, can pivot to other services) → low d → negative effective extraction. New entrants are victims with trapped-level exit (network effects are mathematically insurmountable) → high d → high χ. Consumers are victims with trapped-level exit (cannot exit without coordinating millions) → high d → high χ. Complementary providers are partially victims with constrained-level exit (can exit the specific platform but not the category) → moderate-high d → moderate χ. Interoperability coalition is organized with constrained exit (has power to negotiate but faces suppression) → moderate d → moderate χ. The engine derives these d values from the beneficiary/victim declarations and exit options; the commentary reconstructs the logic chain for transparency.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by distinguishing network effects (natural/mathematical) from proprietary architecture (institutional/choice). Network effects alone do NOT require extraction (SMTP/email prove this). Proprietary architecture PLUS network effects create Tangled Rope. The analytical observer's Mountain classification (network effects are immutable natural law) is a FALSE SUMMIT — it naturalizes an institutional choice. The resolution mechanism: if interoperability removes proprietary barriers while preserving network effects, the Snare and Tangled Rope classifications are reclassified as Rope or Scaffold, and the Mountain is exposed as false naturalization. The constraint is NOT immutable; it is institutional and contestable. Current classification (Tangled Rope) reflects the actual state: both coordination and extraction are present, real, and structurally interdependent in the current institutional design.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    closed_vs_open_network_distinction,
    'Is the extraction driven by network effects themselves or by proprietary architecture that prevents interoperability?',
    'Comparative analysis: SMTP/email (network effects + open protocol) vs Facebook (network effects + closed platform). If open protocols with equivalent scale show lower extraction, the mountain view is false. Network effects alone are not sufficient to cause concentration.',
    'If proprietary architecture is the differentiator: the constraint is institutional/economic (Tangled Rope), not natural (Mountain). Classification shifts from immutable to contestable. Policy interventions (interoperability requirements, data portability) become structurally meaningful.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(closed_vs_open_network_distinction, empirical, 'Whether extraction is inherent to network effects or contingent on proprietary architecture').

omega_variable(
    switching_cost_decomposition,
    'What fraction of switching costs are inherent network coordination costs vs. platform-imposed lock-in mechanisms?',
    'Behavioral analysis: measure user attrition when switching costs are artificially reduced (data export tools, interoperability, bridge protocols). Track whether users exit when friction is removed or whether genuine network lock-in persists.',
    'If majority of switching costs are platform-imposed (API restrictions, data silos, social graph lock-in): suppression metric should increase, reclassifying from Tangled Rope toward Snare. If majority are genuine network coordination costs: suppression metric is appropriate, classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(switching_cost_decomposition, empirical, 'Decomposition of switching costs: network effects vs. platform lock-in').

omega_variable(
    early_adopter_extraction_fairness,
    'Are early adopters justified in capturing network value, or does this constitute extractive privileging?',
    'Normative framework: compare with other coordination mechanisms (e.g., open-source projects with equal contributor reward). Establish whether network-value capture creates Pareto-suboptimal outcomes (some users have lower utility than in competitive equilibrium despite network effects).',
    'If early adopters capturing disproportionate value is fair: beneficiary classification is correct, extraction is justified compensation for risk. If it constitutes unjustified rent extraction: victims list should include ''later users'' or ''consumer surplus'', and suppression metric should account for this.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(early_adopter_extraction_fairness, preference, 'Fairness of early adopter value capture under network effects').

omega_variable(
    regulatory_intervention_effectiveness,
    'Can interoperability requirements (DMA, DSA) reduce network effects concentration without destroying the network''s coordination function?',
    'Post-regulation empirical analysis: measure user growth on interoperable competitors, data portability uptake, protocol bridge effectiveness. Track whether open protocols achieve critical mass under regulatory pressure.',
    'If regulatory intervention succeeds: Scaffold classification becomes viable (sunset: network effects concentration is temporary, solvable through interoperability). If it fails or reduces overall network value: Snare or Mountain classification is correct — the extraction mechanism is structurally unresolvable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_intervention_effectiveness, empirical, 'Effectiveness of interoperability mandates in reducing concentration').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(network_effects_concentration, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(neteff_tr_t0, network_effects_concentration, theater_ratio, 0, 0.35).
narrative_ontology:measurement(neteff_tr_t5, network_effects_concentration, theater_ratio, 5, 0.4).
narrative_ontology:measurement(neteff_tr_t10, network_effects_concentration, theater_ratio, 10, 0.45).

% Extraction over time
narrative_ontology:measurement(neteff_be_t0, network_effects_concentration, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(neteff_be_t5, network_effects_concentration, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(neteff_be_t10, network_effects_concentration, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(network_effects_concentration, global_infrastructure).
narrative_ontology:boltzmann_floor_override(network_effects_concentration, 0.18).
narrative_ontology:affects_constraint(network_effects_concentration, digital_monopoly_gatekeeping).
narrative_ontology:affects_constraint(network_effects_concentration, algorithmic_attention_extraction).
narrative_ontology:affects_constraint(network_effects_concentration, data_portability_restriction).

% DUAL FORMULATION NOTE:
% Network effects concentration is upstream of several specific platform extraction mechanisms (gatekeeping, attention capture, data restriction). The upstream constraint (network effects) has ε=0.58 (Tangled Rope: coordination + extraction coexist). Downstream constraints decompose the extraction mechanism into specific vectors. All three downstream constraints have higher ε values and stricter Snare/Piton classifications because they represent pure extraction layers atop the network effects coordination base.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(network_effects_concentration, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
