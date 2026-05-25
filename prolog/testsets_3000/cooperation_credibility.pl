% ============================================================================
% CONSTRAINT STORY: cooperation_credibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cooperation_credibility, []).

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
 *   constraint_id: cooperation_credibility
 *   human_readable: AI Safety Cooperation Credibility Gap in US-China Strategic Competition
 *   domain: international_relations/technology_governance/strategic_competition
 *
 * SUMMARY:
 *   The cooperation credibility constraint emerges from the structural
 *   tension between stated commitments to AI safety in bilateral US-China
 *   dialogues and the revealed preferences of both nations' AI industries for
 *   competitive advantage through rapid deployment. Chinese participation in
 *   international AI safety discussions (bilateral dialogues, multilateral
 *   forums, academic exchanges) signals concern for alignment risk and
 *   willingness to coordinate on governance. However, domestic incentive
 *   structures—state support for AI champions, permissive deployment
 *   environments, integration of AI into strategic competition—create
 *   systematic pressure to prioritize speed over safety. This is not unique
 *   to China; the US exhibits the same pattern with different institutional
 *   mechanisms. The constraint is a credibility gap: both parties engage in
 *   cooperation theater while defecting on costly safety measures. The gap
 *   extracts from the global AI safety regime (which depends on great power
 *   coordination) and from smaller states (forced to align with one bloc's
 *   standards). It benefits both Chinese and US AI industries by providing
 *   regulatory cover for rapid deployment while maintaining international
 *   legitimacy. The theater ratio (0.58) reflects that bilateral dialogues
 *   produce real technical exchange and norm convergence alongside strategic
 *   signaling. The constraint is not pure theater (piton) because genuine
 *   coordination occurs, nor pure coordination (rope) because structural
 *   incentives systematically undermine safety commitments.
 *
 * KEY AGENTS:
 *   - Chinese AI Industry: Primary beneficiary (institutional/arbitrage) — uses safety rhetoric for legitimacy while operating in permissive domestic environment; arbitrages between international norms and domestic incentives
 *   - US AI Industry: Mirror beneficiary (institutional/arbitrage) — uses Chinese competition as justification for rapid deployment; resists domestic regulation while maintaining responsible AI rhetoric
 *   - Global AI Safety Regime: Primary victim (powerless/trapped) — abstract collective good with no enforcement mechanism; bears full cost of coordination failure between superpowers
 *   - Alignment Research Community: Secondary victim (moderate/constrained) — depends on both ecosystems for collaboration; benefits from dialogues but faces extraction through strategic withholding and dual-use restrictions
 *   - Non-Aligned States: Secondary victim (moderate/constrained) — forced to align with one bloc's standards; excluded from governance decisions; lose technological sovereignty
 *   - Multilateral AI Governance Coalition: Organized agents (organized/mobile) — UN AI Advisory Body, OECD, multi-stakeholder initiatives building alternative verification mechanisms with sunset logic
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees irreducible hybrid structure where cooperation and defection are entangled, not separable
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cooperation_credibility, 0.48).
domain_priors:suppression_score(cooperation_credibility, 0.62).
domain_priors:theater_ratio(cooperation_credibility, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cooperation_credibility, extractiveness, 0.48).
narrative_ontology:constraint_metric(cooperation_credibility, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(cooperation_credibility, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cooperation_credibility, tangled_rope).
narrative_ontology:human_readable(cooperation_credibility, "AI Safety Cooperation Credibility Gap in US-China Strategic Competition").
narrative_ontology:topic_domain(cooperation_credibility, "international_relations/technology_governance/strategic_competition").

domain_priors:requires_active_enforcement(cooperation_credibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cooperation_credibility, chinese_ai_industry).
narrative_ontology:constraint_beneficiary(cooperation_credibility, us_ai_industry).
narrative_ontology:constraint_victim(cooperation_credibility, global_ai_safety_regime).
narrative_ontology:constraint_victim(cooperation_credibility, alignment_research_community).
narrative_ontology:constraint_victim(cooperation_credibility, non_aligned_states).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GLOBAL AI SAFETY REGIME (SNARE) — The abstract collective good of coordinated AI governance cannot exit the strategic competition dynamic. Trapped between two superpowers whose stated commitments to safety are undermined by revealed preferences for competitive advantage. Bears full cost of coordination failure with no agency to enforce compliance or exit the game.
constraint_indexing:constraint_classification(cooperation_credibility, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ALIGNMENT RESEARCH COMMUNITY (TANGLED ROPE) — Constrained by dependence on both Chinese and US research ecosystems for data, compute, and collaboration. Benefits from bilateral dialogues that enable information sharing and norm-setting, but extraction occurs through strategic withholding, dual-use restrictions, and pressure to align research agendas with national security priorities. Mixed coordination and extraction — the dialogues are genuine venues for technical exchange AND tools for competitive intelligence gathering.
constraint_indexing:constraint_classification(cooperation_credibility, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: CHINESE AI INDUSTRY (ROPE) — Primary beneficiary. Participation in safety dialogues provides legitimacy, access to Western research, and regulatory cover for rapid deployment. Can arbitrage between domestic permissive environment and international safety theater. Experiences the constraint as coordination: safety rhetoric enables market access and reduces international pressure while domestic incentives remain unchanged. Net beneficiary of the credibility gap.
constraint_indexing:constraint_classification(cooperation_credibility, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: US AI INDUSTRY (ROPE) — Mirror beneficiary. Uses Chinese safety rhetoric as justification for own rapid deployment ('cannot afford to fall behind'). Arbitrages between domestic calls for regulation and international competitive pressure. Benefits from the credibility gap by using it to resist domestic safety constraints while maintaining rhetorical commitment to responsible AI. Both industries benefit from mutual defection disguised as cooperation.
constraint_indexing:constraint_classification(cooperation_credibility, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: NON-ALIGNED STATES (TANGLED ROPE) — Constrained by dependence on either US or Chinese AI infrastructure and unable to exit the bipolar competition. Benefit from bilateral dialogues that set international norms and create space for smaller players, but bear extraction through forced alignment with one bloc's standards, loss of technological sovereignty, and exclusion from governance decisions. The coordination function (norm-setting) is real but asymmetrically distributed.
constraint_indexing:constraint_classification(cooperation_credibility, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: MULTILATERAL AI GOVERNANCE COALITION (SCAFFOLD) — Organized actors (UN AI Advisory Body, OECD AI Principles, multi-stakeholder initiatives) see the bilateral credibility gap as a temporary coordination failure. Building alternative verification mechanisms: international model registries, third-party auditing, transparency standards that bypass bilateral trust requirements. Sunset logic: as technical verification tools mature and multilateral institutions gain capacity, the bilateral theater becomes less necessary for actual safety coordination. Estimated sunset: 15-25 years for multilateral governance infrastructure to achieve enforcement capacity.
constraint_indexing:constraint_classification(cooperation_credibility, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the constraint exhibits genuine coordination (bilateral dialogues do reduce some risks through information sharing and norm convergence) alongside structural extraction (both parties defect on costly safety measures while maintaining cooperation theater). The credibility gap is not pure extraction (snare) because real technical coordination occurs, nor pure coordination (rope) because strategic competition systematically undermines safety commitments. Tangled rope classification reflects irreducible hybrid structure: cooperation and defection are structurally entangled, not separable phases.
constraint_indexing:constraint_classification(cooperation_credibility, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cooperation_credibility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cooperation_credibility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cooperation_credibility, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cooperation_credibility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(cooperation_credibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The credibility gap extracts from the global safety regime by undermining coordination, from alignment researchers through strategic withholding, and from non-aligned states through forced bloc alignment. However, extraction is not maximal because real technical coordination does occur in bilateral dialogues—information sharing, norm convergence, and joint research reduce some risks. The value reflects genuine hybrid structure: cooperation and extraction are structurally entangled. Suppression (0.62): High. Significant barriers to alternative coordination pathways include: bipolar strategic competition structure, absence of multilateral enforcement mechanisms, dual-use technology restrictions, national security classification of AI capabilities, and career/funding pressure on researchers to align with national priorities. Suppression is not total because multilateral institutions are building alternative pathways (scaffold perspective), but current alternatives lack enforcement capacity. Theater ratio (0.58): Moderate-high. Bilateral dialogues produce real technical exchange (working groups on model evaluation, safety standards, risk assessment methodologies) alongside strategic signaling. The theater component reflects: commitments made in dialogues that are not reflected in domestic policy, safety rhetoric used to justify competitive deployment, and governance frameworks that signal internationally but lack domestic enforcement. Theater has increased over the interval as strategic competition has intensified and the gap between stated commitments and revealed preferences has widened.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon—bilateral AI safety dialogues in the context of strategic competition—appears as different constraint types depending on the observer's structural position. The Chinese and US AI industries see coordination (rope): they are net beneficiaries who experience the dialogues as enabling market access and legitimacy. The multilateral governance coalition sees a temporary problem with a sunset (scaffold): alternative verification mechanisms are maturing and will eventually bypass the bilateral credibility gap. The global AI safety regime sees pure extraction (snare): it is trapped in a coordination failure with no exit and no agency. The alignment research community and non-aligned states see mixed coordination and extraction (tangled rope): they benefit from the dialogues but face systematic extraction through strategic withholding and forced alignment. The analytical observer sees irreducible hybrid structure (tangled rope): cooperation and defection are entangled at the structural level, not separable into phases or perspectives. The perspectival gap is not 'which type is correct?' but 'which structural position are you measuring from?' The presheaf over observation sites captures the full structure.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint exhibits asymmetric extraction with multiple beneficiaries and victims. Chinese and US AI industries are both beneficiaries: they experience the constraint as coordination (safety rhetoric provides legitimacy and regulatory cover) while operating in environments that reward rapid deployment. Both can arbitrage between international norms and domestic incentives, giving them low directionality values and negative or low effective extraction. The global AI safety regime is the primary victim: it is powerless (abstract collective good with no advocate) and trapped (cannot exit the bipolar competition). Maximum directionality and maximum effective extraction. The alignment research community and non-aligned states are secondary victims with moderate power and constrained exit: they depend on the bilateral system for access and resources but face extraction through strategic withholding and forced alignment. The multilateral governance coalition has organized power and mobile exit: they are building alternative pathways and see a sunset, giving them low effective extraction despite operating in the same structural environment. The analytical observer sees the irreducible hybrid structure: cooperation is real (information sharing, norm convergence) AND extraction is real (systematic defection on costly measures). Both are structural features, not separable phases.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves the mandatrophy by demonstrating that tangled rope classification at the analytical level does not collapse to rope or snare when examined from other perspectives—instead, it reveals that different agents genuinely experience different constraint types based on their structural positions. The Chinese and US AI industries genuinely experience rope (they are net beneficiaries with arbitrage options). The global AI safety regime genuinely experiences snare (it is trapped with no exit). The analytical observer's tangled rope classification is not an average or compromise—it reflects the irreducible structural fact that cooperation and extraction are entangled in this constraint. The bilateral dialogues produce real technical coordination (information sharing reduces some risks) AND systematic defection (both parties prioritize competitive advantage over costly safety measures). These are not separable phases (cooperation now, extraction later) or separable components (cooperation in one domain, extraction in another)—they are structurally simultaneous. The mandatrophy is resolved by showing that the tangled rope classification captures a real structural property (entangled cooperation and extraction) that is distinct from both pure coordination (rope) and pure extraction (snare), and that this structural property coexists with different agents experiencing different types based on their positions in the extraction flow.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revealed_preference_measurement,
    'How do we measure the gap between stated safety commitments and revealed preferences in model deployment decisions?',
    'Longitudinal tracking of: (1) safety commitments in bilateral dialogues vs actual deployment timelines, (2) domestic governance frameworks vs enforcement actions, (3) resource allocation to safety research vs capability research, (4) model release decisions vs stated risk thresholds',
    'If gap is small (< 20% divergence): cooperation is credible, constraint is rope from more perspectives. If gap is large (> 60% divergence): cooperation is theater, constraint is snare from more perspectives. Current estimate: 45-55% divergence, supporting tangled rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revealed_preference_measurement, empirical, 'Measurement methodology for stated vs revealed safety preferences').

omega_variable(
    strategic_stability_threshold,
    'At what level of AI capability does strategic instability override safety cooperation incentives?',
    'Game-theoretic modeling of cooperation breakdown points; historical analysis of arms control regime failures; identification of capability thresholds where first-mover advantage dominates mutual safety benefits',
    'If threshold is high (AGI-level): current cooperation is structurally stable, scaffold perspective strengthened. If threshold is low (GPT-5 level): cooperation is fragile, snare perspective strengthened. Threshold uncertainty drives classification ambiguity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(strategic_stability_threshold, conceptual, 'Capability threshold at which cooperation becomes structurally unstable').

omega_variable(
    verification_feasibility,
    'Are AI safety commitments technically verifiable without revealing competitive intelligence?',
    'Development of privacy-preserving verification protocols (zero-knowledge proofs for model properties, secure multi-party computation for capability assessments); feasibility demonstrations in bilateral or multilateral settings',
    'If verification is feasible: cooperation credibility can be established, reducing extraction. If verification is infeasible: cooperation remains unverifiable theater, increasing extraction. Current state: verification protocols exist in research but not deployed at scale.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(verification_feasibility, empirical, 'Technical feasibility of verifying safety commitments without intelligence leakage').

omega_variable(
    domestic_governance_autonomy,
    'To what extent do Chinese domestic AI governance frameworks reflect genuine safety priorities vs international signaling?',
    'Comparative analysis of: (1) enforcement patterns in domestic vs international contexts, (2) resource allocation to safety infrastructure vs capability development, (3) regulatory stringency for domestic vs export models, (4) alignment between stated principles and implemented rules',
    'If frameworks reflect genuine priorities: cooperation has domestic political support, reducing credibility gap. If frameworks are primarily signaling: cooperation is theater, increasing credibility gap. Ambiguity in this variable drives uncertainty in beneficiary vs victim classification for Chinese AI industry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_governance_autonomy, empirical, 'Autonomy of Chinese domestic governance from international signaling incentives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cooperation_credibility, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coop_cred_theater_2019, cooperation_credibility, theater_ratio, 0, 0.35).
narrative_ontology:measurement(coop_cred_theater_2021, cooperation_credibility, theater_ratio, 2, 0.42).
narrative_ontology:measurement(coop_cred_theater_2023, cooperation_credibility, theater_ratio, 4, 0.51).
narrative_ontology:measurement(coop_cred_theater_2025, cooperation_credibility, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(coop_cred_extract_2019, cooperation_credibility, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(coop_cred_extract_2021, cooperation_credibility, base_extractiveness, 2, 0.38).
narrative_ontology:measurement(coop_cred_extract_2023, cooperation_credibility, base_extractiveness, 4, 0.44).
narrative_ontology:measurement(coop_cred_extract_2025, cooperation_credibility, base_extractiveness, 6, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cooperation_credibility, enforcement_mechanism).
narrative_ontology:affects_constraint(cooperation_credibility, ai_compute_governance).
narrative_ontology:affects_constraint(cooperation_credibility, dual_use_export_controls).
narrative_ontology:affects_constraint(cooperation_credibility, multilateral_ai_treaty).

% DUAL FORMULATION NOTE:
% The cooperation credibility constraint is part of a broader AI governance constraint family. It is downstream of compute governance (hardware restrictions shape deployment incentives) and dual-use export controls (technology transfer restrictions shape cooperation feasibility), and upstream of multilateral treaty efforts (bilateral credibility gap undermines multilateral coordination). Each constraint in the family has its own extractiveness value reflecting different structural mechanisms, but they form a coupled system where degradation in one propagates to others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
