% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__quantitative_growth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_legitimacy__quantitative_growth_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: performance_legitimacy__quantitative_growth_reading
 *   human_readable: Performance Legitimacy via Quantitative Growth Rates
 *   domain: political_economy/state_capitalism/development_planning
 *
 * SUMMARY:
 *   In state-capitalist systems, legitimacy derives from demonstrated
 *   capacity to deliver material progress. The quantitative-growth reading
 *   grounds legitimacy in GDP growth rate — an unambiguous, aggregable,
 *   comparable metric — and uses growth targeting to coordinate investment
 *   allocation across a partially decentralized economy. Local officials'
 *   careers depend on hitting growth targets; the industrial-export complex
 *   and state-owned enterprises are privileged beneficiaries; rural
 *   populations, informal workers, and environmental commons bear the costs.
 *   The constraint persists by making growth the primary measure of state
 *   success, even as alternative readings (livelihood security, qualitative
 *   development, techno-nationalist capacity) compete for dominance.
 *
 * KEY AGENTS:
 *   - Central planning authority: sets and enforces growth targets via cadre incentives
 *   - Local government officials: career advancement tied to growth achievement; identity-locked into the growth paradigm
 *   - Industrial-export complex: preferential credit and protection; primary beneficiary
 *   - State-owned enterprises: trapped beneficiaries dependent on capital access
 *   - Rural populations: dispossessed of land and labor; bearing environmental costs
 *   - Informal-sector workers: excluded from growth benefits, suppressed in urban planning
 *   - Qualitative-development advocates: excluded from target-setting apparatus
 *   - Techno-nationalist faction: monitors adequacy of growth targets for strategic goals
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__quantitative_growth_reading, 0.68).
domain_priors:suppression_score(performance_legitimacy__quantitative_growth_reading, 0.52).
domain_priors:theater_ratio(performance_legitimacy__quantitative_growth_reading, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__quantitative_growth_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__quantitative_growth_reading, "Performance Legitimacy via Quantitative Growth Rates").
narrative_ontology:topic_domain(performance_legitimacy__quantitative_growth_reading, "political_economy/state_capitalism/development_planning").

domain_priors:requires_active_enforcement(performance_legitimacy__quantitative_growth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__quantitative_growth_reading, 'e29badde-eefb-459c-aedc-c761841835a0').
narrative_ontology:cs_kernel_codification('e29badde-eefb-459c-aedc-c761841835a0', formalized).
narrative_ontology:cs_authority_grounding('e29badde-eefb-459c-aedc-c761841835a0', extraction).
narrative_ontology:cs_interpretation_layer_present('e29badde-eefb-459c-aedc-c761841835a0').
narrative_ontology:cs_reading_relation('e29badde-eefb-459c-aedc-c761841835a0', performance_legitimacy__livelihood_security_reading, coexists_with).
narrative_ontology:cs_reading_relation('e29badde-eefb-459c-aedc-c761841835a0', performance_legitimacy__qualitative_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('e29badde-eefb-459c-aedc-c761841835a0', performance_legitimacy__techno_nationalist_reading, influences).
narrative_ontology:cs_axiom('e29badde-eefb-459c-aedc-c761841835a0', foundational, growth_rate_primary_legitimacy_measure).
narrative_ontology:cs_axiom_status(growth_rate_primary_legitimacy_measure, holdable).
narrative_ontology:cs_axiom_grounding('e29badde-eefb-459c-aedc-c761841835a0', growth_rate_primary_legitimacy_measure, empirically_contingent).
narrative_ontology:cs_axiom('e29badde-eefb-459c-aedc-c761841835a0', secondary, expansion_per_se_proves_system_success).
narrative_ontology:cs_axiom_status(expansion_per_se_proves_system_success, overridden).
narrative_ontology:cs_axiom_grounding('e29badde-eefb-459c-aedc-c761841835a0', expansion_per_se_proves_system_success, empirically_contingent).
narrative_ontology:cs_reference_frame('e29badde-eefb-459c-aedc-c761841835a0', growth_as_proxy_for_system_legitimacy).
narrative_ontology:cs_drift_state('e29badde-eefb-459c-aedc-c761841835a0', contemporary_plateau_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e29badde-eefb-459c-aedc-c761841835a0', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__quantitative_growth_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__quantitative_growth_reading, industrial_export_complex).
narrative_ontology:constraint_beneficiary(performance_legitimacy__quantitative_growth_reading, local_government_officials).
narrative_ontology:constraint_beneficiary(performance_legitimacy__quantitative_growth_reading, state_owned_enterprises).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, rural_populations).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, informal_sector_workers).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, environmental_commons).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, future_generations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, local_government_officials).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the GDP growth target (typically 5-8% annually), allocates investment credits, and calibrates monetary and fiscal policy to achieve it. Measures provincial and local officials' performance primarily against growth achievement. Justifies the target as proof of system efficacy and social progress. Enforces it by tying cadre promotion, resource allocation, and political survival to growth numbers.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, central_planning_authority, agenda_setter,
    institutional, generational, analytical, national).

% Career advancement, budget authority, and political legitimacy depend almost entirely on hitting growth targets. They benefit from prestige and retention when targets are met. They also pay a diffuse cost: they must tolerate overcapacity, environmental degradation, and distorted investment (building what generates nominal growth, not what communities need). Their professional identity is constituted through the growth target; leaving the system means losing career altogether.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, local_government_officials, beneficiary,
    moderate, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(performance_legitimacy__quantitative_growth_reading, local_government_officials, payer).

% Manufacturing, construction, mining, and export sectors receive preferential access to credit, land, and labor. Growth targets drive investment into these sectors regardless of demand, ensuring capital availability and protected market access. They collect the gains from export dependency and overcapacity-driven margins. They have exit options (relocate production, re-denominate in hard currency) unavailable to local officials.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, industrial_export_complex, beneficiary,
    powerful, generational, arbitrage, global).

% Receive directed credit and monopoly protection to ensure they contribute to growth targets. They benefit from access to capital on non-competitive terms. They are also trapped: they cannot exit or restructure without jeopardizing the growth target they are supposed to hit. Their legitimacy is entirely contingent on growth contribution.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, state_owned_enterprises, beneficiary,
    institutional, generational, trapped, national).

% Bear the extraction via land seizure for industrial zones, agricultural devaluation as rural investment is deprioritized in favor of high-growth sectors, and out-migration as rural employment dries up. They do not participate in the growth gains but provide the labor and natural resources that the growth targets consume. Their constraints are geographic and economic: they cannot arbitrage or relocate.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, rural_populations, payer,
    powerless, biographical, constrained, regional).

% Growth targets privilege formal-sector employment and manufacturing jobs. Informal sector (street trade, domestic work, petty services) is either invisible in the growth accounting or actively suppressed (vendors cleared from high-visibility areas to improve the urban image for growth narratives). They pay through exclusion from growth benefits and wage suppression as formal employment absorption is incomplete.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, informal_sector_workers, payer,
    powerless, biographical, constrained, local).

% Air, water, and soil bear the externalities of overcapacity production and rapid industrial expansion. Degradation accelerates as growth targets incentivize production volume over efficiency. Environmental regulation is loosened when it threatens growth numbers. The commons cannot defend itself or exit.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, environmental_commons, payer,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(performance_legitimacy__quantitative_growth_reading, environmental_commons).

% Inherit the debt, environmental damage, and overcapacity built into the growth-maximizing system. They bear the cost of drawing down natural capital and deferred infrastructure maintenance to achieve near-term growth numbers. They have no voice in the constraint's current operation.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, future_generations, payer,
    powerless, civilizational, trapped, national).
narrative_ontology:stakeholder_non_agent(performance_legitimacy__quantitative_growth_reading, future_generations).

% Economists, planners, and policy advocates who argue for efficiency-adjusted growth, structural transformation, and quality metrics. They are excluded from growth-target-setting machinery; their input is incorporated only when it can be framed as supporting the growth number itself (e.g., 'innovation will raise productivity'). They would contest the constraint's premises if admitted to the apparatus.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, qualitative_development_advocates, excluded,
    moderate, generational, constrained, national).

% Monitors whether growth targets support technological self-sufficiency and strategic industry dominance. They align with the growth reading when expansion in semiconductors, batteries, or rare-earth processing is required, but would reorder priorities if a growth target conflicted with tech-security goals. Analytically positioned to track the constraint's adequacy.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, techno_nationalist_faction, observer,
    institutional, generational, analytical, national).

% Citizens and advocacy groups emphasizing employment quality, healthcare, and education access over raw growth numbers. They are structurally excluded from the growth-setting apparatus; their interests are invoked rhetorically ('growth creates jobs') but not measured independently. They would argue for reorienting the constraint toward direct well-being metrics if represented.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, livelihood_security_advocates, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(performance_legitimacy__quantitative_growth_reading, local_government_officials).
narrative_ontology:fixing_cost_class(performance_legitimacy__quantitative_growth_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates investment allocation, labor deployment, and sectoral priority across a large economy: by targeting GDP growth, the planner can signal which sectors receive capital, which get resources, and which workers move. It solves the alignment problem of how a non-market system distributes investment without decentralized price signals.
% TRANSFER_FUNCTION: Transfers environmental capacity, rural land and labor, informal-sector suppression, and future natural capital into present-period growth numbers, which accrue as prestige and resource-authority to central planners and local officials, and as profit and protection to the industrial-export complex. The growth rate becomes the unit of political legitimacy itself.
% ABSENT_VOICES: Rural populations, informal workers, and environmental advocates are structurally excluded from growth-target-setting; they would contest both the target level and the method of achievement if represented in the apparatus. Future generations have no seat at all. Advocates for livelihood security and qualitative development are excluded in practice; their concerns are acknowledged rhetorically but not measured or enforced.
% DISAPPEARANCE_RATIONALE: If the growth-rate-as-legitimacy constraint vanished, the entire performance-measurement and cadre-promotion system would require recalibration. Investment would cease to be automatically directed into high-growth sectors; local officials' career survival would no longer depend on hitting a number; the industrial-export complex would lose preferential credit access. The political economy would reorganize around different legitimacy criteria (livelihood metrics, efficiency, sustainability, or security metrics). Overcapacity would gradually deflate; environmental regulation could tighten; informal sectors might gain space.
% FOUNDING_PROBLEM: After the structural reform of the 1980s–90s, the planning apparatus needed a metric to coordinate a partially decentralized economy without market prices and to demonstrate systemic legitimacy against the claim that planned economies could not deliver material progress. GDP growth was adopted as that metric — visible, comparable, aggregable, and unambiguous in direction (higher is always better).
% FOUNDING_PROBLEM_CORROBORATION: Central planners and industrial-policy officials attest the founding problem is still live: coordination without prices and legitimacy against market-system claims require a unified growth target. Economists and development advocates (outside benefiting circles) attest the founding problem has been solved (China became the world's second-largest economy by 2010) and the constraint now persists as rent extraction, not coordination. Environmental and livelihood advocates attest the constraint actively prevents solutions to their problems. Independent analyses of overcapacity, environmental damage, and income-quality divergence from growth numbers support the 'problem solved, extraction continues' reading.
narrative_ontology:disappearance_verdict(performance_legitimacy__quantitative_growth_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__quantitative_growth_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__quantitative_growth_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(performance_legitimacy__quantitative_growth_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__quantitative_growth_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_legitimacy__quantitative_growth_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(performance_legitimacy__quantitative_growth_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(performance_legitimacy__quantitative_growth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.68 at interval end (starting 0.42): the constraint has become increasingly extractive as the founding coordination problem (how to signal investment direction post-reform) has been solved but the growth-targeting apparatus persists. Suppression is moderate (0.52) because the constraint does not require constant coercion — it works largely through career incentives and internalized values; officials enforce it voluntarily for advancement. Theater is high and rising (0.28 → 0.61): as real growth has slowed and structural problems (overcapacity, environmental damage) have accumulated, increasingly theatrical 'quality improvement' narratives accompany growth-targeting announcements to preserve legitimacy. The measurement series captures this drift: extractiveness plateaus around t=30 (growth has hit demographic and environmental limits), while theater continues rising (narrative intensification compensating for slowing real gains). Accessibility collapse is moderate-high (0.71): alternatives to growth-targeting do exist (livelihood metrics, efficiency measures) but are structurally inaccessible to officials whose survival depends on the current system.
 *
 * PERSPECTIVAL GAP:
 *   From the central planner's and local official's seats, the constraint is genuine coordination: it solves the problem of allocating investment without market prices and provides clarity on performance expectations. Exit from the framework means loss of political position. From the rural population's and informal worker's seats, the constraint is pure extraction: investment flows away from their needs, their labor is drawn off to factories, and they bear environmental costs. They cannot articulate an alternative within the apparatus because they are excluded from it. From the qualitative-development advocates' seat, the constraint is a false coordinate disguising rent extraction — they see the same real numbers but interpret them as indicating system exhaustion rather than progress. The engine computes these divergences from power level (officials are moderate-institutional; rural populations are powerless), exit options (officials are identity-locked; rural workers are constrained), and beneficiary/victim declarations. The claimed type (tangled_rope) and the authored metrics stand independently: the constraint may compute as snare for powerless seats despite the claim, which is exactly the measurement the corpus captures.
 *
 * DIRECTIONALITY LOGIC:
 *   Local officials: moderate power, identity_locked exit (career depends on apparatus), beneficiary role (prestige/advancement from growth achievement), yet also payer (moral costs of suppressing alternatives, managing overcapacity). Directionality near 0.4 — they are more beneficiary than target, but identity-fusion prevents free exit. Industrial-export complex: powerful, arbitrage exit (can relocate), beneficiary role. Directionality near 0.2 — full beneficiary. Rural populations: powerless, constrained exit, victim role. Directionality near 0.9 — full target. The asymmetry drives classification: the constraint coordinates (benefits officials and exporters) while extracting (from rural and informal populations). This is the tangled-rope pattern: genuine coordination on one side, asymmetric extraction on the other, held together by enforcement (suppression of alternative narratives, exclusion of dissenting voices from apparatus, identity-locking of officials).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem ('how to signal investment direction without market prices and demonstrate system legitimacy') was structurally solved by the late 2000s: China became the world's second-largest economy, demonstrating system efficacy; markets had partially emerged; prices were signaling some allocation; and legitimacy itself had been established. After that solution, the constraint persists primarily as rent extraction — local officials extract career advancement, industrialists extract protection and credit, the export complex extracts monopoly conditions. The apparatus has developed significant theater (high-quality development narratives, efficiency language, innovation discourse) to dress the continuing growth extraction as structural transformation. This is exactly mandatrophy: the original function has atrophied, but the constraint persists due to institutional inertia and the beneficiaries' stake in maintaining it. The theater ratio rising (0.28 → 0.61) is the signal of this atrophy: less real coordination happening, more narrative maintenance required.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    growth_measurement_neutrality,
    'Is GDP growth an objective measure of economic progress, or does the choice of what to include in GDP construction encode a specific reading of development that benefits export-sector and formal-employment interests?',
    'Comparison of GDP-based legitimacy rankings with alternative indices (HDI, inequality-adjusted growth, environmental-corrected growth, livelihood-access metrics). If rankings diverge systematically, measurement choice reveals a reading, not neutrality.',
    'If GDP is revealed as a constructed measure favoring the quantitative reading, it becomes a contestable framing rather than objective truth. The legitimacy claim weakens and the constraint becomes more clearly extractive rather than coordinative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(growth_measurement_neutrality, conceptual, 'Whether GDP growth is an objective metric or a constructed measure encoding a specific development reading.').

omega_variable(
    coordination_vs_rent_separation,
    'What portion of the constraint''s persistence is due to genuine coordination need (signaling investment direction in absence of prices) versus rent extraction (beneficiaries defending the growth targeting to protect their position)?',
    'Structural comparison with market economies'' investment coordination mechanisms, and historical analysis of whether coordination needs persisted after the economy marketized partially. If coordination persists in market contexts via price signals and private credit allocation, the ''need'' for growth targeting diminishes.',
    'If rent extraction is the dominant driver of persistence, the constraint reclassifies from tangled-rope (with genuine coordination value) toward snare (pure extraction dressed in coordination language). The theater ratio rising (measured 0.28 → 0.61 over interval) supports this shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_rent_separation, empirical, 'The balance between genuine coordination function and defensive rent extraction.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the suppression of alternative development narratives structural (institutional exclusion from policy apparatus, resource denial to qualitative-development research) or internalized (cadres have internalized that growth is the only legitimate goal, that livelihood concerns are luxuries to address ''after'' growth)?',
    'Post-constraint-relaxation trajectory: if suppression persists after formal exclusionary rules are removed (e.g., qualitative-development advocates are admitted to target-setting), the suppression is partially internalized. If advocacy immediately flourishes, suppression was primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than measured — targets carry the internalization with them. The 0.52 structural suppression metric understates the constraint''s hold. This favors snare classification and explains how the constraint persists despite low coercive machinery compared to its extraction level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether measured suppression is structural or internalized by cadres and officials.').

omega_variable(
    kernel_reading_divergence,
    'This constraint instantiates ONE reading of a contested kernel: performance legitimacy can be grounded in quantitative growth (THIS reading), livelihood security, qualitative transformation, or techno-nationalist capacity. Which reading actually grounds state legitimacy in practice?',
    'Analysis of policy reversals, budget reallocation, and cadre-incentive restructuring over time. If authorities actually reorder priorities when growth conflicts with livelihood, quality, or tech-security goals, the true grounding emerges. If growth always wins, the quantitative reading dominates practice.',
    'If another reading (livelihood or qualitative) is revealed as dominant in practice, this constraint''s authority weakens; it becomes a facade over a different constraint''s operation. If growth does reliably dominate, this reading is confirmed as the true structure and the coexistence of sibling readings is superficial.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Which performance-legitimacy reading actually governs state decision-making when readings conflict.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__quantitative_growth_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t0, performance_legitimacy__quantitative_growth_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(perf_tr_t5, performance_legitimacy__quantitative_growth_reading, theater_ratio, 5, 0.35).
narrative_ontology:measurement(perf_tr_t10, performance_legitimacy__quantitative_growth_reading, theater_ratio, 10, 0.42).
narrative_ontology:measurement(perf_tr_t15, performance_legitimacy__quantitative_growth_reading, theater_ratio, 15, 0.5).
narrative_ontology:measurement(perf_tr_t20, performance_legitimacy__quantitative_growth_reading, theater_ratio, 20, 0.57).
narrative_ontology:measurement(perf_tr_t25, performance_legitimacy__quantitative_growth_reading, theater_ratio, 25, 0.6).
narrative_ontology:measurement(perf_tr_t30, performance_legitimacy__quantitative_growth_reading, theater_ratio, 30, 0.61).
narrative_ontology:measurement(perf_tr_t35, performance_legitimacy__quantitative_growth_reading, theater_ratio, 35, 0.61).
narrative_ontology:measurement(perf_tr_t40, performance_legitimacy__quantitative_growth_reading, theater_ratio, 40, 0.61).

% Extraction over time
narrative_ontology:measurement(perf_be_t0, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(perf_be_t5, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(perf_be_t10, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(perf_be_t15, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 15, 0.61).
narrative_ontology:measurement(perf_be_t20, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(perf_be_t25, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(perf_be_t30, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(perf_be_t35, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement(perf_be_t40, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t0, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(perf_su_t5, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 5, 0.4).
narrative_ontology:measurement(perf_su_t10, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 10, 0.44).
narrative_ontology:measurement(perf_su_t15, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 15, 0.48).
narrative_ontology:measurement(perf_su_t20, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(perf_su_t25, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 25, 0.51).
narrative_ontology:measurement(perf_su_t30, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement(perf_su_t35, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 35, 0.52).
narrative_ontology:measurement(perf_su_t40, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__quantitative_growth_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(performance_legitimacy__quantitative_growth_reading, 0.18).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, performance_legitimacy__livelihood_security_reading).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, performance_legitimacy__qualitative_development_reading).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, performance_legitimacy__techno_nationalist_reading).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, local_government_fiscal_incentive_structure).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, export_dependency_overcapacity_dynamic).

% DUAL FORMULATION NOTE:
% This constraint is one instantiation of a contested kernel: performance_legitimacy. The sibling constraints (livelihood_security_reading, qualitative_development_reading, techno_nationalist_reading) represent alternative readings of the same founding commitment to state-delivered material progress. The quantitative_growth_reading specifies that growth rate is the measure of legitimacy; siblings specify different measures. These are not equivalent framings — they have different beneficiaries, different extraction patterns, and different operational priorities. The constraint family is linked by kernel identity, not by structural equivalence. All four are live positions held by different institutional constituencies; none has foreclosed the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(performance_legitimacy__quantitative_growth_reading, moderate, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
