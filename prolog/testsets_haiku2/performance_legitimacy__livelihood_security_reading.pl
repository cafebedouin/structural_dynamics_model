% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__livelihood_security_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_legitimacy__livelihood_security_reading, []).

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
 *   constraint_id: performance_legitimacy__livelihood_security_reading
 *   human_readable: Performance Legitimacy: Livelihood Security Reading
 *   domain: political_economy/development_planning/state_capitalism
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested legitimacy
 *   kernel — the 'livelihood security reading,' which grounds regime
 *   legitimacy in tangible improvements citizens directly experience
 *   (employment, healthcare, education, elderly care) rather than growth
 *   rates, technological prowess, or structural transformation metrics. The
 *   state apparatus designates service delivery as the primary legitimacy
 *   standard and allocates resources accordingly, creating coordinated
 *   improvement in daily welfare while imposing opportunity costs on
 *   capital-intensive sectors. This reading coexists with three other
 *   readings of the same kernel: quantitative growth, qualitative
 *   development, and techno-nationalist framings. The constraint described
 *   here is the structural result of choosing the livelihood-security reading
 *   as the governing legitimacy standard.
 *
 * KEY AGENTS:
 *   - state_apparatus: agenda-setter, declares livelihood security as legitimacy standard, enforces through budget and personnel decisions (institutional power, generational horizon)
 *   - service_sector_workers: beneficiaries, gain employment and wage stability from service expansion (moderate power, biographical horizon, constrained exit)
 *   - household_consumption_base: beneficiaries, directly experience healthcare/education/care access, trapped in jurisdiction (powerless, immediate horizon)
 *   - capital_intensive_industries: victims, bear opportunity costs as investment redirects to services (powerful, generational, constrained exit within jurisdiction)
 *   - local_government_infrastructure_budgets: victims, reduced capital spending as livelihood support prioritized (organized, constrained by fiscal dependence)
 *   - development_economists_quantitative_school: excluded, would argue growth sacrifice, voice structurally absent (analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__livelihood_security_reading, 0.62).
domain_priors:suppression_score(performance_legitimacy__livelihood_security_reading, 0.48).
domain_priors:theater_ratio(performance_legitimacy__livelihood_security_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__livelihood_security_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__livelihood_security_reading, "Performance Legitimacy: Livelihood Security Reading").
narrative_ontology:topic_domain(performance_legitimacy__livelihood_security_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__livelihood_security_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__livelihood_security_reading, '0e9b83d9-414f-424a-bdce-7cbe4e2c9d67').
narrative_ontology:cs_kernel_codification('0e9b83d9-414f-424a-bdce-7cbe4e2c9d67', formalized).
narrative_ontology:cs_authority_grounding('0e9b83d9-414f-424a-bdce-7cbe4e2c9d67', extraction).
narrative_ontology:cs_interpretation_layer_present('0e9b83d9-414f-424a-bdce-7cbe4e2c9d67').
narrative_ontology:cs_reading_relation('0e9b83d9-414f-424a-bdce-7cbe4e2c9d67', performance_legitimacy__quantitative_growth_reading, coexists_with).
narrative_ontology:cs_reading_relation('0e9b83d9-414f-424a-bdce-7cbe4e2c9d67', performance_legitimacy__qualitative_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('0e9b83d9-414f-424a-bdce-7cbe4e2c9d67', performance_legitimacy__techno_nationalist_reading, coexists_with).
narrative_ontology:cs_axiom('0e9b83d9-414f-424a-bdce-7cbe4e2c9d67', foundational, livelihood_security_primacy).
narrative_ontology:cs_axiom_status(livelihood_security_primacy, holdable).
narrative_ontology:cs_axiom_grounding('0e9b83d9-414f-424a-bdce-7cbe4e2c9d67', livelihood_security_primacy, instrumental).
narrative_ontology:cs_axiom('0e9b83d9-414f-424a-bdce-7cbe4e2c9d67', foundational, consumption_support_over_capital_accumulation).
narrative_ontology:cs_axiom_status(consumption_support_over_capital_accumulation, holdable).
narrative_ontology:cs_axiom_grounding('0e9b83d9-414f-424a-bdce-7cbe4e2c9d67', consumption_support_over_capital_accumulation, instrumental).
narrative_ontology:cs_reference_frame('0e9b83d9-414f-424a-bdce-7cbe4e2c9d67', service_delivery_legitimacy_regime).
narrative_ontology:cs_drift_state('0e9b83d9-414f-424a-bdce-7cbe4e2c9d67', contemporary_budget_constraints, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0e9b83d9-414f-424a-bdce-7cbe4e2c9d67', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__livelihood_security_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, service_sector_workers).
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, household_consumption_base).
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, elderly_care_recipients).
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, healthcare_access_populations).
narrative_ontology:constraint_victim(performance_legitimacy__livelihood_security_reading, capital_intensive_industries).
narrative_ontology:constraint_victim(performance_legitimacy__livelihood_security_reading, local_government_infrastructure_budgets).
narrative_ontology:constraint_victim(performance_legitimacy__livelihood_security_reading, export_oriented_manufacturing).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, qualitative_development_advocates).
narrative_ontology:constraint_victim(performance_legitimacy__livelihood_security_reading, qualitative_development_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets performance legitimacy standards emphasizing livelihood security — employment stability, healthcare access, education quality, elderly support. Allocates budgets to service delivery and consumption-support mechanisms rather than capital-intensive infrastructure or export-oriented manufacturing. Monitors and reports on these metrics as the primary measure of regime success. Enforces this standard through personnel evaluation, budget allocation, and propaganda.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, state_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Gain employment stability and wage support through state investment in healthcare, education, elderly care, and social services. Direct beneficiaries of the livelihood security standard — their incomes depend on continued service-sector expansion and state commitment to this legitimacy frame. Exit is constrained by skill specialization and lack of alternative sectors at comparable scale.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, service_sector_workers, beneficiary,
    moderate, biographical, constrained, national).

% Access healthcare, education, elderly care, and social safety nets funded through the livelihood security commitment. Directly experience the improvements the regime claims as legitimacy — these services constitute their material reality. Exit is impossible; they are bound to the jurisdiction and depend on public provision.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, household_consumption_base, beneficiary,
    powerless, immediate, trapped, national).

% Receive subsidized or free healthcare as the regime prioritizes medical access as a livelihood-security metric. Without the constraint, private payment would exclude them; with it, they gain access but remain trapped in whatever system the state constructs.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, healthcare_access_populations, beneficiary,
    powerless, immediate, trapped, national).

% Receive pensions, nursing care, and elder support funded through livelihood-security allocation. Directly experience the constraint's benefits; without state provision, they would rely on family or have no support. Exit is impossible by age and circumstance.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, elderly_care_recipients, beneficiary,
    powerless, immediate, trapped, national).

% Bear opportunity costs as the state prioritizes service delivery and consumption support over investment in capital-intensive sectors (steel, heavy manufacturing, infrastructure megaprojects). They receive reduced public funding, lower infrastructure investment priority, and lower tax breaks relative to alternative legitimacy readings. Exit is constrained by regulatory environment and lack of alternative states with comparable scale.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, capital_intensive_industries, payer,
    powerful, generational, constrained, national).

% Bear reduced capital budgets as the livelihood-security reading directs resources to service delivery rather than large infrastructure projects (roads, ports, industrial parks). Their development capacity is constrained by redistribution toward consumption. Exit is limited by fiscal dependence on central-government transfers.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, local_government_infrastructure_budgets, payer,
    organized, biographical, constrained, regional).

% Receives lower prioritization and support as the state emphasizes domestic livelihood security over export-growth metrics. Labor costs may rise as service-sector wages increase; infrastructure investment redirects from export zones. Exit options exist (relocation) but are costly; constraints are substantial within the jurisdiction.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, export_oriented_manufacturing, payer,
    powerful, generational, mobile, global).

% Would argue that the livelihood-security reading sacrifices GDP growth rates and capital accumulation necessary for long-term development. Their voice is structurally excluded from the livelihood-security frame — growth metrics are de-emphasized, making their objections less salient to regime decisions.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, development_economists_quantitative_school, excluded,
    analytical, generational, analytical, global).

% Would argue that livelihood-security prioritization undermines investment in strategic industries and technological self-sufficiency. Their voice is excluded because the constraint's focus on current service delivery crowds out long-term tech development and state capacity. They remain in institutional positions but their framing is subordinated.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, techno_nationalist_advocates, excluded,
    institutional, generational, analytical, national).

% Partially aligned: livelihood security and qualitative development both prioritize human-centered outcomes over raw growth. However, they diverge on emphasis — livelihood security focuses on current welfare access (employment, healthcare now), while qualitative development emphasizes structural transformation and sustainability (which may require investment constraints to differ). Both sit outside the quantitative-growth and techno-nationalist frames.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, qualitative_development_advocates, beneficiary,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(performance_legitimacy__livelihood_security_reading, qualitative_development_advocates, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(performance_legitimacy__livelihood_security_reading, state_apparatus).
narrative_ontology:fixing_cost_class(performance_legitimacy__livelihood_security_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of establishing a shared metric for regime legitimacy that all parties (state, workers, public) can operationalize and observe: instead of competing growth rates or ideological claims, legitimacy is grounded in measurable improvements in employment, healthcare access, education quality, and elderly care. This creates a common evaluation frame.
% TRANSFER_FUNCTION: Moves public resources from capital-intensive industrial investment and local infrastructure expansion toward service-sector employment, healthcare provision, education funding, and elderly-care systems. Transfers opportunity cost from capital-accumulation trajectories to consumption-support mechanisms. Redistribution flows from would-be industrial investment to worker wages and public service provision.
% ABSENT_VOICES: Development economists emphasizing quantitative growth, techno-nationalist advocates prioritizing industrial self-sufficiency, and local-government administrators dependent on infrastructure spending are structurally excluded. They would argue the livelihood-security frame is short-termist, sacrifices capital accumulation, and undermines state capacity. Their objections are not in the room when livelihood metrics drive budget allocation.
% DISAPPEARANCE_RATIONALE: If livelihood-security legitimacy vanished and the state shifted to quantitative-growth or techno-nationalist frames, public-resource allocation would reorganize: healthcare budgets would contract, service-sector hiring would stall, elderly pensions might be cut, and investment would flow to capital-intensive industries and strategic sectors instead. Hundreds of millions of workers and beneficiaries would experience immediate material loss. The constraint's persistence is the primary obstacle preventing that reallocation.
% FOUNDING_PROBLEM: Developed capitalist democracies legitimized themselves through welfare-state provisioning and service delivery; post-socialist and developing states faced a legitimacy deficit if they could not deliver comparable healthcare, education, and employment. The founding problem is: how does a state claim legitimacy without the accumulated capital stock and tax base of mature economies? Answer (the livelihood-security reading): by prioritizing observable, direct improvements in daily life that citizens can verify themselves, rather than claiming growth rates or future potential.
% FOUNDING_PROBLEM_CORROBORATION: State officials attest the founding problem is live: popular discontent rises when service delivery falters, regardless of growth rates. World Bank development literature attests that service access and employment stability predict regime stability and public confidence. However, development economists outside the regime argue the problem is being *resolved by growth itself* — as economies mature, they naturally shift toward service provision, so prioritizing it now is economically myopic. The founding problem status is contested: the state and service-beneficiaries say 'live,' capital-intensive sectors and growth theorists say 'being resolved by growth alone.'
narrative_ontology:disappearance_verdict(performance_legitimacy__livelihood_security_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__livelihood_security_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__livelihood_security_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(performance_legitimacy__livelihood_security_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__livelihood_security_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_legitimacy__livelihood_security_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(performance_legitimacy__livelihood_security_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(performance_legitimacy__livelihood_security_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.62 at interval end) because the constraint genuinely delivers coordination benefits (shared legitimacy metric, observable improvements all parties can verify) AND imposes real opportunity costs on non-favored sectors. The measurement series shows extractiveness rising from 0.48 to 0.62 over the interval as the livelihood-security frame hardens — initial implementation is tentative, but as service metrics become the primary evaluation criterion and budgets lock into that pattern, extraction concentrates. Theater ratio stays low-to-moderate (0.31) because service delivery is functionally real — healthcare, education, and elderly care are genuinely provisioned, not performed. However, 31% of enforcement activity goes to managing capital-sector expectations and suppressing alternative-reading voices rather than delivering services. Suppression requirement (0.48) is moderate because the constraint's beneficiaries (service workers, households, elderly) have no reason to resist it; suppression targets the payers (capital industries, infrastructure administrators) and excluded voices (growth advocates, techno-nationalists). A genuine rope would show lower suppression; the suppression needed here reflects asymmetry — the payers and excluded are concentrated and organized enough that they must be actively managed to prevent them from shifting the legitimacy standard.
 *
 * PERSPECTIVAL GAP:
 *   The state and service-beneficiary seats experience this as pure coordination: 'we solved the legitimacy crisis by establishing a shared, observable metric that justifies resource allocation and creates mutual commitment.' From the capital-sector and infrastructure-administrator seats, the same arrangement is enforced extraction: 'our investment opportunities are crowded out by a political choice to fund services instead; we have no voice in that choice; suppression prevents us from shifting the frame.' The excluded analytical voices (growth economists, techno-nationalists) see it as both extractive (it extracts resources away from growth/tech) and coordinating (it does solve the legitimacy-metric problem, just not in their favor). The engine computes these divergences from the structural data — beneficiary/victim declarations, exit options, power asymmetry — and produces per-seat classifications that will differ across the payer, beneficiary, and agenda-setter seats.
 *
 * DIRECTIONALITY LOGIC:
 *   State apparatus (agenda-setter, institutional power): d near 0.0 → beneficiary, sets the constraint, owns enforcement. Service-sector workers (beneficiary, moderate power, constrained exit): d near 0.15 → net beneficiaries, they collect employment benefits without running the system, but slight extraction because their exit options are limited. Household consumption base (beneficiary, powerless, trapped): d near 0.2 → nominally beneficiary (healthcare/education access) but identity_locked to the state's service provision, so extraction risk is internalized. Capital-intensive industries (payer, powerful, constrained exit): d near 0.75 → near-full targets, they bear the opportunity cost, have some exit (relocation) but it is expensive, suppression is applied to prevent them from shifting the political frame. Local government infrastructure (payer, organized, fiscally dependent): d near 0.8 → full targets, constrained by fiscal transfers, no exit, suppressed to prevent defection to alternative frames. Development economists/excluded voices (analytical, excluded): d near 0.5 → symmetric in principle (they neither collect nor pay materially) but structurally marginalized, so computational directionality may be driven by exclusion rather than symmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exhibits the tangled-rope signature: genuine coordination (shared legitimacy metric, observable improvements, common evaluation frame) AND asymmetric extraction (opportunity costs concentrated on non-favored sectors, voice exclusions prevent alternative framings). The founding problem is live (states do need a legitimacy basis; livelihood security is one defensible answer). The constraint persists because beneficiaries and the state both profit from it; payers persist because exit is constrained and suppression prevents coordination against it. Mandatrophy has not occurred — the founding problem is not yet dead — but the measurement trajectory shows extractiveness plateauing after year 20, suggesting the constraint may be approaching saturation. If extractiveness continues flat while suppression holds, the constraint risks becoming a piton (performance theater replacing real service delivery as budgets tighten). Theater ratio remains low enough (0.31) that this is not imminent, but the omega variables below flag it as a latent risk.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    saturation_and_theater_drift,
    'As extractiveness plateaus but suppression remains necessary, does the constraint risk shifting from tangled_rope toward piton — real service delivery becoming theater to justify resource extraction?',
    'Post-plateau measurement of theater_ratio drift; historical analysis of which services maintain functional provisioning vs. which become performative (waiting-list theater, credential inflation, etc.). Decline in real service quality while metric reporting holds steady would indicate theater rise.',
    'If theater rises to 0.50+, reclassification from tangled_rope toward piton. If theater stays flat, the constraint remains tangled_rope with stable extractiveness. The distinction matters for sustainability: a piton fails when theater becomes indefensible; a tangled_rope can persist as long as beneficiaries and payers remain balanced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(saturation_and_theater_drift, empirical, 'Risk of performance legitimacy degrading into theatrical service delivery as budget constraints tighten.').

omega_variable(
    alternative_reading_coordination_risk,
    'Can capital-intensive sectors and infrastructure administrators coordinate around a competing legitimacy reading (quantitative growth or techno-nationalist frame) strongly enough to shift state priority?',
    'Observation of whether concentrated interests (capital sectors, growth advocates) succeed in reframing regime legitimacy or whether livelihood-security beneficiaries (service workers, households) maintain coalition to defend the current frame. Policy shifts toward growth/tech investment despite livelihood rhetoric would indicate shifting ground.',
    'If payers successfully reframe legitimacy, the constraint transitions toward quantitative_growth_reading; extractiveness and suppression profiles would change. If livelihood frame holds, the constraint remains stable. The risk is concentrated — livelihood-security beneficiaries are numerous but diffuse; payers are concentrated and have institutional voice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_reading_coordination_risk, conceptual, 'Whether livelihood-security reading''s coalition can resist alternative framings or whether concentrated capital interests can shift regime legitimacy standard.').

omega_variable(
    identity_lock_in_service_sectors,
    'To what extent is service-sector employment identity-locked vs. merely beneficiary? If livelihood-security frame collapsed, would service workers exit the constraint or internalize the suppression?',
    'Counterfactual: policy experiment or policy reversal shifting legitimacy to growth frame; observe whether service-sector workers accept lower wages/employment as ''necessary for growth'' or mount resistance. Labor-turnover and strike data post-reframing would indicate identity_lock depth.',
    'High identity-lock means service workers would internalize suppression and remain committed to livelihood-security frame even if state deprioritized it; low identity-lock means they would resist strongly and potentially coordinate with alternative framings. High lock-in makes the constraint more stable; low lock-in makes it vulnerable to collapse if the state reframes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_service_sectors, empirical, 'Depth of professional-identity fusion in service-sector workers to the livelihood-security frame.').

omega_variable(
    reading_selection_as_extractive_choice,
    'Is the selection of livelihood-security reading a genuine coordination solution, or is it a legitimacy cover for a different extraction mechanism (redistribution from poor to service workers, from periphery to urban centers, from young to old)?',
    'Decomposition of beneficiary distribution by income quintile, geography, and age: if livelihood-security reading consistently favors middle-income urban service workers and elderly over poorest rural populations and youth, the reading is a redistributive tool hiding behind universalist legitimacy language.',
    'If the reading masks internal redistribution away from those it claims to benefit, the constraint is more extractive than authored metrics suggest; the true victims are the poorest households and regions, not the capital sectors. Reclassification as snare rather than tangled_rope would follow.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_as_extractive_choice, empirical, 'Whether livelihood-security prioritization masks unequal redistribution under universalist rhetoric.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__livelihood_security_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t0, performance_legitimacy__livelihood_security_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(perf_tr_t5, performance_legitimacy__livelihood_security_reading, theater_ratio, 5, 0.21).
narrative_ontology:measurement(perf_tr_t10, performance_legitimacy__livelihood_security_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement(perf_tr_t15, performance_legitimacy__livelihood_security_reading, theater_ratio, 15, 0.27).
narrative_ontology:measurement(perf_tr_t20, performance_legitimacy__livelihood_security_reading, theater_ratio, 20, 0.29).
narrative_ontology:measurement(perf_tr_t25, performance_legitimacy__livelihood_security_reading, theater_ratio, 25, 0.31).
narrative_ontology:measurement(perf_tr_t30, performance_legitimacy__livelihood_security_reading, theater_ratio, 30, 0.31).
narrative_ontology:measurement(perf_tr_t35, performance_legitimacy__livelihood_security_reading, theater_ratio, 35, 0.31).

% Extraction over time
narrative_ontology:measurement(perf_be_t0, performance_legitimacy__livelihood_security_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(perf_be_t5, performance_legitimacy__livelihood_security_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(perf_be_t10, performance_legitimacy__livelihood_security_reading, base_extractiveness, 10, 0.56).
narrative_ontology:measurement(perf_be_t15, performance_legitimacy__livelihood_security_reading, base_extractiveness, 15, 0.59).
narrative_ontology:measurement(perf_be_t20, performance_legitimacy__livelihood_security_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(perf_be_t25, performance_legitimacy__livelihood_security_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement(perf_be_t30, performance_legitimacy__livelihood_security_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(perf_be_t35, performance_legitimacy__livelihood_security_reading, base_extractiveness, 35, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t0, performance_legitimacy__livelihood_security_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(perf_su_t5, performance_legitimacy__livelihood_security_reading, suppression_requirement, 5, 0.38).
narrative_ontology:measurement(perf_su_t10, performance_legitimacy__livelihood_security_reading, suppression_requirement, 10, 0.41).
narrative_ontology:measurement(perf_su_t15, performance_legitimacy__livelihood_security_reading, suppression_requirement, 15, 0.44).
narrative_ontology:measurement(perf_su_t20, performance_legitimacy__livelihood_security_reading, suppression_requirement, 20, 0.46).
narrative_ontology:measurement(perf_su_t25, performance_legitimacy__livelihood_security_reading, suppression_requirement, 25, 0.48).
narrative_ontology:measurement(perf_su_t30, performance_legitimacy__livelihood_security_reading, suppression_requirement, 30, 0.48).
narrative_ontology:measurement(perf_su_t35, performance_legitimacy__livelihood_security_reading, suppression_requirement, 35, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__livelihood_security_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(performance_legitimacy__livelihood_security_reading, 0.18).
narrative_ontology:affects_constraint(performance_legitimacy__livelihood_security_reading, performance_legitimacy__quantitative_growth_reading).
narrative_ontology:affects_constraint(performance_legitimacy__livelihood_security_reading, performance_legitimacy__qualitative_development_reading).
narrative_ontology:affects_constraint(performance_legitimacy__livelihood_security_reading, performance_legitimacy__techno_nationalist_reading).

% DUAL FORMULATION NOTE:
% The performance_legitimacy kernel decomposes into four structurally distinct constraints, one per reading. This story instantiates the livelihood_security_reading, which prioritizes service delivery and consumption support. The quantitative_growth_reading prioritizes GDP expansion and capital accumulation (downstream adversarial to this constraint: the same resources cannot fund both). The qualitative_development_reading prioritizes structural transformation and sustainability (partially compatible with livelihood security but emphasizes different outcomes). The techno_nationalist_reading prioritizes strategic-sector self-sufficiency and great-power status (extraction profile differs substantially). All four are rooted in the same contested kernel: regime legitimacy grounded in delivering tangible improvements citizens experience. The four readings are not observational variants of one constraint — they propose different constraint structures (different beneficiary/victim sets, different suppression mechanisms, different extraction profiles). Link all four as a constraint family via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(performance_legitimacy__livelihood_security_reading, powerful, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
