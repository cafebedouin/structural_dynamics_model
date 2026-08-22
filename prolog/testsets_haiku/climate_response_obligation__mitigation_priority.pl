% ============================================================================
% CONSTRAINT STORY: climate_response_obligation__mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_obligation__mitigation_priority, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: climate_response_obligation__mitigation_priority
 *   human_readable: Rapid Decarbonization Mandate (Mitigation Priority Reading)
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   Under the mitigation-priority reading of the climate-response kernel,
 *   rapid decarbonization is framed as a moral and practical obligation
 *   grounded in intergenerational justice: future generations (who cannot
 *   consent to present decisions) are the primary beneficiaries of prevented
 *   warming, while the current generation bears transition costs. The Global
 *   North bears disproportionate mitigation burden due to historical
 *   cumulative emissions. Fossil-fuel capital enters the victim set via
 *   stranded assets — infrastructure rendered economically unviable by
 *   policy-driven phase-outs. The constraint instantiates a real coordination
 *   function (solving the collective-action problem of climate mitigation)
 *   AND substantial asymmetric extraction (temporal: current pays for future
 *   benefit; sectoral: carbon workers lose careers; geographic: Global South
 *   adaptation costs underfunded). This reading COEXISTS with
 *   adaptation-priority (accept 2-3°C and invest in resilience) and degrowth
 *   (reduce material throughput) readings of the same kernel; sibling
 *   readings differ on whether prevention is cost-justified, whether
 *   growth-based transition is viable, and which parties are primary
 *   beneficiaries.
 *
 * KEY AGENTS:
 *   - Future generations: powerless, trapped, civilizational time horizon — primary beneficiaries of prevented warming, no voice in policy formation.
 *   - Fossil-fuel incumbents: powerful, constrained exit, biographical horizon — primary payers via stranded assets and phase-out losses.
 *   - Current-generation workers in carbon sectors: moderate power, identity-locked exit, biographical horizon — face job loss and community dislocation; dual position as payers and (contested) long-term beneficiaries.
 *   - Climate-vulnerable populations: organized but powerless, trapped exit, generational horizon — beneficiaries and advocacy voice, but excluded from binding policy setting.
 *   - Agenda-setting governments and IPCC/UNFCCC bodies: institutional power, generational horizon — set and enforce decarbonization targets and phase-out timelines.
 *   - Adaptation-priority advocates: excluded from binding policy in mitigation-priority jurisdictions; powerful economically but sidelined analytically.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_obligation__mitigation_priority, 0.68).
domain_priors:suppression_score(climate_response_obligation__mitigation_priority, 0.72).
domain_priors:theater_ratio(climate_response_obligation__mitigation_priority, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_obligation__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_obligation__mitigation_priority, "Rapid Decarbonization Mandate (Mitigation Priority Reading)").
narrative_ontology:topic_domain(climate_response_obligation__mitigation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_obligation__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_obligation__mitigation_priority, '9d33d8c5-a726-4e63-bd0d-8a81a84f7d49').
narrative_ontology:cs_kernel_codification('9d33d8c5-a726-4e63-bd0d-8a81a84f7d49', distributed).
narrative_ontology:cs_authority_grounding('9d33d8c5-a726-4e63-bd0d-8a81a84f7d49', distributed).
narrative_ontology:cs_reading_relation('9d33d8c5-a726-4e63-bd0d-8a81a84f7d49', climate_response_obligation__adaptation_priority, coexists_with).
narrative_ontology:cs_reading_relation('9d33d8c5-a726-4e63-bd0d-8a81a84f7d49', climate_response_obligation__degrowth_reading, influences).
narrative_ontology:cs_axiom('9d33d8c5-a726-4e63-bd0d-8a81a84f7d49', foundational, prevention_cost_lower_than_adaptation).
narrative_ontology:cs_axiom_status(prevention_cost_lower_than_adaptation, holdable).
narrative_ontology:cs_axiom_grounding('9d33d8c5-a726-4e63-bd0d-8a81a84f7d49', prevention_cost_lower_than_adaptation, empirically_contingent).
narrative_ontology:cs_axiom('9d33d8c5-a726-4e63-bd0d-8a81a84f7d49', foundational, intergenerational_justice_requires_prevention).
narrative_ontology:cs_axiom_status(intergenerational_justice_requires_prevention, holdable).
narrative_ontology:cs_axiom_grounding('9d33d8c5-a726-4e63-bd0d-8a81a84f7d49', intergenerational_justice_requires_prevention, deontological).
narrative_ontology:cs_reference_frame('9d33d8c5-a726-4e63-bd0d-8a81a84f7d49', intergenerational_justice_prevention_mandate).
narrative_ontology:cs_drift_state('9d33d8c5-a726-4e63-bd0d-8a81a84f7d49', contemporary_2024_empirical_climate_moment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9d33d8c5-a726-4e63-bd0d-8a81a84f7d49', '').
narrative_ontology:cs_kernel_id(climate_response_obligation__mitigation_priority, climate_response_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, climate_vulnerable_populations).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, fossil_fuel_incumbents).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, current_generation_transition_bearers).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, global_north_workers_in_carbon_sectors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, global_north_workers_in_carbon_sectors).
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, global_south_populations).
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, current_generation_beneficiaries).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, global_south_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Cannot exit the constraint — they inherit the atmospheric state the current generation's choices lock in. Under the mitigation-priority reading, they are the primary beneficiaries: rapid decarbonization now minimizes warming they will experience. They have no voice in present policy formation and cannot advocate or consent. Their benefit is structural: lower warming threshold means lower adaptation costs, less ecosystem collapse, fewer climate refugee crises in their lifetime.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, future_generations, beneficiary,
    powerless, civilizational, trapped, global).

% Face stranded-asset losses as rapid decarbonization policy phases out coal, oil, and gas infrastructure before end-of-life recovery. Their accumulated capital — reserves, extraction equipment, power plants — becomes economically unviable. They bear the largest direct cost of the mitigation mandate and have invested heavily in delaying or blocking rapid-decarbonization policy.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, fossil_fuel_incumbents, payer,
    powerful, biographical, constrained, global).

% Experience transition costs: higher energy prices during infrastructure changeover, job displacement in carbon-intensive sectors, retraining burden, and investment costs for renewable-energy transition. They pay now for benefit that accrues primarily to future generations — a temporal asymmetry that defines the intergenerational justice frame. Their exit option is constrained: they cannot defer the energy transition.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, current_generation_transition_bearers, payer,
    moderate, biographical, constrained, global).

% Coal miners, oil-rig workers, auto-manufacturing workers in fossil-fuel-dependent regions. They carry dual position: as payers they face job loss and economic dislocation when decarbonization phases out their sectors; as beneficiaries they inherit lower-warming futures. Identity lock is strong — their professional identity, community stability, and regional economic dependence on carbon sectors bind them to the current arrangement even as policy pushes toward alternatives. Exit (retraining, relocation, sectoral shift) is framed as possible but carries psychosocial and economic friction that traps many in the constraint.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, global_north_workers_in_carbon_sectors, payer,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(climate_response_obligation__mitigation_priority, global_north_workers_in_carbon_sectors, beneficiary).

% Climate-vulnerable populations in low-emission-contributing regions (small island states, sub-Saharan Africa, South Asia). They benefit from rapid mitigation — lower warming reduces their disproportionate climate impact burden. They pay indirectly through adaptation-cost burden (the mitigation mandate does not fund adaptation; those costs fall on them) and through delayed development if rapid decarbonization constrains their access to cheap carbon energy for growth. They have minimal voice in setting the constraint.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, global_south_populations, beneficiary,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(climate_response_obligation__mitigation_priority, global_south_populations, payer).

% Communities already experiencing climate impacts: Pacific Islanders facing sea-level rise, Sahel populations in drought, tropical coastal cities in hurricane paths. They are primary beneficiaries of rapid mitigation — every tenth of a degree prevented reduces their future harm. They have organized advocacy and claim intergenerational-justice standing, but their power to set policy is limited; their exclusion from binding policy formation is a structural feature of the constraint.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, climate_vulnerable_populations, beneficiary,
    organized, generational, trapped, global).

% Renewable-energy developers, climate-tech investors, progressive-policy advocates. They benefit from rapid-decarbonization mandates: policy creates markets, investment opportunities, professional careers, and political alignment with their stated values. Their exit option is mobile: they can shift capital and attention if decarbonization becomes unprofitable or deprioritized.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, current_generation_beneficiaries, beneficiary,
    organized, biographical, mobile, global).

% National governments and international bodies (IPCC, UNFCCC) that declare and enforce the mitigation-priority mandate. They set binding decarbonization targets, phase-out timelines, and enforcement mechanisms (carbon pricing, emissions standards, fossil-fuel-subsidy removal). Their exit option is constrained by political commitment and climate-science consensus; backtracking faces legitimacy costs.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, agenda_setting_governments, agenda_setter,
    institutional, generational, constrained, national).

% Sibling-reading advocates who argue for acceptance of 2-3°C warming and investment in adaptation resilience rather than costly prevention. They are structurally excluded from binding policy formation in jurisdictions that adopt mitigation-priority frames; their economic and policy analysis is sidelined or reframed as harmful delay. Inclusion would redirect resources from mitigation to adaptation investment.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, adaptation_priority_advocates, excluded,
    powerful, biographical, constrained, national).

% Climate economists, ethicists, and policy analysts who measure the constraint's actual operation: comparing stated intergenerational-justice rationale against actual benefit/cost distribution, empirical decarbonization rates, and adaptation funding gaps. They observe whether the constraint's enforcement and beneficiary structure match the reading's declared ethical frame.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_obligation__mitigation_priority, diffuse).
narrative_ontology:fixing_cost_class(climate_response_obligation__mitigation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of climate mitigation: individual nations and firms have incentives to free-ride on others' decarbonization while capturing the benefits of a stable climate. A binding global decarbonization mandate (via treaties, domestic legislation, sectoral phase-outs) coordinates action that no market or voluntary mechanism achieves. Without it, each actor waits for others to bear transition costs.
% TRANSFER_FUNCTION: Transfers massive capital investment and operational costs from future generations (who benefit from prevented warming) to the current generation (who pays transition costs and stranded-asset losses). Transfers also flow from Global South to Global North when adaptation funding lags mitigation funding, and from workers in carbon sectors to renewable-energy sectors and climate-tech investors.
% ABSENT_VOICES: Adaptation-priority advocates (excluded from binding policy formation in mitigation-priority jurisdictions); degrowth-reading proponents (sidelined as economically infeasible); unborn future generations (cannot consent or object); workers in carbon-dependent regions (often consulted too late, after policy is set); Global South populations (have minimal voice in setting targets that constrain their development pathways).
% DISAPPEARANCE_RATIONALE: If the rapid-decarbonization mandate vanished, fossil-fuel investment would resume at scale, adaptation priorities would shift resources toward resilience infrastructure, sectoral job loss would reverse (carbon sectors would expand), and atmospheric warming trajectory would accelerate. The entire global energy and industrial system would reorganize around continued fossil-carbon throughput.
narrative_ontology:disappearance_verdict(climate_response_obligation__mitigation_priority, world_rearranges).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_obligation__mitigation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_response_obligation__mitigation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_obligation__mitigation_priority, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_obligation__mitigation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_obligation__mitigation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_obligation__mitigation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at interval end) is high because the constraint transfers massive costs from future generations (powerless, trapped, unborn) to current generation (moderate power, constrained exit), and concentrates sectoral losses (carbon workers, fossil capital) while dispersing sectoral gains (renewable investors, climate-tech developers). Suppression (0.72) is substantial because the mandate requires active enforcement — fossil-fuel-subsidy removal, emissions standards, sectoral phase-outs — to overcome incumbent resistance and prevent free-riding by nations and firms. Suppression is not merely structural (alternatives are economically constrained); it is active and intensifying as fossil incumbents litigate and lobby. Theater (0.41, moderate-low) reflects that decarbonization has real functional content (solving the coordination problem) but increasingly substantial performance: net-zero pledges without binding enforcement, carbon credits that offset rather than reduce, 'just transition' rhetoric without funding commensurate with dislocation costs. Measurement series tracks observed extractiveness and suppression through t=15 (policy hardening phase), then projects to t=35 (stabilization phase where extraction asymptote flattens as remaining high-emission sectors concentrate losses).
 *
 * PERSPECTIVAL GAP:
 *   Dramatic seat divergence: (1) Agenda-setters and climate-vulnerable advocates experience the constraint as legitimate coordination solving an existential collective-action problem — classification likely Rope or light Tangled Rope from their seats. (2) Fossil incumbents experience pure extraction: stranded assets, phase-out timelines they did not consent to, legal barriers to alternative business models — classification likely Snare from their seat. (3) Current-generation workers experience coercion + constrained exit + deferred benefit (they may not live to see warming prevented) — classification Snare or heavy Tangled Rope. (4) Future generations cannot experience it at all in present time; they inherit its outcome. The engine computes per-seat types from directionality (beneficiary vs. payer vs. symmetric) and power/exit; this story's structural asymmetry — beneficiary set is powerless and unborn, payer set is powerful incumbents and moderate workers — ensures the seat divergence is real and substantial. The coordination function is genuine (Q3); the extraction is real (high ε, active enforcement, temporal and sectoral asymmetry); the claim is Tangled Rope from the agenda-setter's seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Future generations: d ≈ 0.0 (full beneficiary pole). They benefit entirely from prevented warming; they bear no transition costs; they have zero exit options (trapped, identity-locked). The engine derives d from beneficiary status + trapped exit → very low d → negative or negligible effective extraction on their seat. Current-generation payers (workers, firms): d ≈ 0.7–0.8 (strong target pole). They are listed as victims; they pay transition costs and stranded-asset losses now; they have constrained or identity-locked exit (cannot refuse energy transition, cannot exit employment sector easily); their time horizon is biographical (benefits accrue to future generations, not them). The engine derives d from victim status + constrained exit → high d → high effective extraction on their seat. Fossil incumbents: d ≈ 0.85 (near-full target). They are payers; their capital is stranded; their exit option is legally constrained (fossil-fuel divestment mandates, subsidy removal, phase-out timelines); their power is powerful (they can resist), but structural barriers (policy lock-in, investor pressure, technological obsolescence) limit de facto exit. Agenda-setters and governments: d ≈ 0.5–0.55 (slight target bias). They enforce the constraint (bearing administrative cost and political friction); they are neither beneficiaries nor payers in the financial sense; they face reputational costs if they backtrack. Global South populations: d ≈ 0.6 (moderate target). They are listed as beneficiaries (lower warming from mitigation), but they also bear costs (constrained development pathways, adaptation funding gap, technology access barriers). The dual role yields moderate directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint does NOT suffer from mandatrophy in the classical sense (founding problem dead, arrangement persists). The founding problem (warming threat) is CONTESTED but LIVE for most stakeholders; adaptation-priority and degrowth readings offer alternatives rather than denial. However, there is a secondary mandatrophy risk: the founding problem was framed as 'urgent coordination is needed because warming is unfolding faster than adaptation can keep pace.' If empirical climate science later shows (1) adaptation capacity is higher than modeled (populations and ecosystems more resilient), or (2) decarbonization is cheaper and faster than modeled (renewable cost curves and electrification pace surprise upward), the mandate's urgency framing becomes contestable. The constraint would not disappear, but its justification would shift. Currently, the mandate is held in place by science consensus and international political lock-in, not by institutional inertia — it is live coordination, not piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intergenerational_consent_paradox,
    'Can future generations meaningfully consent to a constraint that transfers present costs in exchange for future benefits they cannot negotiate or refuse?',
    'Philosophical and legal analysis of whether ''intergenerational justice'' can ground obligations without consent (analogue: parental obligations to children; trusteeship models). Empirical measure: whether present generation''s climate action aligns with what future generations would choose if given voice in present policy (inferred from stated preferences and burden distribution).',
    'If the consent paradox cannot be resolved, the ''intergenerational justice'' framing collapses into present-generation moral choice, not binding obligation — which reframes the constraint from Tangled Rope (coordinating present beneficiaries with future ones) to Snare (present-generation imposition). If resolvable, the framing holds and classification remains Tangled Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_consent_paradox, conceptual, 'Whether intergenerational justice can ground a binding constraint without future consent.').

omega_variable(
    adaptation_cost_underfunding_extraction,
    'Is the gap between mitigation funding and adaptation funding evidence that the constraint extracts from Global South by denying them resources to adapt, while Global North uses the same resources for domestic decarbonization?',
    'Comparative funding analysis: global climate finance committed to mitigation vs. adaptation vs. loss-and-damage over a 10-year window. Attribution of funding gaps to policy priority vs. economic capacity constraints. Survey of Global South governments and climate-vulnerable populations on whether adaptation deficit is experienced as extraction.',
    'High adaptation underfunding + asymmetric burden (Global North funds its own transition; Global South funds its own adaptation) would classify this reading as Snare from the Global South seat, not Tangled Rope. Low underfunding or evidence that it reflects capacity constraints rather than policy priority would support Tangled Rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(adaptation_cost_underfunding_extraction, empirical, 'Whether the constraint''s enforcement structure creates hidden extraction via adaptation-funding asymmetry.').

omega_variable(
    stranded_asset_moral_boundary,
    'What is the moral and legal status of stranded-asset losses imposed by decarbonization policy? Is rapid phase-out a legitimate response to climate emergency (asset holders accept climate cost), or illegitimate confiscation (asset holders are owed compensation)?',
    'Legal and policy analysis of precedent (regulatory takings law, constitutional compensation doctrine). Empirical measure: whether national decarbonization policies include just-transition funds, worker retraining, and carbon-worker compensation at levels asset holders deem adequate. Survey of affected workers and firms on whether compensation is experienced as adequate or coercive.',
    'If stranded-asset losses are treated as unjust confiscation, the constraint''s suppression (enforcement against incumbent resistance) escalates toward Snare-level coercion. If losses are treated as legitimate cost-bearing by capital that built on high-carbon premises, extraction is reframed as penalty rather than theft, and the constraint remains Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stranded_asset_moral_boundary, preference, 'Whether stranded-asset losses are legitimate climate cost or unjust extraction.').

omega_variable(
    reading_coexistence_vs_foreclosure,
    'Can mitigation-priority and adaptation-priority readings coexist in the same policy framework, or does choosing rapid mitigation foreclose meaningful adaptation investment?',
    'Analysis of national climate budgets and policy mixing: do jurisdictions that adopt 1.5–2.0°C mitigation targets also fund adaptation at the scale climate impacts require? Natural experiment from jurisdictions that mandate both (e.g., EU adaptation strategy alongside decarbonization targets). Modeling of adaptation costs under 1.5°C vs. 3°C scenarios to test whether mitigation-priority reading''s claim (prevention is cheaper) holds.',
    'If mitigation-priority and adaptation-priority are genuinely coexistent (both can be pursued; choice is policy, not logic), the reading_relations field marks coexists_with. If choosing mitigation forecloses adaptation (budget constraints, technology trade-offs), the relation should shift toward forecloses or influences. If neither, mitigation-priority remains the more cost-effective choice but not logically unique.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_coexistence_vs_foreclosure, empirical, 'Whether the sibling readings'' policy recommendations can coexist or whether one''s adoption structurally excludes the other.').

omega_variable(
    temporal_asymmetry_of_identity_lock,
    'Do carbon-sector workers experience the constraint as extraction partly because their identity (professional, regional, family) is fused with the disappearing sector, making psychological/relational exit costs higher than economic exit costs?',
    'Psychological and sociological study of post-transition-phase workers: do those who exit coal/oil sectors report persistent attachment to ''former worker'' identity? Do retraining and relocation fail at higher rates when alternative sectors lack cultural resonance or community? Interviews with workers on why identity-locked exit persists even when economic alternatives exist.',
    'If suppression is primarily internalized (identity-locked, self-perpetuated even after barrier removal), the constraint operates more extractively from the workers'' seat than structural measures (economic retraining) alone suggest. If suppression is primarily structural (economic barriers), exit after barrier removal should be faster. This informs whether the identity-locked exit vector is accurate and guides omega_suppression_mechanism toward structural vs. internalized classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporal_asymmetry_of_identity_lock, empirical, 'Whether worker identity-lock in the constraint is structural (economic) or internalized (psychological).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_obligation__mitigation_priority, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_obligation__mitigation_priority, theater_ratio, 0, 0.28).
narrative_ontology:measurement(clim_tr_t5, climate_response_obligation__mitigation_priority, theater_ratio, 5, 0.33).
narrative_ontology:measurement(clim_tr_t10, climate_response_obligation__mitigation_priority, theater_ratio, 10, 0.37).
narrative_ontology:measurement(clim_tr_t15, climate_response_obligation__mitigation_priority, theater_ratio, 15, 0.39).
narrative_ontology:measurement(clim_tr_t20, climate_response_obligation__mitigation_priority, theater_ratio, 20, 0.4).
narrative_ontology:measurement(clim_tr_t25, climate_response_obligation__mitigation_priority, theater_ratio, 25, 0.41).
narrative_ontology:measurement(clim_tr_t30, climate_response_obligation__mitigation_priority, theater_ratio, 30, 0.41).
narrative_ontology:measurement(clim_tr_t35, climate_response_obligation__mitigation_priority, theater_ratio, 35, 0.41).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_obligation__mitigation_priority, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(clim_be_t5, climate_response_obligation__mitigation_priority, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(clim_be_t10, climate_response_obligation__mitigation_priority, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(clim_be_t15, climate_response_obligation__mitigation_priority, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(clim_be_t20, climate_response_obligation__mitigation_priority, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(clim_be_t25, climate_response_obligation__mitigation_priority, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(clim_be_t30, climate_response_obligation__mitigation_priority, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(clim_be_t35, climate_response_obligation__mitigation_priority, base_extractiveness, 35, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_obligation__mitigation_priority, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(clim_su_t5, climate_response_obligation__mitigation_priority, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(clim_su_t10, climate_response_obligation__mitigation_priority, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(clim_su_t15, climate_response_obligation__mitigation_priority, suppression_requirement, 15, 0.69).
narrative_ontology:measurement(clim_su_t20, climate_response_obligation__mitigation_priority, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(clim_su_t25, climate_response_obligation__mitigation_priority, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(clim_su_t30, climate_response_obligation__mitigation_priority, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(clim_su_t35, climate_response_obligation__mitigation_priority, suppression_requirement, 35, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_obligation__mitigation_priority, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(climate_response_obligation__mitigation_priority, 0.12).
narrative_ontology:affects_constraint(climate_response_obligation__mitigation_priority, climate_response_obligation__adaptation_priority).
narrative_ontology:affects_constraint(climate_response_obligation__mitigation_priority, climate_response_obligation__degrowth_reading).
narrative_ontology:affects_constraint(climate_response_obligation__mitigation_priority, stranded_assets_constraint).
narrative_ontology:affects_constraint(climate_response_obligation__mitigation_priority, just_transition_funding_obligation).

% DUAL FORMULATION NOTE:
% This story is one reading (mitigation_priority) of the climate_response_obligation kernel. Sibling readings (adaptation_priority, degrowth_reading) instantiate the same kernel with different beneficiary structures, victim sets, and policy implications. Each reading has its own ε, classified independently; the kernel contest is routed to omega variables and cs_structure fields. The three stories together form a constraint family linked by the kernel and by network affects_constraints edges — each story names the siblings in its affects_constraints array.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_obligation__mitigation_priority, institutional, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
