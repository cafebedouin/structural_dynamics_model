% ============================================================================
% CONSTRAINT STORY: climate_harm_prevention__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_harm_prevention__adaptation_priority, []).

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
 *   constraint_id: climate_harm_prevention__adaptation_priority
 *   human_readable: Adaptation-Priority Climate Response Framework
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   The adaptation-priority reading of climate response treats near-term
 *   resilience building (protective infrastructure in vulnerable regions) as
 *   the legitimate primary response, on the grounds that global mitigation
 *   (emissions reduction via technological transition) is politically and
 *   economically infeasible at required speed. This reading prioritizes
 *   visible, near-term harm reduction to present vulnerable populations over
 *   prevention of future, larger climate impacts via accelerated
 *   decarbonization. The constraint accepts a locked-in warming trajectory of
 *   2.5–3°C+ in exchange for immediate adaptation spending. The reading is
 *   ONE of three contending claims about legitimate climate response: it
 *   coexists with a mitigation-priority reading (which claims decarbonization
 *   acceleration is both feasible and necessary) and a degrowth reading
 *   (which claims emissions reduction requires planned contraction in Global
 *   North material consumption). The adaptation-priority reading is authoring
 *   the standing arrangement and the extraction it imposes under this
 *   specific framing; sibling readings would author different ε values,
 *   different beneficiary/victim structures, and different classifications.
 *
 * KEY AGENTS:
 *   - present_vulnerable_populations: immediate beneficiaries of adaptation funding (powerless, trapped, immediate horizon)
 *   - adaptation_infrastructure_vendors: institutional beneficiaries, profit-collectors (powerful, arbitrage, biographical horizon)
 *   - fossil_fuel_economy_actors: indirect beneficiaries via deferred mitigation pressure (institutional, mobile, biographical horizon)
 *   - future_generations: powerless victims bearing higher warming trajectory (powerless, trapped, civilizational horizon)
 *   - low_adaptation_capacity_regions: victims with insufficient capital to adapt to 2.5–3°C warming (powerless, constrained, generational horizon)
 *   - climate_mitigation_advocates: excluded parties contesting the feasibility claim (organized, constrained, civilizational horizon)
 *   - policy_implementers: agenda-setters enforcing the adaptation-priority framing (institutional, constrained, biographical horizon)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_harm_prevention__adaptation_priority, 0.68).
domain_priors:suppression_score(climate_harm_prevention__adaptation_priority, 0.52).
domain_priors:theater_ratio(climate_harm_prevention__adaptation_priority, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_harm_prevention__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_harm_prevention__adaptation_priority, "Adaptation-Priority Climate Response Framework").
narrative_ontology:topic_domain(climate_harm_prevention__adaptation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_harm_prevention__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_harm_prevention__adaptation_priority, '4c489fe1-ef9e-4b8b-a672-02e5471d3c91').
narrative_ontology:cs_kernel_codification('4c489fe1-ef9e-4b8b-a672-02e5471d3c91', distributed).
narrative_ontology:cs_authority_grounding('4c489fe1-ef9e-4b8b-a672-02e5471d3c91', extraction).
narrative_ontology:cs_reading_relation('4c489fe1-ef9e-4b8b-a672-02e5471d3c91', climate_harm_prevention__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('4c489fe1-ef9e-4b8b-a672-02e5471d3c91', climate_harm_prevention__degrowth_reading, coexists_with).
narrative_ontology:cs_axiom('4c489fe1-ef9e-4b8b-a672-02e5471d3c91', foundational, mitigation_politically_infeasible_now).
narrative_ontology:cs_axiom_status(mitigation_politically_infeasible_now, holdable).
narrative_ontology:cs_axiom_grounding('4c489fe1-ef9e-4b8b-a672-02e5471d3c91', mitigation_politically_infeasible_now, empirically_contingent).
narrative_ontology:cs_axiom('4c489fe1-ef9e-4b8b-a672-02e5471d3c91', foundational, present_harm_prevention_prioritized_over_future_harm_prevention).
narrative_ontology:cs_axiom_status(present_harm_prevention_prioritized_over_future_harm_prevention, holdable).
narrative_ontology:cs_axiom_grounding('4c489fe1-ef9e-4b8b-a672-02e5471d3c91', present_harm_prevention_prioritized_over_future_harm_prevention, deontological).
narrative_ontology:cs_reference_frame('4c489fe1-ef9e-4b8b-a672-02e5471d3c91', present_vulnerable_populations_primary_moral_claim).
narrative_ontology:cs_drift_state('4c489fe1-ef9e-4b8b-a672-02e5471d3c91', mid_century_warming_lock_in, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4c489fe1-ef9e-4b8b-a672-02e5471d3c91', '').
narrative_ontology:cs_kernel_id(climate_harm_prevention__adaptation_priority, climate_harm_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, present_vulnerable_populations).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, near_term_adaptation_investors).
narrative_ontology:constraint_victim(climate_harm_prevention__adaptation_priority, future_generations).
narrative_ontology:constraint_victim(climate_harm_prevention__adaptation_priority, low_adaptation_capacity_regions).
narrative_ontology:constraint_victim(climate_harm_prevention__adaptation_priority, climate_mitigation_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, adaptation_infrastructure_vendors).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, fossil_fuel_economy_actors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Today's poor and climate-exposed communities receive priority funding for seawalls, cooling centers, water infrastructure, and evacuation planning. These measures provide direct, observable protection in their lifetimes. They benefit from the redirection of climate finance toward adaptation that addresses their immediate survival needs rather than waiting for global mitigation efforts to mature.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, present_vulnerable_populations, beneficiary,
    powerless, immediate, trapped, local).

% Engineering firms, construction companies, and technology vendors that design and build adaptation infrastructure (dikes, drainage systems, early-warning networks, green roofs, heat-resilient agriculture) capture contracts and revenue from the acceleration of adaptation spending. They lobby for adaptation prioritization and gain long-term business opportunities.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, adaptation_infrastructure_vendors, beneficiary,
    powerful, biographical, arbitrage, global).

% Institutional investors, development banks, and insurance companies see adaptation as a profitable, manageable risk with near-term ROI. They fund adaptation projects, develop climate-risk indices for adaptation finance, and benefit from the political decision to treat adaptation as the primary climate response vector.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, near_term_adaptation_investors, beneficiary,
    institutional, biographical, arbitrage, global).

% The adaptation-priority framing accepts a higher warming trajectory, which allows delayed or weakened mitigation (emissions reduction). Incumbent fossil-fuel industries, carbon-intensive manufacturers, and developing-nation energy exporters benefit from deferred pressure to decarbonize and the political permission to continue extraction and combustion longer than mitigation-priority would permit.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, fossil_fuel_economy_actors, beneficiary,
    institutional, biographical, mobile, global).

% Born after the interval endpoint, they inherit a world with 2.5–3°C+ warming locked in by today's forgone mitigation. They cannot exit the constraint or negotiate its terms. They will bear the costs of compound climate impacts that adaptation built today cannot fully offset — flooding in mid-century, ecosystem collapse, resource scarcity, and the need for adaptation at scales current infrastructure cannot match.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, future_generations, payer,
    powerless, civilizational, trapped, global).

% Small island nations, least-developed countries, and regions with weak institutional capacity, low capital reserves, and high climate exposure cannot fund adaptation at the scale required for 2.5–3°C warming. While the adaptation-priority framing directs some finance their way, the residual warming trajectory exceeds their adaptation capacity, leaving them as net payers bearing unmitigated climate harm.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, low_adaptation_capacity_regions, payer,
    powerless, generational, constrained, regional).

% Climate scientists, environmental advocates, and climate-justice movements argue that accepting a higher warming trajectory forecloses possibilities and wastes the window for preventing worse outcomes. They are excluded from the policy table if the adaptation-priority framing has been institutionalized; their objections are treated as impractical idealism rather than legitimate disagreement over the constraint's design.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, climate_mitigation_advocates, payer,
    organized, civilizational, constrained, global).
narrative_ontology:stakeholder_secondary_role(climate_harm_prevention__adaptation_priority, climate_mitigation_advocates, excluded).

% National governments, development agencies, and multilateral institutions adopt the adaptation-priority framing as policy. They set spending priorities, define which populations and regions count as 'vulnerable,' allocate climate finance to adaptation projects, and enforce the constraint by directing resources away from mitigation-acceleration pathways. They justify this via assertions about political feasibility and economic constraint.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, policy_implementers, agenda_setter,
    institutional, biographical, constrained, national).

% Views the constraint from outside the political economy: records the structural choice to prioritize near-term adaptation over mitigation, documents the beneficiary/victim asymmetry, and measures whether the claim of mitigation infeasibility was a permanent structural fact or a political choice made under specific conditions.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_harm_prevention__adaptation_priority, adaptation_infrastructure_vendors).
narrative_ontology:fixing_cost_class(climate_harm_prevention__adaptation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates immediate, observable climate harm reduction (seawalls, cooling infrastructure, water security) among vulnerable populations who would otherwise face unmitigated climate impacts before any global mitigation pathway matured. Solves the tragedy of the commons in climate finance by anchoring deployment around visible, local adaptation rather than the slower, harder global decarbonization.
% TRANSFER_FUNCTION: Moves climate finance, development capital, and construction resources toward near-term adaptation projects in vulnerable regions; simultaneously moves future climate harm (higher warming trajectory, locked-in damages) forward to future generations and low-capacity regions by accepting deferred emissions reductions. Transfers political permission for continued fossil-fuel production from present to future, and transfers unmitigated climate costs from present-vulnerable to future-vulnerable.
% ABSENT_VOICES: Unborn future generations cannot negotiate; low-adaptation-capacity regions with weak institutional voice are present but structurally outbid by near-term investors and adaptation vendors. Climate mitigation advocates and climate scientists who contest the framing of mitigation as infeasible are excluded if the constraint is institutionalized (their alternative is not on the table). Fossil-fuel-dependent workers in transition economies are absent from the justification narrative.
% DISAPPEARANCE_RATIONALE: If the adaptation-priority constraint vanished and were replaced by mitigation-priority (global decarbonization acceleration), the world economy would rearrange: fossil-fuel industries would face faster stranded-asset pressures, carbon-intensive infrastructure would be retired earlier, adaptation spending would shift to mitigation R&D and deployment, and the warming trajectory would compress toward 1.5–2°C instead of 2.5–3°C. Finance flows, industrial investment, technological development pathways, and intergenerational harm profiles would all shift.
% FOUNDING_PROBLEM: Present-day vulnerable populations face unmitigated climate impacts (flooding, drought, heat stress) within years to decades. Global mitigation via emissions reduction is slow (technological development, political negotiation, industrial transition all take decades to mature). Adaptation—building seawalls, water infrastructure, cooling systems—is fast and provides observable protection to people alive today. The founding problem is the mismatch between the speed of present climate harm and the speed of global mitigation deployment.
% FOUNDING_PROBLEM_CORROBORATION: Present-vulnerable populations and climate-impacted cities attest the problem is live: they face flooding and heat stress today. Policy implementers cite the founding problem as justification for adaptation prioritization. Climate scientists and mitigation advocates contest the diagnosis: they attest that the problem is not the slow speed of mitigation globally, but the political choice to underfund mitigation and defer the decision to act. They claim the founding problem is framed to justify a choice made under fossil-fuel political pressure, not as a structural necessity.
narrative_ontology:disappearance_verdict(climate_harm_prevention__adaptation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_harm_prevention__adaptation_priority, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_harm_prevention__adaptation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_harm_prevention__adaptation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_harm_prevention__adaptation_priority, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_harm_prevention__adaptation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_harm_prevention__adaptation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_harm_prevention__adaptation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured at 0.68 at interval end because the constraint imposes a transfer of climate harm across time (present adaptation benefit purchased with future, larger damage) and enforces a political choice (accepting higher warming) that benefits present near-term actors while imposing costs on those with no voice (future generations, low-capacity regions). The constraint is active extraction because it requires enforcement: climate mitigation advocates, climate scientists, and some policy voices continuously contest the framing of mitigation as infeasible and push back against the deferred-harm logic. Suppression is moderate-to-high (0.52, rising to 0.58) because the constraint's persistence requires marginalizing the mitigation-advocate voice and asserting (without continuous proof) that the alternative is infeasible. Theater is substantial (0.41 at interval end, rising to 0.46 mid-interval) because much of the adaptive-infrastructure activity is theatrical: it addresses visible, local flooding while the constraint permits continued emissions that guarantee worse future flooding. The measurement series track the growth in theater (more adaptation money spent with diminishing marginal harm reduction) as the warming trajectory locks in and compound impacts accumulate. Accessibility collapse is low-to-moderate (0.48) because alternatives (mitigation acceleration, degrowth) remain available as live policy options in many jurisdictions, even if the adaptation-priority framing has been institutionalized in others.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of present-vulnerable populations and adaptation investors, the constraint is genuine coordination: it solves the immediate harm problem with observable, deplorable capital. From the seat of future generations and low-capacity regions, it is pure extraction: they bear the compounding climate costs that faster mitigation would have prevented, and they have no option to refuse or renegotiate. From the fossil-fuel industry seat, the constraint is a gift: deferred decarbonization pressure. From the climate-mitigation-advocate seat, the constraint is a snare: the assertion of infeasibility is read as a political choice dressed up as necessity, designed to block the alternative they would choose.
 *
 * DIRECTIONALITY LOGIC:
 *   Present-vulnerable populations hold d near 0.0 (beneficiary end): they receive adaptation spending without paying the cost; they benefit directly. Adaptation investors and fossil-fuel actors also hold d near 0.0 (beneficiaries): they profit from the arrangement and have exit options (arbitrage). Future generations hold d near 1.0 (target end): they bear higher warming with no option to refuse or exit. Low-capacity regions hold d near 0.9: they are targets of the deferred mitigation, with constrained exit options. Climate mitigation advocates hold d near 0.8: they are excluded from the policy table, their objections are suppressed, and they bear the intergenerational cost of the deferred decision. Policy implementers hold d near 0.5 (moderate): they benefit from political permission to defer hard mitigation decisions (easier near-term politics) but also bear reputational cost if adaptation fails and future harm becomes visible.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (present-vulnerable need immediate protection) is live and real. The constraint solves it. But the claim that the alternative (mitigation acceleration) is infeasible is where mandatrophy enters: if the infeasibility claim is itself a political choice made under fossil-fuel pressure, then the constraint persists not because the problem it was built for requires it, but because powerful actors benefit from the deferred harm. The mandate (protect present-vulnerable) remains; the constraint's operation (accept higher warming) has drifted from the mandate's intent. Mandatrophy is not yet resolved because the political feasibility claim is still actively contested—no consensus exists that mitigation was truly infeasible, and some jurisdictions continue to fund decarbonization acceleration despite the adaptation-priority framing. Measurement series show theater rising, suggesting mandatrophy is developing: more adaptation spending announced while the underlying warming trajectory worsens, indicating the constraint is increasingly performing its function rather than accomplishing it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mitigation_feasibility_contestation,
    'Is global emissions reduction to 1.5–2°C warming truly infeasible within existing institutional and economic frameworks, or is the infeasibility claim a political choice made under fossil-fuel pressure to defer decarbonization?',
    'Multi-decade longitudinal observation of technological deployment, policy diffusion, and capital allocation. If decarbonization accelerates despite the adaptation-priority framing''s political dominance, the infeasibility claim was contingent, not structural. If multiple jurisdictions achieve near-zero emissions trajectories via renewables, electrification, and industrial transition despite global policy-level adaptation prioritization, infeasibility was overstated.',
    'If infeasibility is contingent (a political choice), the constraint reclassifies from tangled-rope (coordination + asymmetric extraction) toward pure snare: the founding problem (present-vulnerable need adaptation) remains valid, but the constraint''s solution (accept higher warming) is unnecessary and extracts future harm for present benefit. The mandate persists; mandatrophy is resolved as corruption.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mitigation_feasibility_contestation, empirical, 'Whether the claimed infeasibility of mitigation is a structural barrier or a political choice.').

omega_variable(
    adaptation_capacity_ceiling,
    'Can adaptation infrastructure in low-capacity regions scale to offset 2.5–3°C warming, or does a thermal/resource ceiling exist beyond which adaptation fails?',
    'Mid-century observational data on adaptation-infrastructure success rates in vulnerable regions under 2.5–3°C warming. If agricultural yields collapse, water tables deplete, or migration pressure exceeds adaptive capacity despite adaptation spending, the ceiling exists and the constraint''s extraction is revealed as ineffective.',
    'If the ceiling is real and near, the constraint fails its own test: it promises to protect present-vulnerable populations via adaptation, but locks in warming that renders that adaptation insufficient. The constraint reclassifies toward piton: performed harm-reduction with diminishing functional return, persisting via institutional inertia and beneficiary interests (adaptation investors, vendors, policy lock-in) rather than genuine protective function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_capacity_ceiling, empirical, 'Whether adaptation can scale to protect against 2.5–3°C warming or hits a capacity ceiling.').

omega_variable(
    intergenerational_harm_equivalence,
    'Are the present costs of adaptation-priority (reduced near-term mitigation spending) equivalent to the future costs of higher warming (larger impacts, higher adaptation requirements), or does the future cost exceed the present benefit under any reasonable discount rate or intergenerational ethics framework?',
    'Integrated assessment models comparing present-value climate damages under adaptation-priority vs. mitigation-priority trajectories, using multiple discount rate assumptions (0%, 2%, 5%) and equity weightings (utilitarian vs. prioritarian vs. egalitarian). If future damages exceed present adaptation benefits under any major ethical framework, the constraint''s intergenerational fairness is not established.',
    'If future costs exceed present benefits under standard frameworks, the constraint''s claim to legitimacy rests on hidden assumptions (discounting future harm, privileging present over future, accepting intergenerational inequality). The constraint would require explicit ethical defense (e.g., ''we are permitted to harm the future because present need is urgent''), not the neutral framing of ''feasibility.'' This reframes the constraint from coordination into acknowledged extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_harm_equivalence, preference, 'Whether present adaptation benefits outweigh future harm costs under defensible ethical frameworks.').

omega_variable(
    kernel_alternative_sibling_reading_contestation,
    'Which sibling reading (mitigation_priority or degrowth_reading) would be selected if the adaptation-priority reading were not institutionalized?',
    'Observational: in jurisdictions where adaptation-priority framing has not taken institutional root (e.g., some Nordic nations, some small-island coalitions), what alternatives do policy makers and vulnerable populations choose? Do they select mitigation-priority or degrowth tracks? The choice reveals whether adaptation-priority is genuinely preferred or contingent on institutional momentum and vendor capture.',
    'If mitigation-priority is preferred where adaptation-priority framing has not calcified, the constraint is not a solution to a real coordination problem but a path-dependent institutional choice. If degrowth is preferred in some contexts, the constraint faces a second-order challenge: the reading may be legitimate for growth-committed economies, but those economies may be the ones that are least vulnerable and least in need of present adaptation—suggesting the constraint extracts from the powerless (future, low-capacity regions) to benefit the powerful (present near-term investors).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_alternative_sibling_reading_contestation, conceptual, 'Whether adaptation-priority is genuinely preferred when alternatives are institutionally available.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_harm_prevention__adaptation_priority, 2025, 2055).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t2025, climate_harm_prevention__adaptation_priority, theater_ratio, 2025, 0.32).
narrative_ontology:measurement(clim_tr_t2030, climate_harm_prevention__adaptation_priority, theater_ratio, 2030, 0.36).
narrative_ontology:measurement(clim_tr_t2035, climate_harm_prevention__adaptation_priority, theater_ratio, 2035, 0.4).
narrative_ontology:measurement(clim_tr_t2040, climate_harm_prevention__adaptation_priority, theater_ratio, 2040, 0.43).
narrative_ontology:measurement(clim_tr_t2045, climate_harm_prevention__adaptation_priority, theater_ratio, 2045, 0.46).
narrative_ontology:measurement(clim_tr_t2055, climate_harm_prevention__adaptation_priority, theater_ratio, 2055, 0.41).

% Extraction over time
narrative_ontology:measurement(clim_be_t2025, climate_harm_prevention__adaptation_priority, base_extractiveness, 2025, 0.58).
narrative_ontology:measurement(clim_be_t2030, climate_harm_prevention__adaptation_priority, base_extractiveness, 2030, 0.62).
narrative_ontology:measurement(clim_be_t2035, climate_harm_prevention__adaptation_priority, base_extractiveness, 2035, 0.66).
narrative_ontology:measurement(clim_be_t2040, climate_harm_prevention__adaptation_priority, base_extractiveness, 2040, 0.69).
narrative_ontology:measurement(clim_be_t2045, climate_harm_prevention__adaptation_priority, base_extractiveness, 2045, 0.71).
narrative_ontology:measurement(clim_be_t2055, climate_harm_prevention__adaptation_priority, base_extractiveness, 2055, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t2025, climate_harm_prevention__adaptation_priority, suppression_requirement, 2025, 0.45).
narrative_ontology:measurement(clim_su_t2030, climate_harm_prevention__adaptation_priority, suppression_requirement, 2030, 0.5).
narrative_ontology:measurement(clim_su_t2035, climate_harm_prevention__adaptation_priority, suppression_requirement, 2035, 0.54).
narrative_ontology:measurement(clim_su_t2040, climate_harm_prevention__adaptation_priority, suppression_requirement, 2040, 0.56).
narrative_ontology:measurement(clim_su_t2045, climate_harm_prevention__adaptation_priority, suppression_requirement, 2045, 0.58).
narrative_ontology:measurement(clim_su_t2055, climate_harm_prevention__adaptation_priority, suppression_requirement, 2055, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_harm_prevention__adaptation_priority, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_harm_prevention__adaptation_priority, 0.18).
narrative_ontology:affects_constraint(climate_harm_prevention__adaptation_priority, climate_harm_prevention__mitigation_priority).
narrative_ontology:affects_constraint(climate_harm_prevention__adaptation_priority, climate_harm_prevention__degrowth_reading).
narrative_ontology:affects_constraint(climate_harm_prevention__adaptation_priority, fossil_fuel_extraction_path_dependency).
narrative_ontology:affects_constraint(climate_harm_prevention__adaptation_priority, intergenerational_harm_discounting).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel climate_harm_prevention. Sibling readings (mitigation_priority, degrowth_reading) instantiate the same kernel with different structural choices about what counts as harm, what prevention requires, and who is vulnerable. The three stories form a constraint family: all three are authored simultaneously to preserve the kernel's contestation at the structural level. Decomposition was necessary because the three readings have substantially different ε values, different beneficiary/victim sets, and different legitimacy claims—attempting to author them as one story would violate ε-invariance and fabricate a false consensus about the constraint's nature. The family is linked via network.affects_constraints: adaptation-priority institutionalization creates structural pressure on both sibling readings (constrains their political viability, shifts resources away from their proposed solutions) but does not logically foreclose either (both remain live normative positions). All three readings are coexistent under the kernel commitment 'prevent harm to vulnerable populations,' differing only on what counts as harm and how to prevent it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_harm_prevention__adaptation_priority, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
