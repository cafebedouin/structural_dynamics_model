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
 *   constraint_id: climate_harm_prevention__adaptation_priority
 *   human_readable: Climate Adaptation Priority Framework (Legitimate Constraint Reading)
 *   domain: environmental/political_economy/intergenerational
 *
 * SUMMARY:
 *   This constraint instantiates one reading of the contested
 *   climate-harm-prevention kernel: the adaptation-priority reading asserts
 *   that legitimate climate response must prioritize near-term resilience
 *   building for present vulnerable populations, accepting that rapid
 *   emissions reduction is politically and economically infeasible. This
 *   reading coordinates a real present need (coastal communities facing
 *   imminent flooding, agricultural regions in drought, populations exposed
 *   to heat extremes) with a constructed trade-off: the reading accepts
 *   higher future warming as the price of avoiding present disruption to
 *   incumbent economic structures. The constraint benefits present-day
 *   vulnerable populations (through front-loaded adaptation investment) and
 *   adaptation-industry beneficiaries, while imposing costs on future
 *   generations (higher warming trajectory, residual climate risk) and
 *   low-adaptation-capacity regions (insufficient funding to build
 *   independent resilience). The kernel context: this reading competes with
 *   mitigation-priority (prioritize emissions reduction; adaptation follows)
 *   and degrowth (planned economic contraction as the only feasible
 *   mitigation path). Each reading instantiates a different constraint with
 *   different beneficiary/victim structure, different ε, and different
 *   classification. This file generates only the adaptation-priority reading;
 *   the sibling readings are separate constraint stories linked via network
 *   relationships.
 *
 * KEY AGENTS:
 *   - present_vulnerable_populations: Primary beneficiary of front-loaded adaptation; facing imminent climate harm; politically mobilizable around present need
 *   - early_adopter_adaptation_industries: Institutional beneficiary and part-time agenda-setter; capture adaptation contracts; shape policy toward adaptation framing
 *   - wealthy_high_capacity_regions: Institutional beneficiary; can fund large-scale adaptation and arbitrage globally; benefit from adaptation-vs-mitigation trade-off (avoid forced transition)
 *   - future_generations: Primary victim; inherit higher warming trajectory; face cumulative damages as cost of present adaptation prioritization
 *   - low_adaptation_capacity_regions: Victim; lack capital and technology; adaptation-priority reading channels resources away from capacity-building mitigation
 *   - climate_scientists_empirical_consensus: Observer; produce data showing adaptation-alone inadequacy and future harm; excluded from binding policy-making
 *   - political_feasibility_doctrine_proponents: Agenda-setter; frame emissions reduction as infeasible; control the debate framing
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
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_harm_prevention__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_harm_prevention__adaptation_priority, "Climate Adaptation Priority Framework (Legitimate Constraint Reading)").
narrative_ontology:topic_domain(climate_harm_prevention__adaptation_priority, "environmental/political_economy/intergenerational").

domain_priors:requires_active_enforcement(climate_harm_prevention__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_harm_prevention__adaptation_priority, '6c612591-b1fa-486b-b724-c07889c506d5').
narrative_ontology:cs_kernel_codification('6c612591-b1fa-486b-b724-c07889c506d5', formalized).
narrative_ontology:cs_authority_grounding('6c612591-b1fa-486b-b724-c07889c506d5', extraction).
narrative_ontology:cs_interpretation_layer_present('6c612591-b1fa-486b-b724-c07889c506d5').
narrative_ontology:cs_reading_relation('6c612591-b1fa-486b-b724-c07889c506d5', climate_harm_prevention__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('6c612591-b1fa-486b-b724-c07889c506d5', climate_harm_prevention__degrowth_reading, influences).
narrative_ontology:cs_axiom('6c612591-b1fa-486b-b724-c07889c506d5', foundational, near_term_pragmatism).
narrative_ontology:cs_axiom_status(near_term_pragmatism, holdable).
narrative_ontology:cs_axiom_grounding('6c612591-b1fa-486b-b724-c07889c506d5', near_term_pragmatism, instrumental).
narrative_ontology:cs_axiom('6c612591-b1fa-486b-b724-c07889c506d5', foundational, political_infeasibility_of_mitigation).
narrative_ontology:cs_axiom_status(political_infeasibility_of_mitigation, holdable).
narrative_ontology:cs_axiom_grounding('6c612591-b1fa-486b-b724-c07889c506d5', political_infeasibility_of_mitigation, empirically_contingent).
narrative_ontology:cs_reference_frame('6c612591-b1fa-486b-b724-c07889c506d5', near_term_harm_response_necessity).
narrative_ontology:cs_drift_state('6c612591-b1fa-486b-b724-c07889c506d5', contemporary_warming_accumulation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('6c612591-b1fa-486b-b724-c07889c506d5', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(climate_harm_prevention__adaptation_priority, climate_harm_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, present_vulnerable_populations).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, early_adopter_adaptation_industries).
narrative_ontology:constraint_victim(climate_harm_prevention__adaptation_priority, future_generations).
narrative_ontology:constraint_victim(climate_harm_prevention__adaptation_priority, low_adaptation_capacity_regions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, wealthy_high_capacity_regions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communities already experiencing climate harm (coastal flooding, drought, heat extremes) receive front-loaded adaptation investments: seawalls, water infrastructure, early warning systems. They benefit immediately from resilience building. Their geographic specificity and present vulnerability make them legible to political systems. Exit from the constraint is not available—they are embedded in the places being adapted.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, present_vulnerable_populations, beneficiary,
    powerless, immediate, trapped, local).

% Private adaptation infrastructure providers (coastal defense contractors, climate-resilient agriculture firms, insurance and bond markets innovating adaptation finance) capture contracts and profitable growth trajectories funded by adaptation budgets. They shape policy toward adaptation framing because it creates their market. They can arbitrage capital and expertise globally.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, early_adopter_adaptation_industries, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_harm_prevention__adaptation_priority, early_adopter_adaptation_industries, agenda_setter).

% Wealthy nations and regions with capital, technology access, and governance capacity can fund and execute adaptation. They bear lower adaptation costs per capita. They can relocate populations, invest in infrastructure, and shift supply chains. The adaptation-priority framing allows them to pursue localized resilience without the economic disruption mitigation would require.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, wealthy_high_capacity_regions, beneficiary,
    institutional, generational, arbitrage, global).

% Unborn and young cohorts inherit a higher warming trajectory (accepted as the trade-off for current adaptation focus) and the cumulative damages: more frequent extremes, ecosystem collapse, water scarcity, forced migration. They bear extraction costs—the present generation front-loads adaptation to minimize its own disruption while pushing warming risk onto future climates they will inhabit. They cannot exit or negotiate.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, future_generations, payer,
    powerless, civilizational, trapped, universal).

% Poor nations and regions without capital or technology capacity cannot fund large-scale adaptation. They are locked into low-margin adaptation strategies (subsistence diversification, migration). The adaptation-priority reading channels international climate finance toward adaptation, not mitigation or loss-and-damage, which leaves their structural vulnerability unchanged. They face both present harm and residual warming—constrained by poverty and geographic exposure.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, low_adaptation_capacity_regions, payer,
    powerless, biographical, constrained, global).

% Produce models and data showing that adaptation alone cannot prevent severe harm, that emissions reduction is necessary to limit warming to survivable levels, and that the warming trajectory accepted in this reading produces irreversible tipping points. They observe the constraint but do not hold formal power to enforce or dissolve it; policy-makers cite or ignore their work as politically convenient.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, climate_scientists_empirical_consensus, observer,
    analytical, civilizational, analytical, universal).

% Actors advocating for aggressive emissions reduction (renewable-energy deployment, grid transformation, industrial decarbonization, technology innovation) are structurally excluded from shaping the constraint's direction. The adaptation-priority framing crowds out mitigation budgets and political will. They would contest the trade-off but are not in the room when adaptation-vs-mitigation allocation is decided.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, mitigation_technology_advocates, excluded,
    organized, generational, constrained, global).

% Policy elites and economists who frame the constraint assert that rapid emissions reduction is politically impossible because it threatens incumbent interests, requires immediate sacrifice, and lacks broad coalition support. They argue adaptation is the only feasible path. They set the terms of the policy debate and control which framings enter formal decision-making.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, political_feasibility_doctrine_proponents, agenda_setter,
    institutional, biographical, arbitrage, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_harm_prevention__adaptation_priority, early_adopter_adaptation_industries).
narrative_ontology:fixing_cost_class(climate_harm_prevention__adaptation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates present resilience building—seawalls, water infrastructure, disaster preparedness, local adaptive capacity—across vulnerable populations and regions that already face climate stress. Solves the immediate-harm-response problem: people experiencing floods, droughts, heat waves need defenses now, not promises of emissions cuts 20 years hence.
% TRANSFER_FUNCTION: Moves climate finance (public and private capital) from potential emissions-reduction investment toward adaptation infrastructure projects. Transfers future climate risk onto later generations and lower-capacity regions by accepting a higher warming trajectory. Transfers political burden away from incumbent economic structures (fossil fuel divestment, industrial transformation) and onto spatially bounded resilience for present vulnerable communities.
% ABSENT_VOICES: Future generations are structurally excluded—they have no voice in present climate policy and cannot negotiate the warming trajectory they inherit. Low-capacity regions lack the political power to demand mitigation or loss-and-damage instead of adaptation. Climate scientists whose models show adaptation-alone inadequacy are cited selectively but excluded from binding decision-making. Mitigation-technology and degrowth advocates are not in the room when adaptation budgets are allocated.
% DISAPPEARANCE_RATIONALE: If the adaptation-priority constraint vanished, climate finance would reallocate toward mitigation and loss-and-damage compensation (emissions reduction, technology transition, reparations for climate-displaced populations). The warming trajectory would shift downward as mitigation gained political priority. Incumbent fossil-fuel and carbon-intensive industries would face forced transition. Low-capacity regions would receive support for emissions-reduction capacity, not just resilience infrastructure. The present generation would incur larger near-term economic costs.
% FOUNDING_PROBLEM: Present populations face imminent climate harm (flooding, drought, heat waves) while global emissions reduction has stalled politically and economically; the founding problem is the gap between urgent present need and slow mitigation achievement, and the question of whether it is legitimate to deprioritize future harm prevention in favor of immediate resilience.
% FOUNDING_PROBLEM_CORROBORATION: Present-focused policy advocates and adaptation-industry representatives attest the founding problem is live: present harm is observable and immediate action saves lives now. Climate scientists and future-oriented ethicists attest that the problem is mislabeled: the real founding problem is political failure to sustain emissions reduction, not an intrinsic conflict between adaptation and mitigation (both are necessary). Empirical analysis from outside the adaptation-industry beneficiaries shows adaptation is necessary but insufficient—the framing as either/or is a political choice, not a scientific one.
narrative_ontology:disappearance_verdict(climate_harm_prevention__adaptation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_harm_prevention__adaptation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_harm_prevention__adaptation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.68 at interval end) because the constraint transfers future climate risk onto generations that cannot negotiate or consent; it is not merely a present cost-sharing arrangement but an intergenerational externality. The extraction grows over time as the warming trajectory accumulates and future harm becomes irreversible (t=0 to t=40 shows rising extractiveness as the committed warming builds residual damages). Suppression is moderate (0.52–0.54) because the constraint requires active enforcement of the adaptation-priority framing against mitigation and degrowth alternatives: policy-makers must continually suppress or deprioritize competing readings, suppress climate-science warnings that adaptation is insufficient, and manage dissent from excluded voices (mitigation advocates, future-focused ethics). Theater is moderate (0.41–0.44) because the constraint performs substantial genuine coordination (adaptation-vulnerable populations do need resilience infrastructure), but an increasing share of the constraint's operation is theatrical: defending the adaptation-priority frame rather than expanding genuine resilience capacity, using 'political feasibility' claims to avoid contested trade-offs, and performing concern for future generations while institutionalizing higher warming. Accessibility collapse is moderate-high (0.64) because once the adaptation-priority frame is institutionalized as 'the legitimate climate response,' alternatives (rapid mitigation, degrowth, radical fossil-fuel phase-out) are made to appear infeasible or illegitimate—the frame constrains what enters serious policy debate. Resistance is high (0.71) because mitigation advocates, climate scientists, low-capacity regions, and future-focused ethicists actively contest the adaptation-priority reading; the constraint meets substantial resistance, which requires ongoing suppression effort. The measurements series captures the constraint's lifecycle: initial phase (t=0–10) shows rising extractiveness and theater as the adaptation-priority reading becomes institutionalized policy; plateau phase (t=20–40) shows stabilization at high extraction and theater as the reading becomes canonical but resistance persists.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (wealthy regions, adaptation industries), the constraint is perceived as pragmatic and necessary—a realistic response to political limits on emissions reduction. From the victim seats (future generations, low-capacity regions), the constraint is perceived as elite self-protection dressed in pragmatism—a transfer of risk from those with power to those without voice. The extraction gap arises from the power asymmetry: present elites set the frame and enforce it; future generations and poor regions are not in the room when the trade-off is decided. The engine's per-seat computation will show: beneficiary seats computing near-rope (genuine coordination + modest extraction), victim seats computing near-snare (pure extraction of future risk). The divergence IS the mandatrophy signal: a constraint whose claim depends on stable founding problem, but whose metrics show extraction accumulating as the founding problem diminishes.
 *
 * DIRECTIONALITY LOGIC:
 *   Present_vulnerable_populations is nominal beneficiary (receives adaptation investment immediately) but is also partly victim (they are chosen as adaptation recipients precisely because they are exposed to climate risk, and the adaptation-priority reading locks them into adaptation-dependent identity rather than capacity-building mitigation). Their directionality is complex: they are near-beneficiary on immediate investment (low d), but far-target on the constraint's intergenerational extraction (high d). The composition chosen here is low d on near-term, reflecting that present adaptation is genuine benefit even within a constraint structure. Early_adopter_adaptation_industries and wealthy_high_capacity_regions are clear beneficiaries (d near 0.0): they control resources, set policy, and capture the adaptation market without bearing residual climate costs. Future_generations and low_capacity_regions are clear victims (d near 1.0): they inherit the higher warming and lack exit or negotiation capacity. Climate_scientists are observer (d = 0.5): they produce data but do not hold formal power; their directionality is symmetric—the constraint extracts epistemic authority while they remain outside binding decision-making. Political_feasibility doctrine proponents are agenda-setter (partly beneficiary through control, d low): they set the frame and avoid the political costs of mitigation, but they also bear some residual future risk (even wealthy regions are not immune to high-warming scenarios), so not entirely at d=0.0.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (present populations facing imminent harm while global emissions reduction is slow) is live in the present but becomes dead/contested over the interval. The constraint institutionalizes adaptation-priority, but as adaptation is deployed and present harm is reduced (via defenses, early warning, local resilience), the founding problem's urgency diminishes. Meanwhile, the residual future harm (higher warming trajectory) accumulates and becomes undeniable by t=20–40. At interval end, the founding problem has shifted: it is no longer 'urgent present harm vs. slow mitigation,' but rather 'defended present populations vs. indefensible future warming.' This is classic mandatrophy: the constraint outlives its founding justification. The adaptation-priority reading can adapt its narrative (claim future harm is solvable via post-hoc carbon removal, claim adaptation technology will scale infinitely), but the structural mandatrophy remains: the reading was born to solve a present problem, not to legitimize intergenerational extraction. The constraint persists not because the founding problem remains, but because beneficiaries have institutionalized it and it now protects their interests (avoiding costly mitigation). This is the mandatrophy pattern: a coordination mechanism (real present resilience need) that becomes inertial (persists as elite protection after the founding need is diminished or addressed). The theater_ratio plateau at 0.41–0.44 reflects this: the constraint must perform increasing justification for its persistence as the founding problem's urgency fades.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    political_feasibility_empirical_grounding,
    'Is rapid emissions reduction truly politically/economically infeasible, or is the infeasibility claim a self-fulfilling prophecy created by incumbent interests and insufficiently resourced policy advocacy?',
    'Historical counterfactual: analysis of periods when rapid technology transition occurred despite incumbent resistance (energy transitions, industrial policy, health crises); empirical testing of political coalitions for mitigation vs. adaptation in different governance contexts.',
    'If infeasibility is socially constructed rather than structural, the adaptation-priority reading loses its principal legitimacy warrant; the constraint would reclassify from coordination-with-extraction to pure extraction of future climate risk onto later generations. The axiom ''near_term_pragmatism'' would shift from holdable to overridden.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_feasibility_empirical_grounding, conceptual, 'Whether political infeasibility is empirical constraint or constructed by the reading''s own beneficiaries to preserve the adaptation frame.').

omega_variable(
    warming_trajectory_acceptance_boundary,
    'At what warming level does the residual harm to future generations and low-capacity regions become indefensible even within the adaptation-priority reading''s own ethical framework?',
    'Structured elicitation from adaptation-priority advocates and empirical climate science on tipping-point severity, ecosystem collapse rates, and human habitability thresholds; intergenerational ethics analysis on duties to future generations.',
    'A low threshold would show the reading contains an internal contradiction—it claims to build resilience but accepts warming that makes resilience unachievable. A high threshold would expose the reading as permitting severe future harm and would shift classification toward snare (pure extraction of future risk). The beneficiary group for the constraint (present vulnerable populations) might not be the intended beneficiaries (present elites avoiding mitigation costs).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(warming_trajectory_acceptance_boundary, preference, 'Intergenerational ethics boundary—at what future harm does adaptation-priority lose legitimacy?').

omega_variable(
    benefit_distribution_actual_vs_framed,
    'Do present vulnerable populations actually receive the promised front-loaded adaptation investments, or do adaptation budgets concentrate in wealthy regions while poor regions receive minimal funding?',
    'Audit of climate finance flows (UNFCCC reporting, multilateral development bank data, national budgets); field assessment of adaptation project distribution and cost-per-beneficiary.',
    'If adaptation finance is captured by wealthy regions and adaptation-industry beneficiaries while present vulnerable populations receive minimal investment, the constraint reclassifies toward snare—it extracts future climate risk while failing to deliver its promised present benefit. The beneficiary group ''present_vulnerable_populations'' becomes a rhetorical cover for extraction rather than actual beneficiaries. Theater ratio would ratchet upward.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(benefit_distribution_actual_vs_framed, empirical, 'Whether adaptation budgets actually reach present vulnerable populations or concentrate in wealthy-region hands.').

omega_variable(
    kernel_reading_codification,
    'Is this reading (adaptation-priority as the legitimate climate response) formally institutionalized in climate governance (UNFCCC, national policy, binding commitments), or does it remain a contested political framing without legal force?',
    'Analysis of treaty language, national climate laws, multilateral development bank policy, IPCC Assessment Reports, and binding international agreements; assessment of which reading has policy-codification authority.',
    'High codification would establish the constraint as a commitment-system kernel grounding; low codification would reveal it as a contestable political frame without structural authority. Codification status affects whether the reading can be revised and how binding the acceptance of higher warming becomes. Affects cs_structure authority_grounding classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_codification, empirical, 'Whether the adaptation-priority reading is formally codified in climate governance or remains a contested political position.').

omega_variable(
    sibling_reading_feasibility_contest,
    'Is the mitigation-priority reading structurally incompatible with this adaptation-priority reading, or can both coexist as simultaneous policy and advocacy positions (different parties, different scales)?',
    'Comparative institutional analysis: do governance systems that adopt adaptation-priority simultaneously fund emissions reduction, or do adaptation budgets crowd out mitigation? Can a nation do both, or is it a zero-sum choice?',
    'If the readings coexist empirically (wealthy regions do both, poor regions trapped in adaptation-only), the relation is ''coexists_with.'' If adaptation-priority budgets systematically crowd out mitigation (finite climate finance pie), the relation is ''influences'' or ''forecloses.'' If one reading''s axioms logically rule out the other''s, the relation is ''forecloses.'' This omega resolves which cs_structure reading_relation is correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_feasibility_contest, empirical, 'Whether adaptation-priority and mitigation-priority readings are simultaneously achievable or mutually exclusive in practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_harm_prevention__adaptation_priority, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_harm_prevention__adaptation_priority, theater_ratio, 0, 0.28).
narrative_ontology:measurement(clim_tr_t5, climate_harm_prevention__adaptation_priority, theater_ratio, 5, 0.32).
narrative_ontology:measurement(clim_tr_t10, climate_harm_prevention__adaptation_priority, theater_ratio, 10, 0.36).
narrative_ontology:measurement(clim_tr_t15, climate_harm_prevention__adaptation_priority, theater_ratio, 15, 0.39).
narrative_ontology:measurement(clim_tr_t20, climate_harm_prevention__adaptation_priority, theater_ratio, 20, 0.41).
narrative_ontology:measurement(clim_tr_t30, climate_harm_prevention__adaptation_priority, theater_ratio, 30, 0.43).
narrative_ontology:measurement(clim_tr_t40, climate_harm_prevention__adaptation_priority, theater_ratio, 40, 0.44).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_harm_prevention__adaptation_priority, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(clim_be_t5, climate_harm_prevention__adaptation_priority, base_extractiveness, 5, 0.51).
narrative_ontology:measurement(clim_be_t10, climate_harm_prevention__adaptation_priority, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(clim_be_t15, climate_harm_prevention__adaptation_priority, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(clim_be_t20, climate_harm_prevention__adaptation_priority, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(clim_be_t30, climate_harm_prevention__adaptation_priority, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(clim_be_t40, climate_harm_prevention__adaptation_priority, base_extractiveness, 40, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_harm_prevention__adaptation_priority, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(clim_su_t5, climate_harm_prevention__adaptation_priority, suppression_requirement, 5, 0.43).
narrative_ontology:measurement(clim_su_t10, climate_harm_prevention__adaptation_priority, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(clim_su_t15, climate_harm_prevention__adaptation_priority, suppression_requirement, 15, 0.5).
narrative_ontology:measurement(clim_su_t20, climate_harm_prevention__adaptation_priority, suppression_requirement, 20, 0.52).
narrative_ontology:measurement(clim_su_t30, climate_harm_prevention__adaptation_priority, suppression_requirement, 30, 0.53).
narrative_ontology:measurement(clim_su_t40, climate_harm_prevention__adaptation_priority, suppression_requirement, 40, 0.54).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_harm_prevention__adaptation_priority, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_harm_prevention__adaptation_priority, 0.18).
narrative_ontology:affects_constraint(climate_harm_prevention__adaptation_priority, climate_harm_prevention__mitigation_priority).
narrative_ontology:affects_constraint(climate_harm_prevention__adaptation_priority, climate_harm_prevention__degrowth_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the climate-harm-prevention kernel. Sibling readings (mitigation_priority, degrowth_reading) instantiate the same kernel under different readings, with different ε values, different beneficiary/victim structures, and different classifications. Each reading codifies a different legitimate climate response. They coexist as competing policy frames held by different actors; none has achieved total authority. The adaptation-priority reading is the current dominant frame in major multilateral institutions (World Bank adaptation focus, UNFCCC adaptation agenda) and in wealthy-nation policy, but faces systematic pressure from mitigation and climate-justice advocates. The network links capture the institutional coupling: adaptation-priority budgets crowd out mitigation funding (influences relation); mitigation-priority advocates dispute adaptation-priority's framing of infeasibility (coexists_with, contested); degrowth reading forecloses both adaptation-priority and mitigation-priority within its framework (neither can work without growth contraction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_harm_prevention__adaptation_priority, powerless, 0.35).
constraint_indexing:directionality_override(climate_harm_prevention__adaptation_priority, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
