% ============================================================================
% CONSTRAINT STORY: climate_harm_prevention__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-25
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
    narrative_ontology:constraint_vindicates/2,
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
 *   human_readable: Adaptation-First Climate Response Prioritizing Near-Term Resilience Over Mitigation
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint story captures the 'adaptation priority' reading of the
 *   climate_harm_prevention kernel: the claim that legitimate climate
 *   response must prioritize near-term resilience building because deep
 *   mitigation is politically and economically infeasible, accepting a higher
 *   warming trajectory (2.5-3.5°C) as the cost of feasibility. The reading
 *   front-loads expenditure on adaptation infrastructure, disaster risk
 *   reduction, and social protection for presently vulnerable populations.
 *   Its beneficiaries are actors who gain from immediate resource flows:
 *   present-day vulnerable communities receiving adaptation finance, climate
 *   finance institutions channeling funds, governments meeting near-term
 *   electoral and moral demands, and sectors profiting from adaptation
 *   contracts. The victims are future generations inheriting locked-in
 *   warming, regions lacking adaptation capacity (especially small island
 *   states), and ecosystems that cannot adapt on human timescales. The
 *   constraint is actively enforced through international finance rules
 *   (e.g., adaptation/mitigation allocation ratios in climate funds),
 *   national policy frameworks, and the political economy of climate finance
 *   that rewards visible near-term projects over long-term mitigation.
 *
 * KEY AGENTS:
 *   - present_vulnerable_populations: Primary beneficiary (organized/constrained) — receives adaptation finance and resilience investments
 *   - future_generations: Primary victim (powerless/trapped) — bears residual climate costs of higher warming trajectory
 *   - low_adaptation_capacity_regions: Victim (powerless/trapped) — lacks resources to implement resilience at scale
 *   - climate_finance_institutions: Beneficiary/agenda_setter (institutional/arbitrage) — controls allocation, profits from adaptation pipeline
 *   - national_governments_with_adaptation_mandates: Agenda_setter/beneficiary (institutional/arbitrage) — sets policy, captures near-term political credit
 *   - insurance_reinsurance_sector: Beneficiary (powerful/arbitrage) — adaptation reduces near-term payouts, creates new risk products
 *   - construction_infrastructure_firms: Beneficiary (organized/mobile) — direct recipients of adaptation contracts
 *   - small_island_developing_states: Victim (moderate/trapped) — adaptation insufficient for existential threats
 *   - ecosystems_and_biodiversity: Victim (powerless/trapped) — non-adapting, bears full warming impact
 *   - mitigation_advocates: Excluded (organized/constrained) — argues for deep decarbonization, structurally sidelined
 *   - analytical_observer: Observer (analytical/analytical) — assesses structural tradeoffs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_harm_prevention__adaptation_priority, 0.68).
domain_priors:suppression_score(climate_harm_prevention__adaptation_priority, 0.42).
domain_priors:theater_ratio(climate_harm_prevention__adaptation_priority, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_harm_prevention__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_harm_prevention__adaptation_priority, "Adaptation-First Climate Response Prioritizing Near-Term Resilience Over Mitigation").
narrative_ontology:topic_domain(climate_harm_prevention__adaptation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_harm_prevention__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_harm_prevention__adaptation_priority, 'e3907f79-41f3-4969-805c-662738146dd0').
narrative_ontology:cs_kernel_codification('e3907f79-41f3-4969-805c-662738146dd0', formalized).
narrative_ontology:cs_authority_grounding('e3907f79-41f3-4969-805c-662738146dd0', lineage).
narrative_ontology:cs_interpretation_layer_present('e3907f79-41f3-4969-805c-662738146dd0').
narrative_ontology:cs_reading_relation('e3907f79-41f3-4969-805c-662738146dd0', climate_harm_prevention__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('e3907f79-41f3-4969-805c-662738146dd0', climate_harm_prevention__degrowth_reading, coexists_with).
narrative_ontology:cs_axiom('e3907f79-41f3-4969-805c-662738146dd0', foundational, near_term_duty_of_care_trumps_long_term_optimization).
narrative_ontology:cs_axiom_status(near_term_duty_of_care_trumps_long_term_optimization, holdable).
narrative_ontology:cs_axiom_grounding('e3907f79-41f3-4969-805c-662738146dd0', near_term_duty_of_care_trumps_long_term_optimization, deontological).
narrative_ontology:cs_axiom('e3907f79-41f3-4969-805c-662738146dd0', secondary, mitigation_feasibility_constraint_is_structural_not_political).
narrative_ontology:cs_axiom_status(mitigation_feasibility_constraint_is_structural_not_political, holdable).
narrative_ontology:cs_axiom_grounding('e3907f79-41f3-4969-805c-662738146dd0', mitigation_feasibility_constraint_is_structural_not_political, empirically_contingent).
narrative_ontology:cs_reference_frame('e3907f79-41f3-4969-805c-662738146dd0', paris_agreement_adaptation_goal).
narrative_ontology:cs_drift_state('e3907f79-41f3-4969-805c-662738146dd0', post_2023_global_stocktake, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e3907f79-41f3-4969-805c-662738146dd0', '2026-07-25T14:30:00Z').
narrative_ontology:cs_kernel_id(climate_harm_prevention__adaptation_priority, climate_harm_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, present_vulnerable_populations).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, climate_finance_institutions).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, national_governments_with_adaptation_mandates).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, insurance_reinsurance_sector).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, construction_infrastructure_firms).
narrative_ontology:constraint_victim(climate_harm_prevention__adaptation_priority, future_generations).
narrative_ontology:constraint_victim(climate_harm_prevention__adaptation_priority, low_adaptation_capacity_regions).
narrative_ontology:constraint_victim(climate_harm_prevention__adaptation_priority, small_island_developing_states).
narrative_ontology:constraint_victim(climate_harm_prevention__adaptation_priority, ecosystems_and_biodiversity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, small_island_developing_states).
narrative_ontology:constraint_vindicates(climate_harm_prevention__adaptation_priority, political_feasibility_constraint_on_mitigation).
narrative_ontology:constraint_vindicates(climate_harm_prevention__adaptation_priority, near_term_duty_of_care_to_vulnerable).
narrative_ontology:constraint_vindicates(climate_harm_prevention__adaptation_priority, adaptation_as_justice_for_historical_emitters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communities facing immediate climate impacts (floods, heat, drought) who receive adaptation finance, early warning systems, resilient housing, and social protection. They gain tangible near-term resilience but have constrained exit: they cannot individually opt out of the warming trajectory, and their political voice is limited to demanding more adaptation, not choosing mitigation instead.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, present_vulnerable_populations, beneficiary,
    organized, biographical, constrained, global).

% All people born after the decision window who inherit the locked-in warming (2.5-3.5°C) from forgone mitigation. They bear the residual costs: more extreme events, sea-level rise, ecosystem collapse, reduced agricultural productivity. They have zero exit — they cannot choose a different climate, cannot vote in current decisions, and cannot negotiate compensation. Their situation is structural extraction without representation.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, future_generations, payer,
    powerless, civilizational, trapped, universal).

% Regions (primarily Least Developed Countries, parts of Sub-Saharan Africa, South Asia) lacking financial, technical, and institutional capacity to implement adaptation at scale. They receive some adaptation finance but it is insufficient for the magnitude of risk. They are trapped in the constraint because the international finance architecture channels resources through the same institutions that prioritize bankable projects over need.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, low_adaptation_capacity_regions, payer,
    powerless, generational, trapped, regional).

% Multilateral development banks, climate funds (GCF, GEF), bilateral agencies that design and disburse adaptation finance. They set the rules for what counts as adaptation, capture management fees and institutional prestige from the adaptation pipeline, and have arbitrage-grade exit: they can rebalance portfolios, redefine categories, and shift between mitigation/adaptation windows. Their structural position lets them shape the constraint while profiting from its operation.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, climate_finance_institutions, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_harm_prevention__adaptation_priority, climate_finance_institutions, beneficiary).

% Governments that have adopted adaptation-first policies (NAPs, NDCs with adaptation emphasis). They gain near-term political credit for visible resilience projects, control domestic resource allocation, and can access international adaptation finance. Their exit is arbitrage-grade: they can shift rhetoric between adaptation/mitigation, access different finance windows, and delay mitigation commitments. They administer the constraint domestically while benefiting from its international legitimacy.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, national_governments_with_adaptation_mandates, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(climate_harm_prevention__adaptation_priority, national_governments_with_adaptation_mandates, beneficiary).

% Insurance and reinsurance companies that benefit from adaptation investments reducing near-term climate risk to their portfolios, while developing new parametric insurance and resilience-linked products. They have arbitrage exit: they can reprice risk, withdraw from markets, create new financial instruments, and lobby for adaptation standards that reduce their exposure. They do not bear the long-tail warming risk — that is socialized or passed to future policyholders.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, insurance_reinsurance_sector, beneficiary,
    powerful, biographical, arbitrage, global).

% Engineering, construction, and infrastructure firms that win contracts for sea walls, resilient housing, water systems, and urban adaptation projects. They are direct financial beneficiaries of the adaptation priority. Their exit is mobile: they can pursue mitigation contracts (renewables, efficiency) if policy shifts, and operate across jurisdictions. They have no structural lock-in to adaptation — they follow the revenue.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, construction_infrastructure_firms, beneficiary,
    organized, biographical, mobile, global).

% Nation states facing existential threats from sea-level rise and extreme events. They receive adaptation finance (beneficiary) but it is structurally insufficient for territorial survival (payer of residual risk). Their exit is trapped: they cannot relocate populations at scale, cannot unilaterally reduce global emissions, and their diplomatic leverage is limited. The adaptation priority reading offers them resources but accepts a warming trajectory that guarantees their loss.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, small_island_developing_states, payer,
    moderate, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(climate_harm_prevention__adaptation_priority, small_island_developing_states, beneficiary).

% Non-human ecological systems that cannot adapt on human timescales and have no voice in the constraint. They bear the full impact of the accepted warming trajectory: species extinction, biome shifts, coral reef collapse, carbon cycle feedbacks. They are the ultimate trapped payer — zero exit, zero power, total extraction of their persistence.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, ecosystems_and_biodiversity, payer,
    powerless, civilizational, trapped, global).

% Climate justice movements, vulnerable country negotiating blocs (AOSIS, LDCs), scientists, and policy actors arguing for deep mitigation as the primary harm prevention strategy. They are structurally excluded from the resource allocation machinery: adaptation finance flows through separate channels, and the 'feasibility' framing marginalizes their position. Their exit is constrained: they can protest, litigate, and model alternatives, but cannot access the adaptation finance pipeline to redirect it.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, mitigation_advocates, excluded,
    organized, biographical, constrained, global).

% The analytical seat that sees the full structure: the coordination function (near-term protection), the extraction function (future climate space, adaptation rents), the beneficiary coalition, the victim populations, and the kernel contest. This seat does not collect or pay; it classifies.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_harm_prevention__adaptation_priority, climate_finance_institutions).
narrative_ontology:fixing_cost_class(climate_harm_prevention__adaptation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Mobilizes and directs resources toward protecting presently vulnerable populations from climate impacts that are already occurring and will intensify in the near term, solving the immediate collective-action problem of who pays for resilience when impacts are uneven and urgent.
% TRANSFER_FUNCTION: Moves financial resources, technical capacity, and political attention from mitigation investments (which reduce long-term warming) to adaptation projects (which reduce near-term vulnerability), with a commission captured by finance intermediaries and implementing sectors. The transfer is from future climate space (via forgone mitigation) and present public/private budgets to adaptation contractors and intermediaries.
% ABSENT_VOICES: Future generations (by definition absent), ecosystems and non-human species (no standing in climate finance governance), and the global poor in regions where adaptation finance does not reach (structurally excluded by project bankability criteria). Mitigation advocates are present but excluded from the adaptation resource allocation machinery.
% DISAPPEARANCE_RATIONALE: If the adaptation priority constraint vanished overnight, climate finance would need new allocation rules; governments would lose a primary framework for near-term climate action; vulnerable communities would lose promised resilience investments; the mitigation/degrowth readings would contest the vacant 'legitimate response' space. The world would rearrange — but toward what is contested.
% FOUNDING_PROBLEM: The immediate climate impacts already harming vulnerable populations (extreme heat, floods, storms, drought) while mitigation action remained insufficient to prevent near-term worsening. The arrangement was built to answer: 'How do we protect people now, given that emissions reductions take decades to affect warming?'
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (near-term protection of the vulnerable) is attested as live by present_vulnerable_populations, climate_finance_institutions, national_governments, and IPCC Working Group II. It is attested as substantially addressed (or solvable only via mitigation) by mitigation_advocates, small_island_developing_states, and IPCC Working Group III. The IPCC itself — straddling WGII and WGIII — provides corroboration from outside the direct beneficiary set: WGII emphasizes adaptation urgency; WGIII emphasizes mitigation necessity. No single authority outside the beneficiary coalition endorses the adaptation_priority reading as the sole legitimate response.
narrative_ontology:disappearance_verdict(climate_harm_prevention__adaptation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_harm_prevention__adaptation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_harm_prevention__adaptation_priority, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(climate_harm_prevention__adaptation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_harm_prevention__adaptation_priority, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Base extractiveness (0.68 at interval end) reflects the structural transfer: resources flow from future climate space (mitigation potential) to present adaptation projects, with the commission taken by finance intermediaries and implementing sectors. The theater ratio (0.55) is high because the coordination narrative — 'protecting the vulnerable now' — increasingly covers an extraction dynamic where adaptation finance becomes a rent stream for intermediaries and a political credit machine for governments, while the mitigation gap widens. Suppression (0.42) is moderate: the constraint does not physically prevent mitigation, but it structurally suppresses mitigation ambition by legitimizing feasibility arguments and redirecting finance. Accessibility collapse (0.35) is limited because alternatives (mitigation pathways, degrowth, solar radiation management) remain conceptually available and advocated, though politically marginalized. Resistance (0.48) is significant from climate justice movements, small island states, and mitigation advocates who contest the framing of mitigation as infeasible.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute divergent seat types: from the perspective of present_vulnerable_populations (beneficiary, organized, constrained exit), the constraint appears as a rope — genuine coordination delivering life-saving resources. From future_generations (victim, powerless, trapped), it computes as a snare — extraction of their climate space without consent or compensation. From climate_finance_institutions (agenda_setter, institutional, arbitrage), it computes as a tangled_rope — they coordinate adaptation flows while extracting fees and control. From small_island_developing_states (victim, moderate, trapped), the constraint is a snare with existential stakes: adaptation cannot prevent territorial loss, yet they are locked into the framework. This divergence is the structural signature of a contested kernel reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: present_vulnerable_populations (direct recipients of resilience investments), climate_finance_institutions (intermediaries capturing allocation rents), national_governments (near-term political credit and control), insurance_reinsurance_sector (reduced near-term liability, new markets), construction_infrastructure_firms (contract revenue). Victims declared: future_generations (inherited warming, no voice), low_adaptation_capacity_regions (structural inability to protect), small_island_developing_states (existential risk beyond adaptation), ecosystems_and_biodiversity (non-adaptive, total loss). Directionality derives from this structure: beneficiaries have low d (constraint subsidizes them), victims have high d (constraint extracts their future options), agenda_setters sit near symmetric but with arbitrage-grade exit (they can shift portfolios). The excluded mitigation_advocates have constrained exit — they can advocate but cannot access the resource allocation machinery.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — 'protect vulnerable populations from climate impacts now' — remains live (status: contested). But the arrangement has accumulated mandatrophy: the adaptation priority was justified as a temporary complement to mitigation; it has become a substitute. The theater ratio rising from 0.25 to 0.55 tracks this: adaptation increasingly performs the *appearance* of climate action while mitigation stalls. The constraint now persists not because it solves the founding problem better than alternatives, but because its beneficiary coalition (finance institutions, governments, construction sectors) has grown powerful enough to block mitigation that would reduce their adaptation pipeline. This is a tangled_rope drifting toward snare: the coordination function (protecting the vulnerable) is real but increasingly subordinated to the extraction function (rent capture by intermediaries, political credit for governments).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mitigation_feasibility_boundary,
    'Is deep mitigation (1.5-2°C pathway) truly politically/economically infeasible, or is infeasibility manufactured by the adaptation priority coalition''s resource capture?',
    'Counterfactual analysis: if adaptation finance were capped at incremental cost of resilience (not total project cost), would mitigation investment accelerate? Political economy modeling of coalition interests.',
    'If infeasibility is manufactured, the constraint''s claimed coordination function is a cover for extraction — reclassifies toward snare. If genuine, the tangled_rope classification holds: real coordination + real extraction from future generations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mitigation_feasibility_boundary, empirical, 'Whether the feasibility claim is a structural fact or a coalition-maintained narrative.').

omega_variable(
    adaptation_mitigation_separability,
    'Are adaptation and mitigation structurally separable investments, or does every dollar to adaptation reduce mitigation capacity (crowding out)?',
    'Empirical analysis of climate finance flows: marginal adaptation dollar vs. marginal mitigation dollar in constrained budgets. Natural experiments from countries with ring-fenced mitigation funds.',
    'If crowding out is structural, the constraint extracts from mitigation directly. If separable, extraction is only from future climate space (opportunity cost), not from present mitigation capacity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(adaptation_mitigation_separability, empirical, 'Whether the resource tradeoff is zero-sum at the margin.').

omega_variable(
    intergenerational_consent_ambiguity,
    'Does the adaptation priority reading extract from future generations without consent, or does it discharge a present duty of care that future generations would endorse?',
    'Intergenerational ethics framework testing: would a representative future generation facing 3°C warming endorse the adaptation investments made at the cost of mitigation? Survey of philosophical positions; integrated assessment modeling with intergenerational welfare weights.',
    'If consent is absent, the extraction is non-consensual — strengthens snare classification for future_generations seat. If consent is implied by duty of care, the coordination function is stronger.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_consent_ambiguity, conceptual, 'Whether future generations are victims in a normative sense or beneficiaries of a duty discharged.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the ''climate_harm_prevention'' kernel best framed as a single commitment with three readings, or as three distinct kernels (harm_prevention_now, harm_prevention_future, harm_prevention_systemic)?',
    'Trace the institutional genealogy: does UNFCCC/Paris Agreement text contain one kernel or multiple? Analyze whether the three readings cite the same textual provisions or different ones.',
    'If multiple kernels, the CS structure decomposition changes: each reading would have its own kernel_codification and authority_grounding. The current CS block assumes a single kernel with contested readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel boundary is correctly drawn or whether the dispute is actually between different kernels.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_harm_prevention__adaptation_priority, 2015, 2035).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(climate_adapt_tr_t2015, climate_harm_prevention__adaptation_priority, theater_ratio, 2015, 0.25).
narrative_ontology:measurement(climate_adapt_tr_t2020, climate_harm_prevention__adaptation_priority, theater_ratio, 2020, 0.35).
narrative_ontology:measurement(climate_adapt_tr_t2025, climate_harm_prevention__adaptation_priority, theater_ratio, 2025, 0.45).
narrative_ontology:measurement(climate_adapt_tr_t2030, climate_harm_prevention__adaptation_priority, theater_ratio, 2030, 0.5).
narrative_ontology:measurement(climate_adapt_tr_t2035, climate_harm_prevention__adaptation_priority, theater_ratio, 2035, 0.55).

% Extraction over time
narrative_ontology:measurement(climate_adapt_be_t2015, climate_harm_prevention__adaptation_priority, base_extractiveness, 2015, 0.45).
narrative_ontology:measurement(climate_adapt_be_t2020, climate_harm_prevention__adaptation_priority, base_extractiveness, 2020, 0.52).
narrative_ontology:measurement(climate_adapt_be_t2025, climate_harm_prevention__adaptation_priority, base_extractiveness, 2025, 0.6).
narrative_ontology:measurement(climate_adapt_be_t2030, climate_harm_prevention__adaptation_priority, base_extractiveness, 2030, 0.65).
narrative_ontology:measurement(climate_adapt_be_t2035, climate_harm_prevention__adaptation_priority, base_extractiveness, 2035, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(climate_adapt_su_t2015, climate_harm_prevention__adaptation_priority, suppression_requirement, 2015, 0.2).
narrative_ontology:measurement(climate_adapt_su_t2020, climate_harm_prevention__adaptation_priority, suppression_requirement, 2020, 0.3).
narrative_ontology:measurement(climate_adapt_su_t2025, climate_harm_prevention__adaptation_priority, suppression_requirement, 2025, 0.38).
narrative_ontology:measurement(climate_adapt_su_t2030, climate_harm_prevention__adaptation_priority, suppression_requirement, 2030, 0.42).
narrative_ontology:measurement(climate_adapt_su_t2035, climate_harm_prevention__adaptation_priority, suppression_requirement, 2035, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_harm_prevention__adaptation_priority, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_harm_prevention__adaptation_priority, 0.18).
narrative_ontology:affects_constraint(climate_harm_prevention__adaptation_priority, climate_harm_prevention__mitigation_priority).
narrative_ontology:affects_constraint(climate_harm_prevention__adaptation_priority, climate_harm_prevention__degrowth_reading).
narrative_ontology:affects_constraint(climate_harm_prevention__adaptation_priority, climate_finance_architecture).
narrative_ontology:affects_constraint(climate_harm_prevention__adaptation_priority, loss_and_damage_mechanism).
narrative_ontology:affects_constraint(climate_harm_prevention__adaptation_priority, national_adaptation_plan_mandates).

% DUAL FORMULATION NOTE:
% This constraint (adaptation_priority) is one of three readings of the climate_harm_prevention kernel. It structurally influences mitigation_priority by diverting finance and political capital; it influences degrowth_reading by occupying the 'feasible action' space that degrowth would claim. The ε divergence: adaptation_priority ε=0.68 (extraction from future); mitigation_priority ε≈0.35 (coordination-dominant); degrowth_reading ε≈0.55 (extraction from Global North growth). All three share the kernel's legitimacy claim but instantiate different constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_harm_prevention__adaptation_priority, institutional, 0.3).
constraint_indexing:directionality_override(climate_harm_prevention__adaptation_priority, powerless, 0.95).
constraint_indexing:directionality_override(climate_harm_prevention__adaptation_priority, moderate, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
