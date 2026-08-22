% ============================================================================
% CONSTRAINT STORY: acceptable_risk_for_energy__comparative_risk_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_for_energy__comparative_risk_dominant, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: acceptable_risk_for_energy__comparative_risk_dominant
 *   human_readable: Comparative Risk Acceptability Framework for Nuclear Energy
 *   domain: risk_assessment/energy_policy/public_safety
 *
 * SUMMARY:
 *   This constraint instantiates the comparative-risk-dominant reading of
 *   acceptable energy risk. The kernel is contested: three distinct,
 *   structurally different framings compete for legitimacy in energy policy.
 *   THIS reading asserts that nuclear acceptability has no absolute threshold
 *   — risk is acceptable only relative to fossil fuel alternatives (coal
 *   emissions, climate catastrophe). Under this reading, nuclear plants
 *   operating at present-day accident probability levels are defensible if
 *   decarbonization requires them, even though the accident probability is
 *   not zero and waste storage is unsolved. The constraint is claimed as
 *   tangled_rope because it genuinely coordinates energy supply (baseload
 *   carbon-free power) AND extracts asymmetrically (future generations and
 *   low-income populations bear accident concentration risk; climate benefits
 *   accrue to present and near-future). The claim/metric divergence is
 *   authored deliberately: the constraint is claimed as tangled_rope
 *   (coordination + extraction both present), while metrics show substantial
 *   extractiveness (0.68), high suppression (0.62 of alternative risk
 *   framings), and rising theater ratio (0.41, indicating growing share of
 *   enforcement dedicated to narrative maintenance rather than engineering
 *   risk reduction).
 *
 * KEY AGENTS:
 *   - nuclear_industry: agenda-setter (power: institutional, exit: mobile) — frames acceptability, controls narrative, benefits from relative-risk standard
 *   - climate_mitigation_advocates_accepting_nuclear: beneficiary (power: organized, exit: constrained) — needs nuclear for climate timeline, cannot exit from the framework without losing urgency advantage
 *   - nuclear_waste_inheritors: victim (power: powerless, exit: trapped) — civilizational time horizon, bear multi-millennial storage obligations they do not authorize
 *   - low_income_energy_consumers: victim (power: powerless, exit: trapped) — immediate time horizon, accident concentration in climate-vulnerable regions, cannot relocate
 *   - climate_vulnerable_populations: victim-and-beneficiary (power: powerless, exit: trapped) — simultaneous benefit from decarbonization and exposure to accident risk concentration in high-population-density zones
 *   - regulatory_bodies: agenda-setter (power: institutional, exit: constrained) — embed comparative-risk acceptability into standards, shift burden from prevention to response
 *   - fossil_fuel_industry: excluded (power: powerful, exit: mobile) — structurally excluded by the framework's binary logic (carbon-free vs. coal)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_for_energy__comparative_risk_dominant, 0.68).
domain_priors:suppression_score(acceptable_risk_for_energy__comparative_risk_dominant, 0.62).
domain_priors:theater_ratio(acceptable_risk_for_energy__comparative_risk_dominant, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, extractiveness, 0.68).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_for_energy__comparative_risk_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_for_energy__comparative_risk_dominant, "Comparative Risk Acceptability Framework for Nuclear Energy").
narrative_ontology:topic_domain(acceptable_risk_for_energy__comparative_risk_dominant, "risk_assessment/energy_policy/public_safety").

domain_priors:requires_active_enforcement(acceptable_risk_for_energy__comparative_risk_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_for_energy__comparative_risk_dominant, '66aab8bf-4ae8-4809-9858-1a0541f540ba').
narrative_ontology:cs_kernel_codification('66aab8bf-4ae8-4809-9858-1a0541f540ba', distributed).
narrative_ontology:cs_authority_grounding('66aab8bf-4ae8-4809-9858-1a0541f540ba', distributed).
narrative_ontology:cs_reading_relation('66aab8bf-4ae8-4809-9858-1a0541f540ba', acceptable_risk_for_energy__catastrophic_tail_dominant, coexists_with).
narrative_ontology:cs_reading_relation('66aab8bf-4ae8-4809-9858-1a0541f540ba', acceptable_risk_for_energy__expected_value_dominant, influences).
narrative_ontology:cs_axiom('66aab8bf-4ae8-4809-9858-1a0541f540ba', foundational, climate_urgency_overrides_intergenerational_waste_concern).
narrative_ontology:cs_axiom_status(climate_urgency_overrides_intergenerational_waste_concern, holdable).
narrative_ontology:cs_axiom_grounding('66aab8bf-4ae8-4809-9858-1a0541f540ba', climate_urgency_overrides_intergenerational_waste_concern, empirically_contingent).
narrative_ontology:cs_axiom('66aab8bf-4ae8-4809-9858-1a0541f540ba', foundational, comparative_risk_suffices_for_acceptability).
narrative_ontology:cs_axiom_status(comparative_risk_suffices_for_acceptability, holdable).
narrative_ontology:cs_axiom_grounding('66aab8bf-4ae8-4809-9858-1a0541f540ba', comparative_risk_suffices_for_acceptability, instrumental).
narrative_ontology:cs_reference_frame('66aab8bf-4ae8-4809-9858-1a0541f540ba', fossil_fuel_baseline_energy_system).
narrative_ontology:cs_drift_state('66aab8bf-4ae8-4809-9858-1a0541f540ba', climate_crisis_acceleration, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('66aab8bf-4ae8-4809-9858-1a0541f540ba', '').
narrative_ontology:cs_kernel_id(acceptable_risk_for_energy__comparative_risk_dominant, acceptable_risk_for_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_industry).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__comparative_risk_dominant, electricity_dependent_economies).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__comparative_risk_dominant, climate_mitigation_advocates_accepting_nuclear).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_waste_inheritors).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, low_income_energy_consumers).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, climate_vulnerable_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__comparative_risk_dominant, climate_vulnerable_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets policy narratives around comparative risk. Controls technical risk assessments, manages public messaging about 'safe' operation levels, and frames nuclear as necessary climate solution. Benefit: expansion of nuclear generation, access to climate finance, regulatory relief from absolute safety standards. Plants extended operating licenses and new-build opportunities depend on comparative-risk acceptance. Can exit the constraint by shifting to other energy sectors or jurisdictions if framework collapses.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_industry, agenda_setter,
    institutional, biographical, mobile, global).

% Organized advocacy coalitions (climate scientists, environmental orgs, policy thinkers) who have concluded nuclear is necessary for climate targets. Benefit from the framework: it enables rapid deployment without solving waste storage or achieving accident-free operation, accelerating decarbonization. Cost: must defend intergenerational waste burden and rare-event risk as acceptable shadow prices. Cannot exit without undermining their own climate-urgency claims.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, climate_mitigation_advocates_accepting_nuclear, beneficiary,
    organized, generational, constrained, global).

% Industrial, service, and digital economies depend on cheap, reliable baseload power. Nuclear generation (risk-assessed comparatively) offers carbon-free electricity cheaper than renewable infrastructure with storage. Cost: absorb aggregate accident risk as diffuse institutional exposure; licensing burden shifts to preventing accident probability reduction rather than to preventing rare catastrophes.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, electricity_dependent_economies, beneficiary,
    institutional, biographical, constrained, national).

% Future generations (centuries to millennia ahead) inherit multi-millennial waste storage obligations, residual accident risk from repositories and interim storage, and land-use restrictions. They bear costs they did not authorize and cannot exit. Comparative-risk logic subordinates their welfare to present climate optimization. Temporal structure makes this a pure extraction: they cannot participate in the decision.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_waste_inheritors, payer,
    powerless, civilizational, trapped, universal).

% Dependent on grid electricity for basic needs; cannot relocate if accident occurs. Live in dense urban areas or near plants because housing is cheaper there. Lack capital for private resilience (generators, evacuation vehicles, relocation funds). Cannot afford alternative energy sources. Carry high accident concentration risk while benefiting least from cheap baseload power (they absorb cost increases first).
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, low_income_energy_consumers, payer,
    powerless, immediate, trapped, national).

% Suffer immediate climate harm from emissions-based energy systems; comparative-risk framework offers them nuclear-backed decarbonization (benefit). Simultaneously, they have highest population density in flood-prone and drought-prone regions (coastal, river-delta, arid margins), creating highest accident concentration risk if plants cluster there. Cannot exit either risk; temporal urgency logic prioritizes climate benefit over accident exposure.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, climate_vulnerable_populations, payer,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_for_energy__comparative_risk_dominant, climate_vulnerable_populations, beneficiary).

% Excluded from the comparative-risk framing because the framework's logic depends on fossil fuel being THE alternative. If included, fossil defenders would argue for efficiency investment, demand reduction, and absolute safety as true alternatives rather than binary carbon-free-or-coal. Their exclusion is structural: the framework's entire justification rests on coal/gas being unavoidable alternatives.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, fossil_fuel_industry, excluded,
    powerful, biographical, mobile, global).

% Embed comparative-risk acceptability into licensing, operational, and waste-disposal standards. Benefit: removes pressure to solve waste storage or achieve accident-free operation; permits cost-reduced timelines and faster deployment. Cost: must defend non-absolute thresholds; shift burden from prevention to response; bear liability if accidents occur. Cannot exit without reversing policy framework across all existing licensed plants.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, regulatory_bodies, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_for_energy__comparative_risk_dominant, regulatory_bodies, observer).

% Provide evidence that climate risk is high-probability, high-consequence, and near-term. Frame nuclear as necessary component of carbon-abatement portfolio. Do not typically assess or defend nuclear-specific risks outside their domain. Influence policy through urgency signals and energy-transition modeling. Remain analytically outside the risk-trade decision itself.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, climate_scientists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_industry).
narrative_ontology:fixing_cost_class(acceptable_risk_for_energy__comparative_risk_dominant, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the temporal urgency problem in climate mitigation: enables rapid carbon-free electricity deployment without waiting for renewable + storage infrastructure to reach necessary scale. Coordinates between immediate climate action and long-term energy supply, allowing baseload carbon-free power to replace coal while renewables build out.
% TRANSFER_FUNCTION: Transfers accident risk (low-probability, high-consequence events) from present to future; transfers climate benefit (reduced CO2 emissions, averted warming damage) to present and near-future. Also transfers energy cost burden: cheap baseload power subsidizes electricity-dependent industries and low-carbon transition; accident remediation and waste storage costs are pushed to future generations and dispersed across affected populations.
% ABSENT_VOICES: Fossil fuel defenders are structurally excluded (not absent by chance — the framework's logic depends on coal/gas being THE alternative). Younger generations and far-future inheritors of waste repositories have no seat at present risk-acceptance decisions. Engineering-consensus voices advocating absolute safety thresholds are marginalized relative to climate urgency rhetoric.
% DISAPPEARANCE_RATIONALE: If the comparative-risk acceptability framework vanished, energy policy would revert to absolute-threshold licensing (accident probability below 10^-6 per year, zero-release containment standards) or dual-requirement logic (nuclear acceptable only if waste storage solution is proven before plant operation). Both shifts would slow nuclear deployment by 10–30 years, forcing either higher renewable investment targets or extended coal operation. Climate mitigation timelines would restructure; electricity economics would shift.
% FOUNDING_PROBLEM: Climate catastrophe risk is high-consequence, present-tense, and growing; renewable + storage infrastructure cannot scale fast enough to decarbonize grids by mid-century targets without baseload carbon-free power. Nuclear is technically proven baseload capacity; absolute safety thresholds or unresolved waste-disposal requirements would exclude it, leaving only coal/gas as viable interim solutions.
% FOUNDING_PROBLEM_CORROBORATION: Climate scientists and energy modelers from outside the nuclear industry attest the founding problem's urgency: IPCC synthesis reports, IEA net-zero scenarios, and peer-reviewed energy transition models all place nuclear as necessary in least-cost decarbonization pathways. Nuclear industry and pro-nuclear climate advocates affirm the founding problem and its urgency. Anti-nuclear voices and waste-siting communities dispute whether the problem justifies accepting intergenerational waste burden and contest whether renewable alternatives receive equal policy support in modeling. Independent engineering ethics bodies dispute whether comparative-risk framing properly subordinates low-probability catastrophic scenarios.
narrative_ontology:disappearance_verdict(acceptable_risk_for_energy__comparative_risk_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_for_energy__comparative_risk_dominant, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_for_energy__comparative_risk_dominant, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(acceptable_risk_for_energy__comparative_risk_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_for_energy__comparative_risk_dominant, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_for_energy__comparative_risk_dominant_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_for_energy__comparative_risk_dominant, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(acceptable_risk_for_energy__comparative_risk_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 final) and rising over the interval (0.52 → 0.68) because the framework progressively subordinates engineering-absolute-risk concerns to climate-urgency concerns. Early in the interval (t=0), comparative-risk logic competes with absolute-safety framings; by t=35 (projected), the framework is institutionalized as the default policy standard, and engineering objections are treated as climate obstruction rather than valid safety concerns. Suppression is high (0.62) because maintaining comparative-risk acceptability requires actively suppressing alternative framings: absolute-threshold logic (which would slow deployment), expected-value logic that separates climate benefit from accident cost, catastrophic-tail logic that centers intergenerational burden. Theater ratio rises (0.25 → 0.41) because enforcement increasingly relies on climate-urgency rhetoric rather than on engineering demonstration of accident-probability reduction — the framework justifies plants that do not meet absolute safety standards by invoking fossil-fuel harms, not by proving nuclear safety. The measurement series is authored on a single shared time grid (every metric at every time point) so temporal analysis can detect metric divergence (e.g., theater rising while extractiveness plateaus would signal enforcement focus shifting from risk reduction to narrative maintenance).
 *
 * PERSPECTIVAL GAP:
 *   The nuclear_industry and climate_mitigation_advocates seats should compute as beneficiary-proximal (d near 0.2–0.4), while nuclear_waste_inheritors and low_income_energy_consumers should compute as target-proximal (d near 0.7–0.9). The regulatory_bodies seat faces a dual structure: they benefit from reduced licensing burden and accelerated deployment (d near 0.3), but they also bear liability and social-consent risk if accident occurs (pushes d toward 0.5–0.6 depending on jurisdiction and exit options). The engine computes these divergences from the authored beneficiary/victim declarations and the power/exit atoms; the perspectival gap emerges from structural asymmetry, not from opinion variance.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are the nuclear_industry (direct rent from expanded generation and reduced regulatory burden), electricity_dependent_economies (cheap baseload power), and climate_advocates accepting the framework (their mitigation timeline becomes feasible without waiting for full renewable+storage build-out). Victims are nuclear_waste_inheritors (trapped, powerless, civilizational time horizon — they are the archetypal full-target: no exit, no input to the decision, maximum d), low_income_energy_consumers (trapped in geography, powerless, cannot relocate or invest in resilience), and climate_vulnerable_populations (simultaneously beneficiary from decarbonization and victim from accident concentration risk — the secondary_role captures the dual position). Fossil fuel industry is excluded (structurally, by the framework's definition) rather than coordinated or victimized. The comparative-risk framework's persistence depends on preventing the full set of alternative framings (absolute-threshold, catastrophic-tail, expected-value) from achieving policy weight; that suppression is the high-suppression metric (0.62).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live in the reading's own terms: climate catastrophe is high-consequence, present-tense, and accelerating. The comparative-risk framework is mandated by that problem — rapid decarbonization requires baseload carbon-free power, and absolute-safety standards would exclude nuclear from that role. HOWEVER, the constraint also extracts substantially from future generations (waste storage, residual accident risk) and from present-day low-income populations (accident concentration). Mandatrophy does NOT apply here because the founding problem and the constraint's persistence are tightly coupled: if the climate urgency diminishes (or if renewable+storage scales faster), the comparative-risk rationale weakens and pressure for absolute-threshold re-emergence increases. The constraint is not a zombie persisting after its rationale dies; it is actively maintained by climate urgency. The question is whether that justification properly accounts for the weight of intergenerational burden — that is an omega, not a mandatrophy issue.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intergenerational_discount_rate_ambiguity,
    'What discount rate (if any) is legitimate when weighing present climate benefit against civilizational-scale waste storage obligations? Is there a discount rate that properly accounts for intergenerational justice, or does the obligation to future generations foreclose discounting altogether?',
    'Philosophical and ethical examination of intergenerational justice frameworks; policy analysis of waste-storage precedents (e.g., lead contamination, PCB dumps, nuclear weapons testing) and their actual intergenerational burden; explicit policy declaration of a discount rate and its justification.',
    'If present-benefit discounting is not legitimate, the comparative-risk framework collapses — nuclear acceptability cannot be justified by climate urgency alone if the cost is pushed to the future. If a legitimate discount rate exists, the framework''s justification depends on proving the rate is defensible, not on asserting it implicitly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_discount_rate_ambiguity, conceptual, 'Whether and how to discount future waste-storage burden against present climate benefit.').

omega_variable(
    fossil_fuel_alternative_inevitability,
    'Is the binary choice (nuclear or coal/gas) the true alternative set, or does it reflect policy choice to underfund renewable+storage infrastructure? If renewables received the investment nuclear receives, would they scale to replace coal in the same timeframe?',
    'Energy transition modeling with equal capital allocation across nuclear, renewable, and storage pathways; policy analysis of historical investment patterns and regulatory barriers to renewable scaling; counterfactual scenario analysis.',
    'If renewables are a feasible alternative at equal investment, the comparative-risk framework''s core premise (nuclear or fossil fuel) is false, and the constraint collapses. If coal is genuinely inevitable without nuclear, the framework is vindicated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fossil_fuel_alternative_inevitability, empirical, 'Whether the binary alternative (nuclear vs coal) reflects technical necessity or policy choice.').

omega_variable(
    accident_concentration_inequality,
    'Do accident risks concentrate in low-income and climate-vulnerable regions through siting and evacuation-capacity patterns? If so, does the comparative-risk framework properly account for the distributional injustice, or does it implicitly assume risk is homogeneous across populations?',
    'Demographic analysis of nuclear plant siting, evacuation-route analysis showing disparities in evacuation time by income and protected geography, health-outcome modeling for different evacuation scenarios across wealth quintiles.',
    'If accident risk concentrates in vulnerable populations, the constraint extracts more severely from them than from wealthy populations with faster evacuation and greater private-resilience capacity. The framework''s claim to balance climate urgency against accident risk becomes a claim to balance global climate benefit (benefiting all) against localized accident risk (concentrating on the powerless).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(accident_concentration_inequality, empirical, 'Whether accident risk is distributionally concentrated on low-income and climate-vulnerable populations.').

omega_variable(
    reading_kernel_contingency,
    'Is this reading (comparative_risk_dominant) a genuine structurally distinct commitment, or does it collapse into the expected_value_dominant reading once expected values are properly computed across all cost pathways including waste storage?',
    'Explicit modeling of expected accident cost (probability × consequence) across waste-storage scenarios (optimal containment, realistic containment, worst-case release), comparison against expected climate-benefit cost pathways (avoided warming damages by decade), and examination of whether the comparative-risk reading''s dismissal of tail-risk accounting survives systematic expected-value analysis.',
    'If the comparative reading collapses into expected-value analysis, the three readings are not equally live — only expected_value survives scrutiny. If the readings remain genuinely distinct (comparative judgment is irreducible to expected-value calculation), the commitment system shows genuine competing rationalities, not error or incoherence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_contingency, conceptual, 'Whether comparative-risk reasoning is structurally distinct from expected-value reasoning or collapses into it under systematic analysis.').

omega_variable(
    suppression_of_tail_risk_framing,
    'Is the rising theater_ratio (0.25 → 0.41) evidence that the constraint increasingly relies on climate-urgency rhetoric to suppress tail-risk framings (catastrophic accidents, waste-management failure) rather than on engineering improvements to accident probability itself?',
    'Trend analysis of safety improvements (accident probability reduction, engineered containment enhancement, waste-storage technology advancement) vs. trend analysis of policy rhetoric (share of nuclear-defense arguments citing climate urgency vs. citing engineering safety improvements). If rhetoric growth outpaces safety improvement, theater is diagnosed.',
    'High theater indicates the constraint persists through enforcement of narrative rather than through engineering confidence. If enforcement shifts entirely to narrative (theater → 0.7+), the constraint approaches piton status — maintained by institutional inertia and rhetorical defensibility rather than by real safety gains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_tail_risk_framing, empirical, 'Whether rising theater_ratio reflects narrative-dependent enforcement substituting for engineering risk reduction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_for_energy__comparative_risk_dominant, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t0, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(acce_tr_t0, observed).
narrative_ontology:measurement(acce_tr_t5, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(acce_tr_t5, observed).
narrative_ontology:measurement(acce_tr_t10, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(acce_tr_t10, observed).
narrative_ontology:measurement(acce_tr_t15, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 15, 0.36).
narrative_ontology:measurement_basis(acce_tr_t15, observed).
narrative_ontology:measurement(acce_tr_t20, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 20, 0.39).
narrative_ontology:measurement_basis(acce_tr_t20, observed).
narrative_ontology:measurement(acce_tr_t25, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 25, 0.4).
narrative_ontology:measurement_basis(acce_tr_t25, projected).
narrative_ontology:measurement(acce_tr_t30, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(acce_tr_t30, projected).
narrative_ontology:measurement(acce_tr_t35, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 35, 0.41).
narrative_ontology:measurement_basis(acce_tr_t35, projected).

% Extraction over time
narrative_ontology:measurement(acce_be_t0, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(acce_be_t0, observed).
narrative_ontology:measurement(acce_be_t5, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 5, 0.56).
narrative_ontology:measurement_basis(acce_be_t5, observed).
narrative_ontology:measurement(acce_be_t10, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 10, 0.6).
narrative_ontology:measurement_basis(acce_be_t10, observed).
narrative_ontology:measurement(acce_be_t15, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 15, 0.64).
narrative_ontology:measurement_basis(acce_be_t15, observed).
narrative_ontology:measurement(acce_be_t20, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(acce_be_t20, observed).
narrative_ontology:measurement(acce_be_t25, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(acce_be_t25, projected).
narrative_ontology:measurement(acce_be_t30, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(acce_be_t30, projected).
narrative_ontology:measurement(acce_be_t35, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 35, 0.68).
narrative_ontology:measurement_basis(acce_be_t35, projected).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t0, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(acce_su_t0, observed).
narrative_ontology:measurement(acce_su_t5, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 5, 0.52).
narrative_ontology:measurement_basis(acce_su_t5, observed).
narrative_ontology:measurement(acce_su_t10, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 10, 0.56).
narrative_ontology:measurement_basis(acce_su_t10, observed).
narrative_ontology:measurement(acce_su_t15, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 15, 0.59).
narrative_ontology:measurement_basis(acce_su_t15, observed).
narrative_ontology:measurement(acce_su_t20, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 20, 0.61).
narrative_ontology:measurement_basis(acce_su_t20, observed).
narrative_ontology:measurement(acce_su_t25, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 25, 0.62).
narrative_ontology:measurement_basis(acce_su_t25, projected).
narrative_ontology:measurement(acce_su_t30, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 30, 0.62).
narrative_ontology:measurement_basis(acce_su_t30, projected).
narrative_ontology:measurement(acce_su_t35, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 35, 0.62).
narrative_ontology:measurement_basis(acce_su_t35, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_for_energy__comparative_risk_dominant, resource_allocation).
narrative_ontology:boltzmann_floor_override(acceptable_risk_for_energy__comparative_risk_dominant, 0.18).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__comparative_risk_dominant, acceptable_risk_for_energy__catastrophic_tail_dominant).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__comparative_risk_dominant, acceptable_risk_for_energy__expected_value_dominant).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__comparative_risk_dominant, carbon_intensity_regulatory_priority).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__comparative_risk_dominant, electricity_grid_decarbonization_timeline).

% DUAL FORMULATION NOTE:
% This constraint is one reading (comparative_risk_dominant) of a three-reading kernel (acceptable_risk_for_energy). The sibling readings are acceptable_risk_for_energy__catastrophic_tail_dominant and acceptable_risk_for_energy__expected_value_dominant. All three share the same kernel (the decision-rule for nuclear acceptability in energy policy) and the same beneficiary groups (climate_mitigation_advocates, electricity_dependent_economies, nuclear_industry) but differ structurally in victim set, temporal prioritization, and ε value. The comparative reading subordinates intergenerational waste concern to present climate urgency; the catastrophic_tail reading reverses that priority; the expected_value reading separates climate benefit from accident cost and computes risk-benefit net-zero. Decomposition required because ε is reading-indexed (the standing arrangement is the same, but the reading's assessment of extraction from that arrangement differs). Each reading is authored as a clean, ε-invariant constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(acceptable_risk_for_energy__comparative_risk_dominant, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
