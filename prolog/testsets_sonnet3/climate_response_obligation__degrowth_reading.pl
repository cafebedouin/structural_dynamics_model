% ============================================================================
% CONSTRAINT STORY: climate_response_obligation__degrowth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_obligation__degrowth_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: climate_response_obligation__degrowth_reading
 *   human_readable: Climate Response Obligation — Degrowth Reading: Sufficiency Over Efficiency
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This story instantiates the degrowth reading of the
 *   climate_response_obligation kernel: the claim that intergenerational and
 *   ecological obligation requires reducing absolute material and energy
 *   throughput in wealthy economies — sufficiency over efficiency — rather
 *   than pursuing decarbonization within a growth-compatible frame (the
 *   sibling mitigation_priority reading) or accepting warming and investing
 *   in resilience (the sibling adaptation_priority reading). Under this
 *   reading, planetary ecological systems and future generations are the
 *   primary beneficiaries of reduced extraction pressure; Global North
 *   consumption patterns and extractive capital enter the victim set because
 *   their current throughput levels are the named mechanism of harm; and
 *   Global South development is structurally constrained by a global
 *   throughput ceiling set primarily to correct Northern historical
 *   overconsumption, unless the North reduces first. Capital accumulation
 *   itself — not merely emissions — is treated as an extractive mechanism in
 *   this reading's own terms, which is a substantively different claim than
 *   the mitigation reading's carbon-flow-only accounting.
 *
 * KEY AGENTS:
 *   - planetary_ecological_systems: analytical beneficiary — bears cumulative throughput load, has no voice
 *   - future_generations: powerless beneficiary — cannot bargain over present throughput decisions
 *   - global_north_consumer_households: moderate power payer — asked to accept permanent consumption reduction
 *   - global_north_extractive_capital: powerful payer with mobile exit — can relocate or reframe around 'green growth'
 *   - global_south_industrializing_workers: powerless payer/beneficiary — development sequenced behind Northern reduction
 *   - degrowth_policy_advocates: organized agenda-setters who author the sufficiency standard
 *   - global_north_states: institutional agenda-setters who would enforce any binding ceiling
 *   - mitigation_priority_reading and adaptation_priority_reading: excluded sibling claims on the same kernel
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_obligation__degrowth_reading, 0.71).
domain_priors:suppression_score(climate_response_obligation__degrowth_reading, 0.58).
domain_priors:theater_ratio(climate_response_obligation__degrowth_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_obligation__degrowth_reading, tangled_rope).
narrative_ontology:human_readable(climate_response_obligation__degrowth_reading, "Climate Response Obligation — Degrowth Reading: Sufficiency Over Efficiency").
narrative_ontology:topic_domain(climate_response_obligation__degrowth_reading, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_obligation__degrowth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_obligation__degrowth_reading, 'f050ea6f-c71c-435b-99d7-81e20aeb1c95').
narrative_ontology:cs_kernel_codification('f050ea6f-c71c-435b-99d7-81e20aeb1c95', distributed).
narrative_ontology:cs_authority_grounding('f050ea6f-c71c-435b-99d7-81e20aeb1c95', distributed).
narrative_ontology:cs_reading_relation('f050ea6f-c71c-435b-99d7-81e20aeb1c95', climate_response_obligation__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('f050ea6f-c71c-435b-99d7-81e20aeb1c95', climate_response_obligation__adaptation_priority, influences).
narrative_ontology:cs_axiom('f050ea6f-c71c-435b-99d7-81e20aeb1c95', foundational, absolute_throughput_reduction_required).
narrative_ontology:cs_axiom_status(absolute_throughput_reduction_required, holdable).
narrative_ontology:cs_axiom_grounding('f050ea6f-c71c-435b-99d7-81e20aeb1c95', absolute_throughput_reduction_required, empirically_contingent).
narrative_ontology:cs_axiom('f050ea6f-c71c-435b-99d7-81e20aeb1c95', foundational, capital_accumulation_is_extractive_mechanism).
narrative_ontology:cs_axiom_status(capital_accumulation_is_extractive_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('f050ea6f-c71c-435b-99d7-81e20aeb1c95', capital_accumulation_is_extractive_mechanism, conventional).
narrative_ontology:cs_axiom('f050ea6f-c71c-435b-99d7-81e20aeb1c95', secondary, efficiency_gains_structurally_insufficient).
narrative_ontology:cs_axiom_status(efficiency_gains_structurally_insufficient, holdable).
narrative_ontology:cs_axiom_grounding('f050ea6f-c71c-435b-99d7-81e20aeb1c95', efficiency_gains_structurally_insufficient, empirically_contingent).
narrative_ontology:cs_reference_frame('f050ea6f-c71c-435b-99d7-81e20aeb1c95', post_1972_limits_to_growth_framework).
narrative_ontology:cs_drift_state('f050ea6f-c71c-435b-99d7-81e20aeb1c95', post_paris_agreement_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('f050ea6f-c71c-435b-99d7-81e20aeb1c95', '').
narrative_ontology:cs_kernel_id(climate_response_obligation__degrowth_reading, climate_response_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_obligation__degrowth_reading, planetary_ecological_systems).
narrative_ontology:constraint_beneficiary(climate_response_obligation__degrowth_reading, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_obligation__degrowth_reading, global_south_ecosystems).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, global_north_consumer_households).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, global_north_extractive_capital).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, global_south_industrializing_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_response_obligation__degrowth_reading, global_south_industrializing_workers).
narrative_ontology:constraint_vindicates(climate_response_obligation__degrowth_reading, planetary_boundaries_framework).
narrative_ontology:constraint_vindicates(climate_response_obligation__degrowth_reading, biophysical_limits_to_growth_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bears the cumulative load of material and energy throughput — carbon sinks, biodiversity, freshwater systems, soil. Under the degrowth reading, reduced throughput directly relieves this load; the systems have no voice but are the named referent every enforcement mechanism claims to protect.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, planetary_ecological_systems, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(climate_response_obligation__degrowth_reading, planetary_ecological_systems).

% Inherit whatever biophysical envelope current throughput decisions leave behind. Cannot bargain, vote, or exit; their interests are represented only by proxy advocates in the present who may or may not accurately model what they would choose.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, global).

% Asked under the sufficiency framing to accept reduced material consumption, smaller housing, less transport, less discretionary throughput — not as a temporary sacrifice but as a permanent reduction in living standard relative to trajectory. Exit is constrained: opting out individually does little against an aggregate throughput ceiling, and most policy levers (carbon budgets, consumption caps) apply regardless of individual preference.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, global_north_consumer_households, payer,
    moderate, biographical, constrained, national).

% Owns the extraction and production infrastructure whose expansion the degrowth reading names as the mechanism of harm — capital accumulation itself, not merely emissions, is treated as extractive. Can relocate operations, lobby against binding throughput caps, or reframe products as 'green growth' to evade the sufficiency logic; retains substantial exit relative to households.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, global_north_extractive_capital, payer,
    powerful, biographical, mobile, global).

% Live in economies pursuing industrial development pathways historically reliant on rising material throughput. The degrowth reading holds that Global South development must be sequenced behind or decoupled from Northern reduction, meaning these workers' path to higher material living standards is constrained by a global ceiling set primarily to correct Northern historical overconsumption. They benefit from a livable planet but pay in delayed development.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, global_south_industrializing_workers, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(climate_response_obligation__degrowth_reading, global_south_industrializing_workers, beneficiary).

% Researchers, NGOs, and political movements that author sufficiency frameworks, planetary boundary metrics, and consumption-cap proposals. They administer the intellectual and advocacy infrastructure that defines what counts as 'sufficient' throughput and who must reduce first, without holding direct enforcement power themselves — enforcement, if it comes, runs through states.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, degrowth_policy_advocates, agenda_setter,
    organized, generational, analytical, global).

% Would be the actual enforcement mechanism for any binding throughput ceiling — through carbon budgets, consumption taxes, rationing, or growth-limiting regulation. Face domestic political resistance from households and capital alike, and are cross-pressured by international commitments and electoral cycles that reward growth, not contraction.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, global_north_states, agenda_setter,
    institutional, generational, constrained, national).

% Holds that rapid decarbonization within a growth-compatible frame (renewables buildout, efficiency gains, carbon pricing) satisfies the intergenerational obligation without requiring aggregate throughput reduction. Not part of this reading's constraint but a live rival claim on the same underlying obligation, sidelined here by the degrowth premise that efficiency gains are structurally insufficient given rebound effects.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, mitigation_priority_reading, excluded,
    organized, generational, analytical, global).

% Holds that some level of warming is now unavoidable and resources are better spent on resilience than prevention. This reading treats that position as an abdication that shifts costs onto those least able to adapt, and excludes it from consideration rather than engaging its empirical claims about mitigation feasibility.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, adaptation_priority_reading, excluded,
    organized, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a global reduction in aggregate material and energy throughput so that cumulative extraction and waste flows stay within biophysical limits that no single actor can unilaterally maintain — a genuine collective-action problem, since any single nation's or firm's restraint is undermined if others do not restrain proportionally.
% TRANSFER_FUNCTION: Moves material and energy consumption entitlement from current Global North households and capital (who reduce absolute throughput) toward planetary systems (relieved extraction pressure) and toward future generations (preserved biophysical headroom); simultaneously constrains the throughput budget available to Global South industrializing economies, who are asked to accept a lower development ceiling than the historical Northern trajectory used.
% ABSENT_VOICES: Global South governments and populations are frequently spoken for by Northern degrowth advocates rather than setting the terms themselves; workers in industrializing economies who want the consumption gains the North already took are structurally excluded from authoring the sufficiency standard applied to them. Extractive capital's shareholders are present as targets but not represented as a constituency whose foreclosed accumulation counts as a cost in this reading's own accounting.
% DISAPPEARANCE_RATIONALE: If the degrowth obligation vanished as a policy claim, planetary boundary pressure would continue accumulating on the mitigation-priority reading's own terms (efficiency and decarbonization would proceed without an aggregate throughput ceiling) — advocates say ecological overshoot would deepen and future generations would inherit a smaller safe envelope; skeptics of the reading say growth-compatible decarbonization would proceed largely unaffected since the degrowth claim currently has little binding enforcement anywhere. The parties dispute both the causal counterfactual and whether the claim is currently doing real work.
% FOUNDING_PROBLEM: Rising global material and energy throughput, historically concentrated in industrialized economies, is projected to breach multiple planetary boundaries (climate, biodiversity, freshwater, nitrogen/phosphorus cycles) even under aggressive efficiency and decarbonization scenarios, because absolute throughput growth outpaces per-unit efficiency gains (the rebound/Jevons problem).
% FOUNDING_PROBLEM_CORROBORATION: Ecological economists and planetary-boundaries researchers (e.g., Stockholm Resilience Centre lineage) outside the degrowth advocacy movement itself corroborate that several boundaries are already transgressed and that decoupling of GDP growth from absolute material throughput has not been empirically demonstrated at the required scale. Mainstream mitigation-priority economists dispute the inference that this requires aggregate contraction rather than accelerated decoupling, so the status of the founding problem as requiring degrowth specifically (versus decarbonization within growth) remains actively contested outside the movement's own literature.
narrative_ontology:disappearance_verdict(climate_response_obligation__degrowth_reading, contested).
narrative_ontology:founding_problem_status(climate_response_obligation__degrowth_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_obligation__degrowth_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_response_obligation__degrowth_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_obligation__degrowth_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_obligation__degrowth_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_obligation__degrowth_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_obligation__degrowth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.71) because the degrowth reading identifies capital accumulation and Northern consumption growth themselves — not just emissions — as the extractive mechanism, and because the reading's own accounting treats this as substantially unresolved and worsening over the interval (throughput has risen even as decarbonization rhetoric intensified, which the reading reads as decoupling failure). Suppression is authored at 0.58, lower than extractiveness, because as of 2025 no binding global throughput ceiling exists — most of what enforces the sufficiency claim currently is normative and advocacy pressure rather than coercive apparatus; the rising suppression_requirement trajectory reflects the reading's own account that enforcement infrastructure (national consumption caps, carbon budgets, rationing proposals) would need to intensify sharply for the claim to become binding. Theater ratio is moderate (0.42) and rising, reflecting the reading's critique that much decarbonization policy performs sufficiency rhetoric (green growth, ESG framing) while actual material throughput continues to climb — a Goodhart-style substitution the degrowth reading treats as diagnostic of the mitigation-priority reading's insufficiency. Resistance is authored high (0.78) because this reading meets substantial active pushback from households facing living-standard reduction, capital facing accumulation limits, and Global South governments resisting a development ceiling set by others' history.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (degrowth advocates), the arrangement is coordination against a genuine tragedy-of-the-commons in planetary boundaries. From the payer seats (Northern households, extractive capital, Southern industrializing workers), the same arrangement computes as extraction of accustomed or anticipated consumption. The engine should compute these divergently from the structural data — the claimed_type of tangled_rope is authored precisely because both a real coordination function and a real asymmetric cost structure are present simultaneously, not because either seat's framing is privileged.
 *
 * DIRECTIONALITY LOGIC:
 *   Planetary systems and future generations are declared beneficiaries with no exit and no power to resist reduced throughput to their benefit — they sit at the low end of directionality by construction (subsidized, cannot be extracted from further). Global North households and extractive capital are declared victims of the reduction; capital's directionality is dampened relative to households by its mobile exit option, while households, despite moderate power, have only constrained exit from an aggregate throughput ceiling that binds regardless of individual choice. Global South industrializing workers carry a dual role: beneficiaries of a livable planet in the long run, payers of delayed development in the near term — their trapped exit option and powerless standing place their effective directionality closer to the target end despite the reading's stated intent to protect them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (throughput growth outpacing efficiency gains, breaching planetary boundaries) is authored as contested rather than resolved-dead or clearly-live, because the empirical claim about decoupling limits is itself disputed by the sibling mitigation_priority reading. This prevents the story from mislabeling the degrowth claim as either pure coordination (ignoring its real distributive costs on Northern consumption and Southern development timing) or pure extraction (ignoring the genuine, unresolved biophysical commons problem it responds to) — the tangled_rope classification holds both simultaneously as the reading's honest self-account.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decoupling_feasibility_ambiguity,
    'Can material/energy throughput be decoupled from economic welfare and development at the scale and speed required to satisfy planetary boundaries without absolute throughput reduction — i.e., is the mitigation_priority reading''s growth-compatible decarbonization empirically sufficient, or does the degrowth reading''s rebound-effect critique hold?',
    'Long-run empirical tracking of absolute decoupling (not just relative/carbon-intensity decoupling) across economies pursuing aggressive decarbonization policy, compared against required planetary-boundary trajectories.',
    'If absolute decoupling proves achievable at required scale, the degrowth reading''s extractiveness claim against capital accumulation weakens substantially and the constraint approaches a rope (genuine coordination with lower distributive cost); if decoupling proves infeasible at scale, the tangled_rope classification is conservative and the reading''s extraction claim strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decoupling_feasibility_ambiguity, empirical, 'Whether growth-compatible decarbonization can substitute for absolute throughput reduction.').

omega_variable(
    sequencing_fairness_ambiguity,
    'Is it fair, or even coherent, to demand that Global South development be sequenced behind or bounded by Global North reduction, given that the North achieved its current consumption level through the very throughput growth now being restricted?',
    'Historical accounting of cumulative per-capita throughput/emissions by region, combined with negotiated frameworks (e.g., common but differentiated responsibilities) for how a global throughput budget would be allocated.',
    'If sequencing is judged fair given historical responsibility, the Global South victim classification softens toward a temporary, compensated constraint (scaffold-like); if judged unfair or unenforceable, it strengthens the tangled_rope/snare-adjacent reading toward Global South workers as under-compensated victims.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sequencing_fairness_ambiguity, preference, 'Whether North-first sequencing is a just allocation of a shared throughput budget.').

omega_variable(
    reading_selection_ambiguity,
    'Among the three sibling readings of the climate_response_obligation kernel (degrowth, mitigation-priority, adaptation-priority), is there a fact of the matter about which best discharges the underlying intergenerational obligation, or is the choice irreducibly a matter of value commitments (growth valuation, risk tolerance, distributive priorities) that no additional empirical data can settle?',
    'Track whether growing empirical consensus on decoupling feasibility (see decoupling_feasibility_ambiguity) narrows the practical distance between readings, versus whether the readings remain sharply divergent even under empirical convergence — indicating the disagreement is primarily normative.',
    'If the disagreement is substantially empirical, growing evidence could eventually foreclose one or more readings; if substantially normative (as the coexists_with relations in cs_structure assume), the readings will remain live and contested regardless of further data.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_ambiguity, conceptual, 'Whether kernel readings are empirically adjudicable or irreducibly normative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_obligation__degrowth_reading, 1972, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t1972, climate_response_obligation__degrowth_reading, theater_ratio, 1972, 0.2).
narrative_ontology:measurement(clim_tr_t1992, climate_response_obligation__degrowth_reading, theater_ratio, 1992, 0.28).
narrative_ontology:measurement(clim_tr_t2005, climate_response_obligation__degrowth_reading, theater_ratio, 2005, 0.33).
narrative_ontology:measurement(clim_tr_t2015, climate_response_obligation__degrowth_reading, theater_ratio, 2015, 0.36).
narrative_ontology:measurement(clim_tr_t2020, climate_response_obligation__degrowth_reading, theater_ratio, 2020, 0.4).
narrative_ontology:measurement(clim_tr_t2025, climate_response_obligation__degrowth_reading, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(clim_be_t1972, climate_response_obligation__degrowth_reading, base_extractiveness, 1972, 0.35).
narrative_ontology:measurement(clim_be_t1992, climate_response_obligation__degrowth_reading, base_extractiveness, 1992, 0.45).
narrative_ontology:measurement(clim_be_t2005, climate_response_obligation__degrowth_reading, base_extractiveness, 2005, 0.52).
narrative_ontology:measurement(clim_be_t2015, climate_response_obligation__degrowth_reading, base_extractiveness, 2015, 0.62).
narrative_ontology:measurement(clim_be_t2020, climate_response_obligation__degrowth_reading, base_extractiveness, 2020, 0.68).
narrative_ontology:measurement(clim_be_t2025, climate_response_obligation__degrowth_reading, base_extractiveness, 2025, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t1972, climate_response_obligation__degrowth_reading, suppression_requirement, 1972, 0.15).
narrative_ontology:measurement(clim_su_t1992, climate_response_obligation__degrowth_reading, suppression_requirement, 1992, 0.22).
narrative_ontology:measurement(clim_su_t2005, climate_response_obligation__degrowth_reading, suppression_requirement, 2005, 0.3).
narrative_ontology:measurement(clim_su_t2015, climate_response_obligation__degrowth_reading, suppression_requirement, 2015, 0.4).
narrative_ontology:measurement(clim_su_t2020, climate_response_obligation__degrowth_reading, suppression_requirement, 2020, 0.5).
narrative_ontology:measurement(clim_su_t2025, climate_response_obligation__degrowth_reading, suppression_requirement, 2025, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_obligation__degrowth_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_obligation__degrowth_reading, 0.12).
narrative_ontology:affects_constraint(climate_response_obligation__degrowth_reading, mitigation_priority).
narrative_ontology:affects_constraint(climate_response_obligation__degrowth_reading, adaptation_priority).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the climate_response_obligation kernel. mitigation_priority holds that rapid decarbonization within a growth-compatible frame discharges the obligation; adaptation_priority holds that resilience investment given largely-unavoidable warming discharges it; degrowth_reading (this story) holds that absolute throughput reduction is required because growth-compatible decarbonization faces a structural decoupling ceiling. Each reading is authored as its own ε-invariant constraint with its own beneficiary/victim structure — do not average or reconcile ε across the three; link via affects_constraints per the kernel decomposition rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
