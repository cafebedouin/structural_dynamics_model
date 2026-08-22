% ============================================================================
% CONSTRAINT STORY: acceptable_risk_energy__catastrophic_tail_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_energy__catastrophic_tail_dominant, []).

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
 *   constraint_id: acceptable_risk_energy__catastrophic_tail_dominant
 *   human_readable: Catastrophic-Tail-Dominant Risk Framework in Energy Policy
 *   domain: energy_policy/risk_assessment/decision_theory
 *
 * SUMMARY:
 *   Energy policy risk assessment operates under competing frameworks for
 *   what 'acceptable risk' means. The catastrophic-tail-dominant reading
 *   weights low-probability, high-consequence events (nuclear core melt,
 *   multi-site cascade failure, long-term contamination) as the dominant
 *   decision factor, even when this means tolerating higher aggregate
 *   expected harm from distributed sources (coal respiratory disease, air
 *   pollution, climate forcing). This reading treats catastrophe-avoidance as
 *   an overriding ethical constraint; it justifies perpetuating fossil fuel
 *   operation and delaying nuclear deployment as the cost of precaution. The
 *   constraint is CLAIMED as tangled_rope (it coordinates a single decision
 *   rule across energy pathways) while authored metrics describe
 *   substantially extractive, actively enforced operation — the divergence
 *   reflects the reading's own internal claim that the coordination function
 *   (unified risk standards) rides on extractive asymmetry (distributed harms
 *   rendered invisible).
 *
 * KEY AGENTS:
 *   - Nuclear regulatory bodies: Set and enforce catastrophic-tail standards; control licensing and operation rules.
 *   - Precautionary advocates: Benefit from institutional legitimacy and policy influence from the framework; shape public discourse.
 *   - Distributed fossil-fuel harm bearers: Powerless populations in coal regions and downwind of plants; bear chronic, diffuse health costs.
 *   - Energy-poor populations: Delayed development access in regions where precautionary stance slows any high-capacity low-carbon deployment.
 *   - Coal miner communities: Structurally perpetuated in resource extraction by the framework's implicit coal-as-lesser-evil logic.
 *   - Nuclear waste stewards (future generations): Bear indefinite, uncompensated cost; identity-locked by the stewardship frame itself.
 *   - Expected-value risk analysts: Excluded from policy voice; their framework contradicts the precautionary axiom.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_energy__catastrophic_tail_dominant, 0.68).
domain_priors:suppression_score(acceptable_risk_energy__catastrophic_tail_dominant, 0.76).
domain_priors:theater_ratio(acceptable_risk_energy__catastrophic_tail_dominant, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, extractiveness, 0.68).
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_energy__catastrophic_tail_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_energy__catastrophic_tail_dominant, "Catastrophic-Tail-Dominant Risk Framework in Energy Policy").
narrative_ontology:topic_domain(acceptable_risk_energy__catastrophic_tail_dominant, "energy_policy/risk_assessment/decision_theory").

domain_priors:requires_active_enforcement(acceptable_risk_energy__catastrophic_tail_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_energy__catastrophic_tail_dominant, 'c2b16282-ffbe-4f42-a8c6-342595e04350').
narrative_ontology:cs_kernel_codification('c2b16282-ffbe-4f42-a8c6-342595e04350', formalized).
narrative_ontology:cs_authority_grounding('c2b16282-ffbe-4f42-a8c6-342595e04350', extraction).
narrative_ontology:cs_interpretation_layer_present('c2b16282-ffbe-4f42-a8c6-342595e04350').
narrative_ontology:cs_reading_relation('c2b16282-ffbe-4f42-a8c6-342595e04350', acceptable_risk_energy__expected_value_dominant, forecloses).
narrative_ontology:cs_reading_relation('c2b16282-ffbe-4f42-a8c6-342595e04350', acceptable_risk_energy__option_value_preserving, coexists_with).
narrative_ontology:cs_axiom('c2b16282-ffbe-4f42-a8c6-342595e04350', foundational, tail_events_incommensurably_weighty).
narrative_ontology:cs_axiom_status(tail_events_incommensurably_weighty, holdable).
narrative_ontology:cs_axiom_grounding('c2b16282-ffbe-4f42-a8c6-342595e04350', tail_events_incommensurably_weighty, deontological).
narrative_ontology:cs_axiom('c2b16282-ffbe-4f42-a8c6-342595e04350', secondary, fossil_harm_reversible_distributed_natural).
narrative_ontology:cs_axiom_status(fossil_harm_reversible_distributed_natural, overridden).
narrative_ontology:cs_axiom_grounding('c2b16282-ffbe-4f42-a8c6-342595e04350', fossil_harm_reversible_distributed_natural, empirically_contingent).
narrative_ontology:cs_reference_frame('c2b16282-ffbe-4f42-a8c6-342595e04350', precautionary_catastrophe_avoidance).
narrative_ontology:cs_drift_state('c2b16282-ffbe-4f42-a8c6-342595e04350', contemporary_empirical_challenge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c2b16282-ffbe-4f42-a8c6-342595e04350', '').
narrative_ontology:cs_kernel_id(acceptable_risk_energy__catastrophic_tail_dominant, acceptable_risk_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__catastrophic_tail_dominant, nuclear_regulatory_bodies).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__catastrophic_tail_dominant, precautionary_principle_advocates).
narrative_ontology:constraint_victim(acceptable_risk_energy__catastrophic_tail_dominant, distributed_fossil_fuel_harm_bearers).
narrative_ontology:constraint_victim(acceptable_risk_energy__catastrophic_tail_dominant, energy_transition_delayed_populations).
narrative_ontology:constraint_victim(acceptable_risk_energy__catastrophic_tail_dominant, coal_miner_communities_externally_managed).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__catastrophic_tail_dominant, coal_miner_communities).
narrative_ontology:constraint_victim(acceptable_risk_energy__catastrophic_tail_dominant, coal_miner_communities).
narrative_ontology:constraint_victim(acceptable_risk_energy__catastrophic_tail_dominant, nuclear_waste_indefinite_bearers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and enforce risk standards that weigh catastrophic rare events (core-melt, multi-site failure, long-term contamination) as dominant factors in licensing and operation decisions. Justify the asymmetric weighting as protecting public welfare against irreversible tail events. Control permitting, safety standards, and decommissioning timelines. Their institutional identity is built on nuclear safety stewardship; changing the framework would require rethinking their mandate.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, nuclear_regulatory_bodies, agenda_setter,
    institutional, generational, constrained, national).

% Environmental and safety organizations, anti-nuclear movements, some scientific ethicists. Advocate for treating potential catastrophic harm as ethically weightier than distributed harms. Gain institutional legitimacy, policy influence, funding, and media presence by advancing precautionary frameworks. Do not operate the energy system but shape its rules and influence public discourse. Have organized constituencies and political representation.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, precautionary_principle_advocates, beneficiary,
    organized, generational, mobile, global).

% Populations living downwind of coal plants, in mining regions, in communities bearing air and water pollution from fossil fuel combustion. Suffer chronic health impacts — respiratory disease, cancer, contaminated water — that are distributed across millions and attributed to many sources. Individual causation is difficult to trace; collective action is hard to organize. The catastrophic-tail framework implicitly discounts these deaths as 'natural' or 'reversible' by comparison to nuclear worst-case scenarios. Their exit from exposure requires geographic relocation or systemic energy transition, both high-cost and slow.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, distributed_fossil_fuel_harm_bearers, payer,
    powerless, biographical, trapped, global).

% Energy-poor populations in developing regions where the catastrophic-tail framework justifies continued fossil fuel reliance over nuclear deployment. Delayed access to reliable electricity for lighting, refrigeration, medical equipment, and industrial development. They bear the cost of slow transition in reduced living standards and economic opportunity. The framework's precautionary stance toward nuclear in their regions perpetuates dependence on inefficient fossil systems. They cannot exit this constraint without major geopolitical shifts in energy technology distribution.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, energy_transition_delayed_populations, payer,
    powerless, immediate, trapped, regional).

% Workers and families in coal mining regions. The catastrophic-tail framework perpetuates coal's continued operation to displace nuclear, extending coal extraction and its occupational harms — accidents, black lung disease, economic dependence on a single industry. They benefit from continued employment but bear disproportionate health costs and are structurally perpetuated in resource extraction. Their exit requires economic transition support that is not offered within the catastrophic-tail framework; they are externally managed as essential to the framework's logic ('coal as the lesser evil').
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, coal_miner_communities, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_energy__catastrophic_tail_dominant, coal_miner_communities, beneficiary).

% Economists, actuaries, and risk engineers who argue for expected-value frameworks weighting harm by probability × magnitude across all pathways. They produce mortality-per-TWh analyses typically favoring nuclear over coal. Excluded from policy-setting because their framework contradicts the precautionary axiom. Their voices are treated as ethically naive or corporately captured. They can conduct research but cannot shape policy within the catastrophic-tail framework; they must work around or against it.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, expected_value_risk_analysts, excluded,
    organized, biographical, constrained, national).

% Future generations and communities hosting long-term waste storage. Bear an uncompensated, indefinite cost — monitoring, containment risk, exclusion from land use — in exchange for zero benefit from the power generated centuries or millennia ago. The catastrophic-tail framework justifies this arrangement as the cost of avoiding worse catastrophe. Their identity as 'waste stewards' is constructed and imposed by the constraint itself; they have no meaningful choice in accepting the role. Their exit options are civilizationally constrained.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, nuclear_waste_indefinite_bearers, payer,
    powerless, civilizational, identity_locked, local).

% Government energy agencies, legislators, and planning commissions who must choose among competing risk frameworks. Observe all stakeholder positions but do not bear the primary costs or collect the primary rents from the choice. Their selection of which reading to operationalize determines which harms are made visible and which are rendered structural/inevitable. They have analytical exit options (can study alternatives) but political exit is constrained.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, policy_decision_makers, observer,
    institutional, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_energy__catastrophic_tail_dominant, nuclear_regulatory_bodies).
narrative_ontology:fixing_cost_class(acceptable_risk_energy__catastrophic_tail_dominant, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single authoritative framework for comparing energy pathways under uncertainty: one decision rule (catastrophic-tail dominance) applied uniformly to all sources, preventing ad-hoc risk weighting or inconsistent standards.
% TRANSFER_FUNCTION: Moves legitimacy, regulatory authority, and political capital from expected-value frameworks to precautionary frameworks; moves operational viability (permits, finance, public support) away from nuclear toward fossil fuel continuation; implicitly transfers the unpriced cost of distributed fossil harm and indefinite waste stewardship to powerless and future-bearing populations.
% ABSENT_VOICES: Expected-value risk analysts, option-value researchers, and energy-poor populations in developing regions are structurally excluded from policy-setting. Distributed fossil-fuel harm bearers exist but are not organized as a coherent party to the decision. Nuclear waste stewards (future generations) cannot participate in present rules that bind them indefinitely.
% DISAPPEARANCE_RATIONALE: If the catastrophic-tail framework vanished overnight and were replaced by expected-value or option-value analysis, nuclear licensing would accelerate, coal retirement would resume faster, investment in next-generation low-carbon sources would shift, and distributed fossil-fuel harm would increase relative visibility in harm-minimization calculus. The energy mix, retirement schedules, and capital allocation would reorganize significantly.
% FOUNDING_PROBLEM: Early nuclear era (1950s–1980s) had genuine uncertainty about failure modes and had not yet accumulated multi-decade operational safety data. The catastrophic-tail framework emerged as an ethically conservative response: avoid the one tail event that could be civilization-ending, even if statistically less likely than distributed alternatives.
% FOUNDING_PROBLEM_CORROBORATION: Historians of nuclear policy and environmental scientists attest the original uncertainty was real and justified precaution then. Contemporary risk analysts and public health researchers attest the founding problem is substantially resolved: 70+ years of operational data, advanced modeling, and containment technology have reduced catastrophic-tail probability far below many distributed-harm baseline rates. Regulatory bodies and precautionary advocates counter that tail events remain unacceptably possible and that epistemic humility demands ongoing conservatism. The dispute is live; no corroborating witness from outside benefiting parties affirms the founding problem persists as written.
narrative_ontology:disappearance_verdict(acceptable_risk_energy__catastrophic_tail_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_energy__catastrophic_tail_dominant, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_energy__catastrophic_tail_dominant, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(acceptable_risk_energy__catastrophic_tail_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_energy__catastrophic_tail_dominant, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_energy__catastrophic_tail_dominant_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_energy__catastrophic_tail_dominant, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(acceptable_risk_energy__catastrophic_tail_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is 0.68 at interval end, reflecting the constraint's core asymmetry: the framework creates a single decision rule that appears neutral (one standard for all pathways) while actually weighting outcomes asymmetrically — tail events weighted infinitely, distributed harms discounted as 'reversible' or 'natural.' Suppression is higher (0.76) because maintaining this framework requires active exclusion of expected-value analysts' voice, suppression of data on fossil harm comparability, and enforcement of the precautionary narrative against empirical challenge. Theater ratio rises from 0.20 to 0.42, indicating growing gap between the stated function (uniform risk assessment) and actual operation (defending nuclear exclusion): as real-world nuclear safety improves and climate forcing from coal becomes undeniable, more enforcement work goes to rhetorical maintenance of the framework's dominance rather than to substantive risk analysis. The measurement series tracks the constraint over 40 years (roughly 1980–2020), showing extraction and suppression intensifying as the original founding problem (genuine uncertainty about nuclear failure modes) recedes but the framework persists through institutional inertia and organized beneficiary defense.
 *
 * PERSPECTIVAL GAP:
 *   From the regulatory/precautionary seat, the constraint is genuine coordination — one unified rule applied consistently across sources, preventing ad-hoc favoritism. From the distributed-harm and energy-poor seats, the same structure operates as enforced extraction — an asymmetric weighting that privileges low-probability tail events over high-probability distributed harms, sustained by suppressing alternative frameworks. The gap should compute from the directed divergence: beneficiaries perceive a legitimate coordination rule; targets perceive a disguised extraction mechanism. The engine computes this from the power atoms, exit options, and structural roles.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulatory bodies and precautionary advocates sit as beneficiaries (d near 0.1–0.2): they set the rule, collect the legitimacy and political authority, and face no direct enforcement burden. Distributed fossil-harm bearers and energy-poor populations sit as pure targets (d near 0.9): they bear costs (health, access delay) without setting the rule, with low exit options (trapped or identity-locked). Coal miners are partially coordinated (the rule perpetuates their employment) and partially extracted (they bear occupational and community-dependence costs without having chosen the constraint). Nuclear waste stewards are pure targets (d = 1.0) with civilizational time horizon and identity-locked exit (the stewardship role is constructed by the constraint itself). Expected-value analysts are excluded (not stakeholders in the decision, though they would disagree if seated).
 *
 * MANDATROPHY ANALYSIS:
 *   The catastrophic-tail framework shows clear mandatrophy dynamics. Founding problem: genuine uncertainty about nuclear failure modes in the 1960s–1980s justified conservative stance. Current status: 70+ years of operational data, advanced containment, and empirical analysis of fossil versus nuclear comparative harm have substantially resolved the founding uncertainty. Theater ratio (rising 0.20 → 0.42) indicates growing gap between the stated coordination function (unified risk assessment) and actual operation (defending a specific outcome — nuclear exclusion — against empirical challenge). The framework persists because regulatory bodies and precautionary organizations now benefit from its institutional capture; it is maintained theatrically (safety review performed, but new evidence of fossil harm magnitude is rhetorically absorbed rather than changing policy). Classification as tangled_rope rather than piton reflects that the framework still delivers real coordination (one unified standard, not ad-hoc per-source rules) alongside the extraction — but the coordination function is now secondary to the extraction it enables. A piton would have no beneficiary defending it; here the precautionary coalition actively maintains it despite obsolescence of the founding problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    catastrophe_probability_drift,
    'Has the true probability of catastrophic nuclear failure moved substantially since the 1980s, and if so, does the framework''s tail-weighting remain justified at current probability levels?',
    'Meta-analysis of actual failure rates, near-miss frequency, and advanced containment effectiveness across global fleet. Comparison to base-rate catastrophe probability in comparable complex systems (aerospace, deep-water drilling, large infrastructure).',
    'If true catastrophe probability is now lower than the framework assumes, the infinite-weight assumption becomes unjustified, collapsing the catastrophic-tail reading toward expected-value. If it remains high, precaution remains justified. The divergence determines whether the framework addresses genuine risk or performative anxiety.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(catastrophe_probability_drift, empirical, 'Whether the founding problem (genuine tail-risk uncertainty) persists as factually true.').

omega_variable(
    fossil_harm_reversibility_assumption,
    'Are the distributed harms from fossil fuel combustion (respiratory disease, cancer, climate forcing, groundwater contamination) structurally ''reversible'' or ''natural'' as the framework implicitly assumes, or are they equivalent in irreversibility to nuclear accidents?',
    'Health impact modeling comparing reversibility of nuclear worst-case scenarios (decades-to-centuries for contamination cleanup and genetic repair) to fossil scenarios (CO2 persistence, disease burden, ecosystem collapse timeline). Population-level epidemiology establishing causal pathway certainty.',
    'If fossil harms are demonstrably equally or more irreversible, the framework''s asymmetric weighting is indefensible and collapse to expected-value dominance. If fossil harms are demonstrably reversible or mitigatable, the framework holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fossil_harm_reversibility_assumption, empirical, 'Whether the implicit discount on distributed fossil harm is empirically justified.').

omega_variable(
    framework_committer_divergence,
    'Is the catastrophic-tail reading a genuinely distinct logical stance on risk, or is it a deployed framework whose beneficiaries use ''precaution'' as rhetorical cover for the extraction it enables?',
    'Institutional history: examine when the framework was adopted, by whom, for what stated reasons; compare to temporal pattern of precautionary-organization growth and regulatory body empowerment. Counterfactual: would the same organizations propose tail-dominance for their own risks, or is it applied asymmetrically?',
    'If the framework is genuinely neutral logic applied fairly, it should survive challenges and adapt. If it is capture-driven extraction, empirical challenges will be absorbed rhetorically and the theater_ratio will continue rising. The institutionalization pattern determines whether reclassification to snare (pure extraction without coordination function) is warranted.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(framework_committer_divergence, conceptual, 'Whether the reading is a coherent ethical stance or a tactical deployment hiding extraction.').

omega_variable(
    identity_lock_waste_stewardship,
    'Can the identity-locked exit status for nuclear waste stewards be overcome, or is the stewardship role so embedded in institutional and social identity that exit becomes unthinkable?',
    'Post-constraint scenarios: if waste stewardship were legally transferred, would current stewards resist or accept? Would new entrants adopt the role willingly? Does the role carry social stigma or pride that fuses identity?',
    'If identity-lock can be broken by legal/institutional restructuring, suppression is structural and correctible. If identity-lock is internalized, the true suppression is higher than measured, and the victim set includes a psychological dependence the framework actively maintains.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_waste_stewardship, empirical, 'Whether nuclear waste stewardship is a structural trap or an internalized identity constraint.').

omega_variable(
    sibling_reading_exclusion,
    'Why are expected-value and option-value readings structurally excluded from policy voice, rather than coexisting as live alternatives within a pluralist framework?',
    'Institutional analysis: what mechanisms exclude these framings? (Funding capture, regulatory gatekeeping, professional credentialing, public discourse control?) Could they be re-admitted without collapsing the catastrophic-tail reading, or is exclusion necessary for it to function?',
    'If exclusion is necessary, the constraint requires active suppression and cannot be reframed as neutral coordination. If coexistence is possible, the framework is not as dominant or extractive as it appears.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_exclusion, conceptual, 'Whether the catastrophic-tail reading can coexist with alternative frameworks or requires their suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_energy__catastrophic_tail_dominant, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t0, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 0, 0.2).
narrative_ontology:measurement(acce_tr_t5, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 5, 0.25).
narrative_ontology:measurement(acce_tr_t10, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 10, 0.3).
narrative_ontology:measurement(acce_tr_t15, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 15, 0.36).
narrative_ontology:measurement(acce_tr_t20, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 20, 0.4).
narrative_ontology:measurement(acce_tr_t25, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 25, 0.41).
narrative_ontology:measurement(acce_tr_t30, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 30, 0.42).
narrative_ontology:measurement(acce_tr_t40, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(acce_be_t0, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(acce_be_t5, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(acce_be_t10, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 10, 0.56).
narrative_ontology:measurement(acce_be_t15, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(acce_be_t20, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(acce_be_t25, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 25, 0.66).
narrative_ontology:measurement(acce_be_t30, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(acce_be_t40, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t0, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(acce_su_t5, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(acce_su_t10, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(acce_su_t15, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(acce_su_t20, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 20, 0.73).
narrative_ontology:measurement(acce_su_t25, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 25, 0.75).
narrative_ontology:measurement(acce_su_t30, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 30, 0.76).
narrative_ontology:measurement(acce_su_t40, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 40, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_energy__catastrophic_tail_dominant, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(acceptable_risk_energy__catastrophic_tail_dominant, 0.12).
narrative_ontology:affects_constraint(acceptable_risk_energy__catastrophic_tail_dominant, acceptable_risk_energy__expected_value_dominant).
narrative_ontology:affects_constraint(acceptable_risk_energy__catastrophic_tail_dominant, acceptable_risk_energy__option_value_preserving).

% DUAL FORMULATION NOTE:
% The acceptable_risk_energy kernel decomposes into three structurally distinct constraint stories, each instantiating a different reading of what 'acceptable risk' means in energy policy. The catastrophic-tail-dominant reading (this story) operationalizes acceptable risk as weighting low-probability catastrophic outcomes as dominant factors. The expected-value-dominant reading operationalizes it as minimizing aggregate expected harm using mortality-per-TWh analysis. The option-value-preserving reading operationalizes it as maintaining multiple pathways to preserve decision flexibility. These are not the same constraint viewed from different angles — they have different ε values (catastrophic-tail is substantially extractive; expected-value is less so; option-value is coordinative with some extraction). They have different victim sets and beneficiary structures. They compete in policy discourse. Each story is independent; they are linked via network.affects_constraints to preserve the kernel's decomposition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(acceptable_risk_energy__catastrophic_tail_dominant, powerless, 0.92).
constraint_indexing:directionality_override(acceptable_risk_energy__catastrophic_tail_dominant, moderate, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
