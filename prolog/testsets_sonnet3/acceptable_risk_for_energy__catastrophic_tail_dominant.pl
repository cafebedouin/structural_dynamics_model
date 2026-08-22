% ============================================================================
% CONSTRAINT STORY: acceptable_risk_for_energy__catastrophic_tail_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_for_energy__catastrophic_tail_dominant, []).

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
 *   constraint_id: acceptable_risk_for_energy__catastrophic_tail_dominant
 *   human_readable: Catastrophic-Tail-Dominant Reading of Acceptable Nuclear Risk
 *   domain: risk_assessment/energy_policy/public_safety_governance
 *
 * SUMMARY:
 *   This story instantiates the catastrophic_tail_dominant reading of the
 *   contested kernel 'acceptable_risk_for_energy': the position that
 *   low-probability, high-consequence nuclear failure modes and
 *   multi-generational waste custody are categorically disqualifying
 *   considerations that expected-value or comparative-risk math cannot
 *   adequately represent, because irreversibility and intergenerational
 *   burden are treated as lexically prior to probability weighting. Under
 *   this reading, waste disposal is not an engineering problem to be
 *   optimized but a structural constraint that forecloses or radically slows
 *   deployment regardless of comparative mortality data. This is one of three
 *   sibling readings of the same underlying kernel (expected_value_dominant,
 *   comparative_risk_dominant); each is authored as its own constraint with
 *   its own epsilon, per the epsilon-invariance principle, and linked via
 *   network.affects_constraints.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.58).
domain_priors:suppression_score(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.71).
domain_priors:theater_ratio(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, extractiveness, 0.58).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_for_energy__catastrophic_tail_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_for_energy__catastrophic_tail_dominant, "Catastrophic-Tail-Dominant Reading of Acceptable Nuclear Risk").
narrative_ontology:topic_domain(acceptable_risk_for_energy__catastrophic_tail_dominant, "risk_assessment/energy_policy/public_safety_governance").

domain_priors:requires_active_enforcement(acceptable_risk_for_energy__catastrophic_tail_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_for_energy__catastrophic_tail_dominant, 'be7d6dc2-e1a7-4c6f-80f2-61fa745c387d').
narrative_ontology:cs_kernel_codification('be7d6dc2-e1a7-4c6f-80f2-61fa745c387d', distributed).
narrative_ontology:cs_authority_grounding('be7d6dc2-e1a7-4c6f-80f2-61fa745c387d', distributed).
narrative_ontology:cs_reading_relation('be7d6dc2-e1a7-4c6f-80f2-61fa745c387d', acceptable_risk_for_energy__expected_value_dominant, coexists_with).
narrative_ontology:cs_reading_relation('be7d6dc2-e1a7-4c6f-80f2-61fa745c387d', acceptable_risk_for_energy__comparative_risk_dominant, influences).
narrative_ontology:cs_axiom('be7d6dc2-e1a7-4c6f-80f2-61fa745c387d', foundational, irreversible_harm_lexically_prior_to_probability_weighting).
narrative_ontology:cs_axiom_status(irreversible_harm_lexically_prior_to_probability_weighting, holdable).
narrative_ontology:cs_axiom_grounding('be7d6dc2-e1a7-4c6f-80f2-61fa745c387d', irreversible_harm_lexically_prior_to_probability_weighting, deontological).
narrative_ontology:cs_axiom('be7d6dc2-e1a7-4c6f-80f2-61fa745c387d', secondary, intergenerational_custody_burden_disqualifies_absent_consent).
narrative_ontology:cs_axiom_status(intergenerational_custody_burden_disqualifies_absent_consent, holdable).
narrative_ontology:cs_axiom_grounding('be7d6dc2-e1a7-4c6f-80f2-61fa745c387d', intergenerational_custody_burden_disqualifies_absent_consent, deontological).
narrative_ontology:cs_reference_frame('be7d6dc2-e1a7-4c6f-80f2-61fa745c387d', post_chernobyl_precautionary_turn).
narrative_ontology:cs_drift_state('be7d6dc2-e1a7-4c6f-80f2-61fa745c387d', post_ipcc_decarbonization_urgency_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('be7d6dc2-e1a7-4c6f-80f2-61fa745c387d', '').
narrative_ontology:cs_kernel_id(acceptable_risk_for_energy__catastrophic_tail_dominant, acceptable_risk_for_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__catastrophic_tail_dominant, anti_nuclear_advocacy_organizations).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__catastrophic_tail_dominant, incumbent_fossil_fuel_and_renewable_competitors).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__catastrophic_tail_dominant, precautionary_regulatory_bodies).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__catastrophic_tail_dominant, nuclear_utility_operators).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__catastrophic_tail_dominant, future_generations_bearing_waste_custody).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__catastrophic_tail_dominant, regions_dependent_on_decarbonized_baseload_power).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__catastrophic_tail_dominant, displaced_coal_region_workers).
narrative_ontology:constraint_vindicates(acceptable_risk_for_energy__catastrophic_tail_dominant, precautionary_principle_supremacy).
narrative_ontology:constraint_vindicates(acceptable_risk_for_energy__catastrophic_tail_dominant, irreversibility_as_disqualifying_property).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets licensing thresholds, waste-disposal permitting standards, and siting rules using worst-case, irreversibility-weighted scenarios rather than probability-weighted expected harm. Administers the review gates that new and existing nuclear projects must pass, and can escalate documentation and mitigation requirements indefinitely because no finite showing of low probability satisfies the standard. Does not itself bear construction, financing, or waste-custody costs.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, precautionary_regulatory_bodies, agenda_setter,
    institutional, generational, analytical, national).

% Gains political and fundraising capital from the tail-dominant framing, which makes any nonzero catastrophic-tail probability sufficient grounds for indefinite opposition regardless of comparative-risk data. Not exposed to the costs of delayed decarbonization or displaced fossil generation; can shift attention to other causes without bearing consequences of the framing's persistence.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, anti_nuclear_advocacy_organizations, beneficiary,
    organized, generational, mobile, national).

% Benefits competitively when nuclear capacity is foreclosed or delayed by tail-dominant permitting standards, since market share and price floor for their own generation capacity are protected without needing to win on cost or reliability grounds. Can reposition capital toward whichever energy source the regulatory framing currently favors.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, incumbent_fossil_fuel_and_renewable_competitors, beneficiary,
    powerful, biographical, arbitrage, national).

% Bears the cost of proving safety against a standard that treats low-probability catastrophic scenarios as effectively disqualifying rather than as one input to an expected-value calculation. Cannot exit the regulatory relationship without abandoning capital already sunk into plants; can lobby for reform but cannot unilaterally change the standard by which projects are judged.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, nuclear_utility_operators, payer,
    powerful, biographical, constrained, national).

% Inherits both the physical waste-custody burden this framing insists must be treated as effectively permanent, and the foreclosed-decarbonization consequences of nuclear buildout being slowed by that same framing. Has no seat in current deliberation and cannot consent to or renegotiate the terms set today.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, future_generations_bearing_waste_custody, payer,
    powerless, civilizational, trapped, national).

% Needs firm, low-carbon baseload capacity that nuclear could supply, but faces slower buildout and higher costs because the tail-dominant standard treats any residual catastrophic probability as effectively non-negotiable, regardless of the accumulating certainty of climate harm from continued fossil reliance. Cannot easily substitute at scale in the near term.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, regions_dependent_on_decarbonized_baseload_power, payer,
    moderate, biographical, constrained, regional).

% Would benefit from faster, more predictable nuclear buildout replacing coal jobs with comparable industrial employment, but their interests are not represented in a framing organized entirely around catastrophic-tail avoidance rather than comparative regional-economic outcomes.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, displaced_coal_region_workers, excluded,
    powerless, biographical, trapped, regional).

% Studies the divergence between expected-value, comparative-risk, and catastrophic-tail-dominant framings of the same underlying hazard data, without a stake in which framing prevails in any given jurisdiction's regulatory apparatus.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, risk_assessment_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared decision procedure for irreversible, low-probability, high-consequence hazards where standard expected-value math is widely felt to understate harms that cannot be undone or compensated across generations — coordinating public trust around a precautionary floor rather than a probability-weighted average.
% TRANSFER_FUNCTION: Moves decision-making leverage from nuclear operators and future baseload-dependent regions toward regulatory bodies and framing-aligned advocacy and competitor interests; moves the compounding cost of delayed decarbonization and permanent waste custody onto future generations and regions without a voice in the standard's construction.
% ABSENT_VOICES: Future generations who will hold the waste-custody burden and inherit whatever climate trajectory resulted from slowed nuclear buildout have no representation in the standard-setting process. Displaced coal-region workers who would benefit from faster nuclear-driven industrial transition are also absent from the deliberation, which is dominated by advocacy organizations and regulators rather than affected labor.
% DISAPPEARANCE_RATIONALE: If the catastrophic-tail-dominant standard were replaced overnight by expected-value or comparative-risk framings, nuclear licensing timelines would compress, waste-disposal siting would be treated as a solvable engineering and logistics problem rather than a near-permanent disqualifying condition, and capital currently allocated to indefinite risk documentation would redirect toward construction and grid integration. Advocacy organizations and competing generation interests would lose a key point of leverage.
% FOUNDING_PROBLEM: Historical catastrophic nuclear accidents (Chernobyl, Fukushima) demonstrated that low-probability nuclear failure modes can produce consequences — permanent land loss, multi-generational contamination, unbounded liability — that standard actuarial expected-value math was not built to represent, since the harms are not divisible, insurable, or reversible in the way ordinary industrial risk is.
% FOUNDING_PROBLEM_CORROBORATION: Independent risk-assessment scholars and comparative-mortality epidemiologists attest that measured harm-per-unit-energy from nuclear remains far below coal and gas even accounting for accident tails, suggesting the founding problem (irreversibility demands a categorically different calculus) is partly a framing choice rather than a settled empirical verdict; meanwhile disaster-affected communities near Chernobyl and Fukushima, who are outside the current advocacy and regulatory beneficiary set, corroborate that the lived experience of irreversibility and displacement is real and not adequately captured by expected-value aggregation alone.
narrative_ontology:disappearance_verdict(acceptable_risk_for_energy__catastrophic_tail_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_for_energy__catastrophic_tail_dominant, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_for_energy__catastrophic_tail_dominant, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(acceptable_risk_for_energy__catastrophic_tail_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_for_energy__catastrophic_tail_dominant_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_for_energy__catastrophic_tail_dominant, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(acceptable_risk_for_energy__catastrophic_tail_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.58 at interval end) reflects that the tail-dominant standard imposes real, rising costs on nuclear operators and baseload-dependent regions that are not offset by a corresponding transfer back to them — the beneficiaries of the framing (advocacy organizations, competing generation interests, and the regulatory bodies that administer it) do not bear the foreclosed-decarbonization or waste-custody costs they impose. Suppression (0.71) is high because the framing actively suppresses probabilistic trade-off argumentation itself — a proponent of expected-value comparison is not merely disagreed with but treated as having misunderstood what irreversibility means, which forecloses the terms of debate rather than losing it on the merits. Theater ratio is moderate-low (0.28) because the underlying precautionary concern is genuine, not merely performative, even though its operationalization increasingly serves the interests of parties who do not bear its costs.
 *
 * PERSPECTIVAL GAP:
 *   From the regulatory agenda-setter seat, the standard looks like responsible stewardship of an irreducibly special hazard category. From the nuclear operator and dependent-region payer seats, the same standard looks like an indefinitely escalating and structurally unwinnable bar that no finite safety showing can satisfy. The engine computes this divergence from the declared power/exit/scope data; the claimed_type (tangled_rope) is authored independently and is expected to diverge from at least some seats' computed experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Precautionary regulatory bodies and anti-nuclear advocacy organizations sit near the beneficiary end: they gain leverage, legitimacy, and political capital from the framing without bearing its downstream costs. Fossil and renewable competitors benefit incidentally from foreclosed nuclear competition. Nuclear operators, future generations inheriting waste custody, and baseload-dependent regions sit near the target end: they bear delay costs, compliance costs, and permanent custodial burden respectively, with future generations facing the most severe directionality because they are wholly trapped (no exit, no voice, civilizational time horizon) despite bearing the largest cumulative burden.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (irreversible catastrophic accidents outstrip actuarial math) was real and remains partly live — Chernobyl and Fukushima are not hypothetical. But the standard's persistence in its current form is contested: independent comparative-mortality data suggests nuclear's actual tail risk, even including accidents, is lower than the incumbent alternatives whose market position this framing protects. Classifying this as tangled_rope rather than snare or mountain preserves the genuine coordination function (a real precautionary need existed and exists) while flagging the asymmetric extraction (advocacy organizations and competitors benefit from a standard whose costs land on operators, dependent regions, and unrepresented future generations) that a pure mountain or pure rope classification would obscure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    irreversibility_lexical_priority_ambiguity,
    'Is treating irreversibility and intergenerational burden as lexically prior to (i.e., not commensurable with) probability-weighted expected harm a defensible ethical axiom, or is it a rhetorical move that happens to serve incumbent competitors and advocacy organizations who do not bear the framing''s costs?',
    'Philosophical and decision-theoretic analysis of whether any coherent axiology can non-arbitrarily rank all irreversible harms above all reversible ones regardless of magnitude difference, combined with tracing whether beneficiary incentives shaped the standard''s adoption.',
    'If lexical priority is philosophically defensible independent of who benefits, the tangled_rope classification''s extraction component weakens toward genuine (if costly) coordination; if it is primarily incentive-shaped, the extraction component strengthens toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(irreversibility_lexical_priority_ambiguity, conceptual, 'Whether lexical priority for irreversible harm is a defensible axiom or a beneficiary-serving framing choice.').

omega_variable(
    waste_disposal_engineering_vs_constraint_status,
    'Is long-term nuclear waste custody genuinely an unsolved and possibly unsolvable engineering problem across civilizational timescales, or is it a solved-in-principle engineering and institutional-design problem that this reading treats as unsolvable for framing reasons?',
    'Comparative analysis of deep geological repository engineering (e.g. Onkalo, WIPP) against the specific institutional-continuity assumptions the tail-dominant reading requires to hold, and empirical tracking of whether repository programs that proceed encounter the predicted unmanageable failure modes.',
    'If waste custody is engineering-tractable, this reading''s core move (treating it as a constraint rather than a solvable problem) looks more like an extraction-serving framing choice; if genuinely intractable across civilizational time, the reading''s treatment is closer to accurate description of an irreducible hazard.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(waste_disposal_engineering_vs_constraint_status, empirical, 'Whether treating waste disposal as a constraint rather than an engineering problem is empirically warranted.').

omega_variable(
    future_generation_representation_gap,
    'Given that future generations bearing waste custody have no seat in current deliberation, does the tail-dominant standard actually protect their interests better than the sibling readings would, or does it primarily protect present-day advocacy and competitor interests while claiming to speak for the absent future?',
    'Modeling long-run welfare outcomes for future generations under each of the three sibling readings (tail-dominant, expected-value, comparative-risk), accounting for both waste-custody burden and climate-trajectory effects of nuclear deployment pace under each standard.',
    'If tail-dominant framing produces worse long-run outcomes for future generations than the sibling readings (e.g., via slower decarbonization), the framing''s claim to speak for future generations would be substantially undermined, strengthening the tangled_rope extraction reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_generation_representation_gap, empirical, 'Whether the tail-dominant reading actually serves the future-generation interests it claims to represent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_for_energy__catastrophic_tail_dominant, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t0, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 0, 0.14).
narrative_ontology:measurement(acce_tr_t8, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 8, 0.18).
narrative_ontology:measurement(acce_tr_t16, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 16, 0.21).
narrative_ontology:measurement(acce_tr_t24, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 24, 0.24).
narrative_ontology:measurement(acce_tr_t32, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 32, 0.26).
narrative_ontology:measurement(acce_tr_t40, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(acce_be_t0, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(acce_be_t8, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(acce_be_t16, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 16, 0.49).
narrative_ontology:measurement(acce_be_t24, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 24, 0.53).
narrative_ontology:measurement(acce_be_t32, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 32, 0.56).
narrative_ontology:measurement(acce_be_t40, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t0, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(acce_su_t8, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(acce_su_t16, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 16, 0.63).
narrative_ontology:measurement(acce_su_t24, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 24, 0.67).
narrative_ontology:measurement(acce_su_t32, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 32, 0.7).
narrative_ontology:measurement(acce_su_t40, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_for_energy__catastrophic_tail_dominant, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.12).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_dominant, acceptable_risk_for_energy__expected_value_dominant).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_dominant, acceptable_risk_for_energy__comparative_risk_dominant).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the kernel acceptable_risk_for_energy, decomposed per the epsilon-invariance principle because the three readings assign structurally different epsilon values to nuclear risk governance: the tail-dominant reading (this story) treats irreversibility as lexically disqualifying (moderate-high extraction, high suppression of trade-off framing); the expected_value_dominant reading treats probability x consequence as commensurable with other harms (lower extraction, lower suppression); the comparative_risk_dominant reading judges nuclear only against competing energy risks with no absolute threshold (extraction and suppression profile intermediate, shaped by which comparator fuel is politically salient). Each reading is authored as an independent constraint with its own beneficiary/victim structure; they are linked here rather than merged because merging would violate epsilon-invariance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
