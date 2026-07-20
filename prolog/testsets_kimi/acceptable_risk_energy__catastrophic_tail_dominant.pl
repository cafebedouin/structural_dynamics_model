% ============================================================================
% CONSTRAINT STORY: acceptable_risk_energy__catastrophic_tail_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Catastrophic-Tail-Dominant Acceptable Risk Framework (Energy)
 *   domain: risk_assessment/energy_policy/decision_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the catastrophic_tail_dominant reading
 *   of the acceptable_risk_energy kernel: a formalized risk-assessment
 *   framework that assigns lexicographic or infinite weight to
 *   low-probability nuclear catastrophes while discounting the higher
 *   expected aggregate harm of fossil fuel combustion as distributed and
 *   reversible. The framework is institutionalized in energy regulators,
 *   licensing criteria, and public risk communication. It coordinates around
 *   a genuine dread-risk problem but asymmetrically extracts from the nuclear
 *   industry and climate-vulnerable populations while benefiting fossil fuel
 *   incumbents. The claim/metric independence is maintained: the constraint
 *   is claimed as tangled_rope (genuine coordination function plus asymmetric
 *   extraction) while metrics describe high suppression and substantial
 *   extraction.
 *
 * KEY AGENTS:
 *   - risk_regulatory_authorities (agenda_setter, institutional/constrained) â administer the catastrophic-risk threshold
 *   - fossil_fuel_incumbents (beneficiary, powerful/constrained) â avoid displacement by suppressed nuclear competition
 *   - nuclear_industry (payer, moderate/trapped) â bears prohibitive regulatory and social costs of the framework
 *   - climate_vulnerable_populations (payer, powerless/trapped) â bear aggregate climate and pollution harm from continued fossil reliance
 *   - dread_risk_public (beneficiary, organized/identity_locked) â receives institutional validation of nuclear dread
 *   - pro_nuclear_climate_scientists (excluded, moderate/constrained) â empirically challenge the framework but are kept out of regulatory forums
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_energy__catastrophic_tail_dominant, 0.72).
domain_priors:suppression_score(acceptable_risk_energy__catastrophic_tail_dominant, 0.78).
domain_priors:theater_ratio(acceptable_risk_energy__catastrophic_tail_dominant, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, extractiveness, 0.72).
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_energy__catastrophic_tail_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_energy__catastrophic_tail_dominant, "Catastrophic-Tail-Dominant Acceptable Risk Framework (Energy)").
narrative_ontology:topic_domain(acceptable_risk_energy__catastrophic_tail_dominant, "risk_assessment/energy_policy/decision_theory").

domain_priors:requires_active_enforcement(acceptable_risk_energy__catastrophic_tail_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_energy__catastrophic_tail_dominant, 'b5dfb932-a85a-458b-87b6-d4d0714c40ec').
narrative_ontology:cs_kernel_codification('b5dfb932-a85a-458b-87b6-d4d0714c40ec', formalized).
narrative_ontology:cs_authority_grounding('b5dfb932-a85a-458b-87b6-d4d0714c40ec', expertise).
narrative_ontology:cs_interpretation_layer_present('b5dfb932-a85a-458b-87b6-d4d0714c40ec').
narrative_ontology:cs_reading_relation('b5dfb932-a85a-458b-87b6-d4d0714c40ec', acceptable_risk_energy__expected_value_dominant, forecloses).
narrative_ontology:cs_reading_relation('b5dfb932-a85a-458b-87b6-d4d0714c40ec', acceptable_risk_energy__option_value_preserving, influences).
narrative_ontology:cs_axiom('b5dfb932-a85a-458b-87b6-d4d0714c40ec', foundational, tail_risk_lexicographic_priority).
narrative_ontology:cs_axiom_status(tail_risk_lexicographic_priority, holdable).
narrative_ontology:cs_axiom_grounding('b5dfb932-a85a-458b-87b6-d4d0714c40ec', tail_risk_lexicographic_priority, deontological).
narrative_ontology:cs_axiom('b5dfb932-a85a-458b-87b6-d4d0714c40ec', secondary, nuclear_catastrophe_primary_energy_risk).
narrative_ontology:cs_axiom_status(nuclear_catastrophe_primary_energy_risk, holdable).
narrative_ontology:cs_axiom_grounding('b5dfb932-a85a-458b-87b6-d4d0714c40ec', nuclear_catastrophe_primary_energy_risk, empirically_contingent).
narrative_ontology:cs_reference_frame('b5dfb932-a85a-458b-87b6-d4d0714c40ec', catastrophic_risk_avoidance_regime).
narrative_ontology:cs_drift_state('b5dfb932-a85a-458b-87b6-d4d0714c40ec', contemporary_climate_crisis_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b5dfb932-a85a-458b-87b6-d4d0714c40ec', '').
narrative_ontology:cs_kernel_id(acceptable_risk_energy__catastrophic_tail_dominant, acceptable_risk_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__catastrophic_tail_dominant, fossil_fuel_incumbents).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__catastrophic_tail_dominant, dread_risk_public).
narrative_ontology:constraint_victim(acceptable_risk_energy__catastrophic_tail_dominant, nuclear_industry).
narrative_ontology:constraint_victim(acceptable_risk_energy__catastrophic_tail_dominant, climate_vulnerable_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set probabilistic safety criteria and licensing thresholds for nuclear plants; classify catastrophic accident scenarios as beyond acceptable risk regardless of expected mortality comparisons; their institutional mandate depends on maintaining the authority to define unacceptable tail risk.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, risk_regulatory_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from reduced competition from nuclear baseload generation; their fuel source is treated as a distributed, reversible background risk rather than a catastrophic tail risk under the regulatory framework, preserving market share and delaying decarbonization pressure.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, fossil_fuel_incumbents, beneficiary,
    powerful, generational, constrained, global).

% Bears the cost of risk frameworks that classify their technology as uniquely catastrophic; faces prohibitive insurance, licensing, and public acceptance costs; cannot exit to alternative markets because the constraint operates across jurisdictions.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, nuclear_industry, payer,
    moderate, biographical, trapped, global).

% Bear the aggregate harm from continued fossil fuel combustion that the framework effectively subsidizes by blocking nuclear expansion; trapped by geography and poverty in the path of climate impacts that the risk framework treats as secondary to nuclear tail risk.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, climate_vulnerable_populations, payer,
    powerless, generational, trapped, global).

% Receive psychological and political coordination from a framework that validates their dread of low-probability nuclear catastrophe; their risk aversion is institutionalized in safety standards, though they indirectly bear higher aggregate energy costs and climate externalities from the suppressed nuclear pathway.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, dread_risk_public, beneficiary,
    organized, biographical, identity_locked, national).

% Would argue that aggregate climate harm from fossil fuels exceeds expected nuclear catastrophe risk, but are structurally excluded from dread-risk regulatory forums where tail-probability frameworks are set; their empirical findings are treated as irrelevant to the catastrophic-risk threshold.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, pro_nuclear_climate_scientists, excluded,
    moderate, generational, constrained, global).

% Will bear the locked-in climate and air-quality costs of the fossil-fuel-intensive grid that the framework perpetuates; they are not present in the regulatory conversation and have no voice in the risk-preference trade-off.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, future_generations, excluded,
    powerless, generational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_energy__catastrophic_tail_dominant, fossil_fuel_incumbents).
narrative_ontology:fixing_cost_class(acceptable_risk_energy__catastrophic_tail_dominant, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates societal response to potentially catastrophic technological risks by establishing a common, non-market threshold for acceptable tail-risk probability, preventing a race-to-the-bottom in safety standards for high-consequence energy infrastructure and managing public dread of singular accidents.
% TRANSFER_FUNCTION: Moves capital, regulatory approval, and social license away from nuclear baseload generation toward fossil-fuel continuity, transferring the probabilistic cost of low-frequency catastrophic accidents to the certain, distributed cost of aggregate climate and air-quality harm borne by vulnerable populations and future generations.
% ABSENT_VOICES: Nuclear engineers and pro-nuclear climate scientists who argue that aggregate mortality and climate damages of fossil fuels exceed the expected catastrophic risk of modern nuclear; future generations who will bear the locked-in climate costs but are not present in the regulatory room; populations in energy-poor regions who would benefit from nuclear baseload but are excluded from risk-preference discourse dominated by dread-risk frameworks.
% DISAPPEARANCE_RATIONALE: If the catastrophic-tail-dominant framework disappeared, nuclear licensing moratoriums would lift, capital would reallocate toward low-carbon baseload, fossil fuel demand would compress faster, and the institutional mandate of dread-risk regulators would dissolve; the global energy mix and climate trajectory would rearrange.
% FOUNDING_PROBLEM: How to govern technologies that present low-probability, high-consequence failure modes without social acceptance collapsing after each accident, and how to prevent competitive deregulation of catastrophic-risk industries.
% FOUNDING_PROBLEM_CORROBORATION: The regulatory community attests the problem remains live, citing potential reactor accidents. Climate scientists and energy economists outside the benefiting parties attest that the aggregate harm calculus has shifted and the arrangement now functions as fossil-fuel market preservation; independent integrated assessment models corroborate the higher expected harm of the suppressed pathway.
narrative_ontology:disappearance_verdict(acceptable_risk_energy__catastrophic_tail_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_energy__catastrophic_tail_dominant, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_energy__catastrophic_tail_dominant, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(acceptable_risk_energy__catastrophic_tail_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_energy__catastrophic_tail_dominant, 0.72, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.72) is high because the framework blocks a low-carbon baseload source and perpetuates fossil fuel systems with larger aggregate harms. Suppression (0.78) is high because the nuclear pathway is actively suppressed through licensing, insurance, and social licensing barriers. Theater (0.45) reflects that the safety coordination function is genuine but increasingly performative relative to the larger climate catastrophic risk that the framework ignores. Accessibility collapse (0.68) captures that nuclear alternatives become politically inaccessible once the dread-risk framing is institutionalized. Resistance (0.55) reflects growing pushback from climate scientists and some energy planners.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (regulators) experiences the constraint as necessary expertise-driven coordination managing legitimate public fear; the payer seats (nuclear industry, climate vulnerable) experience it as an enforced lock-in that protects incumbents. The engine computes this divergence from structural data rather than authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Fossil fuel incumbents and the dread-risk public sit near the beneficiary end: incumbents avoid displacement, the public receives psychological validation and institutionalized risk aversion. The nuclear industry and climate-vulnerable populations sit near the target end: they bear the costs of suppressed deployment and locked-in fossil harm. Regulatory authorities administer the constraint with constrained exit from their mandate. The pro-nuclear scientific voice is excluded entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   The framework prevents mislabeling by separating the genuine coordination function (managing public dread and tail risk) from the extraction mechanism (asymmetric suppression of nuclear that benefits fossil incumbents). Without declaring both beneficiaries and victims, the framework would read as either a pure rope (if only beneficiaries were named) or a pure snare (if only victims were named). The tangled rope classification captures the hybrid.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    catastrophic_tail_reading_boundary,
    'Does the catastrophic-tail-dominant reading of acceptable risk instantiate a genuine coordination mechanism for dread risks, or a constructed extraction mechanism that protects fossil fuel incumbents?',
    'Comparative institutional analysis: examine whether the framework''s enforcement intensity correlates with fossil fuel market share protection or with measured safety outcomes across energy pathways.',
    'If the latter, reclassification toward snare; if the former, the tangled rope classification holds but with contested coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(catastrophic_tail_reading_boundary, conceptual, 'Structural ambiguity between coordination and extraction in the kernel reading.').

omega_variable(
    nuclear_suppression_mechanism_ambiguity,
    'Is the suppression of the nuclear pathway primarily structural (regulatory, economic barriers) or internalized (public dread that persists independent of regulatory change)?',
    'Observe nuclear deployment rates in jurisdictions that reform regulatory frameworks: if public opposition blocks projects despite licensing reform, suppression is partly internalized.',
    'Internalized suppression raises effective extraction by making exit from the constraint impossible even after structural barriers fall; this would amplify victim directionality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nuclear_suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression of nuclear energy pathway.').

omega_variable(
    expected_value_sibling_foreclosure,
    'If the catastrophic-tail-dominant reading holds that tail risk takes lexical priority over expected aggregate harm, does this logically foreclose the expected-value-dominant reading within a unified policy framework?',
    'Formal analysis of policy consistency: a single jurisdiction or institution cannot simultaneously hold both readings when they prescribe opposing choices (nuclear vs fossil) for the same decision node.',
    'If foreclosed, the kernel is strictly partitioned between competing camps; if coexistent, the framework is incoherent and delegates to power rather than principle.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(expected_value_sibling_foreclosure, conceptual, 'Logical relationship between catastrophic-tail and expected-value readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_energy__catastrophic_tail_dominant, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t0, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 0, 0.25).
narrative_ontology:measurement(acce_tr_t8, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 8, 0.3).
narrative_ontology:measurement(acce_tr_t16, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 16, 0.35).
narrative_ontology:measurement(acce_tr_t24, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 24, 0.4).
narrative_ontology:measurement(acce_tr_t32, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 32, 0.44).
narrative_ontology:measurement(acce_tr_t40, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 40, 0.45).

% Extraction over time
narrative_ontology:measurement(acce_be_t0, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(acce_be_t8, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(acce_be_t16, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(acce_be_t24, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 24, 0.66).
narrative_ontology:measurement(acce_be_t32, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 32, 0.7).
narrative_ontology:measurement(acce_be_t40, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 40, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t0, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(acce_su_t8, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 8, 0.65).
narrative_ontology:measurement(acce_su_t16, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 16, 0.7).
narrative_ontology:measurement(acce_su_t24, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 24, 0.74).
narrative_ontology:measurement(acce_su_t32, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 32, 0.76).
narrative_ontology:measurement(acce_su_t40, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 40, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_energy__catastrophic_tail_dominant, enforcement_mechanism).
narrative_ontology:affects_constraint(acceptable_risk_energy__catastrophic_tail_dominant, acceptable_risk_energy__expected_value_dominant).
narrative_ontology:affects_constraint(acceptable_risk_energy__catastrophic_tail_dominant, acceptable_risk_energy__option_value_preserving).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the acceptable_risk_energy kernel. The kernel decomposes into three structurally distinct claims because the epsilon values, beneficiary structures, and empirical premises differ across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
