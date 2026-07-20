% ============================================================================
% CONSTRAINT STORY: acceptable_risk_for_energy__comparative_risk_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: acceptable_risk_for_energy__comparative_risk_dominant
 *   human_readable: Comparative Risk Dominant Nuclear Acceptability Framework
 *   domain: risk_assessment/energy_policy/public_safety
 *
 * SUMMARY:
 *   This constraint story models the comparative-risk-dominant reading of
 *   nuclear energy acceptability: the normative commitment that nuclear risk
 *   should be evaluated only relative to competing energy risks (principally
 *   coal mortality and climate catastrophe), with no absolute safety
 *   threshold. It is one reading of the contested kernel
 *   'acceptable_risk_for_energy', distinct from catastrophic-tail-dominant
 *   and expected-value-dominant readings. The framework coordinates energy
 *   policy by providing a decision rule when all options carry risks, but
 *   asymmetrically extracts risk-bearing capacity from climate-vulnerable
 *   populations (whose baseline endangerment forms the comparison
 *   denominator) and future generations (whose intergenerational interests
 *   are overridden by temporal urgency).
 *
 * KEY AGENTS:
 *   - energy_regulators (institutional/constrained): Administer the comparative risk framework through licensing and safety standards
 *   - nuclear_industry (powerful/constrained): Commercial beneficiary of comparative rather than absolute safety evaluation
 *   - climate_vulnerable_populations (powerless/trapped): Bear the baseline climate risk that makes nuclear look acceptable by comparison
 *   - intergenerational_future_populations (powerless/trapped): Bear long-term waste risk without absolute threshold protection
 *   - fossil_fuel_incumbents (powerful/mobile): Serve as the comparison baseline while facing displacement
 *   - absolute_safety_advocates (moderate/constrained): Excluded voices arguing for zero-tolerance thresholds
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_for_energy__comparative_risk_dominant, 0.65).
domain_priors:suppression_score(acceptable_risk_for_energy__comparative_risk_dominant, 0.7).
domain_priors:theater_ratio(acceptable_risk_for_energy__comparative_risk_dominant, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, extractiveness, 0.65).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_for_energy__comparative_risk_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_for_energy__comparative_risk_dominant, "Comparative Risk Dominant Nuclear Acceptability Framework").
narrative_ontology:topic_domain(acceptable_risk_for_energy__comparative_risk_dominant, "risk_assessment/energy_policy/public_safety").

domain_priors:requires_active_enforcement(acceptable_risk_for_energy__comparative_risk_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_for_energy__comparative_risk_dominant, '45bbf560-3e75-44db-bcac-e080ace44166').
narrative_ontology:cs_kernel_codification('45bbf560-3e75-44db-bcac-e080ace44166', formalized).
narrative_ontology:cs_authority_grounding('45bbf560-3e75-44db-bcac-e080ace44166', expertise).
narrative_ontology:cs_interpretation_layer_present('45bbf560-3e75-44db-bcac-e080ace44166').
narrative_ontology:cs_reading_relation('45bbf560-3e75-44db-bcac-e080ace44166', acceptable_risk_for_energy__catastrophic_tail_dominant, coexists_with).
narrative_ontology:cs_reading_relation('45bbf560-3e75-44db-bcac-e080ace44166', acceptable_risk_for_energy__expected_value_dominant, coexists_with).
narrative_ontology:cs_axiom('45bbf560-3e75-44db-bcac-e080ace44166', foundational, comparative_sufficiency_for_acceptability).
narrative_ontology:cs_axiom_status(comparative_sufficiency_for_acceptability, holdable).
narrative_ontology:cs_axiom_grounding('45bbf560-3e75-44db-bcac-e080ace44166', comparative_sufficiency_for_acceptability, instrumental).
narrative_ontology:cs_axiom('45bbf560-3e75-44db-bcac-e080ace44166', foundational, climate_urgency_overrides_generational_equity).
narrative_ontology:cs_axiom_status(climate_urgency_overrides_generational_equity, holdable).
narrative_ontology:cs_axiom_grounding('45bbf560-3e75-44db-bcac-e080ace44166', climate_urgency_overrides_generational_equity, instrumental).
narrative_ontology:cs_reference_frame('45bbf560-3e75-44db-bcac-e080ace44166', comparative_risk_equilibrium).
narrative_ontology:cs_drift_state('45bbf560-3e75-44db-bcac-e080ace44166', climate_crisis_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('45bbf560-3e75-44db-bcac-e080ace44166', '').
narrative_ontology:cs_kernel_id(acceptable_risk_for_energy__comparative_risk_dominant, acceptable_risk_for_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_industry).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__comparative_risk_dominant, climate_policy_coalition).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, climate_vulnerable_populations).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, intergenerational_future_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, fossil_fuel_incumbents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set nuclear licensing standards and safety thresholds using comparative risk benchmarks against fossil fuel mortality and climate catastrophe projections. They maintain the framework through periodic regulatory reviews and risk assessments that explicitly compare energy system alternatives rather than establishing absolute safety limits. Their authority depends on scientific credibility and statutory mandate.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, energy_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Operates nuclear power plants under licenses granted on comparative safety grounds. Gains market access and social license because the regulatory framework evaluates facilities against coal mortality and climate catastrophe rather than zero-risk standards. Avoids the compliance burden and capital costs that absolute safety thresholds would impose, while bearing operational and accident liabilities.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_industry, beneficiary,
    powerful, biographical, constrained, global).

% Advocates for rapid decarbonization using all available low-carbon tools. Uses the comparative risk framework to justify nuclear deployment against climate change, arguing that absolute safety standards would foreclose a necessary climate solution. Gains policy legitimacy, regulatory traction, and coalition cohesion from the framework's comparative logic.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, climate_policy_coalition, beneficiary,
    organized, biographical, constrained, national).

% Live in regions most exposed to sea-level rise, extreme heat, and agricultural disruption from climate change. Their ongoing endangerment by fossil fuel emissions is treated as the baseline comparison denominator that makes nuclear risk acceptable, meaning their uncompensated harm is structurally necessary to the framework's arithmetic. They do not choose their energy system but bear its consequences regardless of which technology is selected.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, climate_vulnerable_populations, payer,
    powerless, generational, trapped, global).

% Will inhabit the radioactive legacy and long-term waste storage burden of nuclear energy decisions made today. The comparative framework's override of absolute thresholds means their interests are systematically discounted relative to present climate urgency. They have no representation in current licensing proceedings, risk assessments, or discount-rate deliberations.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, intergenerational_future_populations, payer,
    powerless, civilizational, trapped, global).

% Operate coal, oil, and gas infrastructure that serves as the implicit baseline against which nuclear risk is evaluated. Face gradual market displacement as the comparative framework is used to justify nuclear expansion, but also benefit from the framework's tacit acceptance that fossil fuel harm is the inevitable benchmark rather than an intolerable anomaly to be eliminated.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, fossil_fuel_incumbents, payer,
    powerful, biographical, mobile, global).

% Argue for zero-tolerance or near-zero risk thresholds for nuclear energy, rejecting the legitimacy of comparative evaluation against fossil fuels. Are structurally excluded from regulatory advisory roles and licensing proceedings that have adopted comparative risk as the governing methodology, and their voices are treated as non-serious or anti-scientific in policy venues.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, absolute_safety_advocates, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_for_energy__comparative_risk_dominant, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a decision framework for energy policy when all available energy options carry significant risks, allowing societies to choose the least-harm portfolio rather than being paralyzed by the pursuit of zero-risk alternatives that do not exist, particularly under climate crisis time pressure.
% TRANSFER_FUNCTION: Moves risk tolerance and risk-bearing obligation from institutional regulators and current energy beneficiaries to climate-vulnerable populations (who bear the baseline fossil fuel harm used as the comparison denominator) and future generations (who bear intergenerational waste risk without absolute threshold protection).
% ABSENT_VOICES: Absolute safety advocates who reject comparative evaluation are structurally excluded from regulatory proceedings; future generations have no standing in licensing decisions; climate-vulnerable communities in the Global South are represented only indirectly through aggregate mortality statistics rather than testimony.
% DISAPPEARANCE_RATIONALE: If the comparative risk framework vanished overnight, energy regulators would need to reorganize around absolute safety standards or alternative risk heuristics, nuclear licensing proceedings would lose their primary decision architecture, the nuclear industry would face potentially prohibitive compliance costs, and climate policy coalitions would lose a central argumentative tool for justifying nuclear deployment.
% FOUNDING_PROBLEM: Energy policy paralysis in the face of multiple risky options during escalating climate change; the need to make urgent deployment decisions when no energy source is risk-free and absolute zero-risk standards would foreclose all industrial-scale alternatives.
% FOUNDING_PROBLEM_CORROBORATION: Climate scientists and energy economists outside the nuclear industry attest to the urgency of decarbonization and the analytical necessity of comparative evaluation. Environmental justice advocates and intergenerational ethics scholars contest that the founding problem justifies the current risk distribution, arguing that renewable alternatives and demand reduction have dissolved the original coordination problem.
narrative_ontology:disappearance_verdict(acceptable_risk_for_energy__comparative_risk_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_for_energy__comparative_risk_dominant, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_for_energy__comparative_risk_dominant, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(acceptable_risk_for_energy__comparative_risk_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_for_energy__comparative_risk_dominant, 0.65, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.65) reflects that the framework externalizes substantial risk onto non-consenting populations without compensatory mechanisms. Suppression (0.70) is high because the framework requires active regulatory enforcement to maintain comparative evaluation against persistent absolute-safety advocacy. Theater ratio (0.40) captures the growing performative element: regulators increasingly justify decisions with comparative risk rhetoric while actual safety margins are driven by political and economic feasibility. Accessibility collapse (0.60) is moderate because absolute zero-risk alternatives are conceptually available but practically inaccessible given climate constraints and baseload requirements. Resistance (0.55) reflects sustained opposition from environmental justice and intergenerational ethics advocates.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (energy regulators) experiences the constraint as necessary coordination: without comparative evaluation, energy policy would be paralyzed by unattainable absolute safety standards during a climate crisis. The payer seats (climate-vulnerable populations, future generations) experience it as risk colonization: their uncompensated endangerment is the arithmetic that makes the framework work. The beneficiary seats (nuclear industry, climate policy coalition) experience it as pragmatic problem-solving. These divergences are structurally determined by power and exit asymmetries, not by disagreement about facts.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries derive low directionality: the nuclear industry is subsidized by avoiding absolute compliance costs, and the climate policy coalition gains political capacity. Victims derive high directionality: climate-vulnerable populations are structurally trapped in the comparison denominator, and future populations have no exit from waste legacies. The energy regulators sit near symmetricâthey bear institutional accountability costs but also retain agenda-setting power.
 *
 * MANDATROPHY ANALYSIS:
 *   The framework prevents mandatrophy mislabeling by acknowledging its genuine coordination function (energy policy decision-making under universal risk) while independently documenting its asymmetric extraction (risk externalization onto non-consenting populations). A pure snare reading would miss the coordination; a pure rope reading would miss the extraction. The Tangled Rope classification is structurally required because both coordination and extraction are present and coupled through the same mechanism: the comparative evaluation itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    renewable_alternative_resolvability,
    'Does the emergence of viable renewable energy and storage alternatives dissolve the founding problem that justified comparative risk assessment, or does grid reliability and baseload requirements keep the comparative framework necessary?',
    'Track capacity-factor-adjusted renewable deployment against grid stability metrics in jurisdictions that have abandoned nuclear; if stable grids emerge at scale without comparative nuclear licensing, the founding problem is dead.',
    'If renewables resolve the coordination problem, the constraint persists as extraction rather than genuine coordination, shifting classification toward snare or piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(renewable_alternative_resolvability, empirical, 'Whether renewable advances obsolete the comparative risk framework').

omega_variable(
    baseline_constructedness,
    'Is fossil fuel harm the natural baseline for energy risk comparison, or is it a constructed benchmark that privileges incumbent energy systems and disadvantages absolute safety standards?',
    'Historical genealogy of comparative risk frameworks: trace whether the baseline was selected because fossil fuels were dominant (path dependency) or because they represent a genuine welfare floor (analytic necessity).',
    'If the baseline is constructed, the framework''s neutrality claim collapses and extraction is higher than presented; if natural, the coordination function is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(baseline_constructedness, conceptual, 'Whether the fossil fuel comparison baseline is constructed or necessary').

omega_variable(
    intergenerational_discounting_legitimacy,
    'Does temporal urgency legitimately override intergenerational equity, or is this override a constructed extraction from future populations?',
    'Compare the constraint''s operation in jurisdictions with different discount rates and intergenerational legal standing; observe whether higher future-population representation correlates with absolute threshold adoption.',
    'If override is illegitimate, the victim set expands and directionality toward future populations rises; if legitimate, the coordination function absorbs the intergenerational cost as necessary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_discounting_legitimacy, preference, 'Whether temporal urgency legitimately overrides intergenerational concerns').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_for_energy__comparative_risk_dominant, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t0, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 0, 0.2).
narrative_ontology:measurement(acce_tr_t10, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 10, 0.25).
narrative_ontology:measurement(acce_tr_t20, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 20, 0.3).
narrative_ontology:measurement(acce_tr_t30, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 30, 0.35).
narrative_ontology:measurement(acce_tr_t40, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 40, 0.4).
narrative_ontology:measurement(acce_tr_t50, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(acce_be_t0, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(acce_be_t10, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(acce_be_t20, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(acce_be_t30, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 30, 0.6).
narrative_ontology:measurement(acce_be_t40, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 40, 0.63).
narrative_ontology:measurement(acce_be_t50, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t0, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(acce_su_t10, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(acce_su_t20, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(acce_su_t30, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(acce_su_t40, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 40, 0.72).
narrative_ontology:measurement(acce_su_t50, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 50, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_for_energy__comparative_risk_dominant, enforcement_mechanism).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__comparative_risk_dominant, acceptable_risk_for_energy__catastrophic_tail_dominant).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__comparative_risk_dominant, acceptable_risk_for_energy__expected_value_dominant).

% DUAL FORMULATION NOTE:
% This constraint is the comparative_risk_dominant reading of the acceptable_risk_for_energy kernel, distinct from catastrophic_tail_dominant and expected_value_dominant readings. Decomposition follows the epsilon-invariance principle: each reading has a structurally distinct epsilon, beneficiary/victim set, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
