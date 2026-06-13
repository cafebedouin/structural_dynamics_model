% ============================================================================
% CONSTRAINT STORY: acceptable_risk_for_energy__comparative_risk_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: acceptable_risk_for_energy__comparative_risk_dominant
 *   human_readable: Comparative Risk Framework for Nuclear Energy Acceptability
 *   domain: energy_policy/risk_assessment/public_safety
 *
 * SUMMARY:
 *   A dominant risk framework for energy policy and nuclear deployment holds
 *   that nuclear risk becomes acceptable only when compared against and found
 *   lower than competing risks, particularly coal emissions and climate
 *   catastrophe. This reading lacks an absolute safety threshold; instead,
 *   acceptability is relational—nuclear is good enough if the alternative is
 *   worse. The reading generates beneficiaries (nuclear operators, fossil
 *   incumbents, developed economies avoiding costly decarbonization),
 *   identifiable victims (intergenerational waste bearers, climate-vulnerable
 *   populations, accident-zone residents), and active enforcement mechanisms
 *   (regulatory licensing, environmental review standards anchored to coal
 *   baselines, suppression of alternatives like renewables-only pathways).
 *   The constraint operates as a tangled rope: it genuinely solves the
 *   coordination problem of energy choice under climate urgency (coordination
 *   function), while simultaneously extracting intergenerational burden and
 *   political power from powerless and unborn populations (extraction
 *   function). The metrics reveal rising extractiveness from 1990 (0.38) to
 *   present (0.68), rising theater ratio (performative safety review without
 *   fundamental challenge to the framework) from 0.15 to 0.41, and high
 *   suppression (0.72) maintained through regulatory enforcement. The
 *   constraint is ONE READING of the contested kernel
 *   'acceptable_risk_for_energy', competing with tail-dominance and
 *   expected-value readings that would produce different victim sets and
 *   beneficiaries. The kernel context section details the sibling readings
 *   and their structural relationships to this one.
 *
 * KEY AGENTS:
 *   - nuclear_industry_and_operators: Institutional beneficiary and agenda-setter; controls the framing of acceptability through regulatory engagement; benefits directly from comparative frame.
 *   - fossil_fuel_incumbent_operators: Institutional beneficiary; coal becomes the implicit baseline rather than an alternative to eliminate entirely; benefits from frame's temporal orientation that delays full decarbonization.
 *   - developed_economies_high_electricity_demand: Institutional beneficiary; avoids political cost of demand reduction or rapid renewable buildout; maintains baseload reliability under the constraint.
 *   - climate_vulnerable_populations: Powerless payer; bears acute climate harms AND disproportionate nuclear risk (siting, waste); structurally unrepresented in benefit side.
 *   - intergenerational_future_bearers: Powerless payer (powerless because unborn); inherits waste obligation with timescales exceeding civilization lifespans.
 *   - radioactive_waste_inheritors: Powerless payer; carry the obligation to maintain institutional continuity and containment across timescales that exceed known human governmental lifespans.
 *   - nuclear_accident_proximate_residents: Moderate power payer; live under accident risk with constrained exit (economic and social embededness).
 *   - catastrophic_tail_reading_proponents: Excluded; their reading dominates risk by low-probability high-consequence events, structurally excluded by framework's temporal prioritization.
 *   - regulatory_and_policy_authorities: Agenda-setter; enforces the comparative frame through licensing standards; constrained by inherited legal structures and political pressure.
 *   - renewable_energy_advocates: Excluded; the comparative frame (nuclear vs. coal) pre-selects both away from renewables.
 *   - scientific_expert_communities: Observer seat; produces risk assessments that underpin the framework; substantive but not controlling.
 *   - climate_advocates_and_ngos: Bifurcated payer/beneficiary; benefit from temporal urgency prioritization but pay the cost of accepting intergenerational burden as climate-urgency cost.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_for_energy__comparative_risk_dominant, 0.68).
domain_priors:suppression_score(acceptable_risk_for_energy__comparative_risk_dominant, 0.72).
domain_priors:theater_ratio(acceptable_risk_for_energy__comparative_risk_dominant, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, extractiveness, 0.68).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_for_energy__comparative_risk_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_for_energy__comparative_risk_dominant, "Comparative Risk Framework for Nuclear Energy Acceptability").
narrative_ontology:topic_domain(acceptable_risk_for_energy__comparative_risk_dominant, "energy_policy/risk_assessment/public_safety").

domain_priors:requires_active_enforcement(acceptable_risk_for_energy__comparative_risk_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_for_energy__comparative_risk_dominant, '382ac4c1-6894-4a3b-83d3-917e1219b3c1').
narrative_ontology:cs_kernel_codification('382ac4c1-6894-4a3b-83d3-917e1219b3c1', formalized).
narrative_ontology:cs_authority_grounding('382ac4c1-6894-4a3b-83d3-917e1219b3c1', expertise).
narrative_ontology:cs_interpretation_layer_present('382ac4c1-6894-4a3b-83d3-917e1219b3c1').
narrative_ontology:cs_reading_relation('382ac4c1-6894-4a3b-83d3-917e1219b3c1', acceptable_risk_for_energy__catastrophic_tail_dominant, coexists_with).
narrative_ontology:cs_reading_relation('382ac4c1-6894-4a3b-83d3-917e1219b3c1', acceptable_risk_for_energy__expected_value_dominant, influences).
narrative_ontology:cs_axiom('382ac4c1-6894-4a3b-83d3-917e1219b3c1', foundational, comparative_risk_dominates_absolute_thresholds).
narrative_ontology:cs_axiom_status(comparative_risk_dominates_absolute_thresholds, holdable).
narrative_ontology:cs_axiom_grounding('382ac4c1-6894-4a3b-83d3-917e1219b3c1', comparative_risk_dominates_absolute_thresholds, empirically_contingent).
narrative_ontology:cs_axiom('382ac4c1-6894-4a3b-83d3-917e1219b3c1', foundational, present_climate_urgency_overrides_intergenerational_burden).
narrative_ontology:cs_axiom_status(present_climate_urgency_overrides_intergenerational_burden, holdable).
narrative_ontology:cs_axiom_grounding('382ac4c1-6894-4a3b-83d3-917e1219b3c1', present_climate_urgency_overrides_intergenerational_burden, empirically_contingent).
narrative_ontology:cs_axiom('382ac4c1-6894-4a3b-83d3-917e1219b3c1', secondary, coal_baseline_is_relevant_comparison).
narrative_ontology:cs_axiom_status(coal_baseline_is_relevant_comparison, holdable).
narrative_ontology:cs_axiom_grounding('382ac4c1-6894-4a3b-83d3-917e1219b3c1', coal_baseline_is_relevant_comparison, conventional).
narrative_ontology:cs_reference_frame('382ac4c1-6894-4a3b-83d3-917e1219b3c1', comparative_risk_baseline_coal).
narrative_ontology:cs_drift_state('382ac4c1-6894-4a3b-83d3-917e1219b3c1', contemporary_renewable_deployment_acceleration, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('382ac4c1-6894-4a3b-83d3-917e1219b3c1', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(acceptable_risk_for_energy__comparative_risk_dominant, acceptable_risk_for_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_industry).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__comparative_risk_dominant, fossil_fuel_incumbent_operators).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__comparative_risk_dominant, developed_economies_high_electricity_demand).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, climate_vulnerable_populations).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, intergenerational_future_bearers).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, radioactive_waste_inheritors).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_accident_proximate_residents).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_for_energy__comparative_risk_dominant, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(acceptable_risk_for_energy__comparative_risk_dominant, 'none', 1).

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
 *   The extractiveness metric (0.68 at interval end) reflects the core asymmetry: intergenerational waste obligation is transferred from electricity consumers and policymakers (present) to inheritors (future); accident risk is sited near economically and politically powerless populations; and the framework's temporal urgency overrides intergenerational reversibility concerns. The suppression metric (0.72) measures the active enforcement machinery: regulatory barriers to alternatives (renewables-only pathways), capital and political support concentrated in nuclear/fossil incumbents, and the internalization of coal-comparison framing as 'natural' or 'necessary.' Theater ratio (0.41) reflects the growing divergence between the framework's presented safety-review function and its actual enforcement function (maintaining the comparative frame, defending nuclear against absolute-threshold critiques). The measurement series shows rising extractiveness and theater over the interval (1990–2050), with a plateau after 2035 as the framework matures and stabilizes: early-period values reflect the framework's emergence and climb to dominance; plateau reflects mature institutional capture. Accessibility collapse (0.62) is moderate-low because alternatives (renewables, demand reduction) remain formally available but are politically and institutionally de-prioritized by the framework; they haven't collapsed entirely but the framework has narrowed the choice set to binary (nuclear or coal). Resistance (0.58) indicates substantial pushback from intergenerational ethics scholars, renewable advocates, and some affected populations, but the resistance is organizationally outmatched and excluded from decision tables by the framework's institutional dominance.
 *
 * PERSPECTIVAL GAP:
 *   The nuclear industry and regulatory authorities perceive the constraint as genuine coordination (solving the binding problem of energy choice under climate urgency, enabling decarbonization when perfect alternatives are unavailable). The payer seats—particularly intergenerational inheritors, climate-vulnerable populations, and accident-zone residents—perceive it as enforced extraction masked as technical optimization. The engine should compute these as different type classifications from the same structural data: the agenda-setter seat (regulatory authority + industry) sees rope-like coordination; the powerless payer seats see snare-like extraction. This perspectival gap arises from the asymmetry in beneficiary/victim declarations, exit options, and time horizons authored into the structural data. The analytical observer seat (scientific expert communities) should compute as near-symmetric (produces analyses consumed by both sides, neither fundamentally disadvantaged by the constraint) or slightly toward the beneficiary end (their expertise is valued and funded by the regime).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation chains from beneficiary/victim declarations and exit options. Nuclear industry and fossil incumbents: declared beneficiaries with arbitrage-grade exit (they can switch regulatory jurisdictions, capital flows, business models if needed—though none do, because the constraint benefits them). Directionality near 0.0 (full beneficiary end). Intergenerational future bearers: declared victims with trapped exit (no exit possible; they inherit the waste or face the accident whether or not they participate). Directionality near 1.0 (full target end). Climate-vulnerable populations: declared victims with trap-type exit (geographic, economic dependency on energy systems and locations; exit requires relocation and capital that powerless populations lack). Directionality near 0.9. Nuclear accident proximate residents: moderate power, constrained exit (can leave but at substantial economic cost; cannot bargain because they're local, dispersed, powerless-to-moderate relative to institutional actors). Directionality near 0.75. Developed economy institutional actors: declared beneficiaries with constrained exit (can adopt different energy pathways but face political and capital constraints; no pure arbitrage available because decarbonization timelines are binding). Directionality near 0.15. Regulatory authorities: agenda-setter role, constrained exit (can reframe the acceptable-risk metric but face industry pressure and legal inheritance). Directionality slightly upward from full beneficiary, perhaps 0.20-0.30, because they hold formal power but are informationally and institutionally captured. Climate-advocate NGOs: bifurcated beneficiary/payer role creates conflict; their directionality should split (some orgs move toward full beneficiary at ~0.05 as they embrace nuclear; others move toward payer at ~0.70 as they prioritize intergenerational concern). One directionality override may be warranted for regulatory authorities: structural derivation might place them at 0.35 (moderate power, constrained by industry and legal structure), but explicit override to 0.25 could reflect their formal agenda-setting authority being substantially captured.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (climate catastrophe risk from fossil fuels; energy deployment timelines; decarbonization stalemate) is live and urgent. The constraint's mandate (enable nuclear deployment by establishing comparative risk acceptability against coal) is directly responsive to that problem. However, there are indicators of mandate drift: (1) The theater ratio rising from 0.15 to 0.41 suggests growing performative activity (safety reviews, stakeholder consultations, environmental impact assessments) decoupled from fundamental challenge to the framework—the framework itself is not contested in the regulatory process, only its application is. (2) Suppression remaining at 0.72 despite climate consensus growth suggests enforcement effort is not declining despite problem maturation; this could indicate that alternatives (renewables deployment scaling rapidly) now pose a threat to the framework's dominance, requiring sustained suppression. (3) The accessibility-collapse measure (0.62) is moderate: renewables and demand reduction remain formally available options, but the comparative frame has narrowed institutional and capital allocation toward nuclear/coal, effectively suppressing the accessibility. A genuine mandate-drift signal would be: if climate urgency visibly declined (decarbonization accelerated through unexpected means, climate predictions improved) but the constraint persisted with unchanged suppression, that would signal the constraint has lost its original mandate and is now held by institutional inertia. The authored measurements plateau at 2035, suggesting the framework achieves stable maturity rather than declining. A true mandatrophy reading would require showing rising theater and declining beneficiary function (people stop benefiting and only enforcement machinery remains), which the data does not yet show. Mandatrophy is not yet resolved here, but the theater-ratio trajectory and suppression-under-declining-urgency would be the signals to watch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    baseline_choice_contingency,
    'Is the comparative risk framework''s foundation genuinely that nuclear-vs-coal comparison, or is this an arbitrary selection that could be replaced by nuclear-vs-renewables or nuclear-vs-demand-reduction?',
    'Historical analysis of policy deliberation: who proposed the coal baseline, when, and against what alternatives? Did the choice come from technical risk analysis or from political constraints that made coal the baseline of comparison?',
    'If the coal baseline was chosen because renewables were politically or technically infeasible at the time but are now feasible, the framework''s justification has been superseded; if it was chosen for technical reasons, the framework remains defensible until those technical conditions change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(baseline_choice_contingency, empirical, 'Whether the coal comparison is natural to the problem or contingent on earlier constraints.').

omega_variable(
    intergenerational_commensurability,
    'Is it coherent to discount intergenerational radioactive waste obligations against present-day climate urgency, or does the irreversibility of waste and the timescale of half-lives place it outside the scope of a comparative risk calculus that operates on human decision horizons?',
    'Normative frameworks from intergenerational ethics and environmental philosophy. Can temporal urgency for one generation justify transferring permanent obligation to unknown successors? If not, the framework''s comparison is inapplicable.',
    'If intergenerational waste is incommensurable with present urgency, the framework fails on its own terms; if commensurable, the framework is robust to the objection.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_commensurability, conceptual, 'Whether present climate urgency can legitimately weigh against intergenerational waste burden.').

omega_variable(
    reading_versus_tail_dominance,
    'Do low-probability high-consequence scenarios (10,000-year waste failure, Chernobyl-scale events cascading across multiple reactors, unknown unknowns in geological containment) have a meaningful probability and consequence enough to override the comparative expected-value calculation?',
    'Empirical: models from tail-risk analysis, geological uncertainty assessment, and analysis of accidents whose prior probability was considered negligible (Fukushima, Chernobyl). Conceptual: whether probability-weighted tail risk should dominate expected-value optimization.',
    'If tail risks are material and non-negligible, the comparative framework is incomplete (it optimizes expected value while leaving tail risk uncontrolled). The constraint would need to include tail-dominance safeguards.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_versus_tail_dominance, empirical, 'Whether the comparative framework adequately accounts for low-probability high-consequence scenarios.').

omega_variable(
    suppression_mechanism_internalized,
    'Is the measured suppression (0.72) primarily structural (regulatory barriers, capital investment dependency, geographic siting constraints imposed externally) or internalized (populations near reactors accept risk as inevitable, opponents accept coal-comparison frame as natural, future advocates feel obligated by present urgency)?',
    'Post-framework-change trajectory: if suppression persists after the comparative frame is removed or abandoned, it is partially internalized; if it dissipates, it was primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, and policy changes alone may not alter acceptance. If structural, regulatory reform can shift the equilibrium.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized, empirical, 'Whether suppression is externally imposed or internally accepted.').

omega_variable(
    climate_urgency_as_cover_story,
    'Is the appeal to climate urgency a genuine constraint on the decision (decarbonization really does require nuclear speed, no viable alternative exists to meet the deadline), or a rhetorical strategy to legitimize deployment that would occur for other reasons (capital availability, incumbent power, path dependency)?',
    'Comparative historical analysis: in jurisdictions that chose renewables-only paths, did climate outcomes differ? Did the pace of deployment differ? Are there confounding variables (capital structure, political alignment, resource endowment)?',
    'If urgency is genuine constraint, the comparative frame is justified as a necessary concession to solve climate. If it is rhetorical strategy, the framework is a cover story for incumbent extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(climate_urgency_as_cover_story, empirical, 'Whether climate urgency is a binding constraint or a justification post-hoc.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint a reading of the kernel ''acceptable_risk_for_energy'' (defined by the contestation among catastrophic_tail_dominant, comparative_risk_dominant, and expected_value_dominant readings), or is it itself an instantiation of a deeper kernel about temporal urgency (defined by the contestation between present-climate-urgency and intergenerational-burden readings)?',
    'Structural analysis of the constraint''s core claim: is the distinguishing feature the comparative-risk axiom (vs. tail dominance or expected value), or the temporal-priority axiom (present urgency over future burden)?',
    'If the core is comparative-risk framing, this is a reading of the acceptable_risk kernel; if temporal priority is core, this reading bridges two kernels, and the constraint''s identity is ambiguous.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether this constraint is a reading of one kernel or spans two kernels.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_for_energy__comparative_risk_dominant, 1990, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t1990, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(acce_tr_t2005, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 2005, 0.22).
narrative_ontology:measurement(acce_tr_t2015, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 2015, 0.32).
narrative_ontology:measurement(acce_tr_t2025, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 2025, 0.4).
narrative_ontology:measurement(acce_tr_t2035, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 2035, 0.41).
narrative_ontology:measurement(acce_tr_t2050, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 2050, 0.41).

% Extraction over time
narrative_ontology:measurement(acce_be_t1990, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 1990, 0.38).
narrative_ontology:measurement(acce_be_t2005, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 2005, 0.48).
narrative_ontology:measurement(acce_be_t2015, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 2015, 0.58).
narrative_ontology:measurement(acce_be_t2025, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 2025, 0.65).
narrative_ontology:measurement(acce_be_t2035, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 2035, 0.68).
narrative_ontology:measurement(acce_be_t2050, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 2050, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t1990, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 1990, 0.48).
narrative_ontology:measurement(acce_su_t2005, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 2005, 0.58).
narrative_ontology:measurement(acce_su_t2015, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 2015, 0.66).
narrative_ontology:measurement(acce_su_t2025, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 2025, 0.72).
narrative_ontology:measurement(acce_su_t2035, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 2035, 0.72).
narrative_ontology:measurement(acce_su_t2050, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 2050, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_for_energy__comparative_risk_dominant, resource_allocation).
narrative_ontology:boltzmann_floor_override(acceptable_risk_for_energy__comparative_risk_dominant, 0.18).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__comparative_risk_dominant, acceptable_risk_for_energy__catastrophic_tail_dominant).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__comparative_risk_dominant, acceptable_risk_for_energy__expected_value_dominant).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__comparative_risk_dominant, temporal_urgency_dominates_intergenerational_burden).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel 'acceptable_risk_for_energy'. The constraint family comprises three competing readings of the same kernel: catastrophic_tail_dominant, comparative_risk_dominant (this story), and expected_value_dominant. Each reading instantiates a different beneficiary/victim structure, a different acceptable-risk metric, and a different classification outcome per seat. The three are linked via network.affects_constraints because they compete for institutional dominance and policy adoption; whichever reading dominates shapes the acceptable-risk framework globally. A secondary network link exists to 'temporal_urgency_dominates_intergenerational_burden', a separate kernel contestation about the relative weight of present-day urgency vs. intergenerational burden; the comparative_risk_dominant reading presupposes a specific resolution of that second kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(acceptable_risk_for_energy__comparative_risk_dominant, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
