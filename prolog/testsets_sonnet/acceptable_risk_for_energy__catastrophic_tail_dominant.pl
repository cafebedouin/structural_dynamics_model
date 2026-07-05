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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: acceptable_risk_for_energy__catastrophic_tail_dominant
 *   human_readable: Catastrophic-Tail-Dominant Acceptable Risk Standard for Nuclear Energy
 *   domain: risk_assessment/energy_policy/public_safety_governance
 *
 * SUMMARY:
 *   Regulatory and advocacy institutions in many jurisdictions evaluate
 *   nuclear energy licensing and waste disposal primarily through the lens of
 *   catastrophic, irreversible, multi-generational harm rather than
 *   probability-weighted expected cost. This reading elevates the tail event
 *   and the multi-millennial waste horizon to the controlling variable,
 *   effectively treating waste disposal as an open constraint that can never
 *   be fully discharged rather than an engineering target that can be met and
 *   closed. The framing is defensible as a genuine response to real
 *   irreversibility asymmetries, but it also structurally advantages
 *   incumbent fossil generation (held to no comparable irreversibility
 *   standard despite certain cumulative harm) and the institutions whose
 *   relevance depends on the precautionary frame persisting.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.58).
domain_priors:suppression_score(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.71).
domain_priors:theater_ratio(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, extractiveness, 0.58).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, resistance, 0.69).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_for_energy__catastrophic_tail_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_for_energy__catastrophic_tail_dominant, "Catastrophic-Tail-Dominant Acceptable Risk Standard for Nuclear Energy").
narrative_ontology:topic_domain(acceptable_risk_for_energy__catastrophic_tail_dominant, "risk_assessment/energy_policy/public_safety_governance").

domain_priors:requires_active_enforcement(acceptable_risk_for_energy__catastrophic_tail_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_for_energy__catastrophic_tail_dominant, 'a6caa297-c981-4ff4-8c4f-974c0c9a0346').
narrative_ontology:cs_kernel_codification('a6caa297-c981-4ff4-8c4f-974c0c9a0346', distributed).
narrative_ontology:cs_authority_grounding('a6caa297-c981-4ff4-8c4f-974c0c9a0346', distributed).
narrative_ontology:cs_reading_relation('a6caa297-c981-4ff4-8c4f-974c0c9a0346', acceptable_risk_for_energy__expected_value_dominant, coexists_with).
narrative_ontology:cs_reading_relation('a6caa297-c981-4ff4-8c4f-974c0c9a0346', acceptable_risk_for_energy__comparative_risk_dominant, influences).
narrative_ontology:cs_axiom('a6caa297-c981-4ff4-8c4f-974c0c9a0346', foundational, irreversibility_trumps_expected_value).
narrative_ontology:cs_axiom_status(irreversibility_trumps_expected_value, holdable).
narrative_ontology:cs_axiom_grounding('a6caa297-c981-4ff4-8c4f-974c0c9a0346', irreversibility_trumps_expected_value, deontological).
narrative_ontology:cs_axiom('a6caa297-c981-4ff4-8c4f-974c0c9a0346', foundational, intergenerational_burden_disqualifies_absent_consent).
narrative_ontology:cs_axiom_status(intergenerational_burden_disqualifies_absent_consent, holdable).
narrative_ontology:cs_axiom_grounding('a6caa297-c981-4ff4-8c4f-974c0c9a0346', intergenerational_burden_disqualifies_absent_consent, deontological).
narrative_ontology:cs_reference_frame('a6caa297-c981-4ff4-8c4f-974c0c9a0346', post_chernobyl_precautionary_consensus).
narrative_ontology:cs_drift_state('a6caa297-c981-4ff4-8c4f-974c0c9a0346', contemporary_climate_urgency_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('a6caa297-c981-4ff4-8c4f-974c0c9a0346', '').
narrative_ontology:cs_kernel_id(acceptable_risk_for_energy__catastrophic_tail_dominant, acceptable_risk_for_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__catastrophic_tail_dominant, incumbent_fossil_generators).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__catastrophic_tail_dominant, anti_nuclear_advocacy_organizations).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__catastrophic_tail_dominant, precautionary_regulatory_bodies).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__catastrophic_tail_dominant, nuclear_power_developers).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__catastrophic_tail_dominant, climate_mitigation_timeline).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__catastrophic_tail_dominant, future_energy_ratepayers).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__catastrophic_tail_dominant, waste_storage_host_communities).
narrative_ontology:constraint_vindicates(acceptable_risk_for_energy__catastrophic_tail_dominant, irreversibility_asymmetry_doctrine).
narrative_ontology:constraint_vindicates(acceptable_risk_for_energy__catastrophic_tail_dominant, intergenerational_precaution_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets licensing thresholds and siting rules that weight worst-case, low-probability accident scenarios and multi-millennial waste custody far more heavily than expected annual harm. Administers the standard, adjudicates exceptions, and is insulated from the downstream cost of foreclosed nuclear capacity — its institutional legitimacy is built on having prevented catastrophe, not on having enabled low-carbon generation.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, precautionary_regulatory_bodies, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Bear the cost of the tail-dominant framing directly: projects are delayed or cancelled by licensing regimes calibrated to worst-case irreversibility rather than probability-weighted harm, and long-term waste liability is treated as an open constraint rather than an engineering problem to be solved and closed. Cannot exit the jurisdiction's risk framework and remain in the business; can only lobby for reframing or relocate capital to jurisdictions applying comparative or expected-value standards.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, nuclear_power_developers, payer,
    organized, biographical, constrained, national).

% Benefit competitively when the tail-dominant standard suppresses nuclear entry and expansion, since fossil generation faces no comparable irreversibility-weighted licensing barrier despite continuous, cumulative harm. Free to expand capacity while the rival technology is held to a categorically different risk standard.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, incumbent_fossil_generators, beneficiary,
    powerful, biographical, mobile, national).

% Supply the intellectual and political infrastructure for the tail-dominant framing — funding studies, litigation, and public campaigns emphasizing irreversibility and intergenerational burden. Gain institutional relevance, funding, and policy influence from the framing's persistence; largely insulated from any cost of foregone decarbonization since their mandate is defined by risk prevention, not energy delivery.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, anti_nuclear_advocacy_organizations, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_for_energy__catastrophic_tail_dominant, anti_nuclear_advocacy_organizations, agenda_setter).

% Inherit whatever energy mix results from today's licensing calculus — higher electricity costs and slower decarbonization if nuclear capacity is suppressed in favor of continued fossil generation or expensive intermittent-renewable overbuild. Have no voice in the standard's construction and no capacity to retroactively unwind delayed capacity decisions.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, future_energy_ratepayers, payer,
    powerless, generational, trapped, national).

% Not an actor but a constraint on outcomes: aggressive decarbonization deadlines are structurally harder to meet when a low-carbon, dispatchable technology is held to a standard that treats any nonzero catastrophic-tail probability as disqualifying, regardless of the certain, ongoing harm from the fossil alternative it displaces.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, climate_mitigation_timeline, payer,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(acceptable_risk_for_energy__catastrophic_tail_dominant, climate_mitigation_timeline).

% Live with the consequence of waste disposal being treated as an unresolved constraint rather than a solvable engineering problem — permanent repository siting stalls indefinitely under a standard that cannot certify any timeline as sufficiently safe, leaving interim storage risk concentrated locally for decades beyond original design life.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, waste_storage_host_communities, payer,
    powerless, civilizational, trapped, regional).

% Would argue for probability-weighted comparative risk assessment across the full energy portfolio, including the certain mortality and climate cost of fossil alternatives, but their framing is structurally excluded from the licensing conversation once irreversibility is established as the dominant axis — the tail-dominant standard forecloses the terms on which they could even make their case within the regulatory proceeding.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, expected_value_energy_planners, excluded,
    moderate, generational, constrained, national).

% Study the divergence between probability-weighted and irreversibility-weighted risk frameworks across domains (nuclear, biotech, AI, climate geoengineering) without a stake in any single jurisdiction's licensing outcome.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, risk_theory_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_for_energy__catastrophic_tail_dominant, diffuse).
narrative_ontology:fixing_cost_class(acceptable_risk_for_energy__catastrophic_tail_dominant, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, legible standard for evaluating catastrophic, irreversible, and intergenerational harms that ordinary expected-value cost-benefit analysis systematically underweights — coordinating public trust that worst-case scenarios have been taken seriously before licensing proceeds.
% TRANSFER_FUNCTION: Moves decarbonization capacity and least-cost energy delivery away from nuclear developers and future ratepayers toward incumbent fossil generation and the institutional standing of precautionary regulatory and advocacy bodies, while concentrating unresolved waste-custody risk on host communities.
% ABSENT_VOICES: Expected-value energy planners and comparative-risk analysts who would weigh nuclear's tail risk against fossil generation's certain, cumulative harm are structurally excluded from the licensing frame once irreversibility is established as the controlling axis; future ratepayers and waste-host communities who bear the downstream cost have no seat in the standard-setting process at all.
% DISAPPEARANCE_RATIONALE: If the catastrophic-tail-dominant standard were replaced overnight by expected-value or comparative-risk weighting, nuclear licensing timelines would compress, waste disposal would be re-treated as a closable engineering program with target dates, and the competitive position of fossil generation relative to nuclear would shift substantially — the current energy investment and regulatory landscape depends on this specific weighting persisting.
% FOUNDING_PROBLEM: Historical catastrophic nuclear accidents (Chernobyl, Fukushima) and the multi-millennial hazard horizon of high-level waste demonstrated that standard actuarial expected-value analysis fails to capture harms that are irreversible, unbounded in geographic and temporal scope, or that fall on generations with no voice in the originating decision.
% FOUNDING_PROBLEM_CORROBORATION: Precautionary regulators and anti-nuclear advocacy organizations attest the founding problem remains fully live, citing waste half-lives and accident tail risk. Independent risk-theory analysts and comparative-mortality epidemiologists outside the beneficiary set (studying deaths-per-terawatt-hour across energy sources) attest that the founding problem, as originally framed, is substantially overstated relative to the certain and larger-magnitude harms of the fossil generation the standard indirectly preserves — corroboration for the 'live' status comes almost entirely from parties whose institutional mandate depends on the tail-dominant frame remaining unresolved.
narrative_ontology:disappearance_verdict(acceptable_risk_for_energy__catastrophic_tail_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_for_energy__catastrophic_tail_dominant, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_for_energy__catastrophic_tail_dominant, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.58) and suppression (0.71) are both substantial but suppression dominates: the constraint's persistence depends less on the magnitude of transferred value and more on foreclosing the comparative and expected-value framings from even entering the licensing conversation. Theater ratio (0.42) reflects that a meaningful share of precautionary review activity has drifted toward performing thoroughness on tail scenarios already well-characterized by decades of engineering study, rather than closing the underlying waste-engineering problem. Accessibility collapse (0.62) is high because once irreversibility is established as the controlling axis, alternative framings become very difficult to reintroduce into the same proceeding. Resistance (0.69) is high because nuclear developers, some energy economists, and comparative-mortality researchers actively contest the framing.
 *
 * PERSPECTIVAL GAP:
 *   From the precautionary regulator's seat, the standard is prudent stewardship of an irreversible risk that no expected-value calculation can properly price. From the nuclear developer's seat, the identical standard is a foreclosure mechanism that treats any nonzero catastrophic probability as disqualifying while giving fossil generation's certain, cumulative harm no equivalent scrutiny. The engine computes these as structurally different seat outcomes from the same authored data, not as a dispute to be adjudicated by the story.
 *
 * DIRECTIONALITY LOGIC:
 *   Precautionary regulatory bodies and anti-nuclear advocacy organizations sit near the beneficiary end: their institutional standing is constituted by having prevented catastrophe, and the tail-dominant frame is the source of that standing. Incumbent fossil generators benefit indirectly and substantially by facing no comparable irreversibility-weighted barrier. Nuclear developers, future ratepayers, and waste-host communities sit near the target end: they bear delayed capacity, higher costs, and concentrated local risk respectively, with limited or no exit from the jurisdictional risk framework. Future ratepayers and waste-host communities are additionally trapped by time horizon — the costs land on people not present when the standard was set.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (real catastrophic accidents, real multi-millennial waste hazard) was genuine and remains partially live, which is why this is authored as tangled_rope rather than snare: there is a real coordination function (public trust that worst-case harms were taken seriously) alongside asymmetric extraction (fossil incumbents and precautionary institutions benefit from a frame that a growing body of comparative-risk evidence suggests overweights nuclear's tail relative to fossil generation's certain harm). Classifying this as pure extraction would erase the legitimate irreversibility concern that motivated the standard; classifying it as pure coordination would erase the demonstrable competitive and institutional benefits accruing to specific parties from the frame's persistence. The tangled_rope classification holds both facts simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    irreversibility_weighting_calibration,
    'Is the degree to which this reading weights irreversibility and intergenerational burden proportionate to the actual physical asymmetry between nuclear tail risk and fossil cumulative harm, or does it systematically overweight nuclear''s tail relative to fossil generation''s certain, ongoing damage?',
    'Comparative deaths-per-terawatt-hour and land-use/waste-volume studies across energy sources, cross-checked against the marginal decarbonization value foregone by nuclear capacity suppressed under this standard versus the marginal harm avoided by tail-risk prevention.',
    'If overweighted, the tangled_rope classification is confirmed with a larger extraction component than currently authored; if proportionate, the constraint moves closer to a genuine rope with the extraction component being an artifact of incumbent competitive dynamics rather than the risk standard itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(irreversibility_weighting_calibration, empirical, 'Whether tail-dominant weighting is proportionate to physical risk asymmetry or systematically favors incumbents.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Which of the three readings of the acceptable_risk_for_energy kernel (catastrophic_tail_dominant, comparative_risk_dominant, expected_value_dominant) should govern a given jurisdiction''s licensing regime, and is that selection itself defensible on procedural grounds or is it an artifact of which advocacy coalition captured the standard-setting process first?',
    'Cross-jurisdictional comparison of licensing outcomes, decarbonization pace, and waste-program closure rates under jurisdictions that have adopted each reading, controlling for underlying political economy.',
    'If the reading selection tracks capture rather than principled risk theory, this constraint''s classification shifts further toward tangled_rope/snare; if it tracks genuine, defensible differences in risk tolerance across democratic polities, the constraint is better read as a legitimate policy choice among coexisting frameworks.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether the choice of risk-weighting reading is procedurally principled or an artifact of coalition capture.').

omega_variable(
    waste_disposal_engineering_closability,
    'Is high-level nuclear waste disposal genuinely an open-ended constraint that cannot be discharged with acceptable confidence, or is it a solvable engineering and institutional-trust problem that the tail-dominant frame has prevented from being treated as closable?',
    'Track record of operating deep geological repositories (e.g., Onkalo, WIPP) against original safety-case timelines and independent third-party technical review outside both the nuclear industry and anti-nuclear advocacy organizations.',
    'If closable, the ''waste disposal becomes a constraint rather than an engineering problem'' framing is itself evidence of extraction (indefinite deferral serving institutional interests); if genuinely open-ended, the precautionary treatment is structurally justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(waste_disposal_engineering_closability, empirical, 'Whether waste disposal is a genuinely open constraint or an engineering problem kept artificially open.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_for_energy__catastrophic_tail_dominant, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t0, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 0, 0.22).
narrative_ontology:measurement(acce_tr_t8, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 8, 0.28).
narrative_ontology:measurement(acce_tr_t16, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 16, 0.33).
narrative_ontology:measurement(acce_tr_t24, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 24, 0.37).
narrative_ontology:measurement(acce_tr_t32, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 32, 0.4).
narrative_ontology:measurement(acce_tr_t40, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(acce_be_t0, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(acce_be_t8, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(acce_be_t16, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 16, 0.49).
narrative_ontology:measurement(acce_be_t24, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 24, 0.53).
narrative_ontology:measurement(acce_be_t32, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 32, 0.56).
narrative_ontology:measurement(acce_be_t40, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t0, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(acce_su_t8, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(acce_su_t16, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 16, 0.64).
narrative_ontology:measurement(acce_su_t24, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 24, 0.67).
narrative_ontology:measurement(acce_su_t32, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 32, 0.69).
narrative_ontology:measurement(acce_su_t40, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_for_energy__catastrophic_tail_dominant, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.12).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_dominant, acceptable_risk_for_energy__expected_value_dominant).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_dominant, acceptable_risk_for_energy__comparative_risk_dominant).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language 'acceptable risk for nuclear energy' question, each instantiating a distinct reading of the acceptable_risk_for_energy kernel with its own epsilon, beneficiary/victim structure, and classification: catastrophic_tail_dominant (this story, tangled_rope — irreversibility-weighted, favors incumbent fossil and precautionary institutions), comparative_risk_dominant (nuclear judged only against competing energy risks, no absolute threshold), and expected_value_dominant (probability x consequence products govern acceptability). The three do not average into one constraint; per DP-001 (epsilon-invariance) they are linked via network edges rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(acceptable_risk_for_energy__catastrophic_tail_dominant, organized, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
