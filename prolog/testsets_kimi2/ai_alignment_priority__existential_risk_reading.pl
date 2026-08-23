% ============================================================================
% CONSTRAINT STORY: ai_alignment_priority__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_priority__existential_risk_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: ai_alignment_priority__existential_risk_reading
 *   human_readable: Existential Risk Priority Reading of AI Alignment
 *   domain: ai_governance_technology_ethics
 *
 * SUMMARY:
 *   This constraint is the existential_risk_reading of the contested
 *   ai_alignment_priority kernel. The kernel asks what 'alignment' means and
 *   which priorities it entails. This reading instantiates alignment as the
 *   prevention of catastrophic loss of control over advanced AI systems, with
 *   existential safety as the overriding priority. It structurally channels
 *   resources toward capability-focused research and adversarial red-teaming
 *   while treating humanity as an undifferentiated risk pool. Sibling
 *   readings (nearterm_harms_reading, integrated_reading) instantiate
 *   different constraints from the same kernel and are documented in network
 *   links and cs_structure reading_relations.
 *
 * KEY AGENTS:
 *   - capability_focused_labs: agenda-setter and beneficiary (institutional/arbitrage) â sets technical agendas and captures resource flows.
 *   - xrisk_research_community: beneficiary (organized/mobile) â receives funding and prestige under the framing.
 *   - all_of_humanity: payer (powerless/trapped) â undifferentiated victim pool bearing opportunity cost.
 *   - present_harm_bearers: payer (powerless/trapped) â marginalized communities whose present harms are deprioritized.
 *   - nearterm_harms_researchers: excluded (moderate/constrained) â pushed out of funding and policy priority.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_priority__existential_risk_reading, 0.78).
domain_priors:suppression_score(ai_alignment_priority__existential_risk_reading, 0.72).
domain_priors:theater_ratio(ai_alignment_priority__existential_risk_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_priority__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_priority__existential_risk_reading, "Existential Risk Priority Reading of AI Alignment").
narrative_ontology:topic_domain(ai_alignment_priority__existential_risk_reading, "ai_governance_technology_ethics").

domain_priors:requires_active_enforcement(ai_alignment_priority__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_priority__existential_risk_reading, '984f91f4-5139-4103-8a71-8ce45eba4945').
narrative_ontology:cs_kernel_codification('984f91f4-5139-4103-8a71-8ce45eba4945', distributed).
narrative_ontology:cs_authority_grounding('984f91f4-5139-4103-8a71-8ce45eba4945', expertise).
narrative_ontology:cs_interpretation_layer_present('984f91f4-5139-4103-8a71-8ce45eba4945').
narrative_ontology:cs_reading_relation('984f91f4-5139-4103-8a71-8ce45eba4945', ai_alignment_priority__nearterm_harms_reading, influences).
narrative_ontology:cs_reading_relation('984f91f4-5139-4103-8a71-8ce45eba4945', ai_alignment_priority__integrated_reading, influences).
narrative_ontology:cs_axiom('984f91f4-5139-4103-8a71-8ce45eba4945', foundational, existential_risk_priority).
narrative_ontology:cs_axiom_status(existential_risk_priority, holdable).
narrative_ontology:cs_axiom_grounding('984f91f4-5139-4103-8a71-8ce45eba4945', existential_risk_priority, empirically_contingent).
narrative_ontology:cs_axiom('984f91f4-5139-4103-8a71-8ce45eba4945', foundational, alignment_as_capability_control).
narrative_ontology:cs_axiom_status(alignment_as_capability_control, holdable).
narrative_ontology:cs_axiom_grounding('984f91f4-5139-4103-8a71-8ce45eba4945', alignment_as_capability_control, conventional).
narrative_ontology:cs_reference_frame('984f91f4-5139-4103-8a71-8ce45eba4945', existential_risk_prevention).
narrative_ontology:cs_drift_state('984f91f4-5139-4103-8a71-8ce45eba4945', contemporary_ai_race_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('984f91f4-5139-4103-8a71-8ce45eba4945', '').
narrative_ontology:cs_kernel_id(ai_alignment_priority__existential_risk_reading, ai_alignment_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, xrisk_research_community).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, capability_focused_labs).
narrative_ontology:constraint_victim(ai_alignment_priority__existential_risk_reading, all_of_humanity).
narrative_ontology:constraint_victim(ai_alignment_priority__existential_risk_reading, present_harm_bearers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and deploy frontier AI systems; set technical research agendas under the alignment-as-control framing; receive capital and talent inflows justified by existential safety narratives while scaling capabilities.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, capability_focused_labs, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__existential_risk_reading, capability_focused_labs, beneficiary).

% Receives funding, conference prestige, and policy access through the existential safety framing; conducts adversarial red-teaming and technical safety research that legitimizes continued capability scaling.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, xrisk_research_community, beneficiary,
    organized, generational, mobile, global).

% Treated as an undifferentiated risk pool facing potential extinction; bears the opportunity cost of governance attention and public resources diverted from present welfare to speculative capability research.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, all_of_humanity, payer,
    powerless, civilizational, trapped, global).

% Marginalized communities experiencing discriminatory and extractive impacts from currently deployed AI systems; their harms are deprioritized by the existential safety framing and its resource allocation.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, present_harm_bearers, payer,
    powerless, biographical, trapped, global).

% Research and advocate for mitigating present AI harms; structurally excluded from major funding streams and policy agendas captured by the existential risk narrative.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, nearterm_harms_researchers, excluded,
    moderate, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preventing catastrophic loss of control over advanced AI systems by aligning superhuman capabilities with human values through technical safety research and adversarial evaluation.
% TRANSFER_FUNCTION: Moves financial, human, and governance attention resources from present-harm mitigation and other priorities toward capability-focused existential safety research and adversarial evaluation regimes.
% ABSENT_VOICES: Near-term harms researchers, marginalized communities experiencing present AI harms, and governance actors focused on current discriminatory and extractive impacts are structurally deprioritized in funding and agenda-setting.
% DISAPPEARANCE_RATIONALE: If the existential safety priority vanished overnight, funding flows would reallocate toward present harms and sociotechnical governance, frontier labs would lose the safety-legitimacy mechanism underwriting capability scaling, and the technical research agenda would shift from adversarial red-teaming to impact mitigation.
% FOUNDING_PROBLEM: The potential development of artificial general intelligence or superintelligence with capabilities exceeding human control, creating unilateral extinction-level risk.
% FOUNDING_PROBLEM_CORROBORATION: Technical forecasts from within the existential risk research community and select AI labs attest to the problem's liveness; social scientists and near-term harms researchers contest the empirical basis and argue that no independent corroboration exists outside the benefiting parties.
narrative_ontology:disappearance_verdict(ai_alignment_priority__existential_risk_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_priority__existential_risk_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_priority__existential_risk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_alignment_priority__existential_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_priority__existential_risk_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_priority__existential_risk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_alignment_priority__existential_risk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_alignment_priority__existential_risk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the constraint moves substantial resources toward speculative capability research with distant, unverified safety payoffs. Suppression (0.72) reflects active enforcement through peer-review gatekeeping, funding allocation, and the delegitimization of near-term harms as 'not real alignment.' Theater ratio (0.45) captures the performative dimension of adversarial red-teaming that validates capability scaling under safety branding. Accessibility collapse (0.60) registers that alternative governance frames are marginalized but not fully erased. Resistance (0.55) reflects sustained pushback from the near-term harms community. The claim/metric independence is maintained: the reading self-presents as necessary coordination while the metrics describe substantially extractive, actively enforced operation.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats experience the constraint as necessary coordination to avert existential catastrophe; the payer seats experience it as resource extraction that deprioritizes their immediate interests and present welfare. The engine computes this divergence from the structural asymmetry in power, exit, and declared roles â the authored claim does not adjudicate the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Capability-focused labs and the existential risk research community are structural beneficiaries: they collect resources, set agendas, and enjoy low-exit mobility, placing them near the full-beneficiary end (low d). All_of_humanity and present_harm_bearers are structural victims: they bear the opportunity costs and deprioritization, are powerless, and are trapped in the risk pool or harm trajectory, placing them near the full-target end (high d). The undifferentiated victim framing is itself an extraction mechanism â it collapses differential power and locks all into the same high-d position despite unequal ability to influence the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling by requiring both a genuine coordination function (preventing loss of control over advanced systems) and asymmetric extraction (resource flow to capability research that outpaces safety) to be present for tangled_rope. If the coordination story were cover with no real safety function, the constraint would compute toward snare; if the extraction were negligible, it would compute toward rope. The authored metrics and structural declarations maintain that both are present: the safety function is real but the resource asymmetry is substantial.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_basis_of_existential_risk,
    'Is the extinction-level risk from advanced AI empirically grounded or does it rest on speculative forecasting?',
    'Systematic tracking of AI capability trajectories, incidence of autonomous survival-critical failures, and independent audits of risk assessments against observed behavior rather than extrapolated scaling laws.',
    'If the empirical basis is weak, the constraint''s extractiveness is predicated on a contested forecast and the coordination function may be serving a phantom problem; if strong, the extraction is the price of genuine catastrophe prevention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_basis_of_existential_risk, empirical, 'Whether the existential risk is empirically grounded or speculative.').

omega_variable(
    undifferentiated_victim_pool,
    'Does treating all of humanity as an undifferentiated risk pool erase differential vulnerability and legitimate extraction from marginalized groups?',
    'Disaggregation of AI impacts by population, showing whether present-harm burden correlates with the same groups deprioritized by the existential safety framing.',
    'If differential vulnerability is hidden, the constraint''s victim set is misidentified and effective extraction is higher for the most vulnerable than the scalar measure suggests; if truly undifferentiated, the pooling is structurally accurate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(undifferentiated_victim_pool, conceptual, 'Whether the undifferentiated victim framing obscures differential extraction.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (funding gatekeeping, peer-review exclusion) or internalized (field identity fusion that makes near-term harms research unthinkable)?',
    'Post-exit suppression trajectory: if suppression of near-term framing persists after funding incentives are removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression exceeds the structural measure because the field carries the suppression with it independent of explicit enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression in the research field.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_priority__existential_risk_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_priority__existential_risk_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ai_a_tr_t4, ai_alignment_priority__existential_risk_reading, theater_ratio, 4, 0.25).
narrative_ontology:measurement(ai_a_tr_t8, ai_alignment_priority__existential_risk_reading, theater_ratio, 8, 0.33).
narrative_ontology:measurement(ai_a_tr_t12, ai_alignment_priority__existential_risk_reading, theater_ratio, 12, 0.38).
narrative_ontology:measurement(ai_a_tr_t16, ai_alignment_priority__existential_risk_reading, theater_ratio, 16, 0.42).
narrative_ontology:measurement(ai_a_tr_t20, ai_alignment_priority__existential_risk_reading, theater_ratio, 20, 0.45).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_priority__existential_risk_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(ai_a_be_t4, ai_alignment_priority__existential_risk_reading, base_extractiveness, 4, 0.52).
narrative_ontology:measurement(ai_a_be_t8, ai_alignment_priority__existential_risk_reading, base_extractiveness, 8, 0.62).
narrative_ontology:measurement(ai_a_be_t12, ai_alignment_priority__existential_risk_reading, base_extractiveness, 12, 0.7).
narrative_ontology:measurement(ai_a_be_t16, ai_alignment_priority__existential_risk_reading, base_extractiveness, 16, 0.75).
narrative_ontology:measurement(ai_a_be_t20, ai_alignment_priority__existential_risk_reading, base_extractiveness, 20, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t0, ai_alignment_priority__existential_risk_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(ai_a_su_t4, ai_alignment_priority__existential_risk_reading, suppression_requirement, 4, 0.55).
narrative_ontology:measurement(ai_a_su_t8, ai_alignment_priority__existential_risk_reading, suppression_requirement, 8, 0.63).
narrative_ontology:measurement(ai_a_su_t12, ai_alignment_priority__existential_risk_reading, suppression_requirement, 12, 0.68).
narrative_ontology:measurement(ai_a_su_t16, ai_alignment_priority__existential_risk_reading, suppression_requirement, 16, 0.7).
narrative_ontology:measurement(ai_a_su_t20, ai_alignment_priority__existential_risk_reading, suppression_requirement, 20, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(ai_alignment_priority__existential_risk_reading, nearterm_harms_reading).
narrative_ontology:affects_constraint(ai_alignment_priority__existential_risk_reading, integrated_reading).

% DUAL FORMULATION NOTE:
% The ai_alignment_priority kernel decomposes into three structurally distinct constraints: existential_risk_reading (high epsilon, capability-focused extraction), nearterm_harms_reading (focused on present discriminatory/extractive harms), and integrated_reading (complementary framing). They are linked by shared vocabulary but have different epsilon values, beneficiary/victim structures, and resource flows.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
