% ============================================================================
% CONSTRAINT STORY: ai_alignment_priority__integrated_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_priority__integrated_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: ai_alignment_priority__integrated_reading
 *   human_readable: Integrated AI Alignment Priority Framework (Balanced Reading)
 *   domain: AI Governance / Technology Ethics / Risk Assessment
 *
 * SUMMARY:
 *   This constraint story models the integrated_reading of the
 *   ai_alignment_priority kernel, which holds that AI alignment must
 *   simultaneously address catastrophic (existential) risks and present
 *   (discriminatory/extractive) harms as complementary priorities rather than
 *   competing ones. The constraint is the standing governance arrangement
 *   that enforces this integration through funding requirements, peer-review
 *   norms, and policy frameworks. It claims to solve coordination failure
 *   between safety and fairness communities, but extracts methodological
 *   compliance and resource shifts from both researcher populations while
 *   tokenizing the interests of marginalized communities and future
 *   populations. The claim is tangled_rope: genuine coordination function
 *   against fragmentation, coupled with asymmetric extraction by institutions
 *   that capture legitimacy through comprehensive framing.
 *
 * KEY AGENTS:
 *   - alignment_institutions: Primary agenda-setter (institutional/arbitrage) â administers the integrated framework and captures legitimacy
 *   - catastrophic_risk_researchers: Primary payer (moderate/constrained) â forced to dilute specialization with present-harms compliance
 *   - present_harms_researchers: Primary payer (moderate/constrained) â forced to engage speculative long-term framing to access resources
 *   - marginalized_communities: Payer (powerless/trapped) â bear present harms with limited voice in integrated agenda
 *   - future_populations: Payer (powerless/trapped/non-agent) â invoked rhetorically without agency
 *   - critical_scholars: Observer (analytical/analytical) â evaluates whether integration is genuine or capture
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_priority__integrated_reading, 0.58).
domain_priors:suppression_score(ai_alignment_priority__integrated_reading, 0.52).
domain_priors:theater_ratio(ai_alignment_priority__integrated_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_priority__integrated_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_priority__integrated_reading, "Integrated AI Alignment Priority Framework (Balanced Reading)").
narrative_ontology:topic_domain(ai_alignment_priority__integrated_reading, "AI Governance / Technology Ethics / Risk Assessment").

domain_priors:requires_active_enforcement(ai_alignment_priority__integrated_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_priority__integrated_reading, 'e35b6c7d-cd19-41b1-b83c-206d8045fb2d').
narrative_ontology:cs_kernel_codification('e35b6c7d-cd19-41b1-b83c-206d8045fb2d', formalized).
narrative_ontology:cs_authority_grounding('e35b6c7d-cd19-41b1-b83c-206d8045fb2d', expertise).
narrative_ontology:cs_interpretation_layer_present('e35b6c7d-cd19-41b1-b83c-206d8045fb2d').
narrative_ontology:cs_reading_relation('e35b6c7d-cd19-41b1-b83c-206d8045fb2d', ai_alignment_priority__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('e35b6c7d-cd19-41b1-b83c-206d8045fb2d', ai_alignment_priority__nearterm_harms_reading, coexists_with).
narrative_ontology:cs_axiom('e35b6c7d-cd19-41b1-b83c-206d8045fb2d', foundational, catastrophic_present_complementarity).
narrative_ontology:cs_axiom_status(catastrophic_present_complementarity, holdable).
narrative_ontology:cs_axiom_grounding('e35b6c7d-cd19-41b1-b83c-206d8045fb2d', catastrophic_present_complementarity, instrumental).
narrative_ontology:cs_axiom('e35b6c7d-cd19-41b1-b83c-206d8045fb2d', secondary, dual_mandate_governance_legitimacy).
narrative_ontology:cs_axiom_status(dual_mandate_governance_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('e35b6c7d-cd19-41b1-b83c-206d8045fb2d', dual_mandate_governance_legitimacy, conventional).
narrative_ontology:cs_reference_frame('e35b6c7d-cd19-41b1-b83c-206d8045fb2d', comprehensive_risk_governance).
narrative_ontology:cs_drift_state('e35b6c7d-cd19-41b1-b83c-206d8045fb2d', contemporary_policy_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e35b6c7d-cd19-41b1-b83c-206d8045fb2d', '').
narrative_ontology:cs_kernel_id(ai_alignment_priority__integrated_reading, ai_alignment_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_priority__integrated_reading, alignment_institutions).
narrative_ontology:constraint_victim(ai_alignment_priority__integrated_reading, catastrophic_risk_researchers).
narrative_ontology:constraint_victim(ai_alignment_priority__integrated_reading, present_harms_researchers).
narrative_ontology:constraint_victim(ai_alignment_priority__integrated_reading, marginalized_communities).
narrative_ontology:constraint_victim(ai_alignment_priority__integrated_reading, future_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the integrated research agenda, control funding portfolios, and enforce dual-mandate requirements through grant conditions and peer-review criteria. They gain regulatory legitimacy and institutional standing by claiming comprehensive coverage of AI harms.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, alignment_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Must incorporate present-harms auditing and fairness metrics into catastrophic-safety research to qualify for funding and publication in integrated venues. Their specialized methodologies are treated as insufficient without complementary present-harms analysis.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, catastrophic_risk_researchers, payer,
    moderate, biographical, constrained, global).

% Must engage with catastrophic-risk frameworks and speculative long-term scenarios to access integrated funding streams and policy forums. Their grounded, community-centered work is reframed as one component of a broader safety portfolio.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, present_harms_researchers, payer,
    moderate, biographical, constrained, global).

% Bear the lived costs of present AI harms that the integrated framework claims to address. They have little direct voice in setting the integrated agenda and limited ability to exit the systems that harm them.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, marginalized_communities, payer,
    powerless, immediate, trapped, global).

% Are invoked as stakeholders in catastrophic-risk planning within the integrated framework, but have no present agency. Their interests are interpreted by present institutions, often in tension with present-harms demands on resources and attention.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, future_populations, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(ai_alignment_priority__integrated_reading, future_populations).

% Analyze whether integrated governance achieves its stated complementarity or functions as a capture mechanism. They document power asymmetries, funding dependencies, and methodological trade-offs that the integrated narrative obscures.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, critical_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_priority__integrated_reading, alignment_institutions).
narrative_ontology:fixing_cost_class(ai_alignment_priority__integrated_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents siloing between catastrophic-risk and present-harms research communities, ensuring that AI governance addresses the full spectrum of risks rather than fragmenting into non-communicating subfields that ignore each other's externalities.
% TRANSFER_FUNCTION: Moves research attention, funding, and personnel time from specialized single-focus programs toward dual-mandate projects; moves legitimacy and regulatory standing to institutions that can claim comprehensive coverage of both long-term and near-term risks.
% ABSENT_VOICES: Pure existential-risk advocates who view present-harms integration as dangerous distraction, and pure present-harms advocates who view catastrophic-risk framing as elite capture, are both partially sidelined in integrated fora; directly affected marginalized communities have limited voice in setting the integrated agenda.
% DISAPPEARANCE_RATIONALE: If the integrated priority framework vanished, funding streams would bifurcate back toward separate catastrophic-risk and present-harms silos, dual-mandate peer-review requirements would lapse, and the institutional leverage of comprehensive-governance bodies would diminish as specialized communities reasserted independent agendas.
% FOUNDING_PROBLEM: AI safety research was fragmented: catastrophic-risk work ignored distributive and discriminatory impacts, while present-harms work lacked leverage over powerful frontier-model developers, leaving governance incomplete and enabling mutual dismissal between communities.
% FOUNDING_PROBLEM_CORROBORATION: Independent science and technology studies scholars and some government technology assessment bodies attest to the fragmentation problem. Critical scholars from outside the benefiting institutions attest that the integrated solution has itself become a mechanism of legitimation and control; corroboration is split rather than unanimous.
narrative_ontology:disappearance_verdict(ai_alignment_priority__integrated_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_priority__integrated_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_priority__integrated_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_alignment_priority__integrated_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_priority__integrated_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_priority__integrated_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_alignment_priority__integrated_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_alignment_priority__integrated_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58) because the integrated framework redirects substantial research bandwidth and funding toward dual-mandate compliance without fully delivering on either agenda. Suppression is moderate-high (0.52) because career and funding access depend on accepting the integrated framing. Theater ratio (0.30) reflects growing performative integration: panels, audits, and red-teaming exercises that satisfy procedural requirements without shifting power. Accessibility collapse (0.45) indicates that pure specialized approaches remain conceptually possible but are increasingly unfundable and unpublishable within elite channels. Resistance (0.48) is moderate because both specialized communities chafe against the integration mandate, though they lack coordination to resist collectively. The measurement series share one time grid so temporal analysis is aligned.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (alignment institutions) experiences the constraint as a necessary coordination mechanism that prevents dangerous fragmentation. The payer seats (both researcher communities and affected populations) experience it as a forced marriage that dilutes their priorities and absorbs their resources. Future populations experience it only through proxy representation. The engine will compute these seats differently: low directionality for the institutions, high directionality for the trapped and constrained payers.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (alignment_institutions) derive low directionality because the constraint subsidizes their legitimacy and control. Victims (catastrophic_risk_researchers, present_harms_researchers, marginalized_communities, future_populations) derive high directionality because the constraint extracts compliance, attention, and resources from them. The asymmetric extraction is structural: the same framework that coordinates also concentrates agenda-setting power.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâfragmentation between catastrophic-risk and present-harms communitiesâwas genuine. However, the integrated mandate now persists beyond the point where it demonstrably solves that problem. Institutions continue to enforce dual-mandate compliance even when integration produces methodological incoherence or tokenism. This creates a mandatrophy risk: the constraint resists reclassification as pure snare because its coordination origin is real, but its persistence has become partly inertial and partly theatrical.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    integration_sincerity,
    'Is the integration of catastrophic and present-harms research a genuine methodological synthesis, or a legitimacy mechanism that allows funders to control both agendas without fully delivering on either?',
    'Comparative outcome analysis: measure whether integrated programs produce measurably better outcomes for both catastrophic-risk reduction and present-harms mitigation than specialized programs, controlling for funding levels.',
    'If performative, effective extraction is higher than measured and the constraint leans toward snare; if genuine, the coordination function is validated and the constraint remains tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(integration_sincerity, empirical, 'Whether integration is sincere coordination or legitimation capture').

omega_variable(
    future_populations_agency,
    'Does representing future populations as victims in this framework constitute genuine structural inclusion, or does their non-existence render their inclusion a rhetorical device that obscures present extraction?',
    'Discourse analysis of integrated governance documents to determine whether future-population claims are used to override present-harms demands or to legitimize resource concentration.',
    'If token, future_populations should be removed from the victim set and extraction concentrated on present payers; if genuine, the dual victim set holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_populations_agency, conceptual, 'Whether future populations are structurally included or rhetorically tokenized').

omega_variable(
    dual_methodology_synergy,
    'Do red-teaming and fairness auditing methodologies actually synergize in integrated frameworks, or do they impose compliance costs on both research communities without cross-fertilization?',
    'Empirical study of integrated research outputs: citation patterns, methodological innovation rates, and outcome metrics across integrated vs. specialized labs.',
    'If no synergy, theater_ratio is higher than measured and the coordination story weakens; if synergy, the coordination function is stronger than the metrics suggest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dual_methodology_synergy, empirical, 'Whether dual methodology produces real synergy or compliance theater').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_priority__integrated_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_priority__integrated_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ai_a_tr_t4, ai_alignment_priority__integrated_reading, theater_ratio, 4, 0.14).
narrative_ontology:measurement(ai_a_tr_t8, ai_alignment_priority__integrated_reading, theater_ratio, 8, 0.19).
narrative_ontology:measurement(ai_a_tr_t12, ai_alignment_priority__integrated_reading, theater_ratio, 12, 0.23).
narrative_ontology:measurement(ai_a_tr_t16, ai_alignment_priority__integrated_reading, theater_ratio, 16, 0.27).
narrative_ontology:measurement(ai_a_tr_t20, ai_alignment_priority__integrated_reading, theater_ratio, 20, 0.3).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_priority__integrated_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(ai_a_be_t4, ai_alignment_priority__integrated_reading, base_extractiveness, 4, 0.4).
narrative_ontology:measurement(ai_a_be_t8, ai_alignment_priority__integrated_reading, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(ai_a_be_t12, ai_alignment_priority__integrated_reading, base_extractiveness, 12, 0.51).
narrative_ontology:measurement(ai_a_be_t16, ai_alignment_priority__integrated_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(ai_a_be_t20, ai_alignment_priority__integrated_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t0, ai_alignment_priority__integrated_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(ai_a_su_t4, ai_alignment_priority__integrated_reading, suppression_requirement, 4, 0.36).
narrative_ontology:measurement(ai_a_su_t8, ai_alignment_priority__integrated_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(ai_a_su_t12, ai_alignment_priority__integrated_reading, suppression_requirement, 12, 0.47).
narrative_ontology:measurement(ai_a_su_t16, ai_alignment_priority__integrated_reading, suppression_requirement, 16, 0.5).
narrative_ontology:measurement(ai_a_su_t20, ai_alignment_priority__integrated_reading, suppression_requirement, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_priority__integrated_reading, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
