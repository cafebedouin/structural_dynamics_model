% ============================================================================
% CONSTRAINT STORY: llm_synthesis_capacity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_llm_synthesis_capacity, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: llm_synthesis_capacity
 *   human_readable: LLM-Enabled Cross-Domain Knowledge Synthesis
 *   domain: epistemology/technology/institutional
 *
 * SUMMARY:
 *   Large language models remove the search and integration costs that
 *   previously made cross-domain knowledge synthesis prohibitively expensive
 *   for individuals. A literature review spanning immunology, endocrinology,
 *   and neuroscience that would have required months of manual traversal now
 *   takes days with LLM-assisted search and summarization. The constraint is
 *   claimed as rope: it solves a genuine coordination problem (assembling
 *   multi-domain expertise) with minimal coercive overhead, and participants
 *   are net beneficiaries. The modest extraction (0.18) reflects tool
 *   subscription costs and the risk of coherent-but-false synthesis;
 *   suppression (0.22) reflects the modest barriers of tool access and domain
 *   literacy requirements. Resistance (0.48) is substantial because
 *   institutional gatekeepers contest the legitimacy of synthesis-hypothesis
 *   frameworks that lack direct experimental proof, and some synthesizers
 *   resist the tool due to concerns about hallucination risk or epistemic
 *   shortcuts.
 *
 * KEY AGENTS:
 *   - individual_synthesizers: Primary beneficiaries (moderate/mobile) — gain synthesis capacity previously requiring institutional backing
 *   - interdisciplinary_researchers: Beneficiaries (powerful/mobile) — accelerate existing synthesis work
 *   - patient_advocates: Beneficiaries (moderate/mobile) — access mechanistic explanations for under-researched conditions
 *   - independent_scholars: Beneficiaries (moderate/mobile) — produce cross-domain work without institutional affiliation
 *   - institutional_gatekeepers: Observers (institutional/constrained) — maintain validation standards while facing synthesis proliferation
 *   - academic_institutions: Observers (institutional/constrained) — see synthesis capacity democratized beyond their pipelines
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(llm_synthesis_capacity, 0.18).
domain_priors:suppression_score(llm_synthesis_capacity, 0.22).
domain_priors:theater_ratio(llm_synthesis_capacity, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(llm_synthesis_capacity, extractiveness, 0.18).
narrative_ontology:constraint_metric(llm_synthesis_capacity, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(llm_synthesis_capacity, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(llm_synthesis_capacity, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(llm_synthesis_capacity, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(llm_synthesis_capacity, rope).
narrative_ontology:human_readable(llm_synthesis_capacity, "LLM-Enabled Cross-Domain Knowledge Synthesis").
narrative_ontology:topic_domain(llm_synthesis_capacity, "epistemology/technology/institutional").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(llm_synthesis_capacity, 'd22310c8-d7f6-4222-a162-d35e97cbe6fb').
narrative_ontology:cs_kernel_codification('d22310c8-d7f6-4222-a162-d35e97cbe6fb', distributed).
narrative_ontology:cs_authority_grounding('d22310c8-d7f6-4222-a162-d35e97cbe6fb', distributed).
narrative_ontology:cs_reading_relation('d22310c8-d7f6-4222-a162-d35e97cbe6fb', llm_synthesis_capacity__institutional_validation_reading, coexists_with).
narrative_ontology:cs_reading_relation('d22310c8-d7f6-4222-a162-d35e97cbe6fb', llm_synthesis_capacity__pragmatic_action_reading, coexists_with).
narrative_ontology:cs_axiom('d22310c8-d7f6-4222-a162-d35e97cbe6fb', foundational, mechanistic_plausibility_suffices).
narrative_ontology:cs_axiom_status(mechanistic_plausibility_suffices, holdable).
narrative_ontology:cs_axiom_grounding('d22310c8-d7f6-4222-a162-d35e97cbe6fb', mechanistic_plausibility_suffices, empirically_contingent).
narrative_ontology:cs_axiom('d22310c8-d7f6-4222-a162-d35e97cbe6fb', foundational, synthesis_without_direct_proof_legitimate).
narrative_ontology:cs_axiom_status(synthesis_without_direct_proof_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('d22310c8-d7f6-4222-a162-d35e97cbe6fb', synthesis_without_direct_proof_legitimate, instrumental).
narrative_ontology:cs_reference_frame('d22310c8-d7f6-4222-a162-d35e97cbe6fb', institutional_validation_monopoly).
narrative_ontology:cs_drift_state('d22310c8-d7f6-4222-a162-d35e97cbe6fb', post_llm_synthesis_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d22310c8-d7f6-4222-a162-d35e97cbe6fb', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(llm_synthesis_capacity, individual_synthesizers).
narrative_ontology:constraint_beneficiary(llm_synthesis_capacity, interdisciplinary_researchers).
narrative_ontology:constraint_beneficiary(llm_synthesis_capacity, patient_advocates).
narrative_ontology:constraint_beneficiary(llm_synthesis_capacity, independent_scholars).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Use LLM tools to traverse literature across domains that would previously require years of training or prohibitive search costs. Generate mechanistic hypotheses connecting validated findings from disparate fields. The constraint removes the coordination problem of assembling cross-domain expertise: what required a research team now requires domain literacy plus tool access.
narrative_ontology:constraint_stakeholder(llm_synthesis_capacity, individual_synthesizers, beneficiary,
    moderate, biographical, mobile, global).

% Accelerate literature review and hypothesis generation across their established domains. The tool compresses the search-and-integration phase from months to days, enabling faster iteration on mechanistic models. They retain the judgment filter and experimental design capacity; the constraint removes the bottleneck of manual literature traversal.
narrative_ontology:constraint_stakeholder(llm_synthesis_capacity, interdisciplinary_researchers, beneficiary,
    powerful, biographical, mobile, global).

% Access synthesized mechanistic explanations for conditions affecting them or their communities, without waiting for institutional research pipelines to prioritize their questions. The constraint enables them to generate testable hypotheses and evaluate intervention plausibility independently, shifting some epistemic authority from institutions to informed individuals.
narrative_ontology:constraint_stakeholder(llm_synthesis_capacity, patient_advocates, beneficiary,
    moderate, immediate, mobile, global).

% Produce cross-domain synthesis without institutional affiliation or funding. The constraint removes the access barrier: literature is increasingly open, LLM tools are commodity-priced, and synthesis quality depends on domain literacy and adversarial self-examination rather than credential or institutional backing.
narrative_ontology:constraint_stakeholder(llm_synthesis_capacity, independent_scholars, beneficiary,
    moderate, biographical, mobile, global).

% Observe the proliferation of synthesis-hypothesis frameworks produced outside traditional validation pathways. They maintain peer review and RCT standards as the legitimacy criteria, but face increasing pressure to evaluate mechanistically plausible syntheses that lack direct experimental proof. The constraint does not extract from them but challenges their monopoly on knowledge legitimacy.
narrative_ontology:constraint_stakeholder(llm_synthesis_capacity, institutional_gatekeepers, observer,
    institutional, generational, constrained, global).

% See synthesis capacity democratized beyond their credentialing and training pipelines. The constraint does not prevent them from producing knowledge, but it removes their structural advantage in cross-domain integration. Their response options include adapting evaluation criteria to assess synthesis quality or defending institutional validation as the sole legitimacy pathway.
narrative_ontology:constraint_stakeholder(llm_synthesis_capacity, academic_institutions, observer,
    institutional, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(llm_synthesis_capacity, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of cross-domain literature synthesis: assembling expertise from multiple fields, traversing citation networks, identifying mechanistic connections. Pre-LLM, this required either rare polymaths or expensive research teams; post-LLM, it requires domain literacy plus tool access.
% TRANSFER_FUNCTION: Moves synthesis capacity from institutions and credentialed experts to individuals with domain literacy and LLM access. No monetary transfer; the transfer is epistemic authority and hypothesis-generation capacity.
% ABSENT_VOICES: Populations without LLM access due to cost, language barriers, or infrastructure gaps would benefit from synthesis capacity but are excluded by the digital divide. They are not structurally opposed to the constraint but are not yet reached by it.
% DISAPPEARANCE_RATIONALE: If LLM synthesis tools vanished, cross-domain hypothesis generation would revert to the pre-2023 bottleneck: individuals would abandon synthesis projects mid-stream, interdisciplinary research would slow to pre-tool pace, and epistemic authority would re-concentrate in institutions with the resources to assemble multi-domain teams. The knowledge production landscape would reorganize around the prior coordination costs.
% FOUNDING_PROBLEM: Cross-domain literature synthesis was prohibitively expensive in time and cognitive load for individuals. Traversing citation networks, identifying relevant findings across terminological boundaries, and integrating mechanistic models from disparate fields required either rare polymathic training or institutional research teams. This bottleneck left many plausible cross-domain hypotheses unexplored.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by individual synthesizers, interdisciplinary researchers, and patient advocates who report the time-cost reduction directly. Independent scholars document the shift from inaccessible to feasible synthesis projects. No party contests that the coordination problem existed or that the tool addresses it; the contest is over whether synthesis without direct experimental proof constitutes legitimate knowledge.
narrative_ontology:disappearance_verdict(llm_synthesis_capacity, world_rearranges).
narrative_ontology:founding_problem_status(llm_synthesis_capacity, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(llm_synthesis_capacity, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-18',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'unspecified').
narrative_ontology:story_seed(llm_synthesis_capacity, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(llm_synthesis_capacity_tests).
:- end_tests(llm_synthesis_capacity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low (0.18) because the constraint's operation does not concentrate rents on any party: tool costs are commodity-priced, synthesis capacity accrues to users, and no gatekeeper collects from the arrangement. The modest extraction reflects subscription costs and the risk that some syntheses will be coherent but mechanistically wrong, imposing error costs on those who act on them. Suppression is low (0.22) because adoption is voluntary, exit is trivial (stop using the tool), and alternatives persist (manual literature review, institutional research teams). The modest suppression reflects access barriers (cost, language, infrastructure) and the domain literacy requirement. Theater is very low (0.12): the tool's function is direct and instrumental; there is minimal performative overlay. Accessibility collapse is moderate (0.35): alternatives exist but are substantially more expensive in time; once synthesis capacity is experienced, reverting to manual methods feels prohibitive for many users. Resistance is substantial (0.48): institutional gatekeepers resist the legitimacy of synthesis-hypothesis frameworks, some researchers resist due to hallucination concerns, and methodological purists resist the epistemic shortcut. The measurement series shows modest upward drift in extraction and suppression as tool costs stabilize and access barriers persist, with theater remaining flat.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats and observer seats should compute similarly: from the individual synthesizer's position, the constraint is pure coordination gain (removes bottleneck, enables new work); from the institutional observer's position, the constraint is also coordination (solves a real problem) but with contested legitimacy implications (synthesis without direct proof). The engine should compute rope or low-extraction rope from all seats. The gap is not in type but in legitimacy framing: institutions contest whether synthesis-hypothesis frameworks constitute knowledge, not whether the tool solves a coordination problem.
 *
 * DIRECTIONALITY LOGIC:
 *   All named beneficiaries are genuine beneficiaries: they gain synthesis capacity without bearing extraction. Individual synthesizers, interdisciplinary researchers, patient advocates, and independent scholars all sit near the beneficiary end of the directionality spectrum (d near 0.0-0.2). Institutional gatekeepers and academic institutions are observers rather than targets: the constraint does not extract from them, but it challenges their structural monopoly on cross-domain synthesis. Their directionality is near-symmetric (d near 0.4-0.5): they lose relative advantage but are not directly harmed. No party is a clear victim; the constraint's operation does not concentrate costs on any group.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is not mandatrophy: the founding problem (prohibitive synthesis costs) is live, the tool addresses it directly, and beneficiaries are clear. The legitimacy contest is over what counts as knowledge, not over whether the coordination function persists. If the tool were removed, the synthesis bottleneck would return immediately.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hallucination_error_rate,
    'What is the base rate of mechanistically plausible but factually incorrect syntheses produced by LLM-assisted frameworks, and how does adversarial self-examination by domain-literate users reduce that rate?',
    'Systematic comparison of LLM-generated synthesis claims against ground-truth experimental results in domains where both exist; measurement of error-detection rates by users with varying domain literacy levels.',
    'A high hallucination rate that persists despite user filtering would increase effective extraction (error costs borne by those who act on false syntheses) and support institutional gatekeepers'' resistance. A low rate or effective user filtering would support the rope classification and the synthesis-hypothesis reading''s legitimacy claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hallucination_error_rate, empirical, 'Whether LLM synthesis produces coherent but false narratives at rates that impose substantial error costs.').

omega_variable(
    synthesis_legitimacy_boundary,
    'Is a mechanistically plausible synthesis connecting validated components a legitimate knowledge claim, or does legitimacy require direct experimental proof of the synthesis itself?',
    'This is a conceptual question about epistemic standards, not resolvable by data alone. Resolution depends on which error type (false positive vs false negative) the evaluating community prioritizes, and on the opportunity cost of waiting for direct proof.',
    'If synthesis-hypothesis frameworks are accepted as legitimate, the constraint remains low-extraction rope. If institutional validation remains the sole legitimacy pathway, the constraint''s outputs are classified as speculative rather than knowledge, and adoption is limited to risk-tolerant individuals.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(synthesis_legitimacy_boundary, conceptual, 'Whether mechanistic plausibility constitutes sufficient warrant for knowledge claims absent direct experimental proof.').

omega_variable(
    access_barrier_persistence,
    'Will LLM tool costs, language barriers, and infrastructure gaps persist as structural access barriers, or will they erode to near-zero as the technology commoditizes?',
    'Observation of tool pricing trajectories, multilingual model availability, and infrastructure deployment over the next 5-10 years.',
    'Persistent barriers would increase suppression and create a new digital divide in synthesis capacity. Near-zero barriers would reduce suppression further and universalize the coordination benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(access_barrier_persistence, empirical, 'Whether access barriers will erode or persist as the constraint matures.').

omega_variable(
    institutional_adaptation_pathway,
    'Will institutional gatekeepers adapt evaluation criteria to assess synthesis quality (mechanistic plausibility, falsifiability, component evidence strength), or will they defend peer review and RCT validation as the sole legitimacy pathway?',
    'Observation of journal editorial policies, funding agency criteria, and academic hiring standards over the next decade. Track whether synthesis-hypothesis frameworks gain acceptance in high-impact venues or remain confined to preprint servers and independent platforms.',
    'Institutional adaptation would legitimize the synthesis-hypothesis reading and reduce resistance. Defensive entrenchment would maintain the legitimacy contest and keep synthesis frameworks in a parallel epistemic economy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_adaptation_pathway, preference, 'Whether institutions will adapt to synthesis proliferation or defend existing validation monopolies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(llm_synthesis_capacity, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(llm__tr_t0, llm_synthesis_capacity, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(llm__tr_t0, observed).
narrative_ontology:measurement(llm__tr_t2, llm_synthesis_capacity, theater_ratio, 2, 0.09).
narrative_ontology:measurement_basis(llm__tr_t2, observed).
narrative_ontology:measurement(llm__tr_t4, llm_synthesis_capacity, theater_ratio, 4, 0.1).
narrative_ontology:measurement_basis(llm__tr_t4, observed).
narrative_ontology:measurement(llm__tr_t6, llm_synthesis_capacity, theater_ratio, 6, 0.11).
narrative_ontology:measurement_basis(llm__tr_t6, observed).
narrative_ontology:measurement(llm__tr_t8, llm_synthesis_capacity, theater_ratio, 8, 0.12).
narrative_ontology:measurement_basis(llm__tr_t8, observed).
narrative_ontology:measurement(llm__tr_t10, llm_synthesis_capacity, theater_ratio, 10, 0.12).
narrative_ontology:measurement_basis(llm__tr_t10, projected).

% Extraction over time
narrative_ontology:measurement(llm__be_t0, llm_synthesis_capacity, base_extractiveness, 0, 0.05).
narrative_ontology:measurement_basis(llm__be_t0, observed).
narrative_ontology:measurement(llm__be_t2, llm_synthesis_capacity, base_extractiveness, 2, 0.08).
narrative_ontology:measurement_basis(llm__be_t2, observed).
narrative_ontology:measurement(llm__be_t4, llm_synthesis_capacity, base_extractiveness, 4, 0.12).
narrative_ontology:measurement_basis(llm__be_t4, observed).
narrative_ontology:measurement(llm__be_t6, llm_synthesis_capacity, base_extractiveness, 6, 0.15).
narrative_ontology:measurement_basis(llm__be_t6, observed).
narrative_ontology:measurement(llm__be_t8, llm_synthesis_capacity, base_extractiveness, 8, 0.17).
narrative_ontology:measurement_basis(llm__be_t8, observed).
narrative_ontology:measurement(llm__be_t10, llm_synthesis_capacity, base_extractiveness, 10, 0.18).
narrative_ontology:measurement_basis(llm__be_t10, projected).

% Suppression requirement over time
narrative_ontology:measurement(llm__su_t0, llm_synthesis_capacity, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(llm__su_t0, observed).
narrative_ontology:measurement(llm__su_t2, llm_synthesis_capacity, suppression_requirement, 2, 0.17).
narrative_ontology:measurement_basis(llm__su_t2, observed).
narrative_ontology:measurement(llm__su_t4, llm_synthesis_capacity, suppression_requirement, 4, 0.19).
narrative_ontology:measurement_basis(llm__su_t4, observed).
narrative_ontology:measurement(llm__su_t6, llm_synthesis_capacity, suppression_requirement, 6, 0.2).
narrative_ontology:measurement_basis(llm__su_t6, observed).
narrative_ontology:measurement(llm__su_t8, llm_synthesis_capacity, suppression_requirement, 8, 0.21).
narrative_ontology:measurement_basis(llm__su_t8, observed).
narrative_ontology:measurement(llm__su_t10, llm_synthesis_capacity, suppression_requirement, 10, 0.22).
narrative_ontology:measurement_basis(llm__su_t10, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(llm_synthesis_capacity, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
