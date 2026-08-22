% ============================================================================
% CONSTRAINT STORY: ai_risk_governance_priority__bridge_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_risk_governance_priority__bridge_reading, []).

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
 *   constraint_id: ai_risk_governance_priority__bridge_reading
 *   human_readable: AI Risk Governance Integration Mandate (Bridge Reading)
 *   domain: technology governance / research policy / ethics
 *
 * SUMMARY:
 *   A governance norm requires AI risk governance to address present harms
 *   and long-horizon catastrophic risks jointly, as structurally entangled
 *   concerns handled through unified frameworks rather than prioritized
 *   separately. The norm solves a real fragmentation problem — two research
 *   communities giving policy processes contradictory advice and dropping
 *   intersection risks between their mandates — and it simultaneously
 *   concentrates the machinery of integration in a small set of broker
 *   institutions: the handful of interdisciplinary institutes, dedicated
 *   venues, and funder relationships through which cross-field work, funding,
 *   and agenda-setting pass. Field-structure analysis finds a small share of
 *   papers accounting for the large majority of cross-field links, and the
 *   resource flow to integrated safety-ethics research runs through those
 *   brokers rather than through distributed collaboration, making the
 *   arrangement's resource flow structurally fragile. Epsilon's referent is
 *   the standing arrangement — the operative integration mandate as it
 *   structures funding gates, venue access, and agenda control — assessed by
 *   this reading's own lights, which affirm the coordination function as real
 *   while registering the broker-mediated costs on both victim constituencies
 *   as real.
 *
 * KEY AGENTS:
 *   - bridging_broker_institutions: primary beneficiary and agenda administrator (institutional/identity_locked) — runs the integration venues, defines what counts as integrated work, collects the cross-field rents
 *   - integration_focused_funders: secondary beneficiary (institutional/mobile) — gains niche, portfolio coherence, and standing from the frame
 *   - present_marginalized_populations: primary target, present-harms dimension (powerless/trapped) — claims reach governance only as mediated through frameworks they do not sit in
 *   - future_humanity_constituency: primary target, long-horizon dimension (powerless/trapped) — protection diluted by near-term relevance tests, no members able to object
 *   - near_term_harms_researchers: taxed intermediary (moderate/constrained) — bears compliance costs, receives conditional subsidy
 *   - existential_risk_researchers: taxed intermediary (moderate/constrained) — bears compliance costs, receives conditional subsidy
 *   - affected_community_advocates: excluded voice (powerless/trapped) — would object to being represented without seats
 *   - independent_integration_scholars: excluded competitor (moderate/constrained) — crowded out of the brokered integration landscape
 *   - meta_research_analysts: analytical observer — maps the field structure from outside any camp
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_governance_priority__bridge_reading, 0.55).
domain_priors:suppression_score(ai_risk_governance_priority__bridge_reading, 0.45).
domain_priors:theater_ratio(ai_risk_governance_priority__bridge_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_governance_priority__bridge_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_governance_priority__bridge_reading, "AI Risk Governance Integration Mandate (Bridge Reading)").
narrative_ontology:topic_domain(ai_risk_governance_priority__bridge_reading, "technology governance / research policy / ethics").

domain_priors:requires_active_enforcement(ai_risk_governance_priority__bridge_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_governance_priority__bridge_reading, '9f0e985c-8b59-4f19-8860-5de814f03a2e').
narrative_ontology:cs_kernel_codification('9f0e985c-8b59-4f19-8860-5de814f03a2e', distributed).
narrative_ontology:cs_authority_grounding('9f0e985c-8b59-4f19-8860-5de814f03a2e', practice).
narrative_ontology:cs_interpretation_layer_present('9f0e985c-8b59-4f19-8860-5de814f03a2e').
narrative_ontology:cs_reading_relation('9f0e985c-8b59-4f19-8860-5de814f03a2e', ai_risk_governance_priority__existential_risk_reading, influences).
narrative_ontology:cs_reading_relation('9f0e985c-8b59-4f19-8860-5de814f03a2e', ai_risk_governance_priority__near_term_harms_reading, influences).
narrative_ontology:cs_axiom('9f0e985c-8b59-4f19-8860-5de814f03a2e', foundational, risk_entanglement_thesis).
narrative_ontology:cs_axiom_status(risk_entanglement_thesis, holdable).
narrative_ontology:cs_axiom_grounding('9f0e985c-8b59-4f19-8860-5de814f03a2e', risk_entanglement_thesis, empirically_contingent).
narrative_ontology:cs_axiom('9f0e985c-8b59-4f19-8860-5de814f03a2e', foundational, unified_frameworks_governance_superiority).
narrative_ontology:cs_axiom_status(unified_frameworks_governance_superiority, holdable).
narrative_ontology:cs_axiom_grounding('9f0e985c-8b59-4f19-8860-5de814f03a2e', unified_frameworks_governance_superiority, instrumental).
narrative_ontology:cs_reference_frame('9f0e985c-8b59-4f19-8860-5de814f03a2e', entangled_risk_integrated_governance).
narrative_ontology:cs_drift_state('9f0e985c-8b59-4f19-8860-5de814f03a2e', contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9f0e985c-8b59-4f19-8860-5de814f03a2e', '').
narrative_ontology:cs_kernel_id(ai_risk_governance_priority__bridge_reading, ai_risk_governance_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__bridge_reading, bridging_broker_institutions).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__bridge_reading, integration_focused_funders).
narrative_ontology:constraint_victim(ai_risk_governance_priority__bridge_reading, present_marginalized_populations).
narrative_ontology:constraint_victim(ai_risk_governance_priority__bridge_reading, future_humanity_constituency).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__bridge_reading, near_term_harms_researchers).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__bridge_reading, existential_risk_researchers).
narrative_ontology:constraint_victim(ai_risk_governance_priority__bridge_reading, near_term_harms_researchers).
narrative_ontology:constraint_victim(ai_risk_governance_priority__bridge_reading, existential_risk_researchers).
narrative_ontology:constraint_vindicates(ai_risk_governance_priority__bridge_reading, risk_entanglement_thesis).
narrative_ontology:constraint_vindicates(ai_risk_governance_priority__bridge_reading, unified_framework_efficacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Run the interdisciplinary institutes, dedicated venues, and funder relationships through which cross-field AI risk work passes. They translate between the catastrophic-risk and present-harms research communities, define what counts as properly integrated research, and hold the program committees and advisory boards where integrated agendas are set. Their staffing, endowments, and reputations are built around the bridging role — if the integration frame lost standing, these organizations would lose their reason to exist. A small number of such actors account for most of the cross-field links in the literature.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, bridging_broker_institutions, beneficiary,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__bridge_reading, bridging_broker_institutions, agenda_setter).

% Philanthropic and public funders whose grant strategies are organized around unifying safety and ethics portfolios. The integration frame gives them a distinctive niche, coherent strategy documents, and a claim to field-shaping influence. They could redirect their portfolios if the frame lost standing, but their current programming, staff, and public identity are committed to it.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, integration_focused_funders, beneficiary,
    institutional, generational, mobile, global).

% Communities bearing today's AI harms — automated welfare and benefits decisions, biased screening, content-moderation labor conditions, pervasive monitoring. Their claims reach governance mainly as reframed by integrated frameworks, where present harm appears as one entangled dimension of a larger risk portfolio; attention and resources directed at their situation pass through brokered agendas they do not sit in. They cannot exit the systems that harm them.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, present_marginalized_populations, payer,
    powerless, immediate, trapped, global).

% The constituency whose protection from catastrophic outcomes depends on long-horizon safety work. Under the integration mandate, long-horizon research must demonstrate near-term relevance to clear funding and publication gates, and catastrophic scenarios are weighed as one concern among several in unified assessments. No member of this constituency yet exists to object; they are represented by proxy advocates whose standing the brokered venues partially control.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, future_humanity_constituency, payer,
    powerless, civilizational, trapped, global).

% Researchers working on documented present harms — fairness, accountability, labor and surveillance effects. To clear integrated funding calls and cross-listed venues they must demonstrate relevance to catastrophic-risk concerns, recasting bias and displacement work as evidence about systemic risk. Compliance routes integrated grant money and citation flow back to them, but the terms are set by brokered venues they do not control. Moving to adjacent disciplines would forfeit their accumulated expertise.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, near_term_harms_researchers, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__bridge_reading, near_term_harms_researchers, beneficiary).

% Researchers working on long-horizon catastrophic scenarios — alignment, systemic risk, loss-of-control analyses. Under the integration mandate they must show concrete present-day relevance to qualify for the integrated funding stream and to avoid being characterized as speculative. Compliance opens integrated resources; refusal confines them to a narrower funding base. Their agendas are long by nature, which makes the near-term relevance test costliest for them.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, existential_risk_researchers, payer,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__bridge_reading, existential_risk_researchers, beneficiary).

% Grassroots organizers from communities affected by deployed AI systems. They hold no seats in the institutes, funder panels, or program committees where integrated frameworks are drafted; the frameworks describe their situation in their absence. They would argue that integration language absorbs their concrete demands into abstract risk portfolios without returning decision power or remediation.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, affected_community_advocates, excluded,
    powerless, immediate, trapped, national).

% Scholars outside the broker network who do genuine cross-field work on AI risk. The integrated venues, citation networks, and funding channels are organized around the incumbent brokers, so independent integration work struggles for visibility and support — the bridging role is already occupied. They would benefit from an open integration landscape and are crowded out of the one that exists.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, independent_integration_scholars, excluded,
    moderate, biographical, constrained, global).

% Meta-science and field-structure researchers who map collaboration networks, funding flows, and citation patterns across the AI risk field. Their analyses produced the concentration findings — a small share of papers accounting for most cross-field links — and they track whether integration is becoming distributed or remaining broker-dependent. They hold no stake in which framework wins.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, meta_research_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_risk_governance_priority__bridge_reading, bridging_broker_institutions).
narrative_ontology:fixing_cost_class(ai_risk_governance_priority__bridge_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real collective-action problem in the AI risk field: catastrophic-risk and present-harms research communities grew up separately, duplicate work, bid against each other for funding and attention, and give policy processes contradictory advice. A class of risks — deployment race dynamics, systemic and dual-use failures that produce both present harm and catastrophic tail risk — falls between the two camps' mandates. The integration norm coordinates research agendas, funding portfolios, and policy input so these entangled cases are handled by one framework rather than dropped between two.
% TRANSFER_FUNCTION: Moves grant share, citation flow, venue access, and agenda-setting authority from single-focus programs in both research camps toward integrated safety-ethics programs, with a small set of broker institutions mediating the flow; and moves the claims of present-harm-affected populations and long-horizon risk constituencies into a single bargaining space where each is weighed against the other under brokered terms.
% ABSENT_VOICES: Affected community advocates are not seated in the institutes, funder panels, or program committees where integrated frameworks are drafted — the frameworks describe their situation in their absence. Independent scholars doing integration work outside the broker network are crowded out of the venues their work would populate. The long-horizon constituency has no members able to speak at all and is represented by proxy advocates whose standing the brokered venues partially control.
% DISAPPEARANCE_RATIONALE: If the integration mandate vanished overnight, funding would re-sort along the two camps' existing lines, the broker institutions would collapse or rebrand around one camp, entangled risk cases would fall back between separate mandates, and the policy processes currently receiving unified advice would again receive contradictory input from two uncoordinated communities.
% FOUNDING_PROBLEM: By the late 2010s the AI risk field had split into a catastrophic-risk community and an AI-society/harms community with separate venues, funders, and vocabularies. Policy bodies were receiving contradictory risk assessments; funding was duplicating in some areas and leaving the intersection uncovered; risks produced jointly by deployment scale-up and design choices were falling between the two mandates. The integration mandate was built to end that fragmentation.
% FOUNDING_PROBLEM_CORROBORATION: Meta-science field-structure studies document the fragmentation and the broker concentration from outside any camp; government advisory processes on AI have publicly reported receiving inconsistent risk input; and researchers in both single-focus camps — none of whom collect from the integration stream — attest that the split is real and costly. The founding problem is not attested only by the arrangement's beneficiaries.
narrative_ontology:disappearance_verdict(ai_risk_governance_priority__bridge_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_governance_priority__bridge_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_governance_priority__bridge_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_risk_governance_priority__bridge_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_governance_priority__bridge_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_governance_priority__bridge_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_risk_governance_priority__bridge_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_risk_governance_priority__bridge_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.55) because the arrangement both funds genuinely integrative work and routes the taxed flow through a broker choke point: the compliance terms for both research camps and the mediated standing of both constituencies are set by a handful of institutions that collect the cross-field rents. Suppression (0.45) is structural rather than coercive — funding criteria, review norms, and venue gatekeeping rather than prohibition; single-focus work remains publishable, it is merely disadvantaged, which is why accessibility_collapse is low (0.35): the alternatives, including the two single-priority framings, remain fully workable. Theater_ratio (0.42) reflects the gap between both/and integration rhetoric — workshops, framework documents, holistic language — and integration that changes research practice; the series shows rhetoric proliferating faster than practice. Resistance (0.58) is real and bidirectional: long-horizon researchers resist dilution of urgency, present-harms researchers resist deferral of justice, and affected advocates resist representation without seats. All three measurement series run on one shared grid (t=0 to t=14 in steps of 2; t=0 corresponds to roughly 2019, before the integration frame was funded; t=14 to the present), so every metric is authored at every examined point. The suppression_requirement series is authored because this story specifically tracks enforcement-capacity build-up: the norm matured from aspirational statement to funded gate, plateauing as the machinery consolidated. Suppression here is a raw structural property; only extractiveness is scaled downstream by directionality and scope. Note that the two victim constituencies cannot coalition: one is dispersed and resource-poor, the other does not yet exist.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats and the payer seats should compute differently. From the brokers' position the arrangement is the field finally growing up — they built the only working bridge between two communities that were dropping entangled risks between mandates, and they see their gatekeeping as quality control. From the taxed researchers' position the same machinery is a gate that charges admission in the currency of the other camp's concerns. From the victims' positions it is mediation: their claims enter governance only as reframed by institutions they do not sit in. The funders see portfolio coherence; the excluded scholars see an occupied bridge. Same structure, four experiences — the engine computes the divergence from the structural data, and this story's claimed type does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The brokers sit at the beneficiary end (they collect the cross-field rents and run the gates; identity-locked because the integration frame is their institutional reason to exist). The funders sit near them (beneficiaries with portfolio mobility). The declared victims sit near the target end: future_humanity_constituency is a pure target — the integration frame dilutes long-horizon protection and returns nothing to a constituency that cannot answer back; present_marginalized_populations are targets with a partial secondary subsidy, since integrated funding does direct some resources at present harms, which places their true d slightly inside the pure-target end. No override is authored for the powerless atom because the two victim seats genuinely differ and one atom-level dial would conflate them. The moderate-power seats — the two researcher groups and the independent scholars — carry no beneficiary/victim declaration (the declared victim set is the two populations), so the derivation chain would fall back to the power-atom default for them; the override records their net-payer position (compliance costs exceed conditional subsidy, d approximately 0.62). This slightly understates the independent scholars, who receive no subsidy at all — the residual imprecision of the atom-keyed dial, flagged here rather than papered over.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — camp fragmentation dropping entangled risks between mandates — is live, so this is not yet a mandatrophy case, and the classification work is to keep the arrangement from being mislabeled in either direction. Reading it as pure coordination would erase the broker capture that the field-structure data documents; reading it as pure extraction would erase the coordination function that both camps implicitly rely on whenever they publish across the divide. The specific mandatrophy risk is forward-looking: if distributed cross-field collaboration matures (the broker_dependence_fragility omega), the broker-mediated mandate would outlive its function while the brokers persist as toll-keepers on traffic that no longer needs them. The R5 record is consistent with the current reading — founding_problem_status=live with disappearance_verdict=world_rearranges, so no dead-mandate flag fires; the measurement series tracks whether the extraction component grows (consolidation) or fades (transition).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint instantiates the bridge reading of the ai_risk_governance_priority kernel; the sibling readings (existential-risk-first and present-harms-first) would instantiate structurally different constraints — different victim sets, different beneficiaries, different epsilon. Is the bridge reading''s dual-victim, broker-beneficiary structure the right decomposition of the standing arrangement, or does the priority contest change which arrangement is actually operative?',
    'Author the sibling reading files and compare computed classifications across the kernel family; locate the disagreement in the priority structure (joint weighing versus lexical ordering) and check which structure the operative funding and venue machinery actually enforces.',
    'If a sibling reading better describes the operative arrangement, this file''s moderate epsilon and dual victim set mis-describe the standing constraint; divergence in computed types across the family is expected signal, not error.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one reading of a contested priority kernel; siblings are separate constraints.').

omega_variable(
    entanglement_thesis_status,
    'Is the structural entanglement of present harms and catastrophic risks empirically real — a class of cases (deployment race dynamics, systemic and dual-use failures) that genuinely produces both — or is it substantially a framing device that justifies broker mediation?',
    'Case analysis of risk events classifiable under both headings; comparative evaluation of whether integrated frameworks catch cases the separate mandates miss; audit of whether entanglement claims in the integrated literature cite identified mechanisms or operate as connective rhetoric.',
    'If entanglement is largely rhetorical, the coordination function is cover for broker gatekeeping and the arrangement drifts toward pure extraction; if it is real, the coordination function is load-bearing and the broker-mediated costs are partly the price of a needed bridge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(entanglement_thesis_status, empirical, 'Whether the coordination function rests on a real empirical thesis or framing cover.').

omega_variable(
    broker_dependence_fragility,
    'Is the integration norm''s resource flow genuinely dependent on a handful of broker actors, or is distributed cross-field collaboration emerging that would survive broker withdrawal?',
    'Longitudinal network analysis of cross-field co-authorship, funding flows, and venue governance; natural experiments from broker exits (institution closures, funder pivots) and whether integration traffic reroutes or collapses.',
    'If distributed collaboration is maturing, the brokered toll structure is transitional and the extraction component is a fading cost of a passing phase; if broker dependence is hardening, capture consolidates around the incumbents and the extraction component grows.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(broker_dependence_fragility, empirical, 'Whether the fragile broker dependence is transitional or consolidating.').

omega_variable(
    victim_mediation_effect,
    'Do present-harm-affected populations experience the integration requirement as dilution of their claims (present harm subordinated to portfolio logic) or as amplification (their harms finally connected to decision-relevant risk frames)?',
    'Participatory assessment with affected communities of how integrated frameworks represent and resource their claims; tracking of whether integrated funding streams return measurable decision power and remediation to affected populations.',
    'If dilution dominates, the present-population seat sits near the full-target end and the mediation machinery is a burden on them specifically; if amplification dominates, that seat sits nearer symmetric and the net burden concentrates on the long-horizon constituency''s diluted protection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_mediation_effect, empirical, 'Direction of the integration frame''s effect on present-harm victims'' claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_governance_priority__bridge_reading, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(airg_bridge_reading_tr_t0, ai_risk_governance_priority__bridge_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(airg_bridge_reading_tr_t2, ai_risk_governance_priority__bridge_reading, theater_ratio, 2, 0.25).
narrative_ontology:measurement(airg_bridge_reading_tr_t4, ai_risk_governance_priority__bridge_reading, theater_ratio, 4, 0.28).
narrative_ontology:measurement(airg_bridge_reading_tr_t6, ai_risk_governance_priority__bridge_reading, theater_ratio, 6, 0.32).
narrative_ontology:measurement(airg_bridge_reading_tr_t8, ai_risk_governance_priority__bridge_reading, theater_ratio, 8, 0.35).
narrative_ontology:measurement(airg_bridge_reading_tr_t10, ai_risk_governance_priority__bridge_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement(airg_bridge_reading_tr_t12, ai_risk_governance_priority__bridge_reading, theater_ratio, 12, 0.4).
narrative_ontology:measurement(airg_bridge_reading_tr_t14, ai_risk_governance_priority__bridge_reading, theater_ratio, 14, 0.42).

% Extraction over time
narrative_ontology:measurement(airg_bridge_reading_be_t0, ai_risk_governance_priority__bridge_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(airg_bridge_reading_be_t2, ai_risk_governance_priority__bridge_reading, base_extractiveness, 2, 0.41).
narrative_ontology:measurement(airg_bridge_reading_be_t4, ai_risk_governance_priority__bridge_reading, base_extractiveness, 4, 0.45).
narrative_ontology:measurement(airg_bridge_reading_be_t6, ai_risk_governance_priority__bridge_reading, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(airg_bridge_reading_be_t8, ai_risk_governance_priority__bridge_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(airg_bridge_reading_be_t10, ai_risk_governance_priority__bridge_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(airg_bridge_reading_be_t12, ai_risk_governance_priority__bridge_reading, base_extractiveness, 12, 0.54).
narrative_ontology:measurement(airg_bridge_reading_be_t14, ai_risk_governance_priority__bridge_reading, base_extractiveness, 14, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(airg_bridge_reading_su_t0, ai_risk_governance_priority__bridge_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(airg_bridge_reading_su_t2, ai_risk_governance_priority__bridge_reading, suppression_requirement, 2, 0.33).
narrative_ontology:measurement(airg_bridge_reading_su_t4, ai_risk_governance_priority__bridge_reading, suppression_requirement, 4, 0.36).
narrative_ontology:measurement(airg_bridge_reading_su_t6, ai_risk_governance_priority__bridge_reading, suppression_requirement, 6, 0.39).
narrative_ontology:measurement(airg_bridge_reading_su_t8, ai_risk_governance_priority__bridge_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(airg_bridge_reading_su_t10, ai_risk_governance_priority__bridge_reading, suppression_requirement, 10, 0.44).
narrative_ontology:measurement(airg_bridge_reading_su_t12, ai_risk_governance_priority__bridge_reading, suppression_requirement, 12, 0.45).
narrative_ontology:measurement(airg_bridge_reading_su_t14, ai_risk_governance_priority__bridge_reading, suppression_requirement, 14, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_governance_priority__bridge_reading, resource_allocation).
narrative_ontology:affects_constraint(ai_risk_governance_priority__bridge_reading, ai_risk_governance_priority__existential_risk_reading).
narrative_ontology:affects_constraint(ai_risk_governance_priority__bridge_reading, ai_risk_governance_priority__near_term_harms_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'AI risk governance priorities' covers three structurally distinct governance arrangements. This file is the bridge (integration) member; the two prioritization siblings are separate files linked here. The decomposition follows the epsilon-invariance principle: each reading has its own stable epsilon, victim set, and beneficiary structure, and cross-reading comparison of computed classifications is the measurement the family exists to take.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_risk_governance_priority__bridge_reading, moderate, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
