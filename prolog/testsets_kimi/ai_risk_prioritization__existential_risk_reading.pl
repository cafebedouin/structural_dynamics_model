% ============================================================================
% CONSTRAINT STORY: ai_risk_prioritization__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_risk_prioritization__existential_risk_reading, []).

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
 *   constraint_id: ai_risk_prioritization__existential_risk_reading
 *   human_readable: AI Risk Prioritization: Existential Risk Reading
 *   domain: technology governance / AI safety
 *
 * SUMMARY:
 *   The existential risk reading of AI prioritization treats misaligned
 *   artificial general intelligence as the dominant threat to humanity's
 *   future, demanding a reorientation of research funding, governance
 *   attention, and regulatory design toward long-term alignment and
 *   capability control. This constraint coordinates global action around a
 *   speculative but potentially catastrophic risk while simultaneously
 *   concentrating resources in a specific institutional ecosystem and
 *   suppressing near-term algorithmic justice frameworks as secondary
 *   distractions. It is authored as a Tangled Rope: genuine coordination
 *   against a potential extinction event is coupled with asymmetric
 *   extraction that benefits x-risk institutions and longtermist funders at
 *   the expense of present-harm communities and other governance priorities.
 *
 * KEY AGENTS:
 *   - x_risk_institutions: Primary agenda-setter (institutional/arbitrage) â defines the field and enforces the prioritization boundary
 *   - longtermist_funders: Primary beneficiary (powerful/mobile) â directs capital and collects legitimacy
 *   - near_term_justice_advocates: Primary payer (moderate/constrained) â bears costs of deprioritization and rhetorical suppression
 *   - current_harm_communities: Secondary payer (powerless/trapped) â bears opportunity cost of diverted remediation
 *   - future_humanity: Excluded non-agent (powerless/trapped/universal) â invoked as moral token but has no present voice
 *   - critical_scholars: Analytical observer (analytical/analytical) â tracks epistemic and financial flows
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_prioritization__existential_risk_reading, 0.65).
domain_priors:suppression_score(ai_risk_prioritization__existential_risk_reading, 0.7).
domain_priors:theater_ratio(ai_risk_prioritization__existential_risk_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_prioritization__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_prioritization__existential_risk_reading, "AI Risk Prioritization: Existential Risk Reading").
narrative_ontology:topic_domain(ai_risk_prioritization__existential_risk_reading, "technology governance / AI safety").

domain_priors:requires_active_enforcement(ai_risk_prioritization__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_prioritization__existential_risk_reading, 'b61a3f80-5e1e-411f-98e8-19e8a709a520').
narrative_ontology:cs_kernel_codification('b61a3f80-5e1e-411f-98e8-19e8a709a520', distributed).
narrative_ontology:cs_authority_grounding('b61a3f80-5e1e-411f-98e8-19e8a709a520', expertise).
narrative_ontology:cs_interpretation_layer_present('b61a3f80-5e1e-411f-98e8-19e8a709a520').
narrative_ontology:cs_reading_relation('b61a3f80-5e1e-411f-98e8-19e8a709a520', ai_risk_prioritization__near_term_harms_reading, influences).
narrative_ontology:cs_axiom('b61a3f80-5e1e-411f-98e8-19e8a709a520', foundational, extinction_prevention_overrides_present_harms).
narrative_ontology:cs_axiom_status(extinction_prevention_overrides_present_harms, holdable).
narrative_ontology:cs_axiom_grounding('b61a3f80-5e1e-411f-98e8-19e8a709a520', extinction_prevention_overrides_present_harms, deontological).
narrative_ontology:cs_axiom('b61a3f80-5e1e-411f-98e8-19e8a709a520', foundational, future_persons_full_moral_status).
narrative_ontology:cs_axiom_status(future_persons_full_moral_status, holdable).
narrative_ontology:cs_axiom_grounding('b61a3f80-5e1e-411f-98e8-19e8a709a520', future_persons_full_moral_status, deontological).
narrative_ontology:cs_reference_frame('b61a3f80-5e1e-411f-98e8-19e8a709a520', existential_risk_prevention_norm).
narrative_ontology:cs_drift_state('b61a3f80-5e1e-411f-98e8-19e8a709a520', contemporary_near_term_challenge, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b61a3f80-5e1e-411f-98e8-19e8a709a520', '').
narrative_ontology:cs_kernel_id(ai_risk_prioritization__existential_risk_reading, ai_risk_prioritization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__existential_risk_reading, x_risk_institutions).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__existential_risk_reading, longtermist_funders).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, future_humanity).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, near_term_justice_advocates).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, current_harm_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__existential_risk_reading, taxpaying_public).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, taxpaying_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define the research agenda for AGI alignment, operate institutes, publish threat models, and administer funding flows justified by existential risk prevention. They shape the constraint by determining what counts as legitimate AI safety research and by enforcing the boundary between existential and near-term concerns.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, x_risk_institutions, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Direct philanthropic and investment capital toward existential risk reduction and long-termist causes. They benefit from the institutionalization of their worldview, the tax-advantaged structures of x-risk philanthropy, and the reputational legitimacy conferred by affiliation with a purported civilization-saving mission.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, longtermist_funders, beneficiary,
    powerful, civilizational, mobile, global).

% Advocate for algorithmic accountability, labor protections, and anti-discrimination measures in deployed AI systems. Their agendas and funding opportunities are deprioritized when AI governance frames near-term harms as distractions from existential preparation, and they face rhetorical pressure to recast their work in x-risk terms.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, near_term_justice_advocates, payer,
    moderate, biographical, constrained, national).

% Communities experiencing algorithmic discrimination, surveillance, or labor displacement from current AI deployment. They bear the opportunity cost when regulatory and research attention is diverted toward speculative future risks rather than present remediation, and they lack mobility to exit the systems harming them.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, current_harm_communities, payer,
    powerless, immediate, trapped, local).

% The population of humans who would exist after transformative AI deployment. They are invoked as the primary moral beneficiaries of alignment research but have no present voice, vote, or capacity to resist constraint designs that may ultimately fail to protect them.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, future_humanity, excluded,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(ai_risk_prioritization__existential_risk_reading, future_humanity).

% Provide public funding for AI research through taxes and support governance frameworks that embed the existential priority. They are told the constraint protects their descendants, but they have no direct voice in the prioritization and bear the fiscal cost of the research architecture while receiving diffuse and unverifiable benefits.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, taxpaying_public, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(ai_risk_prioritization__existential_risk_reading, taxpaying_public, beneficiary).

% Analyze the epistemic foundations of existential risk claims, track funding flows, and document the opportunity costs of the x-risk prioritization framework. They do not collect from or pay into the constraint, and their exit is unrestricted analytical distance.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, critical_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_risk_prioritization__existential_risk_reading, x_risk_institutions).
narrative_ontology:fixing_cost_class(ai_risk_prioritization__existential_risk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preventing uncontrolled artificial general intelligence from causing human extinction by coordinating global research efforts, governance attention, and capability restraint programs around long-term alignment.
% TRANSFER_FUNCTION: Moves financial, intellectual, and political resources from near-term governance and harm-reduction efforts toward long-term alignment research institutions and capability restraint programs, justified by the overriding priority of existential risk prevention.
% ABSENT_VOICES: Future generations who would exist after AGI deployment have no present voice; communities currently experiencing algorithmic discrimination and labor displacement are present but structurally deprioritized in the prioritization framework.
% DISAPPEARANCE_RATIONALE: Without the existential priority framing, AI governance discourse would reallocate toward near-term harms, capability development would proceed with different oversight incentives, and the funding architecture supporting long-term alignment research would reorganize around other evaluative criteria.
% FOUNDING_PROBLEM: The prospect that transformative artificial intelligence could escape human control and cause human extinction or permanent disempowerment, requiring proactive technical and governance solutions before the capability threshold is reached.
% FOUNDING_PROBLEM_CORROBORATION: X-risk researchers and some technologists attest the problem is live. Critics from algorithmic justice, social science, and some AI researchers attest the founding problem is inadequately empirically grounded and functions to redirect resources; no independent corroboration exists outside the benefiting parties and their funders that the extinction risk probability warrants this exact prioritization structure.
narrative_ontology:disappearance_verdict(ai_risk_prioritization__existential_risk_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_prioritization__existential_risk_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_prioritization__existential_risk_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_risk_prioritization__existential_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_prioritization__existential_risk_reading, 0.65, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_prioritization__existential_risk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_risk_prioritization__existential_risk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_risk_prioritization__existential_risk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate-high (0.65) because substantial intellectual and financial capital is redirected through this framing; suppression (0.70) is required to maintain the boundary between 'existential' and 'near-term' priorities in policy discourse. Theater ratio (0.45) reflects that while alignment research is technically substantive, an increasing share of institutional activity performs commitment to the framing rather than verifiable technical progress. Accessibility collapse (0.40) is moderate: alternatives (near-term governance, other cause areas) persist but face funding and legitimacy barriers. Resistance (0.55) reflects organized critique from algorithmic justice advocates and social scientists. The measurement series run on one shared time grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   From the x-risk institutional seat, the constraint is a genuine scaffold for civilization-scale coordination; from the near-term advocate seat, it is a snare diverting resources and political will from measurable harms. The engine computes this divergence from the structural data: same constraint, opposite directionalities depending on whether the agent collects from or pays into the prioritization architecture.
 *
 * DIRECTIONALITY LOGIC:
 *   X-risk institutions and longtermist funders sit near the beneficiary pole: they collect resources, set agendas, and retain high mobility. Near-term justice advocates and current-harm communities sit near the target pole: they bear the costs of deprioritization and have constrained or trapped exit. Future humanity is structurally excluded (agent=false) and does not feed directionality derivation. The taxpaying public sits in a mixed position, bearing fiscal costs while purportedly benefiting from extinction prevention.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by declaring its founding problem (uncontrolled AGI) and its contested status. If the founding problem were clearly dead while the constraint persisted, it would signal piton or snare drift. The contested status keeps it in tangled_rope territory: the coordination function may still be live even as extraction accumulates. The declared contested status acts as a mandatrophy guard against premature naturalization.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extinction_probability_empirical_status,
    'Is the claimed extinction-level probability of misaligned AGI empirically grounded, or is it an unverifiable speculative estimate that functions as a moral alarm?',
    'Track record analysis of AI risk predictions, expert survey calibration studies, and operationalization of threat models into testable claims; comparison with similarly structured doomsday predictions in other technologies.',
    'If empirical grounding is weak or unfalsifiable, the constraint functions more as a resource-capture snare using future victims as rhetorical cover; if strong, the coordination function is structurally warranted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extinction_probability_empirical_status, empirical, 'Empirical status of the core extinction prediction').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of near-term justice structural (funding and agenda control) or internalized (advocates accepting the existential framing as overriding their own concerns)?',
    'Observe whether near-term justice advocates shift their own agendas toward x-risk compatibility or language adoption; measure funding migration from present-harm organizations to long-term institutes.',
    'If internalized, the constraint''s effective suppression exceeds the structural measure because the targets carry the suppression with them and self-police their own deprioritization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural vs internalized suppression mechanism').

omega_variable(
    kernel_reading_contest,
    'Does the existential_risk_reading capture the ai_risk_prioritization kernel more accurately than its near_term_harms sibling, or do they represent incommensurable framings?',
    'Corpus-level comparison of the two readings'' metric profiles and their ability to predict resource flows and policy outcomes without ad hoc adjustment.',
    'If the readings are incommensurable rather than competing empirical claims, the kernel is irreducibly contested and no single reading can be certified as the true constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer-frame ambiguity for the ai_risk_prioritization kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_prioritization__existential_risk_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_xrisk_tr_t0, ai_risk_prioritization__existential_risk_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ai_xrisk_tr_t8, ai_risk_prioritization__existential_risk_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(ai_xrisk_tr_t16, ai_risk_prioritization__existential_risk_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement(ai_xrisk_tr_t24, ai_risk_prioritization__existential_risk_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement(ai_xrisk_tr_t32, ai_risk_prioritization__existential_risk_reading, theater_ratio, 32, 0.43).
narrative_ontology:measurement(ai_xrisk_tr_t40, ai_risk_prioritization__existential_risk_reading, theater_ratio, 40, 0.45).

% Extraction over time
narrative_ontology:measurement(ai_xrisk_be_t0, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ai_xrisk_be_t8, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(ai_xrisk_be_t16, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(ai_xrisk_be_t24, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 24, 0.58).
narrative_ontology:measurement(ai_xrisk_be_t32, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 32, 0.62).
narrative_ontology:measurement(ai_xrisk_be_t40, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 40, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(ai_xrisk_su_t0, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(ai_xrisk_su_t8, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(ai_xrisk_su_t16, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 16, 0.65).
narrative_ontology:measurement(ai_xrisk_su_t24, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 24, 0.68).
narrative_ontology:measurement(ai_xrisk_su_t32, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 32, 0.7).
narrative_ontology:measurement(ai_xrisk_su_t40, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 40, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_prioritization__existential_risk_reading, resource_allocation).
narrative_ontology:affects_constraint(ai_risk_prioritization__existential_risk_reading, ai_risk_prioritization__near_term_harms_reading).

% DUAL FORMULATION NOTE:
% This constraint and its sibling are two structurally distinct readings of the ai_risk_prioritization kernel. They share the same empirical domain (AI risk) but differ in victim set, beneficiary set, timescale, and resource allocation. Per the Îµ-invariance principle, they are separate constraints with different Îµ values and are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
