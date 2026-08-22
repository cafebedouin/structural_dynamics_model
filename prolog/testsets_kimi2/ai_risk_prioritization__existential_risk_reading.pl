% ============================================================================
% CONSTRAINT STORY: ai_risk_prioritization__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: ai_risk_prioritization__existential_risk_reading
 *   human_readable: Existential Risk Prioritization Frame in AI Governance
 *   domain: AI Safety / Technology Governance / Risk Assessment
 *
 * SUMMARY:
 *   This constraint story captures the existential-risk reading of the AI
 *   risk prioritization kernel: the claim that artificial general
 *   intelligence poses an extinction-level threat and that alignment research
 *   must therefore be paramount. The constraint is the institutional
 *   arrangement that enacts this readingâconcentrating funding, career
 *   incentives, and epistemic authority in longtermist x-risk institutions
 *   while framing near-term algorithmic justice as a distraction. Key agents
 *   include the x-risk research institutions (who set the agenda and collect
 *   resources), longtermist funders (who supply concentrated capital),
 *   near-term harm communities (whose present suffering is deprioritized),
 *   and algorithmic justice researchers (whose work is marginalized). Future
 *   humanity are invoked rhetorically but are structurally excluded from the
 *   conversation.
 *
 * KEY AGENTS:
 *   - x_risk_research_institutions: Primary agenda-setter and beneficiary (institutional/arbitrage) â administers the constraint and receives the extraction
 *   - longtermist_funders: Primary beneficiary (powerful/mobile) â supplies capital and benefits from the legitimizing frame
 *   - near_term_harm_communities: Primary target (powerless/trapped) â bears the cost of deprioritization
 *   - algorithmic_justice_researchers: Secondary target (moderate/constrained) â pays through funding scarcity and disciplinary marginalization
 *   - future_humanity: Excluded non-agent (powerless/trapped) â invoked but voiceless
 *   - ai_governance_observers: Analytical observer (institutional/analytical) â tracks the asymmetry
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_prioritization__existential_risk_reading, 0.72).
domain_priors:suppression_score(ai_risk_prioritization__existential_risk_reading, 0.78).
domain_priors:theater_ratio(ai_risk_prioritization__existential_risk_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_prioritization__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_prioritization__existential_risk_reading, "Existential Risk Prioritization Frame in AI Governance").
narrative_ontology:topic_domain(ai_risk_prioritization__existential_risk_reading, "AI Safety / Technology Governance / Risk Assessment").

domain_priors:requires_active_enforcement(ai_risk_prioritization__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_prioritization__existential_risk_reading, 'e4591193-7f1a-460c-a871-323cb3e04799').
narrative_ontology:cs_kernel_codification('e4591193-7f1a-460c-a871-323cb3e04799', distributed).
narrative_ontology:cs_authority_grounding('e4591193-7f1a-460c-a871-323cb3e04799', lineage).
narrative_ontology:cs_interpretation_layer_present('e4591193-7f1a-460c-a871-323cb3e04799').
narrative_ontology:cs_reading_relation('e4591193-7f1a-460c-a871-323cb3e04799', ai_risk_prioritization__near_term_harms_reading, influences).
narrative_ontology:cs_axiom('e4591193-7f1a-460c-a871-323cb3e04799', foundational, existential_risk_primacy).
narrative_ontology:cs_axiom_status(existential_risk_primacy, holdable).
narrative_ontology:cs_axiom_grounding('e4591193-7f1a-460c-a871-323cb3e04799', existential_risk_primacy, empirically_contingent).
narrative_ontology:cs_axiom('e4591193-7f1a-460c-a871-323cb3e04799', foundational, alignment_paramountcy).
narrative_ontology:cs_axiom_status(alignment_paramountcy, holdable).
narrative_ontology:cs_axiom_grounding('e4591193-7f1a-460c-a871-323cb3e04799', alignment_paramountcy, instrumental).
narrative_ontology:cs_reference_frame('e4591193-7f1a-460c-a871-323cb3e04799', longtermist_civilizational_security).
narrative_ontology:cs_drift_state('e4591193-7f1a-460c-a871-323cb3e04799', contemporary_capabilities_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e4591193-7f1a-460c-a871-323cb3e04799', '').
narrative_ontology:cs_kernel_id(ai_risk_prioritization__existential_risk_reading, ai_risk_prioritization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__existential_risk_reading, x_risk_research_institutions).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__existential_risk_reading, longtermist_funders).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, near_term_harm_communities).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, algorithmic_justice_researchers).
narrative_ontology:constraint_vindicates(ai_risk_prioritization__existential_risk_reading, existential_risk_hypothesis).
narrative_ontology:constraint_vindicates(ai_risk_prioritization__existential_risk_reading, longtermism_priority_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the research agenda, conference programming, and publication norms for AI safety around existential risk, alignment, and capability control. Receive the majority of longtermist funding for AI safety and administer the credentialing and career structures that determine who counts as a serious safety researcher.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, x_risk_research_institutions, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_prioritization__existential_risk_reading, x_risk_research_institutions, beneficiary).

% Direct philanthropic and investment capital toward existential risk research and capability restraint. Their grant-making strategy instantiates the priority of very-long-term outcomes over present welfare, and they benefit from the legitimizing frame that their giving prevents extinction.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, longtermist_funders, beneficiary,
    powerful, generational, mobile, global).

% Experience algorithmic discrimination, surveillance, and labor displacement from currently deployed AI systems. Their demands for regulation, redress, and public-interest design are systematically deprioritized and framed as distractions from the core mission of preventing extinction.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, near_term_harm_communities, payer,
    powerless, immediate, trapped, national).

% Study bias, fairness, and accountability in deployed AI systems. Face funding scarcity and disciplinary marginalization as AI safety funding concentrates on speculative alignment research; their work is repeatedly framed as outside the core safety mission or as a lower priority than extinction prevention.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, algorithmic_justice_researchers, payer,
    moderate, biographical, constrained, global).

% The hypothetical population of future humans whose existence depends on alignment outcomes. Invoked as the ultimate beneficiaries of existential risk prevention, they have no voice in present prioritization debates and cannot exit the technological world current generations construct for them.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, future_humanity, excluded,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(ai_risk_prioritization__existential_risk_reading, future_humanity).

% Track the allocation of AI safety funding and policy attention between long-term existential risk and near-term harms. Document the growing asymmetry in resources, citations, and institutional voice between the two framings.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, ai_governance_observers, observer,
    institutional, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_risk_prioritization__existential_risk_reading, x_risk_research_institutions).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global attention and research labor toward preventing hypothetical future human extinction from misaligned artificial general intelligence, centralizing agendas around technical alignment and capability control.
% TRANSFER_FUNCTION: Moves funding, research labor, policy attention, and epistemic credibility from near-term algorithmic accountability and harm mitigation toward long-term existential risk research and capability restraint institutions.
% ABSENT_VOICES: Communities currently experiencing algorithmic harm; civil society organizations focused on immediate digital rights and labor impacts; future generations who inherit the governance structures and technological path-dependencies built by present priorities.
% DISAPPEARANCE_RATIONALE: If the existential risk prioritization frame disappeared, AI governance discourse would reallocate toward present harms and accountability, funding flows would shift to algorithmic justice and bias research, and the institutional architecture of longtermist AI safety would lose its primary legitimizing narrative.
% FOUNDING_PROBLEM: The potential for future artificial general intelligence to cause human extinction or permanent civilizational collapse due to misalignment with human values.
% FOUNDING_PROBLEM_CORROBORATION: The problem is attested by x-risk researchers and longtermist philosophers from within the benefiting tradition; independent social scientists, near-term harm communities, and technology policy scholars contest both the probability of the extinction scenario and the claim that it merits overriding present harms, with no corroboration from outside the beneficiary set for the specific prioritization claim.
narrative_ontology:disappearance_verdict(ai_risk_prioritization__existential_risk_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_prioritization__existential_risk_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_prioritization__existential_risk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_risk_prioritization__existential_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_prioritization__existential_risk_reading, 0.72, 'kimi-k2.6', 'none', direct).

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
 *   Base extractiveness (0.72) is high because the constraint systematically redirects resources and legitimacy from present harms to speculative long-term prevention. Suppression (0.78) is higher still because the constraint's persistence depends on actively framing near-term justice as a distraction and maintaining epistemic boundaries around what counts as 'real' safety work. Theater ratio (0.45) reflects growing performative maintenance: as near-term harms become undeniable, an increasing share of the constraint's energy goes to boundary-policing rather than verifiable risk reduction. Accessibility collapse (0.68) captures how alternatives (near-term governance, accountability research) collapse in perceived viability once the existential frame is accepted. Resistance (0.55) registers the growing pushback from algorithmic justice communities and critical technology scholars. The temporal series run on a shared grid and show monotonic intensification as the field institutionalizes.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter/beneficiary seats and the payer seats should compute to markedly different classifications. From the x-risk institutional seat, the constraint is genuine coordination to prevent extinction; from the near-term community and researcher seats, the same structure operates as extractive suppression of their needs and knowledge. The engine computes this divergence from the structural asymmetry in power, exit, and scope. Future humanity occupy a unique position: they are rhetorically constructed as beneficiaries but are structurally trapped and excluded, meaning their effective directionality is ambiguous.
 *
 * DIRECTIONALITY LOGIC:
 *   x_risk_research_institutions sit near the full-beneficiary end: they control the agenda, collect the funding, and have arbitrage-grade exit (can pivot to other research institutions or funding streams). Longtermist funders are also near the beneficiary end with mobile exit. Near-term harm communities sit near the full-target end: they are powerless, trapped in the systems that harm them, and the constraint suppresses their advocacy. Algorithmic justice researchers are mid-to-high target: moderate power but constrained exit due to career path dependence and funding gatekeeping. Future humanity are identity-locked and trapped by definitionâthey cannot exit the future we build.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents both false benignity and false extraction. A pure rope reading would ignore the asymmetric suppression of near-term justice; a pure snare reading would deny the genuine coordination function of concentrating attention on catastrophic risk. By requiring both beneficiaries and victims, plus active enforcement, the tangled_rope gate forces acknowledgment that the constraint genuinely coordinates some researchers around a shared mission while simultaneously extracting from others through the same structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    future_humanity_victim_status,
    'Are future humans genuine victims of the current prioritization constraint, or constructed rhetorical objects whose hypothetical existence justifies present extraction from near-term communities?',
    'Discourse analysis of how future humanity are invoked in funding proposals and policy documents, paired with outcome tracking: if the constraint persists without measurable extinction-risk reduction while near-term harms accumulate, the rhetorical-victim reading strengthens.',
    'If future humanity are primarily rhetorical constructs, the constraint''s legitimacy derives from present extraction rather than genuine future protection, pushing classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_humanity_victim_status, conceptual, 'Whether future humanity are structural victims or legitimizing constructs').

omega_variable(
    existential_risk_empirical_basis,
    'Does the empirical and theoretical evidence support a non-negligible probability of human extinction from misaligned AGI in the 10-100 year horizon?',
    'Systematic expert elicitation with calibrated forecasting; tracking of AI capability trajectories and alignment progress against explicit thresholds; meta-analysis of extinction-risk estimates.',
    'If the empirical basis is weak, the coordination story is cover for resource capture; if strong, the tangled_rope classification holds because genuine coordination coexists with extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(existential_risk_empirical_basis, empirical, 'Empirical grounding of the extinction risk claim').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of near-term justice framing structural (resource diversion, gatekeeping) or internalized (researchers genuinely believing near-term work is less important)?',
    'Career-path interviews and funding-audits: measure whether researchers abandon near-term work due to funding scarcity (structural) or normative conviction (internalized).',
    'If internalized, effective suppression exceeds the structural measure because researchers carry the constraint with them even when external barriers are removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_prioritization__existential_risk_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t0, ai_risk_prioritization__existential_risk_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ai_r_tr_t4, ai_risk_prioritization__existential_risk_reading, theater_ratio, 4, 0.28).
narrative_ontology:measurement(ai_r_tr_t8, ai_risk_prioritization__existential_risk_reading, theater_ratio, 8, 0.35).
narrative_ontology:measurement(ai_r_tr_t12, ai_risk_prioritization__existential_risk_reading, theater_ratio, 12, 0.4).
narrative_ontology:measurement(ai_r_tr_t16, ai_risk_prioritization__existential_risk_reading, theater_ratio, 16, 0.43).
narrative_ontology:measurement(ai_r_tr_t20, ai_risk_prioritization__existential_risk_reading, theater_ratio, 20, 0.45).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(ai_r_be_t4, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 4, 0.52).
narrative_ontology:measurement(ai_r_be_t8, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 8, 0.6).
narrative_ontology:measurement(ai_r_be_t12, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 12, 0.67).
narrative_ontology:measurement(ai_r_be_t16, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 16, 0.7).
narrative_ontology:measurement(ai_r_be_t20, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 20, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t0, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(ai_r_su_t4, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 4, 0.58).
narrative_ontology:measurement(ai_r_su_t8, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 8, 0.65).
narrative_ontology:measurement(ai_r_su_t12, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 12, 0.72).
narrative_ontology:measurement(ai_r_su_t16, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 16, 0.75).
narrative_ontology:measurement(ai_r_su_t20, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 20, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_prioritization__existential_risk_reading, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
