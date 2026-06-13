% ============================================================================
% CONSTRAINT STORY: notability_guidelines__inclusionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_notability_guidelines__inclusionist_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: notability_guidelines__inclusionist_reading
 *   human_readable: Wikipedia Notability Guidelines (Inclusionist Reading)
 *   domain: digital_commons_governance/knowledge_infrastructure
 *
 * SUMMARY:
 *   This constraint represents the Wikipedia Notability Guidelines (WP:N) as
 *   a structural gatekeeping mechanism, interpreted from an inclusionist
 *   perspective. It systematically excludes knowledge from marginalized
 *   communities by privileging sources from established institutions
 *   (academics, mainstream media) that often do not cover such topics. The
 *   constraint is actively enforced through deletion processes, leading to
 *   high extraction from and suppression of alternative knowledge forms.
 *
 * KEY AGENTS:
 *   - wikipedia_editors: Agenda setter (institutional/constrained) — enforce WP:N, often from a mainstream epistemic background.
 *   - institutional_knowledge_producers: Beneficiary (organized/arbitrage) — their work is privileged by WP:N, reinforcing their authority.
 *   - marginalized_communities: Victim (powerless/identity_locked) — their knowledge is systematically excluded, leading to epistemic injustice.
 *   - deletionist_editors: Payer (organized/constrained) — actively enforce WP:N, bearing the social cost of conflict but upholding the perceived quality.
 *   - inclusionist_editors: Payer (organized/constrained) — advocate for broader inclusion, often facing resistance and burnout.
 *   - analytical_observers: Observer (analytical/analytical) — study the systemic effects of WP:N on knowledge representation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(notability_guidelines__inclusionist_reading, 0.85).
domain_priors:suppression_score(notability_guidelines__inclusionist_reading, 0.9).
domain_priors:theater_ratio(notability_guidelines__inclusionist_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(notability_guidelines__inclusionist_reading, snare).
narrative_ontology:human_readable(notability_guidelines__inclusionist_reading, "Wikipedia Notability Guidelines (Inclusionist Reading)").
narrative_ontology:topic_domain(notability_guidelines__inclusionist_reading, "digital_commons_governance/knowledge_infrastructure").

domain_priors:requires_active_enforcement(notability_guidelines__inclusionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(notability_guidelines__inclusionist_reading, '20026c55-155b-46cf-8809-f43dfeeb6899').
narrative_ontology:cs_kernel_codification('20026c55-155b-46cf-8809-f43dfeeb6899', formalized).
narrative_ontology:cs_authority_grounding('20026c55-155b-46cf-8809-f43dfeeb6899', practice).
narrative_ontology:cs_interpretation_layer_present('20026c55-155b-46cf-8809-f43dfeeb6899').
narrative_ontology:cs_reading_relation('20026c55-155b-46cf-8809-f43dfeeb6899', notability_guidelines__deletionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('20026c55-155b-46cf-8809-f43dfeeb6899', notability_guidelines__deliberative_reading, coexists_with).
narrative_ontology:cs_axiom('20026c55-155b-46cf-8809-f43dfeeb6899', foundational, knowledge_is_socially_constructed).
narrative_ontology:cs_axiom_status(knowledge_is_socially_constructed, holdable).
narrative_ontology:cs_axiom_grounding('20026c55-155b-46cf-8809-f43dfeeb6899', knowledge_is_socially_constructed, empirically_contingent).
narrative_ontology:cs_axiom('20026c55-155b-46cf-8809-f43dfeeb6899', secondary, epistemic_justice_is_a_foundational_good).
narrative_ontology:cs_axiom_status(epistemic_justice_is_a_foundational_good, holdable).
narrative_ontology:cs_axiom_grounding('20026c55-155b-46cf-8809-f43dfeeb6899', epistemic_justice_is_a_foundational_good, deontological).
narrative_ontology:cs_reference_frame('20026c55-155b-46cf-8809-f43dfeeb6899', universal_knowledge_commons).
narrative_ontology:cs_drift_state('20026c55-155b-46cf-8809-f43dfeeb6899', contemporary_wikipedia_practice, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('20026c55-155b-46cf-8809-f43dfeeb6899', '').
narrative_ontology:cs_kernel_id(notability_guidelines__inclusionist_reading, notability_guidelines).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(notability_guidelines__inclusionist_reading, institutional_knowledge_producers).
narrative_ontology:constraint_beneficiary(notability_guidelines__inclusionist_reading, established_academics).
narrative_ontology:constraint_beneficiary(notability_guidelines__inclusionist_reading, mainstream_media).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, marginalized_communities).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, indigenous_knowledge_holders).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, activist_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, deletionist_editors).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, inclusionist_editors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Volunteer editors who interpret and enforce WP:N, often acting as gatekeepers. They invest significant time and effort in content review and deletion processes, believing they uphold Wikipedia's quality standards. Their power is derived from community consensus and policy interpretation.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, wikipedia_editors, agenda_setter,
    institutional, generational, constrained, global).

% Academics, journalists, and mainstream media whose work is considered 'reliable sources' by WP:N. Their publications are privileged, reinforcing their epistemic authority and ensuring their topics are well-represented on Wikipedia. They benefit from the amplification and validation of their knowledge.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, institutional_knowledge_producers, beneficiary,
    organized, generational, arbitrage, global).

% Communities whose knowledge, histories, and perspectives are often documented in non-mainstream or oral traditions, which WP:N deems 'unreliable' or 'not notable'. They bear the cost of epistemic exclusion, seeing their contributions rejected and their narratives erased from a major global knowledge platform. Their identity is often tied to their knowledge systems, making 'exit' from the desire to be represented difficult.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, marginalized_communities, payer,
    powerless, generational, identity_locked, global).

% A subset of Wikipedia editors who rigorously apply WP:N to remove articles lacking 'sufficient' sourcing. They see themselves as defending Wikipedia's integrity and quality, but their actions often contribute to the exclusion of marginalized knowledge. They pay in terms of conflict and potential community backlash.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, deletionist_editors, payer,
    moderate, biographical, constrained, global).

% A subset of Wikipedia editors who advocate for broader interpretation of WP:N or for alternative guidelines to include more diverse topics and sources. They often face an uphill battle against established norms and deletionist tendencies, paying in terms of effort and frustration.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, inclusionist_editors, payer,
    moderate, biographical, constrained, global).

% Scholars whose peer-reviewed publications are the gold standard for 'reliable sources' under WP:N. They benefit from the system's reliance on their output, which reinforces the academic publishing ecosystem and their authority within it.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, established_academics, beneficiary,
    organized, generational, arbitrage, global).

% Major news outlets and publications whose reporting is widely accepted as 'reliable sources'. They benefit from their content being used to establish notability, further cementing their role as authoritative information providers.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, mainstream_media, beneficiary,
    organized, biographical, arbitrage, global).

% Bear the brunt of WP:N's Eurocentric bias, as their oral traditions and community-specific knowledge systems are rarely recognized as 'reliable sources'. They experience epistemic violence through the erasure of their heritage from a globally accessible platform.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, indigenous_knowledge_holders, payer,
    powerless, civilizational, identity_locked, global).

% Academics who work with marginalized communities and produce scholarship that challenges mainstream narratives. Their work, while rigorous, may struggle to meet WP:N's criteria if it relies on non-traditional sources or is deemed 'advocacy', placing them in a constrained position.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, activist_scholars, payer,
    moderate, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(notability_guidelines__inclusionist_reading, institutional_knowledge_producers).
narrative_ontology:fixing_cost_class(notability_guidelines__inclusionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To provide a consistent standard for determining which topics merit an encyclopedia article, aiming to ensure verifiability and prevent the inclusion of trivial or unverifiable information.
% TRANSFER_FUNCTION: Transfers epistemic authority and visibility from marginalized knowledge systems to established, institutionally-backed knowledge systems, by privileging sources from the latter and excluding those from the former.
% ABSENT_VOICES: Many marginalized communities and indigenous knowledge holders are absent from the deliberation process, as their epistemic frameworks are not recognized as legitimate within the current system. They would advocate for a broader definition of 'notability' and 'reliable sources' that includes their own forms of knowledge production.
% DISAPPEARANCE_RATIONALE: If WP:N vanished overnight, Wikipedia's content landscape would drastically change. There would be an influx of articles on previously excluded topics, a shift in the types of sources cited, and a significant redistribution of epistemic authority. The current structure of knowledge on Wikipedia would be fundamentally reorganized.
% FOUNDING_PROBLEM: To prevent Wikipedia from becoming a repository of trivial, unverifiable, or self-promotional content, ensuring that articles are based on established knowledge and verifiable facts.
% FOUNDING_PROBLEM_CORROBORATION: Wikipedia's founders and many long-term editors attest that the problem of content quality and verifiability is still live. However, marginalized communities and critical scholars (outside the direct beneficiaries) argue that while the original problem was valid, WP:N's current application has overshot, creating a new problem of systemic exclusion, and that the original problem is largely solved for mainstream topics but weaponized against others.
narrative_ontology:disappearance_verdict(notability_guidelines__inclusionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(notability_guidelines__inclusionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(notability_guidelines__inclusionist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(notability_guidelines__inclusionist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(notability_guidelines__inclusionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(notability_guidelines__inclusionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(notability_guidelines__inclusionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the epistemic cost borne by marginalized communities whose knowledge is deemed 'not notable' and thus excluded, effectively extracting their contributions from the global knowledge commons. Suppression (0.90) is severe due to the rigid enforcement of source requirements and the power imbalance in content disputes. Theater ratio (0.20) is low because the enforcement is genuinely aimed at maintaining the 'quality' as defined by the guidelines, even if that definition is biased. Accessibility collapse is high (0.75) because for many marginalized topics, 'reliable sources' as defined by WP:N simply do not exist in the mainstream, effectively collapsing any alternative path to inclusion. Resistance (0.70) is substantial, manifested in ongoing debates, edit wars, and attempts to reform WP:N.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of institutional knowledge producers, WP:N is a necessary quality filter (a Rope or even Mountain). From the perspective of marginalized communities, it is a Snare that systematically excludes their voices. Wikipedia editors, as agenda-setters, experience it as a complex, often contentious, but necessary tool for maintaining the project's integrity. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional knowledge producers are beneficiaries (d=0.0-0.1) as their work is amplified and validated. Marginalized communities are victims (d=0.9-1.0) as their knowledge is suppressed. Wikipedia editors, particularly deletionists, act as enforcers, bearing the cost of conflict but upholding the system (d=0.5-0.7). Inclusionist editors, while part of the system, are also targets of its rigidity (d=0.7-0.8).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (ensuring verifiability and quality) is still 'live' in a general sense, but its application has drifted to systematically exclude certain forms of knowledge. The classification as a Snare prevents mislabeling this as a neutral coordination mechanism or a natural epistemic limit. It highlights that the 'coordination' of knowledge is achieved through asymmetric extraction and suppression, rather than genuine collective benefit for all potential contributors.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint a necessary epistemic quality filter (deletionist reading), a perpetually negotiated process (deliberative reading), or a structural gatekeeping apparatus (inclusionist reading)?',
    'Analysis of AfD outcomes over time, focusing on the success rate of articles about marginalized topics versus mainstream topics, and the types of sources accepted/rejected.',
    'If the deletionist reading is correct, the constraint is a Rope; if the deliberative reading is correct, it''s a Tangled Rope; if the inclusionist reading is correct, it''s a Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Ambiguity between different readings of Wikipedia''s Notability Guidelines.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (lack of ''reliable sources'' for marginalized knowledge) or internalized (marginalized communities self-censor due to repeated exclusion)?',
    'Post-exclusion content creation trajectory: if marginalized communities continue to produce and cite their own knowledge outside Wikipedia, it suggests structural suppression; if they cease, it suggests internalized suppression.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for marginalized knowledge.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(notability_guidelines__inclusionist_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nota_tr_t0, notability_guidelines__inclusionist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(nota_tr_t5, notability_guidelines__inclusionist_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(nota_tr_t10, notability_guidelines__inclusionist_reading, theater_ratio, 10, 0.21).
narrative_ontology:measurement(nota_tr_t15, notability_guidelines__inclusionist_reading, theater_ratio, 15, 0.2).

% Extraction over time
narrative_ontology:measurement(nota_be_t0, notability_guidelines__inclusionist_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(nota_be_t5, notability_guidelines__inclusionist_reading, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(nota_be_t10, notability_guidelines__inclusionist_reading, base_extractiveness, 10, 0.8).
narrative_ontology:measurement(nota_be_t15, notability_guidelines__inclusionist_reading, base_extractiveness, 15, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(nota_su_t0, notability_guidelines__inclusionist_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(nota_su_t5, notability_guidelines__inclusionist_reading, suppression_requirement, 5, 0.8).
narrative_ontology:measurement(nota_su_t10, notability_guidelines__inclusionist_reading, suppression_requirement, 10, 0.85).
narrative_ontology:measurement(nota_su_t15, notability_guidelines__inclusionist_reading, suppression_requirement, 15, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(notability_guidelines__inclusionist_reading, information_standard).
narrative_ontology:affects_constraint(notability_guidelines__inclusionist_reading, deletionist_reading).
narrative_ontology:affects_constraint(notability_guidelines__inclusionist_reading, deliberative_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'notability_guidelines' kernel. Other readings include 'deletionist_reading' and 'deliberative_reading', which offer different interpretations of WP:N's function and impact.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
