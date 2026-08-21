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
 *   constraint_id: notability_guidelines__inclusionist_reading
 *   human_readable: Wikipedia Notability Guidelines (Inclusionist Reading)
 *   domain: digital_commons/knowledge_governance
 *
 * SUMMARY:
 *   This constraint represents an 'inclusionist' reading of Wikipedia's
 *   Notability Guidelines (WP:N), viewing them as a structural gatekeeping
 *   mechanism. From this perspective, WP:N systematically excludes knowledge
 *   from marginalized communities and non-traditional sources, thereby
 *   reinforcing the epistemic authority of institutional knowledge producers.
 *   The constraint is classified as a Snare due to its high extractiveness
 *   and suppression, which are directed at maintaining a specific knowledge
 *   hierarchy. This reading contrasts with 'deletionist' (WP:N as quality
 *   filter) and 'deliberative' (WP:N as evolving negotiation) readings of the
 *   same kernel.
 *
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
narrative_ontology:topic_domain(notability_guidelines__inclusionist_reading, "digital_commons/knowledge_governance").

domain_priors:requires_active_enforcement(notability_guidelines__inclusionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(notability_guidelines__inclusionist_reading, 'c37b584d-d4a5-4e59-a341-d8e55a3e1a75').
narrative_ontology:cs_kernel_codification('c37b584d-d4a5-4e59-a341-d8e55a3e1a75', formalized).
narrative_ontology:cs_authority_grounding('c37b584d-d4a5-4e59-a341-d8e55a3e1a75', practice).
narrative_ontology:cs_interpretation_layer_present('c37b584d-d4a5-4e59-a341-d8e55a3e1a75').
narrative_ontology:cs_reading_relation('c37b584d-d4a5-4e59-a341-d8e55a3e1a75', notability_guidelines__deletionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('c37b584d-d4a5-4e59-a341-d8e55a3e1a75', notability_guidelines__deliberative_reading, coexists_with).
narrative_ontology:cs_axiom('c37b584d-d4a5-4e59-a341-d8e55a3e1a75', foundational, epistemic_justice_as_foundational).
narrative_ontology:cs_axiom_status(epistemic_justice_as_foundational, holdable).
narrative_ontology:cs_axiom_grounding('c37b584d-d4a5-4e59-a341-d8e55a3e1a75', epistemic_justice_as_foundational, deontological).
narrative_ontology:cs_axiom('c37b584d-d4a5-4e59-a341-d8e55a3e1a75', secondary, knowledge_is_socially_constructed).
narrative_ontology:cs_axiom_status(knowledge_is_socially_constructed, holdable).
narrative_ontology:cs_axiom_grounding('c37b584d-d4a5-4e59-a341-d8e55a3e1a75', knowledge_is_socially_constructed, empirically_contingent).
narrative_ontology:cs_reference_frame('c37b584d-d4a5-4e59-a341-d8e55a3e1a75', universal_knowledge_inclusion).
narrative_ontology:cs_drift_state('c37b584d-d4a5-4e59-a341-d8e55a3e1a75', contemporary_wikipedia_practice, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('c37b584d-d4a5-4e59-a341-d8e55a3e1a75', '').
narrative_ontology:cs_kernel_id(notability_guidelines__inclusionist_reading, notability_guidelines).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(notability_guidelines__inclusionist_reading, institutional_knowledge_producers).
narrative_ontology:constraint_beneficiary(notability_guidelines__inclusionist_reading, wikipedia_editors_mainstream_topics).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, marginalized_communities).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, knowledge_producers_non_traditional_sources).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Their work (academic journals, major news outlets, established publishers) is automatically deemed 'reliable' and 'notable' by WP:N, ensuring their perspectives dominate Wikipedia content and reinforcing their epistemic authority. They benefit from the structural exclusion of alternative knowledge systems.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, institutional_knowledge_producers, beneficiary,
    institutional, generational, arbitrage, global).

% Actively enforce WP:N, often without critical reflection on its systemic biases. They gain status and influence within the Wikipedia community by maintaining the 'quality' and 'neutrality' standards, which are implicitly aligned with mainstream, institutional knowledge. Their editing work is easier due to clear source hierarchies.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, wikipedia_editors_mainstream_topics, agenda_setter,
    organized, biographical, mobile, global).

% Their histories, cultural practices, and knowledge systems are systematically excluded or misrepresented on Wikipedia because they often lack 'reliable sources' as defined by WP:N (e.g., peer-reviewed journals, major media). This perpetuates their marginalization in global knowledge infrastructure.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, marginalized_communities, payer,
    powerless, generational, trapped, global).

% Attempt to contribute knowledge from oral traditions, community archives, independent media, or indigenous epistemologies. They face constant uphill battles, their contributions are frequently deleted, and they are forced to either conform to exclusionary sourcing norms or abandon the platform.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, knowledge_producers_non_traditional_sources, payer,
    moderate, biographical, constrained, global).

% Administers Wikipedia and its policies, including WP:N. While publicly committed to 'knowledge equity,' its institutional inertia and reliance on established community norms mean it implicitly upholds the gatekeeping function of WP:N, benefiting from the perceived 'authority' of Wikipedia while externalizing the costs of exclusion.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, wikipedia_foundation, agenda_setter,
    institutional, generational, constrained, global).

% Analyze the structural biases of Wikipedia's content and governance, documenting how policies like WP:N contribute to epistemic injustice and the reproduction of power hierarchies in digital knowledge. They provide external critique and propose alternative models for knowledge inclusion.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, critical_scholars_digital_humanities, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared standard for determining what topics and sources are considered 'important' or 'verifiable' enough to be included in Wikipedia, aiming to prevent the encyclopedia from becoming a repository of trivial or unreliable information.
% TRANSFER_FUNCTION: Transfers epistemic authority and visibility from marginalized knowledge systems and non-traditional sources to mainstream, institutionally validated knowledge, by systematically prioritizing the latter in Wikipedia's content.
% ABSENT_VOICES: Representatives of marginalized communities and scholars of non-Western epistemologies are often absent from the policy-making and enforcement discussions around WP:N, as their very forms of knowledge are deemed 'not notable' or 'unreliable' by the existing framework. Their perspectives on what constitutes 'notability' are systematically excluded.
% DISAPPEARANCE_RATIONALE: If WP:N vanished overnight, Wikipedia would face an immediate influx of topics and sources previously deemed 'unnotable.' The power dynamics of content creation would shift dramatically, leading to a more diverse but potentially less coherent or 'authoritative' encyclopedia, forcing a fundamental re-evaluation of its purpose and structure.
% FOUNDING_PROBLEM: To prevent Wikipedia from becoming a chaotic repository of every conceivable topic, ensuring that articles are based on verifiable information from reliable, independent sources, and that only topics with sufficient coverage are included.
% FOUNDING_PROBLEM_CORROBORATION: The Wikipedia Foundation and many mainstream editors attest the problem is still live, citing the need for quality control. Marginalized communities and critical scholars attest that while the original problem of chaos was real, WP:N has evolved into a tool for epistemic gatekeeping, and its current function is to maintain existing power structures rather than merely filter for quality; this is corroborated by studies on content bias and deletion patterns.
narrative_ontology:disappearance_verdict(notability_guidelines__inclusionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(notability_guidelines__inclusionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(notability_guidelines__inclusionist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(notability_guidelines__inclusionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(notability_guidelines__inclusionist_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.85) because the constraint effectively 'extracts' the right to define what constitutes legitimate knowledge from marginalized groups, transferring it to established institutions. Suppression is very high (0.90) because the enforcement of WP:N actively suppresses alternative epistemologies and sources, making it nearly impossible for certain types of knowledge to gain entry or persist on the platform. The theater ratio is low (0.20) because, from this reading, the 'quality control' justification is largely a cover for the actual function of maintaining epistemic power structures, with little genuine effort to address systemic biases. Accessibility collapse is high (0.75) as alternatives for knowledge dissemination on Wikipedia are severely limited for those outside the mainstream sourcing paradigm. Resistance is also high (0.70) due to ongoing efforts by inclusion-focused editors and scholars to challenge WP:N's application.
 *
 * PERSPECTIVAL GAP:
 *   The 'inclusionist' reading highlights a significant perspectival gap: what the 'deletionist' reading sees as necessary quality control, the 'inclusionist' reading sees as structural epistemic violence. The engine's classification will reflect this divergence, showing a Snare for the inclusionist reading versus potentially a Rope or Tangled Rope for other readings of the same guidelines.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional knowledge producers and mainstream Wikipedia editors are clear beneficiaries, as their knowledge and authority are amplified. Marginalized communities and producers of non-traditional knowledge are the primary victims, facing systemic exclusion and the invalidation of their epistemologies. The Wikipedia Foundation, while claiming neutrality, implicitly benefits from the perceived 'authority' derived from WP:N's application, even as it faces critique for perpetuating bias.
 *
 * MANDATROPHY ANALYSIS:
 *   From this inclusionist reading, the original mandate of preventing chaos has atrophied into a mechanism for maintaining epistemic power. The constraint prevents mislabeling by exposing the beneficiaries of this 'quality control' and the victims of its exclusionary effects, revealing it as a Snare rather than a neutral Rope or Mountain of epistemic truth.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    epistemic_bias_quantification,
    'To what extent can the systemic bias in WP:N''s application be quantitatively measured (e.g., through content analysis of deletion logs, source citation patterns, or demographic representation of editors)?',
    'Large-scale computational analysis of Wikipedia''s content, editing history, and policy enforcement, correlated with demographic and geographic data.',
    'Strong quantitative evidence of bias would solidify the ''snare'' classification and provide empirical grounds for policy reform; weak or ambiguous evidence would strengthen arguments for WP:N as a neutral quality filter.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(epistemic_bias_quantification, empirical, 'Quantifying the extent of epistemic bias in WP:N''s application.').

omega_variable(
    alternative_notability_models,
    'Are there viable alternative models for ''notability'' or ''verifiability'' that could achieve Wikipedia''s stated goals (e.g., quality, neutrality) without systematically excluding marginalized knowledge?',
    'Pilot projects on alternative platforms or within Wikipedia (e.g., specific language Wikipedias, WikiProjects focused on decolonization) demonstrating successful knowledge inclusion without compromising quality.',
    'Demonstrated viable alternatives would expose WP:N''s current form as a choice rather than a necessity, reinforcing its ''snare'' nature. Lack of viable alternatives would suggest a more ''mountain-like'' or ''rope-like'' structural necessity for some form of gatekeeping.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_notability_models, conceptual, 'Exploring alternative models for knowledge inclusion and notability.').

omega_variable(
    internalized_suppression_among_editors,
    'Is the high suppression observed primarily structural (policy enforcement, deletionism) or is a significant portion internalized by editors from marginalized communities (self-censorship, discouragement from contributing)?',
    'Qualitative studies (interviews, ethnography) with editors from marginalized communities to understand their experiences and decision-making processes regarding content contribution and policy engagement.',
    'If internalized suppression is significant, the effective suppression is higher than structural measures suggest, as the constraint operates even in the absence of direct enforcement. This would highlight the deep-seated nature of epistemic injustice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_among_editors, empirical, 'Structural vs. internalized suppression mechanism for marginalized editors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(notability_guidelines__inclusionist_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nota_tr_t0, notability_guidelines__inclusionist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(nota_tr_t5, notability_guidelines__inclusionist_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(nota_tr_t10, notability_guidelines__inclusionist_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(nota_tr_t15, notability_guidelines__inclusionist_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement(nota_tr_t20, notability_guidelines__inclusionist_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(nota_be_t0, notability_guidelines__inclusionist_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(nota_be_t5, notability_guidelines__inclusionist_reading, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(nota_be_t10, notability_guidelines__inclusionist_reading, base_extractiveness, 10, 0.8).
narrative_ontology:measurement(nota_be_t15, notability_guidelines__inclusionist_reading, base_extractiveness, 15, 0.83).
narrative_ontology:measurement(nota_be_t20, notability_guidelines__inclusionist_reading, base_extractiveness, 20, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(nota_su_t0, notability_guidelines__inclusionist_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(nota_su_t5, notability_guidelines__inclusionist_reading, suppression_requirement, 5, 0.8).
narrative_ontology:measurement(nota_su_t10, notability_guidelines__inclusionist_reading, suppression_requirement, 10, 0.85).
narrative_ontology:measurement(nota_su_t15, notability_guidelines__inclusionist_reading, suppression_requirement, 15, 0.88).
narrative_ontology:measurement(nota_su_t20, notability_guidelines__inclusionist_reading, suppression_requirement, 20, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(notability_guidelines__inclusionist_reading, identity_coordination).
narrative_ontology:affects_constraint(notability_guidelines__inclusionist_reading, wikipedia_neutral_point_of_view_policy).
narrative_ontology:affects_constraint(notability_guidelines__inclusionist_reading, wikipedia_reliable_sources_policy).

% DUAL FORMULATION NOTE:
% This is one of three readings of the 'notability_guidelines' kernel. This 'inclusionist' reading focuses on its gatekeeping function, while 'deletionist_reading' emphasizes quality control and 'deliberative_reading' focuses on community negotiation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
