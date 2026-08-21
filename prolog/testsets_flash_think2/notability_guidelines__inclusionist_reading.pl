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
 *   domain: digital_commons_governance/knowledge_infrastructure
 *
 * SUMMARY:
 *   This constraint story analyzes Wikipedia's Notability Guidelines (WP:N)
 *   from an 'inclusionist' perspective, viewing them as a structural
 *   gatekeeping apparatus. While ostensibly designed to ensure quality and
 *   verifiability, this reading argues that WP:N systematically excludes
 *   marginalized knowledge and privileges institutionally sanctioned sources,
 *   functioning as an extractive snare. The constraint is claimed as a
 *   'snare' because its coordination story (quality control) serves as a
 *   cover for asymmetric extraction of epistemic authority and the
 *   suppression of alternative knowledge forms. The metrics reflect this
 *   interpretation, showing high and increasing extraction and suppression
 *   over time.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(notability_guidelines__inclusionist_reading, 0.85).
domain_priors:suppression_score(notability_guidelines__inclusionist_reading, 0.9).
domain_priors:theater_ratio(notability_guidelines__inclusionist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(notability_guidelines__inclusionist_reading, snare).
narrative_ontology:human_readable(notability_guidelines__inclusionist_reading, "Wikipedia Notability Guidelines (Inclusionist Reading)").
narrative_ontology:topic_domain(notability_guidelines__inclusionist_reading, "digital_commons_governance/knowledge_infrastructure").

domain_priors:requires_active_enforcement(notability_guidelines__inclusionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(notability_guidelines__inclusionist_reading, 'a132b45e-70d1-4480-8d0e-69db9d75735a').
narrative_ontology:cs_kernel_codification('a132b45e-70d1-4480-8d0e-69db9d75735a', fixed_text).
narrative_ontology:cs_authority_grounding('a132b45e-70d1-4480-8d0e-69db9d75735a', extraction).
narrative_ontology:cs_interpretation_layer_present('a132b45e-70d1-4480-8d0e-69db9d75735a').
narrative_ontology:cs_reading_relation('a132b45e-70d1-4480-8d0e-69db9d75735a', notability_guidelines__deletionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('a132b45e-70d1-4480-8d0e-69db9d75735a', notability_guidelines__deliberative_reading, coexists_with).
narrative_ontology:cs_axiom('a132b45e-70d1-4480-8d0e-69db9d75735a', foundational, knowledge_is_power_structured).
narrative_ontology:cs_axiom_status(knowledge_is_power_structured, holdable).
narrative_ontology:cs_axiom_grounding('a132b45e-70d1-4480-8d0e-69db9d75735a', knowledge_is_power_structured, deontological).
narrative_ontology:cs_axiom('a132b45e-70d1-4480-8d0e-69db9d75735a', secondary, reliable_sources_reproduce_bias).
narrative_ontology:cs_axiom_status(reliable_sources_reproduce_bias, holdable).
narrative_ontology:cs_axiom_grounding('a132b45e-70d1-4480-8d0e-69db9d75735a', reliable_sources_reproduce_bias, empirically_contingent).
narrative_ontology:cs_reference_frame('a132b45e-70d1-4480-8d0e-69db9d75735a', hegemonic_knowledge_reproduction).
narrative_ontology:cs_drift_state('a132b45e-70d1-4480-8d0e-69db9d75735a', contemporary_platform_governance_era, gap(stable, substantial, false)).
narrative_ontology:cs_created_at('a132b45e-70d1-4480-8d0e-69db9d75735a', '').
narrative_ontology:cs_kernel_id(notability_guidelines__inclusionist_reading, notability_guidelines).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(notability_guidelines__inclusionist_reading, institutional_knowledge_producers).
narrative_ontology:constraint_beneficiary(notability_guidelines__inclusionist_reading, established_wikipedia_editors).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, marginalized_knowledge_producers).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, excluded_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The community of editors who interpret and enforce WP:N through deletion discussions and content review. They believe they are upholding quality standards, but from an inclusionist perspective, their actions systematically privilege certain forms of knowledge.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, wikipedia_editors, agenda_setter,
    organized, biographical, constrained, global).

% Academic institutions, major media outlets, and established publishers whose work is readily accepted as 'reliable sources' by WP:N. Their knowledge is amplified and legitimized by Wikipedia's authority, without direct effort on their part.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, institutional_knowledge_producers, beneficiary,
    institutional, generational, arbitrage, global).

% Individuals and communities whose knowledge (e.g., oral traditions, community-based research, non-Western epistemologies) often lacks 'reliable sources' as defined by WP:N. Their contributions are frequently deleted, and their labor in attempting to include their knowledge is wasted.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, marginalized_knowledge_producers, payer,
    powerless, generational, trapped, global).

% Groups whose histories, cultures, or perspectives are systematically underrepresented or misrepresented on Wikipedia due to the notability criteria. They are effectively locked out of contributing their own narratives in a way that would be accepted by the platform.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, excluded_communities, excluded,
    powerless, generational, identity_locked, global).

% Academics and researchers who study Wikipedia's governance and content, often highlighting systemic biases and power dynamics embedded in policies like WP:N. They analyze the constraint's operation but do not directly participate in its enforcement.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, critical_scholars_of_wikipedia, observer,
    analytical, biographical, analytical, global).

% Long-standing editors who are adept at navigating WP:N and whose contributions are rarely challenged. They benefit from the stability and perceived quality that WP:N provides, reinforcing their position within the community.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, established_wikipedia_editors, beneficiary,
    moderate, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(notability_guidelines__inclusionist_reading, institutional_knowledge_producers).
narrative_ontology:fixing_cost_class(notability_guidelines__inclusionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Claims to coordinate a shared standard for encyclopedic content, ensuring verifiability and preventing the proliferation of original research or fringe theories, thereby maintaining Wikipedia's reputation as a reliable information source.
% TRANSFER_FUNCTION: Transfers epistemic authority, visibility, and legitimacy from marginalized or non-institutional knowledge forms to institutionally sanctioned knowledge forms, effectively gatekeeping what counts as 'notable' and 'reliable'.
% ABSENT_VOICES: Marginalized communities, indigenous knowledge holders, and activists whose work and perspectives are not typically covered by 'reliable sources' as defined by WP:N. They would advocate for broader definitions of notability and source reliability, but are often excluded from the policy-making and enforcement discussions.
% DISAPPEARANCE_RATIONALE: If WP:N vanished overnight, Wikipedia's content landscape would fundamentally shift. There would be an immediate influx of articles on previously 'non-notable' topics, a broader range of sources would be accepted, and the platform's epistemic authority would be decentralized, leading to a reorganization of editorial power and content focus.
% FOUNDING_PROBLEM: To ensure Wikipedia's reliability and prevent it from becoming an indiscriminate repository of all information, original research, or unverified claims, thereby maintaining its utility as an encyclopedia.
% FOUNDING_PROBLEM_CORROBORATION: Proponents (many Wikipedia editors, some academics) argue the problem of maintaining quality and preventing misinformation is still live. Critics (critical scholars, digital rights advocates, marginalized communities) argue that while the initial problem was real, WP:N has evolved into a tool for systemic bias and gatekeeping, making the 'founding problem' a cover for its current extractive function. Independent research on Wikipedia's systemic bias supports the latter reading.
narrative_ontology:disappearance_verdict(notability_guidelines__inclusionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(notability_guidelines__inclusionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(notability_guidelines__inclusionist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The high extractiveness (0.85) reflects the significant cost borne by marginalized knowledge producers whose contributions are rejected, and the epistemic authority transferred to established institutions. Suppression (0.90) is severe due to the rigid enforcement of 'reliable sources' criteria, which effectively bars many forms of knowledge from inclusion. The theater ratio (0.40) indicates that a substantial portion of the 'quality control' justification is performative, masking the underlying gatekeeping function. The increasing trends in extractiveness and suppression over the interval reflect the ossification of WP:N and its enforcement mechanisms, leading to a hardening of the gatekeeping function.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Wikipedia editors and institutional knowledge producers, WP:N is a necessary 'rope' or 'mountain' for maintaining quality. However, from the perspective of marginalized communities and critical scholars, the same guidelines operate as a 'snare', systematically excluding their voices and knowledge. The engine's computation of per-seat classifications will highlight this divergence, showing a 'snare' classification for the victims and a 'rope' or 'tangled_rope' for the beneficiaries/agenda-setters.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional knowledge producers are clear beneficiaries (d=0.0) as their work is automatically deemed 'notable' and amplified. Established Wikipedia editors also benefit (d=0.15) from the stability and reduced contentiousness that WP:N provides, reinforcing their position. Marginalized knowledge producers and excluded communities are clear targets (d=1.0), bearing the costs of exclusion and wasted effort. Wikipedia editors, as agenda-setters, enforce this structure, benefiting from the perceived order it creates.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    epistemic_function_ambiguity,
    'Is WP:N primarily an epistemic quality filter (deletionist reading) or a structural gatekeeping mechanism (inclusionist reading)?',
    'Longitudinal studies of content inclusion/exclusion patterns, analysis of deletion discussion outcomes, and ethnographic research on editor motivations and community norms.',
    'If primarily a quality filter, the constraint leans towards a Rope or Mountain; if primarily gatekeeping, it confirms the Snare classification. This impacts the perceived legitimacy and necessity of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_function_ambiguity, conceptual, 'Ambiguity in the primary function of Wikipedia''s Notability Guidelines.').

omega_variable(
    reliable_sources_bias,
    'Is the definition and application of ''reliable sources'' within WP:N inherently biased towards Western, institutional, or mainstream knowledge, systematically disadvantaging marginalized epistemologies?',
    'Content analysis comparing source types used for different topics, expert review by scholars of decolonization and critical race theory, and community-led initiatives to broaden source definitions and inclusion criteria.',
    'If systemic bias is confirmed, it strengthens the Snare classification by demonstrating a non-neutral mechanism of extraction and suppression. If the bias is found to be negligible, it would weaken the Snare classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reliable_sources_bias, empirical, 'Bias in the ''reliable sources'' criteria.').

omega_variable(
    internalized_exclusion,
    'To what extent do marginalized knowledge producers internalize WP:N''s criteria, leading to self-censorship or disengagement from Wikipedia, even when structural barriers might be overcome?',
    'Surveys and interviews with marginalized communities about their experiences and perceptions of Wikipedia, and analysis of editing patterns from these groups over time.',
    'If internalized exclusion is significant, the effective suppression of the constraint is higher than structural measures alone suggest, as agents carry the suppression with them, reinforcing the Snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_exclusion, empirical, 'Internalized suppression mechanism among marginalized communities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(notability_guidelines__inclusionist_reading, 2005, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nota_tr_t2005, notability_guidelines__inclusionist_reading, theater_ratio, 2005, 0.2).
narrative_ontology:measurement(nota_tr_t2009, notability_guidelines__inclusionist_reading, theater_ratio, 2009, 0.25).
narrative_ontology:measurement(nota_tr_t2013, notability_guidelines__inclusionist_reading, theater_ratio, 2013, 0.3).
narrative_ontology:measurement(nota_tr_t2017, notability_guidelines__inclusionist_reading, theater_ratio, 2017, 0.35).
narrative_ontology:measurement(nota_tr_t2021, notability_guidelines__inclusionist_reading, theater_ratio, 2021, 0.38).
narrative_ontology:measurement(nota_tr_t2025, notability_guidelines__inclusionist_reading, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(nota_be_t2005, notability_guidelines__inclusionist_reading, base_extractiveness, 2005, 0.65).
narrative_ontology:measurement(nota_be_t2009, notability_guidelines__inclusionist_reading, base_extractiveness, 2009, 0.72).
narrative_ontology:measurement(nota_be_t2013, notability_guidelines__inclusionist_reading, base_extractiveness, 2013, 0.78).
narrative_ontology:measurement(nota_be_t2017, notability_guidelines__inclusionist_reading, base_extractiveness, 2017, 0.82).
narrative_ontology:measurement(nota_be_t2021, notability_guidelines__inclusionist_reading, base_extractiveness, 2021, 0.84).
narrative_ontology:measurement(nota_be_t2025, notability_guidelines__inclusionist_reading, base_extractiveness, 2025, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(nota_su_t2005, notability_guidelines__inclusionist_reading, suppression_requirement, 2005, 0.7).
narrative_ontology:measurement(nota_su_t2009, notability_guidelines__inclusionist_reading, suppression_requirement, 2009, 0.78).
narrative_ontology:measurement(nota_su_t2013, notability_guidelines__inclusionist_reading, suppression_requirement, 2013, 0.83).
narrative_ontology:measurement(nota_su_t2017, notability_guidelines__inclusionist_reading, suppression_requirement, 2017, 0.87).
narrative_ontology:measurement(nota_su_t2021, notability_guidelines__inclusionist_reading, suppression_requirement, 2021, 0.89).
narrative_ontology:measurement(nota_su_t2025, notability_guidelines__inclusionist_reading, suppression_requirement, 2025, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
