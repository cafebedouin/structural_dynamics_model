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
 *   This constraint story represents an 'inclusionist' reading of Wikipedia's
 *   Notability Guidelines (WP:N), viewing them as a structural gatekeeping
 *   mechanism. From this perspective, WP:N systematically excludes knowledge
 *   from marginalized communities and non-traditional sources, thereby
 *   reinforcing existing epistemic hierarchies. The constraint is claimed as
 *   a Snare, reflecting its high extractiveness and suppression, despite its
 *   stated purpose of quality control. This reading contrasts with
 *   'deletionist' views (WP:N as quality filter) and 'deliberative' views
 *   (WP:N as evolving through community consensus).
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
narrative_ontology:cs_story_uid(notability_guidelines__inclusionist_reading, '820965aa-f34d-46d7-bcee-3924c1191a7f').
narrative_ontology:cs_kernel_codification('820965aa-f34d-46d7-bcee-3924c1191a7f', formalized).
narrative_ontology:cs_authority_grounding('820965aa-f34d-46d7-bcee-3924c1191a7f', practice).
narrative_ontology:cs_interpretation_layer_present('820965aa-f34d-46d7-bcee-3924c1191a7f').
narrative_ontology:cs_reading_relation('820965aa-f34d-46d7-bcee-3924c1191a7f', notability_guidelines__deletionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('820965aa-f34d-46d7-bcee-3924c1191a7f', notability_guidelines__deliberative_reading, coexists_with).
narrative_ontology:cs_axiom('820965aa-f34d-46d7-bcee-3924c1191a7f', foundational, epistemic_pluralism_is_foundational).
narrative_ontology:cs_axiom_status(epistemic_pluralism_is_foundational, holdable).
narrative_ontology:cs_axiom_grounding('820965aa-f34d-46d7-bcee-3924c1191a7f', epistemic_pluralism_is_foundational, deontological).
narrative_ontology:cs_axiom('820965aa-f34d-46d7-bcee-3924c1191a7f', foundational, notability_criteria_reflect_power_structures).
narrative_ontology:cs_axiom_status(notability_criteria_reflect_power_structures, holdable).
narrative_ontology:cs_axiom_grounding('820965aa-f34d-46d7-bcee-3924c1191a7f', notability_criteria_reflect_power_structures, empirically_contingent).
narrative_ontology:cs_reference_frame('820965aa-f34d-46d7-bcee-3924c1191a7f', wikipedia_as_universal_knowledge_commons).
narrative_ontology:cs_drift_state('820965aa-f34d-46d7-bcee-3924c1191a7f', contemporary_knowledge_equity_debates, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('820965aa-f34d-46d7-bcee-3924c1191a7f', '').
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

% As the primary enforcers of WP:N, they maintain the existing knowledge hierarchy. Their editing efforts are validated by the availability of 'reliable sources' for mainstream topics, making their work easier and more impactful within the system. They benefit from the clarity and enforceability of the guidelines as applied to their areas of interest.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, wikipedia_editors_mainstream_topics, agenda_setter,
    organized, biographical, mobile, global).

% Their histories, cultural practices, and knowledge systems often lack documentation in 'reliable sources' (as defined by WP:N), leading to their systematic underrepresentation or misrepresentation on Wikipedia. They bear the cost of epistemic erasure and the perpetuation of colonial knowledge structures. Exit means abandoning the hope of self-representation in a globally influential knowledge commons.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, marginalized_communities, payer,
    powerless, generational, identity_locked, global).

% These individuals or groups attempt to contribute knowledge from oral traditions, community archives, or non-Western academic frameworks. They face constant uphill battles to establish 'notability' and 'reliability' for their sources, often leading to their contributions being deleted or rejected. Their efforts are systematically devalued by the guidelines.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, knowledge_producers_non_traditional_sources, payer,
    moderate, biographical, constrained, global).

% The institutional body overseeing Wikipedia. While not directly enforcing WP:N, they are aware of the systemic biases and the ongoing debates. They face pressure to address equity issues but are also committed to the existing governance model and the principle of verifiability, which WP:N is meant to uphold.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, wikipedia_foundation, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a common standard for determining what topics merit an encyclopedia article, aiming to prevent the inclusion of trivial or unverifiable information and maintain a consistent quality bar across millions of articles.
% TRANSFER_FUNCTION: Transfers epistemic authority and visibility from marginalized knowledge systems and non-traditional sources to established, institutionalized knowledge producers and their documented outputs, by defining 'notability' and 'reliability' in terms of the latter.
% ABSENT_VOICES: Scholars and activists advocating for epistemic justice and decolonization of knowledge are often marginalized in the direct policy-making processes, their arguments framed as challenging core Wikipedia principles rather than refining them. Indigenous knowledge holders and community historians are also largely absent from the definitional debates.
% DISAPPEARANCE_RATIONALE: If WP:N vanished overnight, Wikipedia would face an immediate influx of articles on topics previously deemed 'non-notable,' leading to a chaotic period of re-evaluation and a fundamental shift in its content landscape. The power dynamics of knowledge inclusion would be radically altered, forcing a re-negotiation of what constitutes 'encyclopedic knowledge.'
% FOUNDING_PROBLEM: To prevent Wikipedia from becoming a repository of indiscriminate information, self-promotion, or unverifiable claims, ensuring that articles are based on reliable, published sources and cover topics of sufficient public interest.
% FOUNDING_PROBLEM_CORROBORATION: The Wikipedia Foundation and many editors attest the problem is still live, citing the need to maintain quality and prevent spam. Marginalized communities and critical scholars attest that while the original problem of indiscriminate information was real, WP:N has evolved into a tool for systemic exclusion, and its current function is primarily gatekeeping, not quality control. Independent studies on Wikipedia's content biases corroborate the disproportionate impact on non-Western and marginalized topics.
narrative_ontology:disappearance_verdict(notability_guidelines__inclusionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(notability_guidelines__inclusionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(notability_guidelines__inclusionist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.85) because the guidelines effectively extract the right to self-representation and epistemic validity from marginalized groups, channeling authority to established sources. Suppression is very high (0.90) due to the active and often exhausting enforcement of WP:N against contributions from non-traditional perspectives, making it extremely difficult for alternative knowledge to gain entry. Theater ratio is low (0.20) because, from this reading, the 'quality control' justification is largely a cover for maintaining the existing power structure, with little genuine effort to adapt to diverse knowledge forms. The increasing extractiveness and suppression over time reflect the hardening of these gatekeeping functions.
 *
 * PERSPECTIVAL GAP:
 *   The inclusionist reading highlights a significant perspectival gap: what mainstream editors perceive as neutral quality control, marginalized communities experience as active epistemic violence. The engine's classification as a Snare from this reading directly measures this divergence from the claimed Rope-like coordination function.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional knowledge producers and mainstream Wikipedia editors are beneficiaries, as their existing practices and sources are validated and amplified. Marginalized communities and knowledge producers from non-traditional sources are victims, bearing the costs of exclusion and epistemic devaluation. The Wikipedia Foundation acts as an observer, aware of the issues but constrained by its commitment to existing principles.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    epistemic_bias_quantification,
    'To what extent can the ''reliability'' and ''notability'' criteria in WP:N be quantitatively shown to correlate with existing power structures and historical biases in publishing, rather than objective epistemic quality?',
    'Large-scale computational analysis of Wikipedia content, source citations, and deletion logs, correlated with demographic and geopolitical data of knowledge production and representation.',
    'Strong correlation would empirically validate the Snare classification and strengthen calls for policy reform to address systemic bias; weak correlation would challenge the inclusionist reading''s core premise.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(epistemic_bias_quantification, empirical, 'Quantifying the link between WP:N criteria and epistemic bias.').

omega_variable(
    alternative_notability_frameworks,
    'Are there viable alternative notability frameworks that could maintain Wikipedia''s quality and verifiability while being more inclusive of diverse knowledge systems?',
    'Pilot projects implementing alternative notability criteria (e.g., community-based validation, oral history protocols) in specific Wikipedia language editions or topic areas, followed by evaluation of quality and inclusivity outcomes.',
    'Successful alternatives would demonstrate that the current WP:N is a constructed Snare, not an inevitable Mountain of quality control, opening pathways for reclassification and policy change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_notability_frameworks, conceptual, 'Exploring alternative, more inclusive notability frameworks.').

omega_variable(
    internalized_gatekeeping,
    'To what extent has the ''inclusionist'' perspective itself internalized aspects of the dominant ''deletionist'' framework, leading to self-censorship or a focus on ''fitting in'' rather than fundamentally challenging the criteria?',
    'Qualitative sociological studies of inclusionist communities within Wikipedia, analyzing their discourse, strategies, and internal debates for signs of co-optation or strategic essentialism.',
    'Evidence of internalized gatekeeping would suggest that the suppression mechanism is partly cognitive/identity-locked, making resistance harder and requiring deeper cultural shifts beyond policy changes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internalized_gatekeeping, empirical, 'Assessing internalized gatekeeping within inclusionist advocacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(notability_guidelines__inclusionist_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nota_tr_t0, notability_guidelines__inclusionist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(nota_tr_t5, notability_guidelines__inclusionist_reading, theater_ratio, 5, 0.23).
narrative_ontology:measurement(nota_tr_t10, notability_guidelines__inclusionist_reading, theater_ratio, 10, 0.21).
narrative_ontology:measurement(nota_tr_t15, notability_guidelines__inclusionist_reading, theater_ratio, 15, 0.2).
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
narrative_ontology:affects_constraint(notability_guidelines__inclusionist_reading, wikipedia_verifiability_policy).
narrative_ontology:affects_constraint(notability_guidelines__inclusionist_reading, wikipedia_deletion_process).

% DUAL FORMULATION NOTE:
% This is one of three readings of the 'notability_guidelines' kernel: deletionist (quality filter), deliberative (negotiation), and inclusionist (gatekeeping). Each is a distinct constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
