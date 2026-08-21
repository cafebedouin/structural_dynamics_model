% ============================================================================
% CONSTRAINT STORY: notability_guidelines__deletionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_notability_guidelines__deletionist_reading, []).

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
 *   constraint_id: notability_guidelines__deletionist_reading
 *   human_readable: Wikipedia Notability Guidelines (Deletionist Reading)
 *   domain: digital_commons_governance/knowledge_infrastructure
 *
 * SUMMARY:
 *   This constraint story represents the 'deletionist reading' of Wikipedia's
 *   notability guidelines (WP:N). From this perspective, WP:N functions as a
 *   necessary epistemic quality filter, preventing the degradation of the
 *   digital commons by ensuring content is verifiable, encyclopedic, and
 *   non-trivial. The guidelines are seen as a coordination mechanism that
 *   justly excludes spam, vanity, and non-notable topics, thereby preserving
 *   the integrity and utility of Wikipedia for its readership.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(notability_guidelines__deletionist_reading, 0.15).
domain_priors:suppression_score(notability_guidelines__deletionist_reading, 0.25).
domain_priors:theater_ratio(notability_guidelines__deletionist_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(notability_guidelines__deletionist_reading, rope).
narrative_ontology:human_readable(notability_guidelines__deletionist_reading, "Wikipedia Notability Guidelines (Deletionist Reading)").
narrative_ontology:topic_domain(notability_guidelines__deletionist_reading, "digital_commons_governance/knowledge_infrastructure").

domain_priors:requires_active_enforcement(notability_guidelines__deletionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(notability_guidelines__deletionist_reading, '4487de49-1e4e-4f4b-9270-64dc15f583e4').
narrative_ontology:cs_kernel_codification('4487de49-1e4e-4f4b-9270-64dc15f583e4', formalized).
narrative_ontology:cs_authority_grounding('4487de49-1e4e-4f4b-9270-64dc15f583e4', practice).
narrative_ontology:cs_interpretation_layer_present('4487de49-1e4e-4f4b-9270-64dc15f583e4').
narrative_ontology:cs_reading_relation('4487de49-1e4e-4f4b-9270-64dc15f583e4', notability_guidelines__inclusionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('4487de49-1e4e-4f4b-9270-64dc15f583e4', notability_guidelines__deliberative_reading, coexists_with).
narrative_ontology:cs_axiom('4487de49-1e4e-4f4b-9270-64dc15f583e4', foundational, verifiability_is_paramount).
narrative_ontology:cs_axiom_status(verifiability_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('4487de49-1e4e-4f4b-9270-64dc15f583e4', verifiability_is_paramount, empirically_contingent).
narrative_ontology:cs_axiom('4487de49-1e4e-4f4b-9270-64dc15f583e4', foundational, encyclopedic_scope_is_finite).
narrative_ontology:cs_axiom_status(encyclopedic_scope_is_finite, holdable).
narrative_ontology:cs_axiom_grounding('4487de49-1e4e-4f4b-9270-64dc15f583e4', encyclopedic_scope_is_finite, conventional).
narrative_ontology:cs_reference_frame('4487de49-1e4e-4f4b-9270-64dc15f583e4', encyclopedic_quality_standard).
narrative_ontology:cs_drift_state('4487de49-1e4e-4f4b-9270-64dc15f583e4', contemporary_wikipedia_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('4487de49-1e4e-4f4b-9270-64dc15f583e4', '').
narrative_ontology:cs_kernel_id(notability_guidelines__deletionist_reading, notability_guidelines).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(notability_guidelines__deletionist_reading, wikipedia_readership).
narrative_ontology:constraint_beneficiary(notability_guidelines__deletionist_reading, deletionist_editors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(notability_guidelines__deletionist_reading, inclusionist_editors).
narrative_ontology:constraint_victim(notability_guidelines__deletionist_reading, new_article_creators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively enforce Wikipedia's notability guidelines through AfD (Articles for Deletion) processes and content review. They view themselves as guardians of encyclopedic quality, preventing the degradation of the commons by non-notable or unverifiable content.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, deletionist_editors, agenda_setter,
    institutional, biographical, constrained, global).

% Benefits from a high-quality, reliable, and verifiable encyclopedia. The guidelines protect them from encountering spam, vanity projects, or trivial information, ensuring the content they access meets a consistent standard.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, wikipedia_readership, beneficiary,
    organized, biographical, mobile, global).

% Expend significant effort advocating for broader inclusion of topics and sources, often engaging in lengthy debates in AfD. From their perspective, the deletionist reading imposes a cost in terms of lost content and editorial friction, even if they agree with the overall goal of quality.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, inclusionist_editors, payer,
    powerful, biographical, constrained, global).

% Must conform to the notability guidelines to ensure their contributions are accepted and retained. They bear the cost of research and formatting to meet the standards, and risk deletion if their topic is deemed non-notable by deletionist interpretations.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, new_article_creators, payer,
    moderate, immediate, constrained, global).

% Their content, which is self-promotional, trivial, or unverifiable, is explicitly targeted for exclusion by the notability guidelines. They are structurally prevented from using Wikipedia for their purposes, which the deletionist reading considers a just and necessary function.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, spam_vanity_promoters, excluded,
    powerless, immediate, trapped, global).

% Focus on the process of negotiation and consensus-building within AfD, seeking to evolve notability boundaries through community discussion rather than strict application of rules. They observe the tension between deletionist and inclusionist views.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, deliberative_editors, observer,
    moderate, biographical, constrained, global).

narrative_ontology:fixing_cost_class(notability_guidelines__deletionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish and maintain a consistent, verifiable standard for content inclusion on Wikipedia, preventing the degradation of its encyclopedic quality and reliability due to an influx of non-notable, promotional, or trivial information.
% TRANSFER_FUNCTION: This constraint transfers editorial authority and effort towards maintaining quality from individual, potentially self-interested, contributors to a collective, quality-focused editorial body. It also transfers the burden of proof for notability onto new content creators.
% ABSENT_VOICES: Spam and vanity promoters are structurally excluded; their content is deemed unworthy of inclusion. Additionally, some advocates for marginalized knowledge might feel their perspectives are absent when notability is interpreted too narrowly, though the deletionist reading views this as a necessary consequence of maintaining quality.
% DISAPPEARANCE_RATIONALE: If the notability guidelines and their enforcement vanished overnight, Wikipedia would rapidly become inundated with unverifiable, self-promotional, or trivial content. Its value as a reliable, encyclopedic knowledge source would quickly erode, leading to a fundamental reorganization of its function and user trust.
% FOUNDING_PROBLEM: Wikipedia's open editing model, while powerful, created a vulnerability to an uncontrolled influx of non-notable, unverifiable, or promotional content, threatening its core mission as a reliable, high-quality encyclopedia.
% FOUNDING_PROBLEM_CORROBORATION: The Wikipedia Foundation, academic studies on information quality in open platforms, and the general readership's expectation of reliability all corroborate the ongoing need for quality control mechanisms. While the *interpretation* of notability is contested, the underlying problem of content quality is widely acknowledged as live.
narrative_ontology:disappearance_verdict(notability_guidelines__deletionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(notability_guidelines__deletionist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(notability_guidelines__deletionist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(notability_guidelines__deletionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(notability_guidelines__deletionist_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(notability_guidelines__deletionist_reading_tests).
:- end_tests(notability_guidelines__deletionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low 'extractiveness' (0.15) reflects the deletionist view that the guidelines impose a necessary cost for quality, not an unjust extraction. The 'suppression' (0.25) is also low, as it's seen as legitimate enforcement of community-agreed rules, not coercion. 'Theater ratio' is very low (0.05) because the function of quality control is considered entirely genuine and essential. 'Accessibility collapse' is high (0.80) because the guidelines are effective at preventing non-notable content from being included. 'Resistance' is moderate (0.40) due to ongoing debates with inclusionist perspectives, but the deletionist view holds its ground as essential for Wikipedia's mission.
 *
 * PERSPECTIVAL GAP:
 *   This story explicitly adopts the deletionist perspective. Other readings, such as the inclusionist (viewing WP:N as gatekeeping) or deliberative (viewing WP:N as a process of negotiation), would assign different metric values and identify different beneficiaries/victims. This story does not attempt to reconcile these views but presents one coherent structural analysis.
 *
 * DIRECTIONALITY LOGIC:
 *   From the deletionist perspective, the Wikipedia readership is the primary beneficiary, receiving a high-quality, reliable encyclopedia. Deletionist editors also benefit by fulfilling their perceived role as guardians of quality. New article creators and inclusionist editors bear the 'cost' of conforming to or debating the guidelines, but this is framed as a necessary contribution to the collective good. Spam/vanity promoters are justly excluded, not victimized.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    notability_as_epistemic_vs_social_filter,
    'Is Wikipedia''s notability guideline primarily an epistemic filter (for quality/verifiability) or a social filter (reflecting power dynamics and excluding marginalized knowledge)?',
    'Empirical analysis of content deletion patterns and their correlation with the social identity of contributors or the subject matter''s mainstream status, compared against strict adherence to verifiability criteria.',
    'If primarily a social filter, the ''suppression'' and ''extractiveness'' metrics would be higher, and the ''claimed_type'' might shift towards a Snare or Tangled Rope, as the coordination story would be seen as cover for exclusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(notability_as_epistemic_vs_social_filter, empirical, 'Ambiguity in the primary function of notability guidelines.').

omega_variable(
    kernel_reading_context,
    'This constraint is one specific reading of the ''notability_guidelines'' kernel. What would be the structural implications if a different reading (e.g., inclusionist or deliberative) were adopted as the dominant interpretation?',
    'Analysis of counterfactual scenarios or policy changes in Wikipedia''s governance that explicitly prioritize inclusion or process over strict deletionist criteria.',
    'An inclusionist reading would likely lower ''suppression'' and ''accessibility_collapse'' and increase ''resistance'' (from deletionists), potentially shifting the type towards a more open Rope or even a Scaffold if temporary. A deliberative reading would emphasize the process, potentially increasing ''theater_ratio'' if the process becomes more performative than functional.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_context, conceptual, 'Structural implications of alternative readings of the notability guidelines kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(notability_guidelines__deletionist_reading, 2001, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nota_tr_t2001, notability_guidelines__deletionist_reading, theater_ratio, 2001, 0.03).
narrative_ontology:measurement(nota_tr_t2007, notability_guidelines__deletionist_reading, theater_ratio, 2007, 0.04).
narrative_ontology:measurement(nota_tr_t2013, notability_guidelines__deletionist_reading, theater_ratio, 2013, 0.05).
narrative_ontology:measurement(nota_tr_t2018, notability_guidelines__deletionist_reading, theater_ratio, 2018, 0.05).
narrative_ontology:measurement(nota_tr_t2024, notability_guidelines__deletionist_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(nota_be_t2001, notability_guidelines__deletionist_reading, base_extractiveness, 2001, 0.1).
narrative_ontology:measurement(nota_be_t2007, notability_guidelines__deletionist_reading, base_extractiveness, 2007, 0.12).
narrative_ontology:measurement(nota_be_t2013, notability_guidelines__deletionist_reading, base_extractiveness, 2013, 0.14).
narrative_ontology:measurement(nota_be_t2018, notability_guidelines__deletionist_reading, base_extractiveness, 2018, 0.15).
narrative_ontology:measurement(nota_be_t2024, notability_guidelines__deletionist_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(nota_su_t2001, notability_guidelines__deletionist_reading, suppression_requirement, 2001, 0.2).
narrative_ontology:measurement(nota_su_t2007, notability_guidelines__deletionist_reading, suppression_requirement, 2007, 0.22).
narrative_ontology:measurement(nota_su_t2013, notability_guidelines__deletionist_reading, suppression_requirement, 2013, 0.24).
narrative_ontology:measurement(nota_su_t2018, notability_guidelines__deletionist_reading, suppression_requirement, 2018, 0.25).
narrative_ontology:measurement(nota_su_t2024, notability_guidelines__deletionist_reading, suppression_requirement, 2024, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(notability_guidelines__deletionist_reading, information_standard).
narrative_ontology:affects_constraint(notability_guidelines__deletionist_reading, notability_guidelines__inclusionist_reading).
narrative_ontology:affects_constraint(notability_guidelines__deletionist_reading, notability_guidelines__deliberative_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'notability_guidelines' kernel. Its sibling readings, 'inclusionist_reading' and 'deliberative_reading', represent alternative structural interpretations of the same underlying commitment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
