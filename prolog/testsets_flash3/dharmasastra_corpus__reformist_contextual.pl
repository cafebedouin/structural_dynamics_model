% ============================================================================
% CONSTRAINT STORY: dharmasastra_corpus__reformist_contextual
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dharmasastra_corpus__reformist_contextual, []).

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
 *   constraint_id: dharmasastra_corpus__reformist_contextual
 *   human_readable: Dharmasastra: Reformist Contextual Reading
 *   domain: religious_law/textual_interpretation/normative_authority
 *
 * SUMMARY:
 *   This constraint represents the 'reformist contextual' reading of
 *   Dharmasastra, which seeks to reconcile the ancient Hindu legal and
 *   ethical texts with modern values. It interprets the texts as reflecting
 *   historical social conditions, separating an enduring ethical core (dharma
 *   as righteous conduct) from time-bound caste and gender prescriptions.
 *   This reading aims to preserve the authority of the texts while mitigating
 *   their oppressive elements, leading to a reduced victim set and medium
 *   extraction compared to a literalist reading. This is one reading of the
 *   'dharmasastra_corpus' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dharmasastra_corpus__reformist_contextual, 0.45).
domain_priors:suppression_score(dharmasastra_corpus__reformist_contextual, 0.3).
domain_priors:theater_ratio(dharmasastra_corpus__reformist_contextual, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, extractiveness, 0.45).
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dharmasastra_corpus__reformist_contextual, tangled_rope).
narrative_ontology:human_readable(dharmasastra_corpus__reformist_contextual, "Dharmasastra: Reformist Contextual Reading").
narrative_ontology:topic_domain(dharmasastra_corpus__reformist_contextual, "religious_law/textual_interpretation/normative_authority").

domain_priors:requires_active_enforcement(dharmasastra_corpus__reformist_contextual).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dharmasastra_corpus__reformist_contextual, 'bbdd2065-aeec-4a03-a75a-bb8dcf45584f').
narrative_ontology:cs_kernel_codification('bbdd2065-aeec-4a03-a75a-bb8dcf45584f', fixed_text).
narrative_ontology:cs_authority_grounding('bbdd2065-aeec-4a03-a75a-bb8dcf45584f', lineage).
narrative_ontology:cs_interpretation_layer_present('bbdd2065-aeec-4a03-a75a-bb8dcf45584f').
narrative_ontology:cs_reading_relation('bbdd2065-aeec-4a03-a75a-bb8dcf45584f', dharmasastra_corpus__orthodox_literalist, influences).
narrative_ontology:cs_reading_relation('bbdd2065-aeec-4a03-a75a-bb8dcf45584f', dharmasastra_corpus__abolitionist_rejection, coexists_with).
narrative_ontology:cs_axiom('bbdd2065-aeec-4a03-a75a-bb8dcf45584f', foundational, dharma_is_contextual_and_evolving).
narrative_ontology:cs_axiom_status(dharma_is_contextual_and_evolving, holdable).
narrative_ontology:cs_axiom_grounding('bbdd2065-aeec-4a03-a75a-bb8dcf45584f', dharma_is_contextual_and_evolving, conventional).
narrative_ontology:cs_axiom('bbdd2065-aeec-4a03-a75a-bb8dcf45584f', foundational, ethical_core_separable_from_social_prescriptions).
narrative_ontology:cs_axiom_status(ethical_core_separable_from_social_prescriptions, holdable).
narrative_ontology:cs_axiom_grounding('bbdd2065-aeec-4a03-a75a-bb8dcf45584f', ethical_core_separable_from_social_prescriptions, deontological).
narrative_ontology:cs_reference_frame('bbdd2065-aeec-4a03-a75a-bb8dcf45584f', adaptable_ethical_tradition).
narrative_ontology:cs_drift_state('bbdd2065-aeec-4a03-a75a-bb8dcf45584f', contemporary_globalized_hinduism, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('bbdd2065-aeec-4a03-a75a-bb8dcf45584f', '').
narrative_ontology:cs_kernel_id(dharmasastra_corpus__reformist_contextual, dharmasastra_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__reformist_contextual, reformist_hindu_scholars).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__reformist_contextual, hindu_community_leaders).
narrative_ontology:constraint_victim(dharmasastra_corpus__reformist_contextual, lower_caste_individuals_symbolic).
narrative_ontology:constraint_victim(dharmasastra_corpus__reformist_contextual, women_symbolic).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret Dharmasastra texts to emphasize universal ethical principles while recontextualizing or de-emphasizing caste-based and gender-specific prescriptions. They seek to maintain the texts' authority and relevance in modern society.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, reformist_hindu_scholars, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from a framework that allows them to engage with modern ethical standards while retaining a connection to traditional religious texts, avoiding accusations of promoting outdated social hierarchies. They gain legitimacy by presenting a progressive interpretation.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, hindu_community_leaders, beneficiary,
    organized, biographical, constrained, national).

% While direct legal enforcement of caste is largely absent, the symbolic persistence of caste concepts, even if reinterpreted as spiritual stages, can still subtly influence social status and self-perception. They bear the cost of this lingering symbolic hierarchy.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, lower_caste_individuals_symbolic, payer,
    powerless, biographical, identity_locked, local).

% Similar to lower-caste individuals, women may still experience subtle social expectations or limitations derived from reinterpreted traditional roles, even if overt discrimination is rejected. They bear the cost of these symbolic constraints.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, women_symbolic, payer,
    moderate, biographical, identity_locked, local).

% Reject the reformist contextual reading, insisting on the literal and eternal validity of all Dharmasastra prescriptions, including caste and gender hierarchies. They are excluded from the mainstream discourse shaped by reformist interpretations.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, orthodox_literalist_adherents, excluded,
    organized, generational, constrained, regional).

% Advocate for the complete rejection of Dharmasastra due to its historical association with oppressive social structures. They view any attempt at reinterpretation as legitimizing a flawed system and are excluded from the internal reformist debate.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, abolitionist_critics, excluded,
    moderate, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for ethical conduct and social order within Hindu traditions, allowing for adaptation to modern values while maintaining continuity with ancient texts.
% TRANSFER_FUNCTION: Transfers interpretive authority from rigid literalism to contextual scholarship, allowing reformist scholars to define 'dharma' for contemporary society. It also transfers symbolic legitimacy to community leaders who adopt this interpretation, while subtly imposing lingering symbolic costs on historically marginalized groups.
% ABSENT_VOICES: Orthodox literalists are marginalized in this discourse, as their views are deemed incompatible with modern ethics. Abolitionist critics are also excluded, as their call for complete rejection undermines the reformist goal of textual preservation.
% DISAPPEARANCE_RATIONALE: If this reformist reading vanished, the Hindu community would face a stark choice between orthodox literalism and complete rejection of Dharmasastra, leading to significant internal fragmentation and a loss of a coherent, adaptable ethical framework for many adherents.
% FOUNDING_PROBLEM: The challenge of reconciling ancient religious texts, which contain socially regressive elements (like caste hierarchy), with modern ethical sensibilities and the need for a relevant, unifying moral code for a global Hindu community.
% FOUNDING_PROBLEM_CORROBORATION: Reformist scholars and community leaders attest to the ongoing challenge. Independent sociological studies and interfaith dialogues corroborate the need for religious traditions to adapt to contemporary ethical standards to maintain relevance and avoid internal conflict.
narrative_ontology:disappearance_verdict(dharmasastra_corpus__reformist_contextual, world_rearranges).
narrative_ontology:founding_problem_status(dharmasastra_corpus__reformist_contextual, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dharmasastra_corpus__reformist_contextual, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(dharmasastra_corpus__reformist_contextual, 'none', 1).
narrative_ontology:epsilon_provenance(dharmasastra_corpus__reformist_contextual, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dharmasastra_corpus__reformist_contextual_tests).
:- end_tests(dharmasastra_corpus__reformist_contextual_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate because while overt oppressive elements are rejected, the symbolic weight of the texts can still subtly influence social dynamics, particularly for historically marginalized groups. Suppression (0.30) is relatively low, as this reading relies more on persuasion and reinterpretation than overt coercion, but it still actively suppresses literalist interpretations. Theater ratio (0.20) is present as some reinterpretation might be seen as performative to maintain relevance. The trend shows a decrease in extractiveness and suppression over time as the reformist reading gains acceptance and its more oppressive aspects are further softened.
 *
 * PERSPECTIVAL GAP:
 *   Reformist scholars perceive this as a necessary and beneficial adaptation, preserving tradition while promoting justice. However, those who still experience subtle social costs, or those who advocate for complete rejection, would view it as insufficient or even complicit in maintaining a flawed system.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformist scholars and community leaders are beneficiaries, gaining legitimacy and a coherent framework. Lower-caste individuals and women are symbolic payers, as the lingering influence of traditional categories, even if reinterpreted, can still impose subtle social costs. Orthodox literalists and abolitionist critics are excluded, as their positions are incompatible with the reformist project.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading actively prevents mandatrophy by adapting the texts to remain relevant. It avoids the piton trap by maintaining a genuine coordination function (ethical guidance) and actively addressing the obsolescence of certain prescriptions, rather than merely performing maintenance. It is a tangled rope because it coordinates a community around a shared ethical framework while still extracting symbolic costs from certain groups, requiring active enforcement of its interpretive authority against literalist views.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    symbolic_vs_material_extraction,
    'To what extent does the symbolic persistence of caste and gender categories, even under reformist interpretation, translate into material or social disadvantages for individuals?',
    'Sociological studies tracking social mobility, access to resources, and discrimination experiences among individuals from historically marginalized groups within communities adhering to this reformist reading.',
    'If symbolic persistence correlates strongly with material disadvantage, the effective extractiveness of this reading is higher than currently estimated, potentially pushing it closer to a Snare. If the correlation is weak, the reformist claim of mitigating harm is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symbolic_vs_material_extraction, empirical, 'Distinguishing symbolic from material extraction in the reformist reading.').

omega_variable(
    interpretive_authority_legitimacy,
    'Is the interpretive authority claimed by reformist scholars genuinely accepted by the broader Hindu community, or is it a top-down imposition that masks ongoing dissent?',
    'Surveys of diverse Hindu populations, analysis of local religious practices, and examination of internal debates within different community segments regarding the validity of reformist interpretations.',
    'If acceptance is widespread, the constraint functions more as a Rope, genuinely coordinating. If dissent is significant and suppressed, the constraint''s suppression metric is higher, and its classification leans more towards a Snare or a more extractive Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_authority_legitimacy, empirical, 'Assessing the legitimacy and acceptance of reformist interpretive authority.').

omega_variable(
    ethical_core_universality,
    'Is the ''ethical core'' identified by reformist scholars truly universal and separable from the historical context, or is it itself a modern construct projected onto ancient texts?',
    'Comparative theological and philosophical analysis across diverse religious and secular ethical traditions to identify genuine cross-cultural ethical commonalities, and historical-critical analysis of the texts to trace the evolution of ethical concepts.',
    'If the ethical core is largely a modern projection, the reformist reading''s claim to textual fidelity is weakened, potentially reducing its legitimacy and increasing its ''theater_ratio'' as it performs a connection that is not structurally present. If genuinely universal, its coordination function is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ethical_core_universality, conceptual, 'Examining the universality and textual grounding of the ''ethical core''.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dharmasastra_corpus__reformist_contextual, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dhar_tr_t0, dharmasastra_corpus__reformist_contextual, theater_ratio, 0, 0.25).
narrative_ontology:measurement(dhar_tr_t10, dharmasastra_corpus__reformist_contextual, theater_ratio, 10, 0.22).
narrative_ontology:measurement(dhar_tr_t20, dharmasastra_corpus__reformist_contextual, theater_ratio, 20, 0.2).
narrative_ontology:measurement(dhar_tr_t30, dharmasastra_corpus__reformist_contextual, theater_ratio, 30, 0.2).
narrative_ontology:measurement(dhar_tr_t40, dharmasastra_corpus__reformist_contextual, theater_ratio, 40, 0.2).
narrative_ontology:measurement(dhar_tr_t50, dharmasastra_corpus__reformist_contextual, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(dhar_be_t0, dharmasastra_corpus__reformist_contextual, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(dhar_be_t10, dharmasastra_corpus__reformist_contextual, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(dhar_be_t20, dharmasastra_corpus__reformist_contextual, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(dhar_be_t30, dharmasastra_corpus__reformist_contextual, base_extractiveness, 30, 0.46).
narrative_ontology:measurement(dhar_be_t40, dharmasastra_corpus__reformist_contextual, base_extractiveness, 40, 0.45).
narrative_ontology:measurement(dhar_be_t50, dharmasastra_corpus__reformist_contextual, base_extractiveness, 50, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(dhar_su_t0, dharmasastra_corpus__reformist_contextual, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(dhar_su_t10, dharmasastra_corpus__reformist_contextual, suppression_requirement, 10, 0.35).
narrative_ontology:measurement(dhar_su_t20, dharmasastra_corpus__reformist_contextual, suppression_requirement, 20, 0.32).
narrative_ontology:measurement(dhar_su_t30, dharmasastra_corpus__reformist_contextual, suppression_requirement, 30, 0.3).
narrative_ontology:measurement(dhar_su_t40, dharmasastra_corpus__reformist_contextual, suppression_requirement, 40, 0.3).
narrative_ontology:measurement(dhar_su_t50, dharmasastra_corpus__reformist_contextual, suppression_requirement, 50, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dharmasastra_corpus__reformist_contextual, identity_coordination).
narrative_ontology:affects_constraint(dharmasastra_corpus__reformist_contextual, dharmasastra_corpus__orthodox_literalist).
narrative_ontology:affects_constraint(dharmasastra_corpus__reformist_contextual, dharmasastra_corpus__abolitionist_rejection).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'dharmasastra_corpus' kernel. This 'reformist_contextual' reading attempts to adapt the texts to modern ethics, influencing but not foreclosing the 'orthodox_literalist' reading, and coexisting with the 'abolitionist_rejection' reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
