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
    narrative_ontology:constraint_vindicates/2,
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
 *   Dharmasastra, which seeks to interpret the ancient Hindu legal and
 *   ethical texts by separating a universal ethical core (dharma as righteous
 *   conduct) from time-bound social prescriptions, particularly those related
 *   to caste and gender. This reading aims to preserve the textual authority
 *   and relevance of Dharmasastra in modern society while discarding or
 *   reinterpreting its oppressive elements. It is a 'tangled rope' because it
 *   genuinely coordinates the adaptation of tradition to modernity, but still
 *   carries a residual, symbolic extraction from historically marginalized
 *   groups by maintaining the text's overall authority.
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
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dharmasastra_corpus__reformist_contextual, tangled_rope).
narrative_ontology:human_readable(dharmasastra_corpus__reformist_contextual, "Dharmasastra: Reformist Contextual Reading").
narrative_ontology:topic_domain(dharmasastra_corpus__reformist_contextual, "religious_law/textual_interpretation/normative_authority").

domain_priors:requires_active_enforcement(dharmasastra_corpus__reformist_contextual).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dharmasastra_corpus__reformist_contextual, '5ec0e042-c918-49e8-954c-fbe6a52e287b').
narrative_ontology:cs_kernel_codification('5ec0e042-c918-49e8-954c-fbe6a52e287b', fixed_text).
narrative_ontology:cs_authority_grounding('5ec0e042-c918-49e8-954c-fbe6a52e287b', lineage).
narrative_ontology:cs_interpretation_layer_present('5ec0e042-c918-49e8-954c-fbe6a52e287b').
narrative_ontology:cs_reading_relation('5ec0e042-c918-49e8-954c-fbe6a52e287b', dharmasastra_corpus__orthodox_literalist, coexists_with).
narrative_ontology:cs_reading_relation('5ec0e042-c918-49e8-954c-fbe6a52e287b', dharmasastra_corpus__abolitionist_rejection, coexists_with).
narrative_ontology:cs_axiom('5ec0e042-c918-49e8-954c-fbe6a52e287b', foundational, dharma_as_universal_ethical_principle).
narrative_ontology:cs_axiom_status(dharma_as_universal_ethical_principle, holdable).
narrative_ontology:cs_axiom_grounding('5ec0e042-c918-49e8-954c-fbe6a52e287b', dharma_as_universal_ethical_principle, deontological).
narrative_ontology:cs_axiom('5ec0e042-c918-49e8-954c-fbe6a52e287b', foundational, textual_prescriptions_are_historically_contingent).
narrative_ontology:cs_axiom_status(textual_prescriptions_are_historically_contingent, holdable).
narrative_ontology:cs_axiom_grounding('5ec0e042-c918-49e8-954c-fbe6a52e287b', textual_prescriptions_are_historically_contingent, empirically_contingent).
narrative_ontology:cs_reference_frame('5ec0e042-c918-49e8-954c-fbe6a52e287b', ethical_dharma_in_historical_context).
narrative_ontology:cs_drift_state('5ec0e042-c918-49e8-954c-fbe6a52e287b', contemporary_global_ethics_discourse, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('5ec0e042-c918-49e8-954c-fbe6a52e287b', '').
narrative_ontology:cs_kernel_id(dharmasastra_corpus__reformist_contextual, dharmasastra_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__reformist_contextual, reformist_scholars).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__reformist_contextual, modern_hindu_institutions).
narrative_ontology:constraint_victim(dharmasastra_corpus__reformist_contextual, lower_caste_communities_symbolic).
narrative_ontology:constraint_victim(dharmasastra_corpus__reformist_contextual, women_symbolic).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__reformist_contextual, ethical_conduct_as_dharma).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__reformist_contextual, textual_continuity_of_hinduism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret Dharmasastra to emphasize its ethical core while recontextualizing or discarding caste-based prescriptions. They seek to maintain the text's authority and relevance in modern society, often engaging in public discourse and educational initiatives.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, reformist_scholars, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from a reading that allows them to present Hinduism as a progressive, ethically-grounded tradition, attracting adherents and maintaining social legitimacy without fully alienating those who hold more traditional views. They selectively apply or reinterpret texts.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, modern_hindu_institutions, beneficiary,
    organized, generational, constrained, national).

% While direct legal enforcement of caste is diminished, they still bear the symbolic and social weight of a textual tradition that historically justified their subjugation. The reformist reading softens, but does not fully erase, this historical burden, leaving a residual sense of hierarchy.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, lower_caste_communities_symbolic, payer,
    powerless, generational, identity_locked, local).

% Experience a similar symbolic burden, as Dharmasastra historically prescribed restrictive roles for women. The reformist reading attempts to reinterpret these, but the underlying textual authority still carries historical baggage that impacts social perceptions and expectations.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, women_symbolic, payer,
    moderate, generational, identity_locked, local).

% Adhere to a literal interpretation of Dharmasastra, including its caste and gender prescriptions. They are often marginalized in modern public discourse by reformist interpretations but maintain influence within traditional communities and institutions. They would object to the recontextualization.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, orthodox_literalists, excluded,
    organized, generational, constrained, national).

% Advocate for the complete rejection of Dharmasastra due to its historical role in justifying oppression. They view any attempt at reformist interpretation as perpetuating the problematic authority of the text. They are outside the conversation of how to interpret the text, arguing for its abandonment.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, abolitionist_critics, excluded,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for ethical conduct and social order within Hindu traditions, allowing for adaptation to modern values while maintaining a connection to ancient texts.
% TRANSFER_FUNCTION: Transfers moral authority and social legitimacy to reformist interpreters and institutions, while symbolically extracting from historically marginalized groups by maintaining the text's overall authority.
% ABSENT_VOICES: Orthodox literalists would object to the reinterpretation of sacred texts, arguing for their immutable nature. Abolitionist critics would object to any continued engagement with Dharmasastra, advocating for its complete rejection due to its oppressive history.
% DISAPPEARANCE_RATIONALE: If this reformist reading vanished, modern Hindu institutions would struggle to reconcile tradition with contemporary ethics, potentially leading to a schism between traditionalists and those seeking a more progressive framework. The public perception of Hinduism would become more polarized.
% FOUNDING_PROBLEM: The challenge of reconciling ancient religious texts, which contain socially regressive elements, with evolving modern ethical standards and the need for Hinduism to remain relevant and appealing in a globalized world.
% FOUNDING_PROBLEM_CORROBORATION: Scholars of comparative religion and sociology attest to the ongoing tension between religious tradition and modernity in various faiths. Public debates and internal reform movements within Hinduism corroborate the live status of this problem, from outside the directly benefiting institutions.
narrative_ontology:disappearance_verdict(dharmasastra_corpus__reformist_contextual, world_rearranges).
narrative_ontology:founding_problem_status(dharmasastra_corpus__reformist_contextual, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dharmasastra_corpus__reformist_contextual, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(dharmasastra_corpus__reformist_contextual, 'none', 1).

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
 *   Extractiveness (0.45) is moderate, reflecting the softening of direct enforcement but the persistence of symbolic hierarchy. Suppression (0.30) is relatively low, as direct legal enforcement of caste has largely diminished, but social pressure and identity-locked exits still play a role. Theater ratio (0.20) is low, indicating that the reformist interpretation is a genuine attempt at adaptation, not merely a performance. The historical measurements show a decreasing extractiveness and suppression, and a slight increase in theater, reflecting the ongoing process of reinterpretation and the diminishing direct enforcement of traditional norms.
 *
 * PERSPECTIVAL GAP:
 *   Reformist scholars perceive this reading as a necessary and beneficial adaptation, a 'rope' that allows tradition to endure. However, for historically marginalized groups, the continued authority of the text, even with reinterpretation, still functions as a 'tangled rope' due to the lingering symbolic extraction and the identity-locked nature of their relationship to the tradition. The engine's classification as 'tangled_rope' reflects this hybrid nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformist scholars and modern Hindu institutions are beneficiaries, gaining legitimacy and relevance. Historically marginalized groups (lower caste communities, women) are symbolic payers, as the text's authority, even reinterpreted, still carries the weight of past oppression. Orthodox literalists and abolitionist critics are excluded, representing alternative readings that are not part of this specific interpretive framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    symbolic_vs_material_extraction,
    'To what extent does the ''symbolic'' extraction from lower-caste communities and women translate into ongoing material or social disadvantage, despite the reformist interpretation?',
    'Sociological studies measuring contemporary discrimination, access to resources, and social mobility for these groups, specifically linking outcomes to the lingering influence of Dharmasastra''s authority.',
    'If symbolic extraction correlates strongly with material disadvantage, the effective extractiveness of this reading is higher than measured, pushing it closer to a Snare. If the correlation is weak, the ''tangled rope'' classification is more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symbolic_vs_material_extraction, empirical, 'Distinguishing between symbolic and material impacts of textual authority.').

omega_variable(
    textual_authority_vs_ethical_core_separability,
    'Is the ethical core of Dharmasastra truly separable from its time-bound social prescriptions, or is the attempt to separate them an act of selective interpretation that risks undermining the text''s coherence?',
    'Philosophical and theological analysis of the internal consistency of the reformist interpretation, and its acceptance by a broad range of Hindu philosophical schools.',
    'If the separation is deemed incoherent, the reformist reading''s claim to textual authority weakens, potentially reducing its coordination function and increasing its ''theater ratio'' as it becomes more performative than genuinely interpretive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_authority_vs_ethical_core_separability, conceptual, 'Assessing the coherence and internal consistency of the reformist separation of ethical core from social prescriptions.').

omega_variable(
    reformist_legitimacy_vs_orthodox_resistance,
    'How much does the persistence of orthodox literalist readings undermine the legitimacy and effectiveness of the reformist contextual reading in shaping broader social norms?',
    'Analysis of public discourse, religious education curricula, and community practices to gauge the relative influence of reformist versus orthodox interpretations over time.',
    'Strong and persistent orthodox resistance would indicate that the reformist reading''s coordination function is more contested, potentially increasing its effective suppression as it requires more active defense against alternative interpretations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reformist_legitimacy_vs_orthodox_resistance, empirical, 'Impact of competing interpretations on the reformist reading''s social efficacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dharmasastra_corpus__reformist_contextual, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dhar_tr_t1900, dharmasastra_corpus__reformist_contextual, theater_ratio, 1900, 0.1).
narrative_ontology:measurement(dhar_tr_t1950, dharmasastra_corpus__reformist_contextual, theater_ratio, 1950, 0.15).
narrative_ontology:measurement(dhar_tr_t2000, dharmasastra_corpus__reformist_contextual, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(dhar_tr_t2024, dharmasastra_corpus__reformist_contextual, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(dhar_be_t1900, dharmasastra_corpus__reformist_contextual, base_extractiveness, 1900, 0.6).
narrative_ontology:measurement(dhar_be_t1950, dharmasastra_corpus__reformist_contextual, base_extractiveness, 1950, 0.55).
narrative_ontology:measurement(dhar_be_t2000, dharmasastra_corpus__reformist_contextual, base_extractiveness, 2000, 0.48).
narrative_ontology:measurement(dhar_be_t2024, dharmasastra_corpus__reformist_contextual, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(dhar_su_t1900, dharmasastra_corpus__reformist_contextual, suppression_requirement, 1900, 0.5).
narrative_ontology:measurement(dhar_su_t1950, dharmasastra_corpus__reformist_contextual, suppression_requirement, 1950, 0.4).
narrative_ontology:measurement(dhar_su_t2000, dharmasastra_corpus__reformist_contextual, suppression_requirement, 2000, 0.35).
narrative_ontology:measurement(dhar_su_t2024, dharmasastra_corpus__reformist_contextual, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dharmasastra_corpus__reformist_contextual, identity_coordination).
narrative_ontology:affects_constraint(dharmasastra_corpus__reformist_contextual, dharmasastra_corpus__orthodox_literalist).
narrative_ontology:affects_constraint(dharmasastra_corpus__reformist_contextual, dharmasastra_corpus__abolitionist_rejection).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the Dharmasastra corpus, each with different structural properties and classifications. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
