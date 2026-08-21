% ============================================================================
% CONSTRAINT STORY: vedic_corpus_social_prescription__colonial_orientalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_corpus_social_prescription__colonial_orientalist_reading, []).

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
 *   constraint_id: vedic_corpus_social_prescription__colonial_orientalist_reading
 *   human_readable: Colonial Codification of 'Hindu Law'
 *   domain: religious_studies/social_stratification/hermeneutics
 *
 * SUMMARY:
 *   This constraint represents the colonial-orientalist reading of
 *   Vedic/Dharmashastra texts as a unified, timeless 'Hindu law' system,
 *   which was then codified for administrative governance during the British
 *   Raj. It functioned as a scaffold, providing temporary support for
 *   colonial administration by creating legible legal subjects and
 *   simplifying governance, but with a declared sunset (end of colonial
 *   rule). The extraction was moderate, primarily in the form of
 *   administrative convenience and enhanced control for the colonial power,
 *   at the cost of rigidifying social structures and suppressing indigenous
 *   legal diversity. The claimed type is 'scaffold' because it was a
 *   transitional legal framework tied to the colonial project, which
 *   eventually ended.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.45).
domain_priors:suppression_score(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.7).
domain_priors:theater_ratio(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_corpus_social_prescription__colonial_orientalist_reading, scaffold).
narrative_ontology:human_readable(vedic_corpus_social_prescription__colonial_orientalist_reading, "Colonial Codification of 'Hindu Law'").
narrative_ontology:topic_domain(vedic_corpus_social_prescription__colonial_orientalist_reading, "religious_studies/social_stratification/hermeneutics").

domain_priors:requires_active_enforcement(vedic_corpus_social_prescription__colonial_orientalist_reading).
narrative_ontology:has_sunset_clause(vedic_corpus_social_prescription__colonial_orientalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_corpus_social_prescription__colonial_orientalist_reading, '1c279597-73f2-4821-9ab1-92330b0546dd').
narrative_ontology:cs_kernel_codification('1c279597-73f2-4821-9ab1-92330b0546dd', formalized).
narrative_ontology:cs_authority_grounding('1c279597-73f2-4821-9ab1-92330b0546dd', extraction).
narrative_ontology:cs_interpretation_layer_present('1c279597-73f2-4821-9ab1-92330b0546dd').
narrative_ontology:cs_reading_relation('1c279597-73f2-4821-9ab1-92330b0546dd', vedic_corpus_social_prescription__orthodox_varna_reading, influences).
narrative_ontology:cs_reading_relation('1c279597-73f2-4821-9ab1-92330b0546dd', vedic_corpus_social_prescription__reformist_spiritual_reading, influences).
narrative_ontology:cs_axiom('1c279597-73f2-4821-9ab1-92330b0546dd', foundational, vedic_texts_constitute_unified_law).
narrative_ontology:cs_axiom_status(vedic_texts_constitute_unified_law, holdable).
narrative_ontology:cs_axiom_grounding('1c279597-73f2-4821-9ab1-92330b0546dd', vedic_texts_constitute_unified_law, conventional).
narrative_ontology:cs_axiom('1c279597-73f2-4821-9ab1-92330b0546dd', foundational, codification_is_administrative_necessity).
narrative_ontology:cs_axiom_status(codification_is_administrative_necessity, holdable).
narrative_ontology:cs_axiom_grounding('1c279597-73f2-4821-9ab1-92330b0546dd', codification_is_administrative_necessity, instrumental).
narrative_ontology:cs_reference_frame('1c279597-73f2-4821-9ab1-92330b0546dd', colonial_administrative_legibility).
narrative_ontology:cs_drift_state('1c279597-73f2-4821-9ab1-92330b0546dd', post_independence_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('1c279597-73f2-4821-9ab1-92330b0546dd', '').
narrative_ontology:cs_kernel_id(vedic_corpus_social_prescription__colonial_orientalist_reading, vedic_corpus_social_prescription).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__colonial_orientalist_reading, colonial_administration).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__colonial_orientalist_reading, colonized_legal_subjects).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__colonial_orientalist_reading, indigenous_legal_traditions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__colonial_orientalist_reading, orientalist_scholars).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sought to create a legible and administrable legal system for governance, taxation, and dispute resolution by codifying what it perceived as a unified 'Hindu law' based on selected Vedic and Dharmashastra texts. Benefited from simplified administration and enhanced control.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, colonial_administration, agenda_setter,
    institutional, generational, arbitrage, regional).

% Were subjected to a rigid, codified legal system that often misrepresented or ignored their diverse, fluid, and localized customary laws. Their social status and legal rights became fixed by colonial interpretations, leading to new forms of social stratification and injustice.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, colonized_legal_subjects, payer,
    powerless, generational, trapped, local).

% Were largely ignored or suppressed in favor of a singular, codified 'Hindu law'. Their fluid, context-dependent, and orally transmitted legal practices were deemed illegitimate or primitive, leading to their marginalization and eventual erosion.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, indigenous_legal_traditions, excluded,
    powerless, civilizational, identity_locked, local).

% Provided the intellectual framework and textual interpretations that supported the colonial project of codification. Their careers and academic authority were enhanced by their role in 'discovering' and systematizing 'Hindu law' for the administration.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, orientalist_scholars, beneficiary,
    powerful, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aimed to coordinate legal administration across diverse populations by establishing a uniform, predictable legal framework, thereby simplifying governance for the colonial power.
% TRANSFER_FUNCTION: Transferred legal authority and interpretive power from diverse indigenous traditions to the colonial state and its appointed legal experts. It also transferred social fluidity into rigid, administratively convenient categories, often to the detriment of local communities.
% ABSENT_VOICES: The diverse indigenous legal practitioners, local community leaders, and those whose customary laws were ignored or suppressed. They would have argued for the recognition of plural legal systems and the contextual nature of social norms, rather than a monolithic 'Hindu law'.
% DISAPPEARANCE_RATIONALE: If the colonial codification of 'Hindu law' had never occurred, the legal landscape would have remained far more diverse and fluid, with local customary laws holding greater sway. Post-colonial legal systems would have developed along different lines, potentially incorporating more indigenous legal principles rather than inheriting a colonial construct.
% FOUNDING_PROBLEM: The colonial administration faced the problem of governing a vast and diverse population with myriad local customs and legal traditions, which it perceived as chaotic and difficult to administer.
% FOUNDING_PROBLEM_CORROBORATION: While the colonial administration claimed the problem was live (lack of unified law), post-colonial legal historians and scholars of indigenous legal systems widely corroborate that the 'problem' was largely a construct of colonial administrative convenience, and the diverse legal traditions were functional within their own contexts. The problem of 'chaos' was solved by imposing a new order, not by genuinely coordinating existing ones.
narrative_ontology:disappearance_verdict(vedic_corpus_social_prescription__colonial_orientalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_corpus_social_prescription__colonial_orientalist_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_corpus_social_prescription__colonial_orientalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(vedic_corpus_social_prescription__colonial_orientalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_corpus_social_prescription__colonial_orientalist_reading_tests).
:- end_tests(vedic_corpus_social_prescription__colonial_orientalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) reflects the administrative gains for the colonial power and the imposition of a foreign legal framework, which, while not purely extractive, served colonial interests. Suppression (0.70) was high due to the active suppression of diverse indigenous legal traditions and the enforcement of a singular, rigid code. The theater ratio (0.20) indicates that while there was a genuine administrative function, a significant portion of the effort was performative, aimed at legitimizing colonial rule through the guise of 'discovering' and 'restoring' ancient law. Accessibility collapse (0.60) was substantial as alternative legal systems were delegitimized. Resistance (0.50) was present in various forms, from local non-compliance to later nationalist movements challenging colonial legal authority.
 *
 * PERSPECTIVAL GAP:
 *   From the colonial administration's perspective, this was a necessary and beneficial coordination mechanism (a 'rope' or 'scaffold') for effective governance. From the perspective of colonized legal subjects and indigenous traditions, it was an imposed, extractive 'snare' that rigidified social hierarchies and undermined their autonomy. The engine's classification will reflect this divergence based on the declared roles and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   The colonial administration is the primary beneficiary, gaining administrative efficiency and control. Colonized legal subjects are the victims, bearing the costs of rigidified social structures and loss of legal autonomy. Indigenous legal traditions are excluded, their very existence challenged by the imposition of a singular 'Hindu law'. Orientalist scholars are beneficiaries through enhanced academic authority and career advancement.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate was tied to colonial governance. With the end of colonial rule, the specific mandate to codify 'Hindu law' for administrative purposes became obsolete. However, its legacy continues to influence post-colonial legal systems, demonstrating how a scaffold can leave behind enduring structures even after its original mandate has expired. The 'dead' status of the founding problem, coupled with the 'world_rearranges' verdict, signals a potential for zombie effects where the structure persists beyond its original justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unity_vs_diversity_of_texts,
    'Are the Vedic and Dharmashastra texts genuinely a unified legal corpus, or were they diverse, often contradictory, and context-dependent traditions?',
    'Comprehensive philological and historical analysis of the texts, comparing colonial interpretations with pre-colonial indigenous commentaries and legal practices.',
    'If diverse, the ''unified law'' claim is a colonial construct, increasing the constraint''s extractiveness and suppression by misrepresenting indigenous legal reality. If unified, the coordination function is more genuine.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unity_vs_diversity_of_texts, empirical, 'Ambiguity regarding the inherent unity or diversity of the source texts.').

omega_variable(
    administrative_necessity_vs_rent_seeking,
    'To what extent was the codification driven by genuine administrative necessity, versus serving as a tool for colonial control and resource extraction?',
    'Analysis of colonial administrative records, economic policies, and the impact of codification on local economies and social structures.',
    'If primarily administrative necessity, the scaffold classification is stronger. If primarily control/extraction, the constraint leans more towards a snare, with the ''scaffold'' framing serving as cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(administrative_necessity_vs_rent_seeking, empirical, 'Distinguishing genuine administrative need from colonial rent-seeking.').

omega_variable(
    sunset_clause_effectiveness,
    'Did the ''sunset clause'' (end of colonial rule) genuinely resolve the constraint, or did its codified structures persist and influence post-colonial legal systems?',
    'Comparative legal analysis of post-colonial legal reforms and the continued application of codified ''Hindu law'' principles in independent India.',
    'If the structures persisted, the ''scaffold'' classification is weakened, as the temporary nature was not fully realized, suggesting a longer-term, more entrenched constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sunset_clause_effectiveness, empirical, 'The long-term impact and persistence of colonial legal structures post-independence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_corpus_social_prescription__colonial_orientalist_reading, 1757, 1947).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t1757, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 1757, 0.1).
narrative_ontology:measurement(vedi_tr_t1800, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 1800, 0.15).
narrative_ontology:measurement(vedi_tr_t1850, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 1850, 0.18).
narrative_ontology:measurement(vedi_tr_t1900, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 1900, 0.2).
narrative_ontology:measurement(vedi_tr_t1947, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 1947, 0.2).

% Extraction over time
narrative_ontology:measurement(vedi_be_t1757, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 1757, 0.3).
narrative_ontology:measurement(vedi_be_t1800, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 1800, 0.35).
narrative_ontology:measurement(vedi_be_t1850, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 1850, 0.4).
narrative_ontology:measurement(vedi_be_t1900, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 1900, 0.43).
narrative_ontology:measurement(vedi_be_t1947, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 1947, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t1757, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 1757, 0.5).
narrative_ontology:measurement(vedi_su_t1800, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 1800, 0.6).
narrative_ontology:measurement(vedi_su_t1850, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 1850, 0.65).
narrative_ontology:measurement(vedi_su_t1900, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 1900, 0.68).
narrative_ontology:measurement(vedi_su_t1947, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 1947, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_corpus_social_prescription__colonial_orientalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__colonial_orientalist_reading, orthodox_varna_reading).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__colonial_orientalist_reading, reformist_spiritual_reading).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__colonial_orientalist_reading, post_colonial_personal_law_systems).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'vedic_corpus_social_prescription' kernel. Its codification influenced the subsequent interpretation and contestation of Vedic texts, creating a fixed legal referent that other readings had to contend with.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
