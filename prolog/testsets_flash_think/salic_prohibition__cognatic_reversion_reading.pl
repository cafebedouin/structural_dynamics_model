% ============================================================================
% CONSTRAINT STORY: salic_prohibition__cognatic_reversion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_salic_prohibition__cognatic_reversion_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: salic_prohibition__cognatic_reversion_reading
 *   human_readable: Salic Law as Anachronistic, Non-Binding on Non-Frankish Territories (Cognatic Reversion Reading)
 *   domain: constitutional_law/dynastic_succession/political_history
 *
 * SUMMARY:
 *   This constraint story instantiates the 'cognatic_reversion_reading' of
 *   the 'salic_prohibition' kernel. It argues that Salic Law, originally a
 *   Frankish land tenure rule, was anachronistic and never properly binding
 *   on non-Frankish territories when applied to dynastic succession. This
 *   reading prioritizes female succession via cognatic primogeniture and the
 *   integrity of the realm over strict agnatic purity, framing the Salic
 *   prohibition as an illegitimate imposition rather than a fundamental law.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(salic_prohibition__cognatic_reversion_reading, 0.25).
domain_priors:suppression_score(salic_prohibition__cognatic_reversion_reading, 0.3).
domain_priors:theater_ratio(salic_prohibition__cognatic_reversion_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(salic_prohibition__cognatic_reversion_reading, rope).
narrative_ontology:human_readable(salic_prohibition__cognatic_reversion_reading, "Salic Law as Anachronistic, Non-Binding on Non-Frankish Territories (Cognatic Reversion Reading)").
narrative_ontology:topic_domain(salic_prohibition__cognatic_reversion_reading, "constitutional_law/dynastic_succession/political_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(salic_prohibition__cognatic_reversion_reading, '39a3f5f5-7219-43ec-9e01-36f05357ff90').
narrative_ontology:cs_kernel_codification('39a3f5f5-7219-43ec-9e01-36f05357ff90', fixed_text).
narrative_ontology:cs_authority_grounding('39a3f5f5-7219-43ec-9e01-36f05357ff90', practice).
narrative_ontology:cs_interpretation_layer_present('39a3f5f5-7219-43ec-9e01-36f05357ff90').
narrative_ontology:cs_reading_relation('39a3f5f5-7219-43ec-9e01-36f05357ff90', salic_prohibition__immutable_mandate_reading, forecloses).
narrative_ontology:cs_reading_relation('39a3f5f5-7219-43ec-9e01-36f05357ff90', salic_prohibition__sovereign_override_reading, coexists_with).
narrative_ontology:cs_axiom('39a3f5f5-7219-43ec-9e01-36f05357ff90', foundational, cognatic_primogeniture_legitimacy).
narrative_ontology:cs_axiom_status(cognatic_primogeniture_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('39a3f5f5-7219-43ec-9e01-36f05357ff90', cognatic_primogeniture_legitimacy, deontological).
narrative_ontology:cs_axiom('39a3f5f5-7219-43ec-9e01-36f05357ff90', foundational, territorial_integrity_supremacy).
narrative_ontology:cs_axiom_status(territorial_integrity_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('39a3f5f5-7219-43ec-9e01-36f05357ff90', territorial_integrity_supremacy, conventional).
narrative_ontology:cs_reference_frame('39a3f5f5-7219-43ec-9e01-36f05357ff90', enlightenment_constitutionalism).
narrative_ontology:cs_drift_state('39a3f5f5-7219-43ec-9e01-36f05357ff90', contemporary_constitutional_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('39a3f5f5-7219-43ec-9e01-36f05357ff90', '').
narrative_ontology:cs_kernel_id(salic_prohibition__cognatic_reversion_reading, salic_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(salic_prohibition__cognatic_reversion_reading, female_heirs).
narrative_ontology:constraint_beneficiary(salic_prohibition__cognatic_reversion_reading, territorial_integrity_advocates).
narrative_ontology:constraint_beneficiary(salic_prohibition__cognatic_reversion_reading, constitutional_monarchists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(salic_prohibition__cognatic_reversion_reading, agnatic_dynastic_claimants).
narrative_ontology:constraint_victim(salic_prohibition__cognatic_reversion_reading, traditionalist_nobility).
narrative_ontology:constraint_vindicates(salic_prohibition__cognatic_reversion_reading, cognatic_primogeniture_principle).
narrative_ontology:constraint_vindicates(salic_prohibition__cognatic_reversion_reading, territorial_integrity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Historically excluded from succession by Salic Law, this reading would restore their right to inherit the throne based on birth order, regardless of sex.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, female_heirs, beneficiary,
    powerless, biographical, trapped, national).

% Prioritize the stability and unity of the realm, arguing that succession rules should prevent dynastic crises or fragmentation, which cognatic primogeniture often supports by providing a clearer line.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, territorial_integrity_advocates, beneficiary,
    organized, generational, constrained, national).

% Seek to modernize and legitimize monarchical institutions by aligning succession laws with contemporary principles of equality and constitutional governance, thereby strengthening public support for the monarchy.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, constitutional_monarchists, agenda_setter,
    institutional, generational, constrained, national).

% Would lose their exclusive claim to succession based solely on male lineage, as this reading opens the line to female heirs, potentially displacing them in the order of precedence.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, agnatic_dynastic_claimants, payer,
    powerful, generational, constrained, national).

% Resist changes to long-established dynastic norms and traditions, viewing the Salic prohibition as a fundamental aspect of their historical identity and social order.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, traditionalist_nobility, payer,
    powerful, generational, constrained, national).

% Analyze the historical origins, evolution, and application of Salic Law, providing critical interpretations of its original intent and later adaptations, often supporting the anachronistic reading.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, historical_legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a clear and legitimate line of succession that prioritizes territorial stability and modern principles of inheritance over anachronistic gender-based exclusion, thereby coordinating dynastic and national identity.
% TRANSFER_FUNCTION: Transfers the right to succession from exclusively male heirs to the eldest child regardless of sex, and shifts the basis of dynastic legitimacy from ancient Frankish custom to broader constitutional principles.
% ABSENT_VOICES: Historically, the voices of female heirs and their supporters were suppressed by the prevailing agnatic legal frameworks. Modern constitutional theorists who advocate for gender equality in all public roles would object to the original Salic prohibition.
% DISAPPEARANCE_RATIONALE: If the Salic prohibition (and the debate around its applicability) vanished, many European monarchies would have different historical and current lines of succession, potentially altering national identities and political landscapes. The legal basis for many dynastic claims would shift, and the principle of cognatic primogeniture would be universally accepted.
% FOUNDING_PROBLEM: The original Salic Law aimed to prevent land from passing out of Frankish families through female inheritance, and later became a tool to exclude female rulers from dynastic succession, particularly in non-Frankish territories where it was adopted.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and constitutional scholars, independent of dynastic claimants, corroborate that the original intent of Salic Law was tied to specific Frankish land tenure and later reinterpreted for dynastic exclusion, a problem now largely considered anachronistic or unjust by modern legal principles. Legislative hearings and public discourse in constitutional monarchies also support this view.
narrative_ontology:disappearance_verdict(salic_prohibition__cognatic_reversion_reading, world_rearranges).
narrative_ontology:founding_problem_status(salic_prohibition__cognatic_reversion_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(salic_prohibition__cognatic_reversion_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(salic_prohibition__cognatic_reversion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(salic_prohibition__cognatic_reversion_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(salic_prohibition__cognatic_reversion_reading_tests).
:- end_tests(salic_prohibition__cognatic_reversion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.25) and suppression (0.30) reflect this reading's challenge to the Salic prohibition's legitimacy and its efforts to dismantle its extractive and suppressive effects on female heirs. The resistance (0.70) is high because this reading actively contests a deeply entrenched historical and legal tradition. The theater ratio (0.40) is moderate, reflecting the ongoing performative aspects of dynastic claims and legal interpretations, even as the underlying principles are debated. The claimed type is 'rope' because this reading seeks to coordinate a more legitimate and stable succession system, benefiting a broader set of stakeholders.
 *
 * PERSPECTIVAL GAP:
 *   This reading fundamentally diverges from the 'immutable_mandate_reading' which views Salic Law as an unchangeable, universally binding principle. While the 'sovereign_override_reading' acknowledges the law's revisability, this 'cognatic_reversion_reading' goes further by questioning its original legitimacy and scope, not just its current status.
 *
 * DIRECTIONALITY LOGIC:
 *   Female heirs and advocates for territorial integrity are beneficiaries, as this reading would grant them rights and stability. Agnatic dynastic claimants and traditionalist nobility are payers, as they would lose their exclusive claims and see established norms challenged. Constitutional monarchists are agenda-setters, seeking to implement this modernized interpretation. Historical legal scholars act as observers, providing analytical context.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading implicitly argues that the application of Salic Law to dynastic succession outside its original Frankish context is a mandatrophic constraint. The original mandate (preventing land alienation) is long dead, but the rule persists as a tool for dynastic exclusion. This reading seeks to resolve this mandatrophy by reasserting the original, limited scope of the law and prioritizing contemporary constitutional values.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    salic_law_historical_applicability,
    'Was Salic Law genuinely anachronistic and non-binding outside Frankish territories, or did its adoption by other dynasties grant it new, legitimate force through customary or positive law?',
    'Detailed historical-legal analysis of specific dynastic adoptions, including the motivations for adoption, the legal instruments used, and the extent of contemporary challenge to its legitimacy in those contexts.',
    'If its adoption was widely accepted and legally formalized, this would weaken the ''anachronistic'' claim, potentially increasing the extractiveness and suppression of the Salic prohibition in those territories. If it was consistently contested or weakly applied, it strengthens this reading''s claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(salic_law_historical_applicability, empirical, 'Ambiguity regarding the legitimate scope and binding force of Salic Law beyond its Frankish origins.').

omega_variable(
    territorial_integrity_vs_agnatic_purity,
    'Is the preservation of territorial integrity (often supported by cognatic succession) a higher constitutional value than strict agnatic dynastic purity (which Salic Law enforces)?',
    'Analysis of constitutional preambles, historical legal debates, and judicial rulings in relevant monarchies, focusing on explicit declarations of foundational values. Public opinion surveys on preferred succession principles could also provide insight into societal values.',
    'If territorial integrity is widely recognized as a superior value, it strengthens the legitimacy of this reading and reduces the perceived extraction of cognatic succession. If agnatic purity is still held as paramount, this reading''s arguments face greater resistance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(territorial_integrity_vs_agnatic_purity, preference, 'Contest over the hierarchy of constitutional values in dynastic succession.').

omega_variable(
    kernel_reading_identity,
    'This constraint is the ''cognatic_reversion_reading'' of the ''salic_prohibition'' kernel. What are the specific structural elements that differentiate this reading from its siblings?',
    'Comparative analysis of the ''immutable_mandate_reading'' and ''sovereign_override_reading'' to precisely map their core axioms, authority groundings, and claimed beneficiaries/victims, highlighting the points of structural divergence.',
    'Clarifies the precise nature of the contestation within the ''salic_prohibition'' kernel, enabling more accurate classification of each reading and their interrelationships.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Identification of this constraint as one specific reading within a contested kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(salic_prohibition__cognatic_reversion_reading, 1700, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sali_tr_t1700, salic_prohibition__cognatic_reversion_reading, theater_ratio, 1700, 0.3).
narrative_ontology:measurement(sali_tr_t1740, salic_prohibition__cognatic_reversion_reading, theater_ratio, 1740, 0.33).
narrative_ontology:measurement(sali_tr_t1780, salic_prohibition__cognatic_reversion_reading, theater_ratio, 1780, 0.36).
narrative_ontology:measurement(sali_tr_t1820, salic_prohibition__cognatic_reversion_reading, theater_ratio, 1820, 0.38).
narrative_ontology:measurement(sali_tr_t1860, salic_prohibition__cognatic_reversion_reading, theater_ratio, 1860, 0.39).
narrative_ontology:measurement(sali_tr_t1900, salic_prohibition__cognatic_reversion_reading, theater_ratio, 1900, 0.4).

% Extraction over time
narrative_ontology:measurement(sali_be_t1700, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 1700, 0.35).
narrative_ontology:measurement(sali_be_t1740, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 1740, 0.3).
narrative_ontology:measurement(sali_be_t1780, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 1780, 0.28).
narrative_ontology:measurement(sali_be_t1820, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 1820, 0.27).
narrative_ontology:measurement(sali_be_t1860, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 1860, 0.26).
narrative_ontology:measurement(sali_be_t1900, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 1900, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(sali_su_t1700, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 1700, 0.4).
narrative_ontology:measurement(sali_su_t1740, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 1740, 0.35).
narrative_ontology:measurement(sali_su_t1780, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 1780, 0.33).
narrative_ontology:measurement(sali_su_t1820, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 1820, 0.32).
narrative_ontology:measurement(sali_su_t1860, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 1860, 0.31).
narrative_ontology:measurement(sali_su_t1900, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 1900, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(salic_prohibition__cognatic_reversion_reading, identity_coordination).
narrative_ontology:affects_constraint(salic_prohibition__cognatic_reversion_reading, constitutional_monarchy_legitimacy).
narrative_ontology:affects_constraint(salic_prohibition__cognatic_reversion_reading, gender_equality_in_public_office).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'salic_prohibition' kernel. This reading focuses on the historical anachronism and limited applicability of Salic Law, advocating for cognatic succession and territorial integrity. It is linked to 'salic_prohibition__immutable_mandate_reading' and 'salic_prohibition__sovereign_override_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
