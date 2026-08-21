% ============================================================================
% CONSTRAINT STORY: dharmasastra_corpus__orthodox_literalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dharmasastra_corpus__orthodox_literalist, []).

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
 *   constraint_id: dharmasastra_corpus__orthodox_literalist
 *   human_readable: Dharmasastra: Orthodox Literalist Reading (Eternal, Revealed Truth)
 *   domain: religious_law/textual_interpretation/normative_authority
 *
 * SUMMARY:
 *   This constraint represents the orthodox, literalist reading of
 *   Dharmasastra, asserting its prescriptions (especially the varna/jati
 *   hierarchy) as eternal, revealed truth requiring strict observance. This
 *   reading is characterized by high extraction from lower castes and women,
 *   enforced through social, ritual, and sometimes physical coercion. It is a
 *   specific interpretation of a broader textual corpus, distinct from
 *   reformist or abolitionist readings. The claimed type is 'snare' because
 *   the coordination story (cosmic order) is cover for systematic extraction
 *   and suppression of alternatives.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dharmasastra_corpus__orthodox_literalist, 0.9).
domain_priors:suppression_score(dharmasastra_corpus__orthodox_literalist, 0.95).
domain_priors:theater_ratio(dharmasastra_corpus__orthodox_literalist, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, extractiveness, 0.9).
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dharmasastra_corpus__orthodox_literalist, snare).
narrative_ontology:human_readable(dharmasastra_corpus__orthodox_literalist, "Dharmasastra: Orthodox Literalist Reading (Eternal, Revealed Truth)").
narrative_ontology:topic_domain(dharmasastra_corpus__orthodox_literalist, "religious_law/textual_interpretation/normative_authority").

domain_priors:requires_active_enforcement(dharmasastra_corpus__orthodox_literalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dharmasastra_corpus__orthodox_literalist, 'e805cf8e-4bee-4789-8c4e-76a0007da1d5').
narrative_ontology:cs_kernel_codification('e805cf8e-4bee-4789-8c4e-76a0007da1d5', fixed_text).
narrative_ontology:cs_authority_grounding('e805cf8e-4bee-4789-8c4e-76a0007da1d5', lineage).
narrative_ontology:cs_interpretation_layer_present('e805cf8e-4bee-4789-8c4e-76a0007da1d5').
narrative_ontology:cs_reading_relation('e805cf8e-4bee-4789-8c4e-76a0007da1d5', dharmasastra_corpus__reformist_contextual, coexists_with).
narrative_ontology:cs_reading_relation('e805cf8e-4bee-4789-8c4e-76a0007da1d5', dharmasastra_corpus__abolitionist_rejection, forecloses).
narrative_ontology:cs_axiom('e805cf8e-4bee-4789-8c4e-76a0007da1d5', foundational, dharmasastra_is_eternal_revelation).
narrative_ontology:cs_axiom_status(dharmasastra_is_eternal_revelation, holdable).
narrative_ontology:cs_axiom_grounding('e805cf8e-4bee-4789-8c4e-76a0007da1d5', dharmasastra_is_eternal_revelation, theological).
narrative_ontology:cs_axiom('e805cf8e-4bee-4789-8c4e-76a0007da1d5', foundational, varna_jati_hierarchy_is_divinely_ordained).
narrative_ontology:cs_axiom_status(varna_jati_hierarchy_is_divinely_ordained, holdable).
narrative_ontology:cs_axiom_grounding('e805cf8e-4bee-4789-8c4e-76a0007da1d5', varna_jati_hierarchy_is_divinely_ordained, theological).
narrative_ontology:cs_reference_frame('e805cf8e-4bee-4789-8c4e-76a0007da1d5', ancient_vedic_social_order).
narrative_ontology:cs_drift_state('e805cf8e-4bee-4789-8c4e-76a0007da1d5', contemporary_globalized_society, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('e805cf8e-4bee-4789-8c4e-76a0007da1d5', '').
narrative_ontology:cs_kernel_id(dharmasastra_corpus__orthodox_literalist, dharmasastra_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__orthodox_literalist, brahmin_priestly_class).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__orthodox_literalist, upper_caste_men).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, dalits).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, shudras).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, women_of_all_castes).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, lower_caste_men).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__orthodox_literalist, varna_jati_hierarchy_doctrine).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__orthodox_literalist, divine_revelation_of_dharma).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets, transmits, and enforces Dharmasastra as eternal, revealed truth. Benefits from the ritual and social authority derived from this interpretation, which places them at the apex of the varna/jati hierarchy. Actively suppresses alternative interpretations and practices.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, brahmin_priestly_class, agenda_setter,
    institutional, generational, arbitrage, regional).

% Benefit from social status, ritual purity, and access to resources and education prescribed by the Dharmasastra. While some may question aspects, their overall position is enhanced by the orthodox reading.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, upper_caste_men, beneficiary,
    powerful, biographical, mobile, regional).

% Are systematically excluded from ritual, education, and social mobility, facing severe discrimination and violence based on Dharmasastra-derived caste prescriptions. Their identity is locked into a position of extreme disadvantage by this reading.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, dalits, payer,
    powerless, generational, trapped, local).

% Are assigned roles of service and manual labor, denied access to Vedic study and certain rituals. Bear significant social and economic costs, with limited avenues for upward mobility within the orthodox framework.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, shudras, payer,
    powerless, generational, identity_locked, local).

% Are generally denied independent agency, access to Vedic education, and ritual authority, often confined to domestic roles. Their status is subordinate to men, regardless of caste, under this interpretation.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, women_of_all_castes, payer,
    powerless, biographical, identity_locked, regional).

% Face restrictions on social interaction, marriage, and occupation, though less severe than Dalits. Their opportunities are limited by the hierarchical structure, and they bear the costs of maintaining the system's lower tiers.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, lower_caste_men, payer,
    moderate, biographical, constrained, local).

% Propose contextual or ethical interpretations of Dharmasastra, challenging the literalist view of hierarchy. They are often marginalized or condemned by orthodox institutions, facing social and academic exclusion.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, reformist_scholars, excluded,
    moderate, generational, constrained, global).

% Document and challenge the human rights violations stemming from caste discrimination and gender inequality perpetuated by orthodox interpretations of Dharmasastra. They seek legal and social reforms.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, human_rights_advocates, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a rigid social order and ritual purity system, coordinating roles and duties within a divinely ordained cosmic framework, providing a sense of stability and meaning for adherents.
% TRANSFER_FUNCTION: Transfers social status, ritual authority, economic resources, and educational opportunities from lower castes and women to upper castes and men, based on birth and gender.
% ABSENT_VOICES: Abolitionist voices, who reject the entire Dharmasastra framework as inherently oppressive, are systematically excluded from any legitimate discourse within orthodox institutions. Their arguments for dismantling the system are dismissed as external or heretical.
% DISAPPEARANCE_RATIONALE: If the orthodox literalist interpretation of Dharmasastra vanished overnight, the social, ritual, and economic structures it underpins would collapse. Caste-based discrimination would lose its 'divine' justification, leading to massive social upheaval, re-negotiation of power, and potential for greater equality, though likely with significant friction.
% FOUNDING_PROBLEM: To establish a stable, divinely sanctioned social order (dharma) and guide individuals in righteous conduct, ensuring cosmic harmony and individual spiritual progress.
% FOUNDING_PROBLEM_CORROBORATION: Orthodox adherents attest the problem is live, citing moral decay and social disorder when dharma is not strictly observed. Reformist and abolitionist scholars, along with human rights organizations, attest that the 'problem' has shifted from cosmic order to social oppression, and the original justification is now a cover for extraction; historical and sociological analyses from outside the benefiting parties corroborate this shift.
narrative_ontology:disappearance_verdict(dharmasastra_corpus__orthodox_literalist, world_rearranges).
narrative_ontology:founding_problem_status(dharmasastra_corpus__orthodox_literalist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dharmasastra_corpus__orthodox_literalist, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(dharmasastra_corpus__orthodox_literalist, 'none', 1).
narrative_ontology:epsilon_provenance(dharmasastra_corpus__orthodox_literalist, 0.9, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dharmasastra_corpus__orthodox_literalist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dharmasastra_corpus__orthodox_literalist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dharmasastra_corpus__orthodox_literalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.9) due to the systematic deprivation of rights, resources, and dignity from large segments of the population. Suppression is also extremely high (0.95) as the system relies on deep-seated social norms, religious authority, and active exclusion to prevent dissent or exit. Accessibility collapse is high (0.8) because alternatives are culturally and socially foreclosed. Resistance is significant (0.7) from those targeted by the system, but often met with severe backlash. Theater ratio is low (0.1) because the system is actively functional in its extractive purpose, not merely performative.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Brahmin priestly class, this is a divinely ordained, stable social order (claimed as a Mountain or Rope). From the perspective of Dalits and women, it is a brutal, inescapable Snare. The engine's computation will highlight this divergence, showing a claimed 'snare' from the author's seat, while the beneficiaries' computed type would be much closer to a 'rope' or even 'mountain' due to their structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   The Brahmin priestly class and upper-caste men are clear beneficiaries, deriving immense social, ritual, and economic power (d near 0.0). Dalits, Shudras, and women of all castes are primary victims, bearing the brunt of the system's extraction and suppression (d near 1.0). Lower-caste men are also victims, though with slightly more agency than Dalits. Reformist scholars and human rights advocates are excluded or observers, challenging the system from outside its internal logic.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_revelation_ambiguity,
    'Is the Dharmasastra truly eternal, revealed truth, or a human-authored text reflecting historical power structures?',
    'Historical-critical textual analysis, archaeological evidence, and comparative religious studies to trace the evolution and authorship of the texts, alongside theological debate on the nature of revelation.',
    'If proven human-authored and historically contingent, the ''mountain'' claim of this reading collapses, reclassifying it as a constructed Snare. If divine revelation is affirmed, its legitimacy within its own framework is strengthened, though its ethical implications remain contested.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(divine_revelation_ambiguity, conceptual, 'Ambiguity regarding the ontological status of Dharmasastra texts.').

omega_variable(
    internalized_suppression_proportion,
    'What proportion of the measured suppression is structural (external barriers) versus internalized (cognitive patterns, identity fusion) among the victim groups?',
    'Sociological studies on post-exit trajectories of individuals who leave caste-bound communities, psychological research on identity formation under oppressive systems, and ethnographic accounts of resistance and resilience.',
    'If a high proportion is internalized, the effective suppression is higher than structural measures suggest, as victims carry the suppression with them even after physical exit. This would deepen the ''snare'' classification by highlighting the difficulty of true liberation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_proportion, empirical, 'Structural vs. internalized suppression mechanism in caste system.').

omega_variable(
    mandatrophy_of_cosmic_order,
    'Has the original ''founding problem'' of establishing cosmic order (dharma) atrophied, and does the constraint now primarily serve to maintain social hierarchy and extraction?',
    'Sociological analysis of contemporary social functions versus historical justifications, and ethical evaluation of whether the ''order'' produced is just or merely stable. Corroboration from non-beneficiary scholars and activists.',
    'If the founding problem is ''dead'' and the constraint persists for extraction, it strengthens the ''snare'' classification and highlights a severe case of mandatrophy, where the original mandate is a cover for ongoing harm.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandatrophy_of_cosmic_order, empirical, 'Whether the constraint''s original purpose has been superseded by extractive functions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dharmasastra_corpus__orthodox_literalist, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dhar_tr_t0, dharmasastra_corpus__orthodox_literalist, theater_ratio, 0, 0.1).
narrative_ontology:measurement(dhar_tr_t10, dharmasastra_corpus__orthodox_literalist, theater_ratio, 10, 0.1).
narrative_ontology:measurement(dhar_tr_t20, dharmasastra_corpus__orthodox_literalist, theater_ratio, 20, 0.1).
narrative_ontology:measurement(dhar_tr_t30, dharmasastra_corpus__orthodox_literalist, theater_ratio, 30, 0.1).
narrative_ontology:measurement(dhar_tr_t40, dharmasastra_corpus__orthodox_literalist, theater_ratio, 40, 0.1).
narrative_ontology:measurement(dhar_tr_t50, dharmasastra_corpus__orthodox_literalist, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(dhar_be_t0, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(dhar_be_t10, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 10, 0.87).
narrative_ontology:measurement(dhar_be_t20, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 20, 0.88).
narrative_ontology:measurement(dhar_be_t30, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 30, 0.89).
narrative_ontology:measurement(dhar_be_t40, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 40, 0.9).
narrative_ontology:measurement(dhar_be_t50, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 50, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(dhar_su_t0, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 0, 0.9).
narrative_ontology:measurement(dhar_su_t10, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 10, 0.92).
narrative_ontology:measurement(dhar_su_t20, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 20, 0.93).
narrative_ontology:measurement(dhar_su_t30, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 30, 0.94).
narrative_ontology:measurement(dhar_su_t40, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 40, 0.95).
narrative_ontology:measurement(dhar_su_t50, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 50, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dharmasastra_corpus__orthodox_literalist, identity_coordination).
narrative_ontology:affects_constraint(dharmasastra_corpus__orthodox_literalist, dharmasastra_corpus__reformist_contextual).
narrative_ontology:affects_constraint(dharmasastra_corpus__orthodox_literalist, dharmasastra_corpus__abolitionist_rejection).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Dharmasastra corpus kernel. This 'orthodox_literalist' reading asserts the texts as eternal, revealed truth, emphasizing literal observance of hierarchy. It is linked to 'reformist_contextual' and 'abolitionist_rejection' readings, which offer alternative interpretations or outright rejections of the corpus's authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
