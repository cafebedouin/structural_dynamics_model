% ============================================================================
% CONSTRAINT STORY: dharmasastra_corpus__abolitionist_rejection
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dharmasastra_corpus__abolitionist_rejection, []).

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
 *   constraint_id: dharmasastra_corpus__abolitionist_rejection
 *   human_readable: Dharmasastra Corpus (Abolitionist Rejection Reading)
 *   domain: religious_law/textual_interpretation/normative_authority
 *
 * SUMMARY:
 *   This constraint represents the abolitionist rejection reading of the
 *   Dharmasastra corpus, which views the texts as fundamentally oppressive
 *   and lacking any legitimate authority in contemporary society. It asserts
 *   that the caste system and the textual framework must be wholly abandoned.
 *   The reading identifies the Dharmasastra as a snare, actively extracting
 *   from and suppressing Dalits, lower castes, and women, with no genuine
 *   coordination function remaining. The high extractiveness and suppression
 *   reflect the severe impact of the caste system and associated patriarchal
 *   norms.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dharmasastra_corpus__abolitionist_rejection, 0.95).
domain_priors:suppression_score(dharmasastra_corpus__abolitionist_rejection, 0.9).
domain_priors:theater_ratio(dharmasastra_corpus__abolitionist_rejection, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, extractiveness, 0.95).
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dharmasastra_corpus__abolitionist_rejection, snare).
narrative_ontology:human_readable(dharmasastra_corpus__abolitionist_rejection, "Dharmasastra Corpus (Abolitionist Rejection Reading)").
narrative_ontology:topic_domain(dharmasastra_corpus__abolitionist_rejection, "religious_law/textual_interpretation/normative_authority").

domain_priors:requires_active_enforcement(dharmasastra_corpus__abolitionist_rejection).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dharmasastra_corpus__abolitionist_rejection, '0176a081-1a96-4fe4-a5ba-9f1cc41f8380').
narrative_ontology:cs_kernel_codification('0176a081-1a96-4fe4-a5ba-9f1cc41f8380', fixed_text).
narrative_ontology:cs_authority_grounding('0176a081-1a96-4fe4-a5ba-9f1cc41f8380', extraction).
narrative_ontology:cs_interpretation_layer_present('0176a081-1a96-4fe4-a5ba-9f1cc41f8380').
narrative_ontology:cs_reading_relation('0176a081-1a96-4fe4-a5ba-9f1cc41f8380', dharmasastra_corpus__orthodox_literalist, forecloses).
narrative_ontology:cs_reading_relation('0176a081-1a96-4fe4-a5ba-9f1cc41f8380', dharmasastra_corpus__reformist_contextual, forecloses).
narrative_ontology:cs_axiom('0176a081-1a96-4fe4-a5ba-9f1cc41f8380', foundational, dharmasastra_is_fundamentally_oppressive).
narrative_ontology:cs_axiom_status(dharmasastra_is_fundamentally_oppressive, holdable).
narrative_ontology:cs_axiom_grounding('0176a081-1a96-4fe4-a5ba-9f1cc41f8380', dharmasastra_is_fundamentally_oppressive, deontological).
narrative_ontology:cs_axiom('0176a081-1a96-4fe4-a5ba-9f1cc41f8380', foundational, no_legitimate_authority_remains).
narrative_ontology:cs_axiom_status(no_legitimate_authority_remains, holdable).
narrative_ontology:cs_axiom_grounding('0176a081-1a96-4fe4-a5ba-9f1cc41f8380', no_legitimate_authority_remains, empirically_contingent).
narrative_ontology:cs_reference_frame('0176a081-1a96-4fe4-a5ba-9f1cc41f8380', post_enlightenment_human_rights).
narrative_ontology:cs_drift_state('0176a081-1a96-4fe4-a5ba-9f1cc41f8380', contemporary_global_discourse, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('0176a081-1a96-4fe4-a5ba-9f1cc41f8380', '').
narrative_ontology:cs_kernel_id(dharmasastra_corpus__abolitionist_rejection, dharmasastra_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, dalits).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, lower_castes).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, women).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, religious_minorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for the complete dismantling of the caste system and the rejection of Dharmasastra as a legitimate source of authority. They analyze the texts as instruments of oppression and seek to empower victims to resist and exit the system.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, abolitionist_scholars_activists, observer,
    organized, generational, mobile, global).

% Bear the brunt of the caste system's discrimination, economic exploitation, and social exclusion, which they see as directly sanctioned by Dharmasastra. Their identity is often intertwined with their social position, making exit extremely difficult and costly.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, dalits, payer,
    powerless, biographical, identity_locked, local).

% Experience social and economic disadvantages, though often less severe than Dalits. They are subject to the hierarchical norms and restrictions derived from Dharmasastra, limiting their opportunities and social mobility.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, lower_castes, payer,
    powerless, biographical, constrained, local).

% Are subjected to patriarchal norms and restrictions on their autonomy, property rights, and social roles, as prescribed by Dharmasastra. Their social identity and family structures often make direct exit from these norms highly punitive.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, women, payer,
    powerless, biographical, identity_locked, local).

% Are often marginalized and discriminated against within societies structured by Dharmasastra-derived norms, facing social exclusion and legal disadvantages. Their ability to practice their faith freely can be constrained.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, religious_minorities, payer,
    powerless, biographical, constrained, local).

% Historically and currently interpret and enforce Dharmasastra, deriving their authority and social status from its preservation. They actively resist any reinterpretation or rejection that would undermine the traditional social order.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, orthodox_religious_authorities, agenda_setter,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading asserts no legitimate coordination function remains; any perceived order is a byproduct of enforced hierarchy.
% TRANSFER_FUNCTION: Transfers social status, economic resources, and political power from lower castes, women, and minorities to upper castes and patriarchal structures, legitimized by the textual framework.
% ABSENT_VOICES: The voices of those historically silenced and oppressed by the caste system, whose experiences are systematically excluded from traditional interpretations of Dharmasastra, are now being amplified by abolitionist movements.
% DISAPPEARANCE_RATIONALE: If the Dharmasastra corpus and its interpretive authority were wholly abandoned, the social, economic, and political structures built upon its hierarchical principles (especially the caste system) would lose their primary legitimizing framework, leading to a fundamental reorganization of society and power dynamics.
% FOUNDING_PROBLEM: The Dharmasastra corpus was constructed to establish and maintain social order, ritual purity, and a hierarchical social structure (varna/jati system) in ancient Indian society.
% FOUNDING_PROBLEM_CORROBORATION: Abolitionist scholars and activists, along with historical analysis, attest that the 'problem' of maintaining a rigid caste hierarchy is not a legitimate problem to solve in contemporary society. They argue that the original problem was itself a mechanism of oppression, and its persistence serves only to perpetuate injustice, not to solve a genuine societal need. Orthodox authorities, however, maintain the problem is still live, framing it as the preservation of dharma and cosmic order.
narrative_ontology:disappearance_verdict(dharmasastra_corpus__abolitionist_rejection, world_rearranges).
narrative_ontology:founding_problem_status(dharmasastra_corpus__abolitionist_rejection, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dharmasastra_corpus__abolitionist_rejection, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(dharmasastra_corpus__abolitionist_rejection, 'none', 1).
narrative_ontology:epsilon_provenance(dharmasastra_corpus__abolitionist_rejection, 0.95, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dharmasastra_corpus__abolitionist_rejection_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dharmasastra_corpus__abolitionist_rejection, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dharmasastra_corpus__abolitionist_rejection_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is extremely high (0.95) because the system sanctioned by Dharmasastra (caste, gender hierarchy) systematically denies rights, resources, and dignity to large segments of the population. Suppression is also very high (0.9) due to the deep institutionalization of caste discrimination, social ostracization, and the internalized nature of identity-locked positions. Theater ratio is low (0.05) because this reading sees little performative maintenance; the system is still actively functional in its oppressive capacity, not merely inertial. Resistance is high (0.9) reflecting the ongoing and intense struggle against caste discrimination and patriarchal norms.
 *
 * PERSPECTIVAL GAP:
 *   The abolitionist reading fundamentally diverges from orthodox and reformist interpretations. While orthodox views see the Dharmasastra as a source of cosmic order and legitimate authority, and reformist views seek to separate ethical core from historical context, the abolitionist reading sees only an instrument of oppression. This gap is not merely one of interpretation but of fundamental moral and structural assessment, leading to a classification of Snare where other readings might claim Mountain or Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   From this abolitionist perspective, there are no legitimate beneficiaries of the Dharmasastra corpus; any perceived benefits are a function of unearned privilege derived from an unjust system. The victims are clearly identified as Dalits, lower castes, women, and religious minorities, who bear the full cost of the system. Orthodox religious authorities are seen as agenda-setters who perpetuate the system for their own institutional power and social status, despite the abolitionist claim that their authority is illegitimate.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading asserts that the original mandate of Dharmasastra (establishing hierarchical social order) is not only dead but was inherently unjust. The persistence of the system, therefore, is pure extraction, not a degraded coordination function. The classification as a Snare prevents mislabeling it as a Piton (which would imply a lack of concentrated beneficiaries) or a Tangled Rope (which would imply a genuine, albeit extractive, coordination function).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_authority,
    'Does any legitimate authority remain for the Dharmasastra corpus in contemporary society, or is its persistence solely a function of power and inertia?',
    'Sociological analysis of adherence patterns, legal challenges to caste-based discrimination, and the outcomes of social movements advocating for its abolition. If adherence is primarily coercive or inertial, legitimacy is absent.',
    'If no legitimate authority remains, the constraint is a pure Snare. If some residual, non-coercive authority is identified (e.g., voluntary spiritual guidance), it might suggest a degraded Rope or Piton for those specific contexts, but not for the overall caste system.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_of_authority, conceptual, 'Whether the Dharmasastra holds any non-coercive authority.').

omega_variable(
    internalized_suppression_proportion,
    'What proportion of the measured suppression is structural (external barriers) versus internalized (cognitive patterns, identity fusion) for victims like Dalits and women?',
    'Post-exit trajectory analysis: if suppression persists (e.g., self-limiting beliefs, social stigma) after structural barriers are removed, the internalized component is significant. Qualitative sociological studies and psychological assessments.',
    'If internalized suppression is a major component, the effective suppression is higher and more resistant to external remedies, requiring deeper cultural and psychological interventions beyond legal or economic reforms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_proportion, empirical, 'Structural vs. internalized suppression mechanism for victims.').

omega_variable(
    caste_system_persistence_mechanism,
    'Is the persistence of the caste system primarily due to the direct influence of Dharmasastra texts, or has it become an autonomous social structure that merely references the texts for historical justification?',
    'Comparative analysis of societies with similar historical caste structures but differing textual authority. If the system persists strongly even where textual authority is weak, it suggests autonomy. Legal and ethnographic studies.',
    'If autonomous, dismantling the textual authority alone would be insufficient to dismantle the caste system, requiring more direct social and economic interventions. If text-dependent, textual rejection is a more direct path to abolition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(caste_system_persistence_mechanism, empirical, 'Direct textual influence vs. autonomous social structure in caste persistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dharmasastra_corpus__abolitionist_rejection, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dhar_tr_t1900, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 1900, 0.02).
narrative_ontology:measurement(dhar_tr_t1930, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 1930, 0.03).
narrative_ontology:measurement(dhar_tr_t1960, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 1960, 0.04).
narrative_ontology:measurement(dhar_tr_t1990, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 1990, 0.05).
narrative_ontology:measurement(dhar_tr_t2010, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 2010, 0.05).
narrative_ontology:measurement(dhar_tr_t2024, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(dhar_be_t1900, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 1900, 0.98).
narrative_ontology:measurement(dhar_be_t1930, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 1930, 0.97).
narrative_ontology:measurement(dhar_be_t1960, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 1960, 0.95).
narrative_ontology:measurement(dhar_be_t1990, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 1990, 0.93).
narrative_ontology:measurement(dhar_be_t2010, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 2010, 0.94).
narrative_ontology:measurement(dhar_be_t2024, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 2024, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(dhar_su_t1900, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 1900, 0.95).
narrative_ontology:measurement(dhar_su_t1930, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 1930, 0.93).
narrative_ontology:measurement(dhar_su_t1960, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 1960, 0.91).
narrative_ontology:measurement(dhar_su_t1990, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 1990, 0.88).
narrative_ontology:measurement(dhar_su_t2010, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 2010, 0.89).
narrative_ontology:measurement(dhar_su_t2024, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
