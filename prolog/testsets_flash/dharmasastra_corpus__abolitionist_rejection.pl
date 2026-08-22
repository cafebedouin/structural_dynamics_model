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
 *   constraint_id: dharmasastra_corpus__abolitionist_rejection
 *   human_readable: Dharmasastra Corpus (Abolitionist Rejection Reading)
 *   domain: religious_law/textual_interpretation/normative_authority
 *
 * SUMMARY:
 *   This constraint represents the 'abolitionist rejection' reading of the
 *   Dharmasastra corpus, which views the entire textual framework and its
 *   associated caste system as fundamentally oppressive. From this
 *   perspective, the Dharmasastra is a Snare, designed to extract labor,
 *   dignity, and autonomy from lower castes and women, with no legitimate
 *   coordination function. Its persistence relies on active suppression and
 *   the denial of alternatives, rather than any inherent naturalness or
 *   consensual agreement. The abolitionist stance demands the complete
 *   abandonment of the textual framework and the dismantling of the social
 *   hierarchy it underpins.
 *
 * KEY AGENTS:
 *   - dalits: Primary target (powerless/trapped) — bears extreme extraction and suppression.
 *   - lower_castes: Primary target (powerless/identity_locked) — bears significant extraction and suppression.
 *   - women: Primary target (powerless/identity_locked) — bears extraction and suppression across all castes.
 *   - brahmins_and_upper_castes: Historical beneficiaries (institutional/arbitrage) — benefited from the system's hierarchy.
 *   - abolitionist_activists: Agenda setter/Observer (organized/generational) — actively resists the constraint and advocates for its dismantling.
 *   - orthodox_scholars: Agenda setter (institutional/generational) — defends the traditional interpretation and authority of Dharmasastra.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dharmasastra_corpus__abolitionist_rejection, 0.95).
domain_priors:suppression_score(dharmasastra_corpus__abolitionist_rejection, 0.9).
domain_priors:theater_ratio(dharmasastra_corpus__abolitionist_rejection, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, extractiveness, 0.95).
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dharmasastra_corpus__abolitionist_rejection, snare).
narrative_ontology:human_readable(dharmasastra_corpus__abolitionist_rejection, "Dharmasastra Corpus (Abolitionist Rejection Reading)").
narrative_ontology:topic_domain(dharmasastra_corpus__abolitionist_rejection, "religious_law/textual_interpretation/normative_authority").

domain_priors:requires_active_enforcement(dharmasastra_corpus__abolitionist_rejection).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dharmasastra_corpus__abolitionist_rejection, '185d2436-76af-4b3f-a9a5-6f200a25818f').
narrative_ontology:cs_kernel_codification('185d2436-76af-4b3f-a9a5-6f200a25818f', fixed_text).
narrative_ontology:cs_authority_grounding('185d2436-76af-4b3f-a9a5-6f200a25818f', extraction).
narrative_ontology:cs_interpretation_layer_present('185d2436-76af-4b3f-a9a5-6f200a25818f').
narrative_ontology:cs_reading_relation('185d2436-76af-4b3f-a9a5-6f200a25818f', dharmasastra_corpus__orthodox_literalist, forecloses).
narrative_ontology:cs_reading_relation('185d2436-76af-4b3f-a9a5-6f200a25818f', dharmasastra_corpus__reformist_contextual, forecloses).
narrative_ontology:cs_axiom('185d2436-76af-4b3f-a9a5-6f200a25818f', foundational, inherent_equality_of_all_beings).
narrative_ontology:cs_axiom_status(inherent_equality_of_all_beings, holdable).
narrative_ontology:cs_axiom_grounding('185d2436-76af-4b3f-a9a5-6f200a25818f', inherent_equality_of_all_beings, deontological).
narrative_ontology:cs_axiom('185d2436-76af-4b3f-a9a5-6f200a25818f', foundational, textual_legitimacy_derived_from_justice).
narrative_ontology:cs_axiom_status(textual_legitimacy_derived_from_justice, holdable).
narrative_ontology:cs_axiom_grounding('185d2436-76af-4b3f-a9a5-6f200a25818f', textual_legitimacy_derived_from_justice, deontological).
narrative_ontology:cs_reference_frame('185d2436-76af-4b3f-a9a5-6f200a25818f', universal_human_equality).
narrative_ontology:cs_drift_state('185d2436-76af-4b3f-a9a5-6f200a25818f', contemporary_human_rights_era, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('185d2436-76af-4b3f-a9a5-6f200a25818f', '').
narrative_ontology:cs_kernel_id(dharmasastra_corpus__abolitionist_rejection, dharmasastra_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, dalits).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, lower_castes).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, women).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__abolitionist_rejection, brahmins_and_upper_castes).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__abolitionist_rejection, social_justice_imperative).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__abolitionist_rejection, human_equality_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Historically and currently subjected to extreme social exclusion, economic exploitation, and violence, with virtually no means of escaping the prescribed roles and indignities imposed by the caste system derived from Dharmasastra. Their identity is deeply intertwined with their assigned status.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, dalits, payer,
    powerless, generational, trapped, local).

% Subjected to various forms of discrimination, limited opportunities, and social stigma, though with slightly more mobility than Dalits. Their social and economic life is heavily constrained by caste norms, making exit from the system's influence extremely difficult due to identity and community ties.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, lower_castes, payer,
    powerless, generational, identity_locked, local).

% Across all castes, women are subjected to patriarchal norms and restrictions on autonomy, education, and property rights, as prescribed by Dharmasastra. Their identity and social standing are often defined by their marital and familial roles, making individual exit from these norms highly challenging.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, women, payer,
    powerless, generational, identity_locked, local).

% Historically and currently benefit from the social, economic, and ritual privileges conferred by the caste system. They occupy positions of power and influence, with their status often reinforced by the traditional interpretations of Dharmasastra. They have significant power to maintain or alter the system, but benefit from its current form.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, brahmins_and_upper_castes, beneficiary,
    institutional, generational, arbitrage, local).

% Actively challenge the legitimacy and authority of Dharmasastra and the caste system. They advocate for radical social reform and the complete dismantling of hierarchical structures, often facing significant resistance and personal risk. Their power comes from collective action and moral authority.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, abolitionist_activists, agenda_setter,
    organized, generational, mobile, national).

% Interpret and uphold the traditional authority of Dharmasastra, often emphasizing its ritual and moral prescriptions while defending or reinterpreting its caste-related aspects. Their careers and social standing are deeply tied to the preservation of the textual tradition and its interpretive lineage.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, orthodox_scholars, agenda_setter,
    institutional, generational, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From this reading, the Dharmasastra has no legitimate coordination function; its primary function is to establish and maintain a hierarchical social order through coercion.
% TRANSFER_FUNCTION: Transfers social status, economic resources, and ritual purity from lower castes and women to upper castes, while simultaneously extracting labor and obedience from the former.
% ABSENT_VOICES: The voices of those historically silenced and marginalized by the caste system, particularly Dalits and lower-caste women, have been systematically excluded from the interpretive tradition. Their perspectives would unequivocally condemn the corpus as an instrument of oppression.
% DISAPPEARANCE_RATIONALE: If the Dharmasastra corpus and its associated social structures were to vanish overnight, the entire social, economic, and religious fabric of societies where it holds sway would undergo a radical, transformative rearrangement. Hierarchies would collapse, new forms of social organization would emerge, and the distribution of power and resources would be fundamentally altered.
% FOUNDING_PROBLEM: The Dharmasastra was constructed to establish and legitimize a hierarchical social order (varna/jati system) and to regulate social conduct, ritual purity, and legal obligations within that order.
% FOUNDING_PROBLEM_CORROBORATION: Orthodox scholars and traditionalists attest that the founding problem of maintaining social order and dharma is still live. Abolitionist activists and social scientists, however, argue that the 'problem' it solved was the consolidation of power for certain groups, and that this 'problem' (i.e., the desire for hierarchical control) is indeed still live, but illegitimate. Independent historical and sociological analyses corroborate the role of Dharmasastra in establishing and perpetuating social hierarchy.
narrative_ontology:disappearance_verdict(dharmasastra_corpus__abolitionist_rejection, world_rearranges).
narrative_ontology:founding_problem_status(dharmasastra_corpus__abolitionist_rejection, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dharmasastra_corpus__abolitionist_rejection, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.95) is extremely high, reflecting the systemic deprivation and exploitation inherent in the caste system. Suppression (0.9) is also very high, as the system historically relied on severe social, economic, and physical coercion, and continues to suppress dissent and alternative social structures. Theater ratio (0.1) is low because, from this reading, there is little performative justification; the system's function is direct extraction. Accessibility collapse (0.8) is high due to the pervasive nature of caste, making exit from its social and economic strictures extremely difficult. Resistance (0.9) is high, reflecting centuries of struggle against the caste system.
 *
 * PERSPECTIVAL GAP:
 *   From the abolitionist perspective, there is no legitimate 'beneficiary' of the Dharmasastra, only historical perpetrators of extraction. The system is a pure Snare for its victims. Those who historically benefited (brahmins_and_upper_castes) did so through the oppression of others, not through genuine coordination. The engine's classification will reflect this extreme asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Dalits, lower_castes, and women are full targets (d=1.0) due to their extreme lack of exit options and the direct extraction they face. Brahmins and upper castes were historical beneficiaries (d=0.0) as the system subsidized their status and resources. Abolitionist activists are external agents seeking to dismantle the constraint, effectively acting as analytical observers with high resistance.
 *
 * MANDATROPHY ANALYSIS:
 *   From the abolitionist reading, the Dharmasastra never had a legitimate coordination function that atrophied. It was always a mechanism of extraction and oppression. Therefore, the concept of mandatrophy (a mandate outliving its function) does not apply; the constraint's function was always illegitimate. The classification as a Snare prevents mislabeling it as a degraded coordination mechanism (Piton) or a hybrid (Tangled Rope).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine, independent structural claim, or one reading of the Dharmasastra corpus kernel?',
    'This is explicitly declared as one reading of the ''dharmasastra_corpus'' kernel, instantiated by the ''abolitionist_rejection'' perspective. Its structural properties are derived from this specific interpretive frame.',
    'Recognizing it as a reading clarifies that its classification (Snare) is contingent on the abolitionist interpretive frame, which identifies the corpus as fundamentally extractive. Other readings (e.g., orthodox_literalist, reformist_contextual) would yield different classifications for the same underlying text.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint is the ''abolitionist_rejection'' reading of the ''dharmasastra_corpus'' kernel.').

omega_variable(
    legitimacy_of_authority,
    'Does the Dharmasastra corpus retain any legitimate authority, or is its authority wholly derived from historical oppression?',
    'Analysis of the historical and contemporary impact of Dharmasastra on social structures, focusing on whether its prescriptive elements can be disentangled from their historical role in caste enforcement. If all beneficial elements are separable and re-groundable outside the corpus, its authority is wholly derived from oppression.',
    'If no legitimate authority remains, the constraint is a pure Snare, with no coordination function. If some separable, legitimate authority could be identified, it might be reclassified as a Tangled Rope, acknowledging a vestigial coordination function alongside extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_of_authority, conceptual, 'Ambiguity regarding the residual legitimate authority of the Dharmasastra corpus.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (external barriers) or internalized (cognitive patterns that persist after barrier removal)?',
    'Post-dismantling social trajectory: if hierarchical social patterns and self-limiting beliefs persist after formal caste structures are removed, reclassify as partially internalized suppression.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making full liberation more difficult.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the caste system.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dharmasastra_corpus__abolitionist_rejection, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dhar_tr_t0, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 0, 0.15).
narrative_ontology:measurement(dhar_tr_t100, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 100, 0.12).
narrative_ontology:measurement(dhar_tr_t200, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 200, 0.1).

% Extraction over time
narrative_ontology:measurement(dhar_be_t0, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 0, 0.9).
narrative_ontology:measurement(dhar_be_t100, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 100, 0.92).
narrative_ontology:measurement(dhar_be_t200, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 200, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(dhar_su_t0, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(dhar_su_t100, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 100, 0.88).
narrative_ontology:measurement(dhar_su_t200, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 200, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dharmasastra_corpus__abolitionist_rejection, identity_coordination).
narrative_ontology:affects_constraint(dharmasastra_corpus__abolitionist_rejection, caste_system_social_hierarchy).
narrative_ontology:affects_constraint(dharmasastra_corpus__abolitionist_rejection, gender_roles_patriarchal_norms).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'dharmasastra_corpus' kernel. Its structural properties are derived from the abolitionist interpretive frame, which views the corpus as fundamentally oppressive and extractive. Other readings (orthodox_literalist, reformist_contextual) offer different classifications and structural properties for the same underlying text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
