% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__indigenous_return_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__indigenous_return_reading, []).

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
 *   constraint_id: jewish_self_determination__indigenous_return_reading
 *   human_readable: Jewish Indigenous Return and Self-Determination
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint represents the 'indigenous return' reading of Jewish
 *   self-determination, which asserts that Jewish people are indigenous to
 *   the land of Israel with an unbroken historical and cultural connection.
 *   Under this reading, Zionism is framed as a movement of decolonization and
 *   indigenous liberation, rather than a settler-colonial project. This
 *   perspective seeks to align Jewish national aspirations with global
 *   indigenous rights movements, distinguishing it from purely religious or
 *   liberal nationalist claims. The constraint's classification as 'rope'
 *   reflects its function in coordinating a collective identity and political
 *   program, while acknowledging the contestation and resistance it faces.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__indigenous_return_reading, 0.15).
domain_priors:suppression_score(jewish_self_determination__indigenous_return_reading, 0.3).
domain_priors:theater_ratio(jewish_self_determination__indigenous_return_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__indigenous_return_reading, rope).
narrative_ontology:human_readable(jewish_self_determination__indigenous_return_reading, "Jewish Indigenous Return and Self-Determination").
narrative_ontology:topic_domain(jewish_self_determination__indigenous_return_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_self_determination__indigenous_return_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__indigenous_return_reading, '2f049d72-5193-4991-aeee-e124b75edf9d').
narrative_ontology:cs_kernel_codification('2f049d72-5193-4991-aeee-e124b75edf9d', distributed).
narrative_ontology:cs_authority_grounding('2f049d72-5193-4991-aeee-e124b75edf9d', practice).
narrative_ontology:cs_reading_relation('2f049d72-5193-4991-aeee-e124b75edf9d', jewish_self_determination__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('2f049d72-5193-4991-aeee-e124b75edf9d', jewish_self_determination__settler_colonial_reading, forecloses).
narrative_ontology:cs_reading_relation('2f049d72-5193-4991-aeee-e124b75edf9d', jewish_self_determination__religious_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('2f049d72-5193-4991-aeee-e124b75edf9d', jewish_self_determination__diasporist_reading, coexists_with).
narrative_ontology:cs_axiom('2f049d72-5193-4991-aeee-e124b75edf9d', foundational, jewish_people_are_indigenous_to_the_land).
narrative_ontology:cs_axiom_status(jewish_people_are_indigenous_to_the_land, holdable).
narrative_ontology:cs_axiom_grounding('2f049d72-5193-4991-aeee-e124b75edf9d', jewish_people_are_indigenous_to_the_land, empirically_contingent).
narrative_ontology:cs_axiom('2f049d72-5193-4991-aeee-e124b75edf9d', foundational, zionism_is_a_decolonization_movement).
narrative_ontology:cs_axiom_status(zionism_is_a_decolonization_movement, holdable).
narrative_ontology:cs_axiom_grounding('2f049d72-5193-4991-aeee-e124b75edf9d', zionism_is_a_decolonization_movement, instrumental).
narrative_ontology:cs_reference_frame('2f049d72-5193-4991-aeee-e124b75edf9d', ancestral_indigenous_connection).
narrative_ontology:cs_drift_state('2f049d72-5193-4991-aeee-e124b75edf9d', contemporary_postcolonial_discourse, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('2f049d72-5193-4991-aeee-e124b75edf9d', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(jewish_self_determination__indigenous_return_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__indigenous_return_reading, jewish_claimants_to_ancestral_land).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jewish_self_determination__indigenous_return_reading, palestinian_people).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts an unbroken, multi-millennial indigenous connection to the land, framing modern Jewish return as decolonization and self-determination. Benefits from the recognition of this indigenous status, which legitimizes their presence and claims to sovereignty.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, jewish_claimants_to_ancestral_land, beneficiary,
    organized, generational, identity_locked, regional).

% Their claims to indigeneity and self-determination are challenged or subordinated by this reading, which reframes their presence as later arrival or co-indigenous with a lesser claim. Bears the cost of diminished legitimacy for their own national aspirations and historical narrative.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, palestinian_people, payer,
    powerless, generational, trapped, regional).

% Analyze the historical, legal, and anthropological arguments for Jewish indigeneity, and how these claims interact with existing frameworks of self-determination, indigenous rights, and postcolonial theory. Their analysis can influence the constraint's legitimacy.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, international_law_scholars, observer,
    analytical, generational, analytical, global).

% Examine whether Zionism, under this indigenous return reading, aligns with or contradicts the principles of decolonization, considering the historical context of European colonialism and the dispossession of existing populations. Their interpretations shape academic and activist discourse.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, postcolonial_theorists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the collective identity and political aspirations of Jewish people around a narrative of indigenous return, providing a framework for self-determination and national belonging rooted in ancestral land claims.
% TRANSFER_FUNCTION: Transfers legitimacy and historical narrative authority to Jewish claims of indigeneity and self-determination in the land, while implicitly or explicitly diminishing or subordinating Palestinian claims.
% ABSENT_VOICES: Palestinian voices asserting their own primary and continuous indigeneity are often excluded or marginalized in the discourse that centers this reading, as their narrative directly challenges the 'decolonization not colonization' framing.
% DISAPPEARANCE_RATIONALE: If the indigenous return reading vanished, the foundational narrative for a significant segment of Zionist thought would collapse, forcing a re-evaluation of the nature of Jewish claims to the land and the character of Zionism itself. This would profoundly alter political discourse and potentially international relations concerning the region.
% FOUNDING_PROBLEM: The historical displacement and persecution of Jewish people, coupled with a desire for self-determination and a secure homeland rooted in ancestral connection, in a world where other nations had achieved sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: Historical records, archaeological evidence, and continuous cultural and religious practices attest to a long-standing Jewish connection to the land. However, the interpretation of this connection as 'indigenous return' and 'decolonization' is contested by Palestinian historians, postcolonial scholars, and some Jewish diasporist thinkers, who offer alternative historical narratives and political framings.
narrative_ontology:disappearance_verdict(jewish_self_determination__indigenous_return_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__indigenous_return_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__indigenous_return_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jewish_self_determination__indigenous_return_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__indigenous_return_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__indigenous_return_reading_tests).
:- end_tests(jewish_self_determination__indigenous_return_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.15) because, if accepted, indigenous status is a binary claim that primarily legitimizes rather than extracts. However, the high resistance (0.75) and moderate suppression (0.30) reflect the intense contestation this reading faces, particularly from Palestinian counter-claims of indigeneity and settler-colonial critiques. The 'rope' classification is chosen because it functions as a powerful coordinating principle for Jewish identity and political action, even as its legitimacy is fiercely debated. It's not a 'mountain' because its 'naturalness' (as an indigenous claim) is deeply contested, and not a 'snare' because its primary function is identity coordination, not pure extraction, though it has extractive consequences for others.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Jewish claimants, this reading is a self-evident truth, a historical fact that justifies their return and self-determination. From the perspective of Palestinians and many postcolonial theorists, it is a constructed narrative that serves to legitimize dispossession and obscures settler-colonial dynamics. The engine's classification as 'rope' captures this tension: it coordinates effectively for its beneficiaries but faces significant resistance and has extractive consequences for others.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish claimants to ancestral land are the primary beneficiaries, as this reading provides a powerful legitimizing framework for their self-determination and presence (low directionality). The Palestinian people are positioned as payers, as their own indigenous claims and national aspirations are implicitly or explicitly subordinated or challenged by this reading (high directionality). International law scholars and postcolonial theorists act as observers, analyzing and critiquing the claims, influencing the broader discourse and the constraint's perceived legitimacy.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    indigeneity_definition_ambiguity,
    'Is the concept of ''indigeneity'' being applied consistently with international legal and anthropological definitions, or is it being selectively applied to legitimize a particular national claim?',
    'Comparative analysis with other recognized indigenous movements and international legal precedents, assessing criteria such as pre-colonial presence, self-identification, distinct culture, and experience of dispossession.',
    'If inconsistent, the claim to ''indigenous return'' loses its moral and legal force, reclassifying the constraint closer to a ''liberal nationalist'' or ''settler colonial'' reading. If consistent, it strengthens the ''rope'' classification by validating the coordination function around indigenous rights.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigeneity_definition_ambiguity, conceptual, 'Ambiguity in the application of ''indigeneity'' to Jewish claims.').

omega_variable(
    co_indigeneity_and_hierarchy,
    'How does this reading account for the indigeneity of the Palestinian people, and does it implicitly or explicitly establish a hierarchy of indigenous claims?',
    'Detailed textual analysis of proponents'' arguments and their policy implications, examining whether Palestinian indigeneity is acknowledged, denied, or subordinated, and what practical consequences follow.',
    'If Palestinian indigeneity is denied or subordinated, the constraint''s extractiveness increases significantly, as it actively dispossesses another indigenous group. If co-indigeneity is genuinely recognized without hierarchy, the ''rope'' classification is more robust, but the coordination problem becomes more complex.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(co_indigeneity_and_hierarchy, conceptual, 'The relationship between Jewish and Palestinian indigenous claims within this framework.').

omega_variable(
    historical_continuity_vs_disruption,
    'To what extent does the historical connection asserted by this reading represent an unbroken continuity, versus a re-establishment or re-interpretation after significant historical disruptions and diasporic experiences?',
    'Historical and archaeological scholarship, combined with critical analysis of how ''unbroken connection'' is defined and evidenced, particularly across periods of forced displacement and cultural evolution.',
    'If historical continuity is found to be significantly disrupted or re-interpreted, the ''mountain'' aspect of the claim (as an immutable historical fact) weakens, potentially shifting the constraint towards a more constructed ''rope'' or ''tangled_rope'' based on political and cultural assertion rather than inherent historical right.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_continuity_vs_disruption, empirical, 'The empirical basis for ''unbroken connection'' in Jewish indigeneity claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__indigenous_return_reading, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(jewi_be_t1900, jewish_self_determination__indigenous_return_reading, base_extractiveness, 1900, 0.05).
narrative_ontology:measurement(jewi_be_t1948, jewish_self_determination__indigenous_return_reading, base_extractiveness, 1948, 0.1).
narrative_ontology:measurement(jewi_be_t1967, jewish_self_determination__indigenous_return_reading, base_extractiveness, 1967, 0.15).
narrative_ontology:measurement(jewi_be_t2000, jewish_self_determination__indigenous_return_reading, base_extractiveness, 2000, 0.15).
narrative_ontology:measurement(jewi_be_t2024, jewish_self_determination__indigenous_return_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1900, jewish_self_determination__indigenous_return_reading, suppression_requirement, 1900, 0.1).
narrative_ontology:measurement(jewi_su_t1948, jewish_self_determination__indigenous_return_reading, suppression_requirement, 1948, 0.2).
narrative_ontology:measurement(jewi_su_t1967, jewish_self_determination__indigenous_return_reading, suppression_requirement, 1967, 0.3).
narrative_ontology:measurement(jewi_su_t2000, jewish_self_determination__indigenous_return_reading, suppression_requirement, 2000, 0.3).
narrative_ontology:measurement(jewi_su_t2024, jewish_self_determination__indigenous_return_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__indigenous_return_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, jewish_self_determination__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, jewish_self_determination__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, jewish_self_determination__religious_covenant_reading).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, jewish_self_determination__diasporist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of multiple readings of the 'Jewish self-determination' kernel. Each reading presents a distinct structural claim with different beneficiaries, victims, and classifications, linked by their common kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
