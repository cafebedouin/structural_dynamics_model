% ============================================================================
% CONSTRAINT STORY: family_law_authority__parsi_zoroastrian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__parsi_zoroastrian_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: family_law_authority__parsi_zoroastrian_reading
 *   human_readable: Parsi Zoroastrian Marriage Law (Community Preservation Reading)
 *   domain: religious/social/legal
 *
 * SUMMARY:
 *   This constraint describes marriage within the Parsi Zoroastrian
 *   community, viewed through the lens of its religious law and social norms,
 *   which strongly emphasize endogamy for community preservation. It is one
 *   reading of the broader 'family_law_authority' kernel. The constraint
 *   functions to maintain the distinct identity of a small, diaspora
 *   community, but does so by imposing significant costs on individuals who
 *   seek to marry outside its boundaries. Priestly authority and community
 *   pressure are key enforcement mechanisms.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__parsi_zoroastrian_reading, 0.78).
domain_priors:suppression_score(family_law_authority__parsi_zoroastrian_reading, 0.85).
domain_priors:theater_ratio(family_law_authority__parsi_zoroastrian_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__parsi_zoroastrian_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__parsi_zoroastrian_reading, "Parsi Zoroastrian Marriage Law (Community Preservation Reading)").
narrative_ontology:topic_domain(family_law_authority__parsi_zoroastrian_reading, "religious/social/legal").

domain_priors:requires_active_enforcement(family_law_authority__parsi_zoroastrian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__parsi_zoroastrian_reading, '104602b0-f92f-40bc-a150-566cce1dbbac').
narrative_ontology:cs_kernel_codification('104602b0-f92f-40bc-a150-566cce1dbbac', formalized).
narrative_ontology:cs_authority_grounding('104602b0-f92f-40bc-a150-566cce1dbbac', lineage).
narrative_ontology:cs_interpretation_layer_present('104602b0-f92f-40bc-a150-566cce1dbbac').
narrative_ontology:cs_reading_relation('104602b0-f92f-40bc-a150-566cce1dbbac', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('104602b0-f92f-40bc-a150-566cce1dbbac', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('104602b0-f92f-40bc-a150-566cce1dbbac', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('104602b0-f92f-40bc-a150-566cce1dbbac', family_law_authority__secular_contractual_reading, coexists_with).
narrative_ontology:cs_axiom('104602b0-f92f-40bc-a150-566cce1dbbac', foundational, community_survival_through_endogamy).
narrative_ontology:cs_axiom_status(community_survival_through_endogamy, holdable).
narrative_ontology:cs_axiom_grounding('104602b0-f92f-40bc-a150-566cce1dbbac', community_survival_through_endogamy, conventional).
narrative_ontology:cs_axiom('104602b0-f92f-40bc-a150-566cce1dbbac', foundational, religious_law_governs_marriage).
narrative_ontology:cs_axiom_status(religious_law_governs_marriage, holdable).
narrative_ontology:cs_axiom_grounding('104602b0-f92f-40bc-a150-566cce1dbbac', religious_law_governs_marriage, theological).
narrative_ontology:cs_reference_frame('104602b0-f92f-40bc-a150-566cce1dbbac', traditional_community_preservation).
narrative_ontology:cs_drift_state('104602b0-f92f-40bc-a150-566cce1dbbac', contemporary_globalized_society, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('104602b0-f92f-40bc-a150-566cce1dbbac', '').
narrative_ontology:cs_kernel_id(family_law_authority__parsi_zoroastrian_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__parsi_zoroastrian_reading, parsi_zoroastrian_community).
narrative_ontology:constraint_victim(family_law_authority__parsi_zoroastrian_reading, individual_zoroastrians_seeking_exogamy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(family_law_authority__parsi_zoroastrian_reading, individual_zoroastrians_seeking_endogamy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The collective body of Parsi Zoroastrians, which benefits from the preservation of its distinct religious and ethnic identity through endogamous marriage. It sets and upholds the religious laws and social norms governing marriage.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, parsi_zoroastrian_community, agenda_setter,
    institutional, generational, identity_locked, global).

% The religious authority responsible for interpreting and enforcing Zoroastrian marriage laws, performing rituals, and validating marriages. Their authority is central to the constraint's operation.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, zoroastrian_priesthood, agenda_setter,
    organized, generational, constrained, global).

% Individuals within the community who seek partners within the prescribed endogamous framework. They benefit from community cohesion, shared cultural values, and the social support structure.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, individual_zoroastrians_seeking_endogamy, beneficiary,
    moderate, biographical, constrained, local).

% Individuals who wish to marry outside the Parsi Zoroastrian community. They face significant social pressure, potential ostracization, and their marriages may not be recognized religiously, leading to a loss of community status for themselves and their children.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, individual_zoroastrians_seeking_exogamy, payer,
    powerless, biographical, identity_locked, local).

% The state legal framework that provides for civil marriage, which may or may not recognize religious marriages and typically does not enforce religious endogamy. It offers an alternative but does not negate the religious and social consequences within the community.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, secular_legal_system, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(family_law_authority__parsi_zoroastrian_reading, parsi_zoroastrian_community).
narrative_ontology:fixing_cost_class(family_law_authority__parsi_zoroastrian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To preserve the distinct religious, cultural, and ethnic identity of the small Parsi Zoroastrian community, particularly in diaspora, by regulating marriage to ensure endogamy and adherence to religious traditions.
% TRANSFER_FUNCTION: Transfers individual marital autonomy and choice to the collective goal of community preservation; transfers authority over marital validity from individual consent to priestly and communal religious law.
% ABSENT_VOICES: Individuals who have been ostracized or have left the community due to exogamous marriages, and those who advocate for greater individual freedom in marital choice over strict endogamy. Their experiences are often marginalized within community discourse.
% DISAPPEARANCE_RATIONALE: If the religious laws and social norms enforcing endogamy vanished overnight, the Parsi Zoroastrian community's distinct identity would rapidly dilute through intermarriage, leading to significant demographic and cultural shifts, and potentially assimilation into larger populations.
% FOUNDING_PROBLEM: The existential threat of assimilation and loss of distinct religious and ethnic identity for a small, diaspora community, particularly through intermarriage with larger populations.
% FOUNDING_PROBLEM_CORROBORATION: Community leaders, historians, and sociologists specializing in diaspora studies consistently corroborate the ongoing challenge of preserving the Parsi Zoroastrian identity and traditions in the face of modern pressures and demographic realities. This is attested by demographic studies and community-commissioned reports from outside the immediate beneficiaries.
narrative_ontology:disappearance_verdict(family_law_authority__parsi_zoroastrian_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__parsi_zoroastrian_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__parsi_zoroastrian_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(family_law_authority__parsi_zoroastrian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__parsi_zoroastrian_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__parsi_zoroastrian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(family_law_authority__parsi_zoroastrian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(family_law_authority__parsi_zoroastrian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.78) reflects the significant social and religious costs borne by individuals who choose exogamous marriage, including potential loss of community status and religious recognition. Suppression (0.85) is high due to the pervasive social pressure, religious invalidation, and the 'identity_locked' exit option for community members. The theater ratio is low (0.15) because the community genuinely believes in and actively enforces these rules for its survival; there is little performative maintenance. The claimed type is 'tangled_rope' because it serves a genuine coordination function (community preservation) but achieves this through asymmetric extraction from individual marital choices, requiring active enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Parsi Zoroastrian community and priesthood, the constraint is a vital 'rope' for collective survival and identity preservation. However, from the perspective of individuals wishing to marry outside the community, it operates as a 'snare' or highly extractive 'tangled_rope', severely limiting personal autonomy and imposing significant social penalties. The engine's classification will reflect this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The Parsi Zoroastrian community and its priesthood are the primary beneficiaries and agenda-setters, as the constraint directly serves their goal of identity preservation. Individuals seeking endogamous marriage also benefit from the stable community structure. Individuals seeking exogamous marriage are the primary targets/victims, bearing the costs of social exclusion and religious non-recognition. The secular legal system acts as an observer, offering an alternative but not directly influencing the religious constraint's internal dynamics.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem of community preservation in a diaspora context remains 'live', indicating that the constraint's mandate has not atrophied. The high extractiveness and suppression are directly tied to the ongoing effort to maintain community boundaries against assimilation pressures. This prevents mislabeling it as a 'piton' or a 'snare' whose coordination story is mere cover, as the coordination function is still actively pursued and deemed essential by the community.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a valid and distinct reading of the ''family_law_authority'' kernel, or is it merely a variant of a broader religious law constraint?',
    'Comparative legal and sociological analysis of Parsi Zoroastrian family law against other religious and secular legal systems, focusing on unique structural elements like endogamy''s role in community preservation.',
    'If distinct, it validates this specific reading. If not, it might be subsumed under a broader ''religious family law'' constraint, altering its network relationships and potentially its classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Confirms the distinctiveness of the Parsi Zoroastrian reading within the family law kernel.').

omega_variable(
    community_preservation_vs_individual_rights,
    'To what extent is the endogamy requirement a genuine, necessary mechanism for community preservation, versus an extractive mechanism that unduly restricts individual rights?',
    'Longitudinal studies of Parsi Zoroastrian communities with varying degrees of endogamy enforcement, assessing demographic trends, cultural retention, and individual well-being. Also, comparative analysis with other small diaspora communities.',
    'If community preservation is demonstrably dependent on strict endogamy, the coordination aspect is strengthened. If not, the extractive aspect (from individual choice) becomes more prominent, potentially shifting the classification closer to a ''snare'' for individuals.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(community_preservation_vs_individual_rights, empirical, 'Assesses the balance between collective benefit and individual cost in the endogamy requirement.').

omega_variable(
    impact_of_secular_legal_pluralism,
    'How does the existence of a secular legal system offering civil marriage options (without endogamy requirements) influence the effective suppression and extractiveness of the religious constraint?',
    'Analysis of marriage patterns and community engagement among Parsi Zoroastrians in jurisdictions with strong secular legal pluralism, comparing rates of exogamous civil marriages and their social consequences within the community.',
    'If secular options significantly reduce the social cost of exogamy, the religious constraint''s effective suppression and extractiveness might be lower than measured. If social costs remain high despite legal alternatives, the constraint''s power is primarily social/religious, not legal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impact_of_secular_legal_pluralism, empirical, 'Examines how secular legal alternatives modulate the religious constraint''s force.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__parsi_zoroastrian_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t0, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(fami_tr_t10, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(fami_tr_t20, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(fami_tr_t30, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement(fami_tr_t40, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement(fami_tr_t50, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 50, 0.15).

% Extraction over time
narrative_ontology:measurement(fami_be_t0, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(fami_be_t10, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 10, 0.76).
narrative_ontology:measurement(fami_be_t20, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 20, 0.77).
narrative_ontology:measurement(fami_be_t30, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 30, 0.78).
narrative_ontology:measurement(fami_be_t40, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 40, 0.78).
narrative_ontology:measurement(fami_be_t50, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 50, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t0, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 0, 0.88).
narrative_ontology:measurement(fami_su_t10, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 10, 0.87).
narrative_ontology:measurement(fami_su_t20, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 20, 0.86).
narrative_ontology:measurement(fami_su_t30, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 30, 0.85).
narrative_ontology:measurement(fami_su_t40, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 40, 0.85).
narrative_ontology:measurement(fami_su_t50, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 50, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__parsi_zoroastrian_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'family_law_authority' kernel, focusing on the Parsi Zoroastrian interpretation. Other readings (Hindu Dharmashastra, Muslim Shariat, Christian Canonical, Secular Contractual) are distinct constraints that interact within a pluralistic legal landscape.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
