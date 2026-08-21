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
 *   constraint_id: family_law_authority__parsi_zoroastrian_reading
 *   human_readable: Parsi Zoroastrian Marriage as Community Preservation
 *   domain: comparative_law/religious_governance
 *
 * SUMMARY:
 *   This constraint describes the Parsi Zoroastrian community's marriage
 *   norms, which are heavily influenced by religious law and a strong
 *   emphasis on endogamy to preserve the community's distinct identity. It is
 *   a reading of the broader 'family_law_authority' kernel, focusing on the
 *   specific mechanisms of community preservation through marriage. The
 *   constraint is claimed as a Rope by its proponents (community
 *   preservation) but operates with significant extraction and suppression,
 *   making it a Tangled Rope in practice.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__parsi_zoroastrian_reading, 0.65).
domain_priors:suppression_score(family_law_authority__parsi_zoroastrian_reading, 0.78).
domain_priors:theater_ratio(family_law_authority__parsi_zoroastrian_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__parsi_zoroastrian_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__parsi_zoroastrian_reading, "Parsi Zoroastrian Marriage as Community Preservation").
narrative_ontology:topic_domain(family_law_authority__parsi_zoroastrian_reading, "comparative_law/religious_governance").

domain_priors:requires_active_enforcement(family_law_authority__parsi_zoroastrian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__parsi_zoroastrian_reading, '878d0371-37ef-406d-b692-1f892e8751f2').
narrative_ontology:cs_kernel_codification('878d0371-37ef-406d-b692-1f892e8751f2', formalized).
narrative_ontology:cs_authority_grounding('878d0371-37ef-406d-b692-1f892e8751f2', lineage).
narrative_ontology:cs_interpretation_layer_present('878d0371-37ef-406d-b692-1f892e8751f2').
narrative_ontology:cs_reading_relation('878d0371-37ef-406d-b692-1f892e8751f2', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('878d0371-37ef-406d-b692-1f892e8751f2', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('878d0371-37ef-406d-b692-1f892e8751f2', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('878d0371-37ef-406d-b692-1f892e8751f2', family_law_authority__secular_contractual_reading, coexists_with).
narrative_ontology:cs_axiom('878d0371-37ef-406d-b692-1f892e8751f2', foundational, endogamy_as_community_preservation).
narrative_ontology:cs_axiom_status(endogamy_as_community_preservation, holdable).
narrative_ontology:cs_axiom_grounding('878d0371-37ef-406d-b692-1f892e8751f2', endogamy_as_community_preservation, conventional).
narrative_ontology:cs_axiom('878d0371-37ef-406d-b692-1f892e8751f2', foundational, priestly_authority_over_ritual_validity).
narrative_ontology:cs_axiom_status(priestly_authority_over_ritual_validity, holdable).
narrative_ontology:cs_axiom_grounding('878d0371-37ef-406d-b692-1f892e8751f2', priestly_authority_over_ritual_validity, theological).
narrative_ontology:cs_reference_frame('878d0371-37ef-406d-b692-1f892e8751f2', traditional_parsi_community_norms).
narrative_ontology:cs_drift_state('878d0371-37ef-406d-b692-1f892e8751f2', contemporary_globalized_society, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('878d0371-37ef-406d-b692-1f892e8751f2', '').
narrative_ontology:cs_kernel_id(family_law_authority__parsi_zoroastrian_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__parsi_zoroastrian_reading, parsi_community_elders).
narrative_ontology:constraint_beneficiary(family_law_authority__parsi_zoroastrian_reading, zoroastrian_priesthood).
narrative_ontology:constraint_victim(family_law_authority__parsi_zoroastrian_reading, parsi_youth_seeking_intermarriage).
narrative_ontology:constraint_victim(family_law_authority__parsi_zoroastrian_reading, intermarried_parsi_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(family_law_authority__parsi_zoroastrian_reading, parsi_community_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer and uphold community norms, including endogamy, to preserve the distinct identity and genetic pool of the Parsi Zoroastrian community. They benefit from the continuity and cohesion of the community, which their authority helps maintain.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, parsi_community_elders, agenda_setter,
    institutional, generational, constrained, local).

% Performs and validates marriage rituals according to religious law. Their authority is central to the constraint's operation, as they determine who is considered legitimately married within the religious framework. They benefit from the preservation of religious tradition and their role within it.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, zoroastrian_priesthood, agenda_setter,
    institutional, generational, constrained, local).

% Face social ostracization, loss of community status, and exclusion from religious ceremonies if they marry outside the Parsi community. They bear the cost of the endogamy requirement, often having to choose between personal relationships and community belonging.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, parsi_youth_seeking_intermarriage, payer,
    moderate, biographical, identity_locked, local).

% Have already married outside the community and experience the full consequences of exclusion, including their children not being recognized as Parsi. They are targets of the constraint's enforcement and bear its social and religious costs.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, intermarried_parsi_individuals, payer,
    powerless, biographical, identity_locked, local).

% Benefit from the preservation of their cultural and religious identity, the maintenance of community cohesion, and the perceived purity of their lineage. They are coordinated by the endogamy rules, which reinforce their collective identity.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, parsi_community_members, beneficiary,
    organized, generational, constrained, local).

% Recognize civil marriages regardless of religious endogamy rules, creating a tension between religious and state authority. They observe the social impact of religious endogamy but generally do not intervene unless civil rights are violated.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, secular_legal_systems, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates marriage practices to preserve the distinct genetic, cultural, and religious identity of the small Parsi Zoroastrian community, ensuring continuity across generations.
% TRANSFER_FUNCTION: Transfers social status, religious recognition, and community belonging to those who adhere to endogamous marriage rules, while extracting these from those who intermarry.
% ABSENT_VOICES: Individuals who have left the community due to intermarriage, or those who advocate for a more inclusive interpretation of Parsi identity, are largely excluded from the decision-making processes that uphold these rules.
% DISAPPEARANCE_RATIONALE: If the religious laws governing Parsi marriage and endogamy vanished, the community's distinct identity would rapidly dilute through intermarriage, leading to a significant demographic and cultural shift within a generation. The community as currently constituted would cease to exist.
% FOUNDING_PROBLEM: The Parsi community, as a small diaspora group, faced the existential threat of cultural and genetic assimilation, leading to the establishment of strict endogamy rules to ensure its survival.
% FOUNDING_PROBLEM_CORROBORATION: Community leaders and historians attest to the ongoing demographic challenges faced by the Parsi community, corroborating the live status of the founding problem. Independent sociological studies also highlight the community's declining population and the role of endogamy in its preservation efforts.
narrative_ontology:disappearance_verdict(family_law_authority__parsi_zoroastrian_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__parsi_zoroastrian_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__parsi_zoroastrian_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(family_law_authority__parsi_zoroastrian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__parsi_zoroastrian_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.65) due to the severe social and religious costs imposed on individuals who intermarry, effectively extracting their full community belonging. Suppression is also high (0.78) because the community's social structures and religious authority actively enforce endogamy, with limited avenues for dissent or alternative interpretations without facing ostracization. Theater ratio is low (0.20) as the community preservation function is genuinely active, not merely performative, though some rituals may reinforce the rules theatrically. The slight dip in extractiveness and suppression towards the end of the interval reflects increasing external pressures from secular societies and internal debates, making enforcement slightly more challenging.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of community elders, the constraint is a necessary Rope for survival. From the perspective of intermarried individuals, it is a Snare that extracts their identity and belonging. The engine's classification as Tangled Rope captures this hybrid nature, acknowledging both the coordination function and the asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Parsi community elders and the Zoroastrian priesthood are clear beneficiaries and agenda-setters (low d), as they maintain their authority and the community's continuity. Parsi youth seeking intermarriage and intermarried individuals are targets (high d), bearing the direct costs of exclusion. Other community members are beneficiaries (low d) as they gain from the preserved identity. Secular legal systems are observers, not directly affected by the constraint's internal dynamics.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (community preservation) is still live, preventing it from being a Piton. However, the high extractiveness and suppression, coupled with the 'contested' status of the founding problem, indicate that the coordination function is intertwined with significant extraction, preventing it from being a pure Rope. The classification as Tangled Rope correctly identifies this hybrid nature, where the coordination for the community comes at a high cost to individuals who deviate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    endogamy_necessity_ambiguity,
    'Is strict endogamy genuinely necessary for the long-term preservation of Parsi identity, or are there alternative, less extractive strategies for cultural and genetic continuity?',
    'Comparative sociological studies of other small diaspora communities that have maintained identity with more flexible marriage norms, or internal community initiatives exploring alternative integration models.',
    'If alternatives exist, the constraint''s suppression and extractiveness could be re-evaluated as higher than necessary for coordination, potentially reclassifying it closer to a Snare. If endogamy is proven uniquely effective, the coordination function''s justification strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(endogamy_necessity_ambiguity, empirical, 'Whether endogamy is the only viable strategy for Parsi community preservation.').

omega_variable(
    internalized_suppression_degree,
    'To what extent is the suppression experienced by Parsi youth seeking intermarriage structural (community rules, priestly authority) versus internalized (fear of social disapproval, desire for belonging)?',
    'Qualitative studies and surveys of Parsi youth, including those who have intermarried, to assess their perceived freedom of choice and the psychological costs of deviation versus adherence.',
    'If suppression is largely internalized, the effective suppression is higher than the structural measure suggests, as individuals carry the constraint with them even in the absence of direct enforcement. This would amplify the effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_degree, empirical, 'Structural vs. internalized suppression mechanism for intermarriage.').

omega_variable(
    reading_divergence_on_community_definition,
    'The ''parsi_zoroastrian_reading'' defines community primarily by lineage and endogamy. How would a ''liberal_parsi_reading'' (a hypothetical sibling) that prioritizes religious conversion and cultural adoption over birthright alter the constraint''s structure?',
    'Analysis of historical debates within the Parsi community regarding conversion and adoption, and examination of other Zoroastrian communities that permit conversion.',
    'A ''liberal_parsi_reading'' would likely reduce extractiveness and suppression by expanding the definition of community membership, potentially shifting the constraint towards a Rope or even a Scaffold if seen as a transitional phase to a more inclusive identity. This highlights the conceptual choice in defining the community''s boundaries.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_divergence_on_community_definition, conceptual, 'Impact of alternative community definitions on marriage constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__parsi_zoroastrian_reading, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t1900, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 1900, 0.1).
narrative_ontology:measurement(fami_tr_t1930, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 1930, 0.12).
narrative_ontology:measurement(fami_tr_t1960, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 1960, 0.15).
narrative_ontology:measurement(fami_tr_t1990, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(fami_tr_t2010, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 2010, 0.22).
narrative_ontology:measurement(fami_tr_t2024, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(fami_be_t1900, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 1900, 0.55).
narrative_ontology:measurement(fami_be_t1930, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 1930, 0.58).
narrative_ontology:measurement(fami_be_t1960, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 1960, 0.62).
narrative_ontology:measurement(fami_be_t1990, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 1990, 0.68).
narrative_ontology:measurement(fami_be_t2010, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 2010, 0.66).
narrative_ontology:measurement(fami_be_t2024, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t1900, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 1900, 0.7).
narrative_ontology:measurement(fami_su_t1930, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 1930, 0.72).
narrative_ontology:measurement(fami_su_t1960, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 1960, 0.75).
narrative_ontology:measurement(fami_su_t1990, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 1990, 0.8).
narrative_ontology:measurement(fami_su_t2010, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 2010, 0.79).
narrative_ontology:measurement(fami_su_t2024, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__parsi_zoroastrian_reading, identity_coordination).
narrative_ontology:affects_constraint(family_law_authority__parsi_zoroastrian_reading, family_law_authority__hindu_dharmashastra_reading).
narrative_ontology:affects_constraint(family_law_authority__parsi_zoroastrian_reading, family_law_authority__muslim_shariat_reading).
narrative_ontology:affects_constraint(family_law_authority__parsi_zoroastrian_reading, family_law_authority__christian_canonical_reading).
narrative_ontology:affects_constraint(family_law_authority__parsi_zoroastrian_reading, family_law_authority__secular_contractual_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'family_law_authority' kernel, focusing on the Parsi Zoroastrian community's specific marriage norms. It is linked to other readings of the same kernel, each representing a distinct approach to family law authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
