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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   norms, which are governed by religious law and tradition, with a strong
 *   emphasis on endogamy to preserve the community's distinct identity. It is
 *   a reading of the broader 'family_law_authority' kernel, focusing on the
 *   specific mechanisms and justifications within this small ethno-religious
 *   group. The constraint is claimed as a Tangled Rope due to its genuine
 *   coordination function (community preservation) coupled with asymmetric
 *   extraction from those who deviate from endogamous norms. The metrics
 *   reflect moderate extraction and significant suppression, as social and
 *   religious penalties for intermarriage are real and actively enforced.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__parsi_zoroastrian_reading, 0.45).
domain_priors:suppression_score(family_law_authority__parsi_zoroastrian_reading, 0.65).
domain_priors:theater_ratio(family_law_authority__parsi_zoroastrian_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__parsi_zoroastrian_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__parsi_zoroastrian_reading, "Parsi Zoroastrian Marriage as Community Preservation").
narrative_ontology:topic_domain(family_law_authority__parsi_zoroastrian_reading, "comparative_law/religious_governance").

domain_priors:requires_active_enforcement(family_law_authority__parsi_zoroastrian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__parsi_zoroastrian_reading, '182cba4d-f70b-491e-beec-fe543ea0d646').
narrative_ontology:cs_kernel_codification('182cba4d-f70b-491e-beec-fe543ea0d646', formalized).
narrative_ontology:cs_authority_grounding('182cba4d-f70b-491e-beec-fe543ea0d646', lineage).
narrative_ontology:cs_interpretation_layer_present('182cba4d-f70b-491e-beec-fe543ea0d646').
narrative_ontology:cs_reading_relation('182cba4d-f70b-491e-beec-fe543ea0d646', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('182cba4d-f70b-491e-beec-fe543ea0d646', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('182cba4d-f70b-491e-beec-fe543ea0d646', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('182cba4d-f70b-491e-beec-fe543ea0d646', family_law_authority__secular_contractual_reading, coexists_with).
narrative_ontology:cs_axiom('182cba4d-f70b-491e-beec-fe543ea0d646', foundational, endogamy_as_community_preservation).
narrative_ontology:cs_axiom_status(endogamy_as_community_preservation, holdable).
narrative_ontology:cs_axiom_grounding('182cba4d-f70b-491e-beec-fe543ea0d646', endogamy_as_community_preservation, conventional).
narrative_ontology:cs_axiom('182cba4d-f70b-491e-beec-fe543ea0d646', foundational, priestly_authority_over_ritual_validity).
narrative_ontology:cs_axiom_status(priestly_authority_over_ritual_validity, holdable).
narrative_ontology:cs_axiom_grounding('182cba4d-f70b-491e-beec-fe543ea0d646', priestly_authority_over_ritual_validity, theological).
narrative_ontology:cs_reference_frame('182cba4d-f70b-491e-beec-fe543ea0d646', traditional_parsi_community_norms).
narrative_ontology:cs_drift_state('182cba4d-f70b-491e-beec-fe543ea0d646', contemporary_globalized_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('182cba4d-f70b-491e-beec-fe543ea0d646', '').
narrative_ontology:cs_kernel_id(family_law_authority__parsi_zoroastrian_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__parsi_zoroastrian_reading, parsi_zoroastrian_community).
narrative_ontology:constraint_beneficiary(family_law_authority__parsi_zoroastrian_reading, priestly_council).
narrative_ontology:constraint_victim(family_law_authority__parsi_zoroastrian_reading, intermarrying_parsis).
narrative_ontology:constraint_victim(family_law_authority__parsi_zoroastrian_reading, parsi_youth).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the preservation of its distinct cultural and religious identity through endogamous marriage, maintaining a small, cohesive group. The community's survival is seen as tied to these marital norms.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, parsi_zoroastrian_community, beneficiary,
    organized, generational, identity_locked, local).

% Administers and enforces the religious laws governing marriage, including ritual validity and community recognition. They are the primary interpreters of tradition and hold significant social authority within the community.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, priestly_council, agenda_setter,
    institutional, generational, identity_locked, local).

% Individuals who marry outside the Parsi Zoroastrian community. They face social ostracization, loss of community status, and their children may not be recognized as Parsi, bearing the direct costs of the endogamy requirement.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, intermarrying_parsis, payer,
    powerless, biographical, constrained, local).

% Younger generations who may face pressure to conform to endogamous norms, limiting their choice of partners. While not directly penalized for intermarriage until it occurs, the social expectation is a significant constraint on their personal autonomy.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, parsi_youth, payer,
    moderate, biographical, identity_locked, local).

% National legal frameworks (e.g., in India) that recognize personal laws for religious communities but also uphold principles of individual rights and equality. They observe and sometimes adjudicate conflicts between religious and secular marital norms.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, secular_legal_systems, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the preservation of the distinct Parsi Zoroastrian ethno-religious identity and cultural heritage by regulating marriage practices to maintain endogamy and ritual purity.
% TRANSFER_FUNCTION: Transfers social status, community recognition, and religious legitimacy to endogamously married couples and their offspring, while denying these to intermarrying individuals and their families.
% ABSENT_VOICES: Potential Parsi reformers advocating for more inclusive interpretations of community membership and marriage, who are often marginalized or silenced by traditional authorities. Also, non-Parsi spouses of intermarrying individuals, who have no standing within the community's internal governance.
% DISAPPEARANCE_RATIONALE: If the religious laws governing Parsi marriage and the associated social enforcement vanished, the community's endogamous structure would rapidly dissolve. Intermarriage rates would likely increase, leading to a more diverse, less distinct Parsi population over generations, fundamentally altering the community's identity and demographic profile.
% FOUNDING_PROBLEM: The Parsi Zoroastrian community, as a small diaspora group, faced the existential threat of assimilation and loss of identity in foreign lands, particularly after migration from Persia.
% FOUNDING_PROBLEM_CORROBORATION: Community elders and religious scholars consistently attest to the ongoing threat of demographic decline and cultural assimilation. While some younger members contest the efficacy or fairness of endogamy as a solution, the underlying problem of community preservation is widely acknowledged within and outside the community, including by sociological studies of diaspora groups.
narrative_ontology:disappearance_verdict(family_law_authority__parsi_zoroastrian_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__parsi_zoroastrian_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__parsi_zoroastrian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(family_law_authority__parsi_zoroastrian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__parsi_zoroastrian_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__parsi_zoroastrian_reading_tests).
:- end_tests(family_law_authority__parsi_zoroastrian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) stems from the social and religious costs imposed on intermarrying Parsis, including loss of community status and non-recognition of children. Suppression (0.65) is high due to strong social pressure, religious authority, and the limited exit options for individuals deeply embedded in the community. The theater ratio (0.20) is relatively low, indicating that the primary function of community preservation is still actively pursued, though some rituals may have performative elements. The slight increase in extractiveness and suppression over the mid-20th century reflects a period of heightened concern over demographic decline and a tightening of community boundaries, with a slight relaxation in recent decades as globalized norms exert counter-pressure.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the priestly council and many community elders, these marriage laws are a necessary Rope for the survival of a unique and ancient faith. From the perspective of intermarrying Parsis or those seeking greater individual autonomy, the same structure operates as a Snare, extracting personal freedom and imposing significant costs for choices that are legally permissible in the wider society. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Parsi Zoroastrian community and its priestly council are clear beneficiaries, as the constraint directly serves their goal of cultural and religious preservation. Intermarrying Parsis and Parsi youth are the primary payers, bearing the social and personal costs of the endogamy requirement. Secular legal systems act as observers, potentially intervening if religious norms conflict with broader human rights, but generally respecting community autonomy in personal law matters.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    endogamy_necessity_ambiguity,
    'Is strict endogamy truly necessary for the long-term preservation of Parsi Zoroastrian identity, or are there alternative, more inclusive strategies for community survival?',
    'Longitudinal sociological studies comparing demographic and cultural outcomes of Parsi communities with varying degrees of endogamy enforcement, or successful adoption of alternative community-building models.',
    'If endogamy is found not to be strictly necessary, the justification for the constraint''s suppressive and extractive elements would weaken, potentially reclassifying it closer to a Snare. If it is found to be critical, the Rope-like coordination function would be reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(endogamy_necessity_ambiguity, empirical, 'Whether endogamy is a necessary or merely traditional mechanism for community preservation.').

omega_variable(
    internalized_vs_structural_suppression,
    'To what extent is the suppression experienced by Parsi youth structural (social ostracism, religious penalties) versus internalized (self-censorship, identity fusion with community norms)?',
    'Qualitative sociological research on Parsi youth experiences, including post-exit interviews with those who intermarried, to assess the persistence of self-imposed constraints after external pressures are removed.',
    'If suppression is largely internalized, the effective suppression is higher than the structural measure suggests, as individuals carry the constraint with them. If primarily structural, external reforms would be more effective in reducing its impact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression, empirical, 'Structural vs. internalized suppression mechanism for Parsi youth.').

omega_variable(
    community_definition_ambiguity,
    'What constitutes ''Parsi Zoroastrian identity'' in the modern era, and can it accommodate individuals with mixed heritage or non-traditional marital choices without dissolving?',
    'Community-wide referendums, theological reinterpretations by influential scholars, or the emergence of new, widely accepted community norms that redefine membership criteria.',
    'A more inclusive definition would reduce the extractive and suppressive aspects of the marriage constraint, potentially shifting its classification towards a Rope or even a Scaffold (if transitional). A rigid definition reinforces the current Tangled Rope structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(community_definition_ambiguity, conceptual, 'The conceptual boundary of Parsi Zoroastrian identity and its implications for marriage norms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__parsi_zoroastrian_reading, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t1900, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 1900, 0.1).
narrative_ontology:measurement(fami_tr_t1930, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 1930, 0.15).
narrative_ontology:measurement(fami_tr_t1960, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 1960, 0.2).
narrative_ontology:measurement(fami_tr_t1990, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 1990, 0.25).
narrative_ontology:measurement(fami_tr_t2010, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 2010, 0.22).
narrative_ontology:measurement(fami_tr_t2024, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(fami_be_t1900, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 1900, 0.35).
narrative_ontology:measurement(fami_be_t1930, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 1930, 0.38).
narrative_ontology:measurement(fami_be_t1960, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 1960, 0.42).
narrative_ontology:measurement(fami_be_t1990, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 1990, 0.45).
narrative_ontology:measurement(fami_be_t2010, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 2010, 0.46).
narrative_ontology:measurement(fami_be_t2024, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t1900, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 1900, 0.55).
narrative_ontology:measurement(fami_su_t1930, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 1930, 0.6).
narrative_ontology:measurement(fami_su_t1960, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 1960, 0.65).
narrative_ontology:measurement(fami_su_t1990, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 1990, 0.68).
narrative_ontology:measurement(fami_su_t2010, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 2010, 0.67).
narrative_ontology:measurement(fami_su_t2024, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__parsi_zoroastrian_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(family_law_authority__parsi_zoroastrian_reading, 0.08).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'family_law_authority' kernel, focusing on the Parsi Zoroastrian community's specific marriage laws and their role in identity preservation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
