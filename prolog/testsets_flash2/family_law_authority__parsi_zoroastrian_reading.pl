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
 *   This constraint describes the Parsi Zoroastrian community's religious
 *   laws governing marriage, particularly the strong emphasis on endogamy, as
 *   a mechanism for community preservation. It is a reading of the broader
 *   'family_law_authority' kernel. The constraint is claimed as a Tangled
 *   Rope because it genuinely coordinates community identity and survival
 *   (beneficiaries) but does so through asymmetric extraction from
 *   individuals who deviate from endogamous norms (victims), requiring active
 *   enforcement by the priesthood and social structures. The metrics reflect
 *   this: high suppression and extractiveness due to the social and religious
 *   penalties for intermarriage, with a relatively low theater ratio as the
 *   community genuinely believes in the necessity of these rules for its
 *   survival.
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
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__parsi_zoroastrian_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__parsi_zoroastrian_reading, "Parsi Zoroastrian Marriage as Community Preservation").
narrative_ontology:topic_domain(family_law_authority__parsi_zoroastrian_reading, "comparative_law/religious_governance").

domain_priors:requires_active_enforcement(family_law_authority__parsi_zoroastrian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__parsi_zoroastrian_reading, '06a28ecb-55ea-47d0-ae84-8c4881384957').
narrative_ontology:cs_kernel_codification('06a28ecb-55ea-47d0-ae84-8c4881384957', formalized).
narrative_ontology:cs_authority_grounding('06a28ecb-55ea-47d0-ae84-8c4881384957', lineage).
narrative_ontology:cs_interpretation_layer_present('06a28ecb-55ea-47d0-ae84-8c4881384957').
narrative_ontology:cs_reading_relation('06a28ecb-55ea-47d0-ae84-8c4881384957', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('06a28ecb-55ea-47d0-ae84-8c4881384957', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('06a28ecb-55ea-47d0-ae84-8c4881384957', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('06a28ecb-55ea-47d0-ae84-8c4881384957', family_law_authority__secular_contractual_reading, coexists_with).
narrative_ontology:cs_axiom('06a28ecb-55ea-47d0-ae84-8c4881384957', foundational, community_survival_through_endogamy).
narrative_ontology:cs_axiom_status(community_survival_through_endogamy, holdable).
narrative_ontology:cs_axiom_grounding('06a28ecb-55ea-47d0-ae84-8c4881384957', community_survival_through_endogamy, conventional).
narrative_ontology:cs_axiom('06a28ecb-55ea-47d0-ae84-8c4881384957', foundational, priestly_authority_over_ritual_validity).
narrative_ontology:cs_axiom_status(priestly_authority_over_ritual_validity, holdable).
narrative_ontology:cs_axiom_grounding('06a28ecb-55ea-47d0-ae84-8c4881384957', priestly_authority_over_ritual_validity, theological).
narrative_ontology:cs_reference_frame('06a28ecb-55ea-47d0-ae84-8c4881384957', traditional_zoroastrian_community_norms).
narrative_ontology:cs_drift_state('06a28ecb-55ea-47d0-ae84-8c4881384957', contemporary_globalized_society, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('06a28ecb-55ea-47d0-ae84-8c4881384957', '').
narrative_ontology:cs_kernel_id(family_law_authority__parsi_zoroastrian_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__parsi_zoroastrian_reading, parsi_zoroastrian_community).
narrative_ontology:constraint_beneficiary(family_law_authority__parsi_zoroastrian_reading, zoroastrian_priesthood).
narrative_ontology:constraint_victim(family_law_authority__parsi_zoroastrian_reading, intermarried_individuals).
narrative_ontology:constraint_victim(family_law_authority__parsi_zoroastrian_reading, children_of_intermarriage).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the preservation of its distinct cultural and religious identity through endogamous marriage. The community's survival is seen as tied to these rules, which reinforce group cohesion and prevent assimilation. Individuals are deeply embedded in this social fabric.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, parsi_zoroastrian_community, beneficiary,
    organized, generational, identity_locked, local).

% Administers and interprets religious law, including marriage rites and rules. They hold authority over the validity of marriages and the social standing of individuals within the community, actively enforcing endogamy through social and ritual sanctions. Their authority is central to the constraint's persistence.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, zoroastrian_priesthood, agenda_setter,
    institutional, generational, constrained, local).

% Bear the costs of social exclusion, loss of community status, and potential disinheritance if they marry outside the Parsi Zoroastrian community. Their identity is deeply tied to their community, making exit extremely costly despite the personal desire to marry a non-Parsi.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, intermarried_individuals, payer,
    powerless, biographical, identity_locked, local).

% Often face ambiguous or denied religious and community status, regardless of their personal beliefs or upbringing. They are born into a situation where their identity is contested by the community's rules, with no agency to change their status.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, children_of_intermarriage, payer,
    powerless, biographical, trapped, local).

% Observe and sometimes adjudicate conflicts arising from religious marriage laws, particularly when they clash with secular civil rights or contract law. They do not directly enforce the religious constraint but can influence its practical application through legal rulings.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, secular_legal_authorities, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the preservation of the distinct Parsi Zoroastrian ethno-religious identity and cultural heritage by regulating marriage practices to maintain endogamy and community cohesion.
% TRANSFER_FUNCTION: Transfers social status, religious legitimacy, and community belonging to those who adhere to endogamous marriage rules, while extracting these from those who intermarry or are born of intermarriage.
% ABSENT_VOICES: Younger generations of Parsi Zoroastrians who seek greater individual autonomy in marriage choices, and non-Parsi spouses who are excluded from full community integration, are often marginalized in discussions about these laws.
% DISAPPEARANCE_RATIONALE: If the religious laws governing Parsi Zoroastrian marriage and its endogamy requirements vanished overnight, the community's demographic and social structure would rapidly change. Intermarriage rates would likely increase, leading to a more diverse community but also potentially diluting its distinct identity, forcing a redefinition of what it means to be Parsi Zoroastrian.
% FOUNDING_PROBLEM: The Parsi Zoroastrian community, as a small ethno-religious minority, faced existential threats to its survival and distinct identity in diaspora, necessitating strict rules to prevent assimilation and maintain its unique heritage.
% FOUNDING_PROBLEM_CORROBORATION: Community elders and religious leaders consistently attest that the threat of assimilation remains live due to the community's small size and global dispersion. While some younger members contest the efficacy or fairness of endogamy, the core concern for community preservation is widely acknowledged within and outside the community as a historical and ongoing challenge.
narrative_ontology:disappearance_verdict(family_law_authority__parsi_zoroastrian_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__parsi_zoroastrian_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__parsi_zoroastrian_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.65) and suppression (0.78) are high due to the severe social and religious consequences for intermarriage, including loss of community status and exclusion from rituals. The accessibility collapse (0.70) is significant because for many, the Parsi Zoroastrian identity is deeply ingrained, making 'exit' from the community a profound personal loss. Resistance (0.30) is present but often internalized or expressed subtly due to the strong social pressures. The theater ratio (0.20) is low because the community genuinely believes these rules are vital for its survival, so enforcement is functional, not merely performative.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Parsi Zoroastrian community and priesthood, these laws are a necessary Rope for cultural and religious survival. From the perspective of intermarried individuals, it operates as a Snare, trapping them between personal choice and deep-seated identity. The engine's classification will reflect this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The Parsi Zoroastrian community and its priesthood are beneficiaries (d near 0.0) as they gain from the preservation of their distinct identity and authority. Intermarried individuals and their children are victims (d near 1.0) as they bear the social and religious costs of non-adherence. Secular legal authorities are observers, analyzing the constraint's impact without direct participation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    endogamy_necessity_empirical,
    'Is strict endogamy empirically necessary for the long-term survival of the Parsi Zoroastrian community, or are there alternative strategies for cultural and religious preservation?',
    'Comparative demographic and sociological studies of other small ethno-religious groups that have adopted more open marriage policies, assessing their rates of cultural retention and assimilation.',
    'If endogamy is not empirically necessary, the constraint''s justification shifts from a ''Mountain of survival'' to a ''Snare of tradition,'' increasing its effective extractiveness and reducing its legitimacy as a coordination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(endogamy_necessity_empirical, empirical, 'Empirical necessity of endogamy for community survival.').

omega_variable(
    identity_vs_choice_conceptual,
    'To what extent is individual identity inextricably linked to community membership, such that ''exit'' from the community through intermarriage constitutes an identity-destroying act rather than a free choice?',
    'Qualitative sociological research and personal narratives from individuals who have intermarried and either maintained or redefined their Parsi Zoroastrian identity, exploring the subjective experience of ''identity-lock''.',
    'If identity-lock is primarily an internalized social construct rather than an inherent feature of Parsi identity, the suppression mechanism is more coercive than it appears, and the ''identity_locked'' exit option becomes more akin to ''trapped'' for individuals, increasing effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_vs_choice_conceptual, conceptual, 'The nature of identity-lock in the context of community-preserving marriage laws.').

omega_variable(
    priesthood_authority_legitimacy,
    'Is the authority of the Zoroastrian priesthood over marriage rituals and community status primarily derived from religious tradition and communal consent, or is it sustained by the social and economic leverage it holds over individuals?',
    'Historical analysis of shifts in priestly authority and community adherence, alongside contemporary surveys of community members regarding their perceptions of priestly legitimacy and the consequences of non-compliance.',
    'If authority is primarily sustained by leverage, the constraint''s ''requires_active_enforcement'' becomes more extractive, and the priesthood''s role shifts from ''benevolent coordinator'' to ''enforcer of rents'', pushing the classification closer to Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(priesthood_authority_legitimacy, empirical, 'Grounds of priestly authority in enforcing marriage norms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__parsi_zoroastrian_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t0, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(fami_tr_t10, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement(fami_tr_t20, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement(fami_tr_t30, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(fami_tr_t40, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 40, 0.19).
narrative_ontology:measurement(fami_tr_t50, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(fami_be_t0, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(fami_be_t10, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(fami_be_t20, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(fami_be_t30, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(fami_be_t40, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 40, 0.64).
narrative_ontology:measurement(fami_be_t50, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t0, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(fami_su_t10, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 10, 0.72).
narrative_ontology:measurement(fami_su_t20, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 20, 0.74).
narrative_ontology:measurement(fami_su_t30, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 30, 0.76).
narrative_ontology:measurement(fami_su_t40, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 40, 0.77).
narrative_ontology:measurement(fami_su_t50, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 50, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
