% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__parsi_communal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__parsi_communal_reading, []).

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
 *   constraint_id: marriage_authority_kernel__parsi_communal_reading
 *   human_readable: Parsi Marriage and Family Law (Communal Reading)
 *   domain: comparative_law/religious_governance/social
 *
 * SUMMARY:
 *   This constraint describes the Parsi community's marriage and family law
 *   in India, as codified in the Parsi Marriage and Divorce Act 1936, from
 *   the perspective of the Parsi communal reading. It is one reading of the
 *   broader 'marriage_authority_kernel' in India, which encompasses various
 *   religious and secular legal frameworks. The Parsi reading emphasizes the
 *   preservation of community identity through customary law, particularly
 *   endogamy, which is actively enforced through social pressure and
 *   community tribunals. The metrics reflect the internal equity of Parsi law
 *   in some aspects, but also the significant extraction of individual
 *   marriage choice for collective survival, and the active suppression
 *   required to maintain endogamy in the face of demographic decline.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__parsi_communal_reading, 0.6).
domain_priors:suppression_score(marriage_authority_kernel__parsi_communal_reading, 0.75).
domain_priors:theater_ratio(marriage_authority_kernel__parsi_communal_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__parsi_communal_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__parsi_communal_reading, "Parsi Marriage and Family Law (Communal Reading)").
narrative_ontology:topic_domain(marriage_authority_kernel__parsi_communal_reading, "comparative_law/religious_governance/social").

domain_priors:requires_active_enforcement(marriage_authority_kernel__parsi_communal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__parsi_communal_reading, 'af2eaed0-4b8b-4c2e-8439-eac67284fdea').
narrative_ontology:cs_kernel_codification('af2eaed0-4b8b-4c2e-8439-eac67284fdea', formalized).
narrative_ontology:cs_authority_grounding('af2eaed0-4b8b-4c2e-8439-eac67284fdea', lineage).
narrative_ontology:cs_interpretation_layer_present('af2eaed0-4b8b-4c2e-8439-eac67284fdea').
narrative_ontology:cs_reading_relation('af2eaed0-4b8b-4c2e-8439-eac67284fdea', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('af2eaed0-4b8b-4c2e-8439-eac67284fdea', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('af2eaed0-4b8b-4c2e-8439-eac67284fdea', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('af2eaed0-4b8b-4c2e-8439-eac67284fdea', marriage_authority_kernel__secular_civil_reading, coexists_with).
narrative_ontology:cs_axiom('af2eaed0-4b8b-4c2e-8439-eac67284fdea', foundational, parsi_identity_through_endogamy).
narrative_ontology:cs_axiom_status(parsi_identity_through_endogamy, holdable).
narrative_ontology:cs_axiom_grounding('af2eaed0-4b8b-4c2e-8439-eac67284fdea', parsi_identity_through_endogamy, conventional).
narrative_ontology:cs_axiom('af2eaed0-4b8b-4c2e-8439-eac67284fdea', foundational, community_custom_as_supreme_law).
narrative_ontology:cs_axiom_status(community_custom_as_supreme_law, holdable).
narrative_ontology:cs_axiom_grounding('af2eaed0-4b8b-4c2e-8439-eac67284fdea', community_custom_as_supreme_law, conventional).
narrative_ontology:cs_reference_frame('af2eaed0-4b8b-4c2e-8439-eac67284fdea', parsi_communal_identity_preservation).
narrative_ontology:cs_drift_state('af2eaed0-4b8b-4c2e-8439-eac67284fdea', contemporary_demographic_pressure, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('af2eaed0-4b8b-4c2e-8439-eac67284fdea', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__parsi_communal_reading, parsi_community_elders).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__parsi_communal_reading, parsi_community_members).
narrative_ontology:constraint_victim(marriage_authority_kernel__parsi_communal_reading, individual_parsis_seeking_exogamy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and enforce Parsi customary law, including endogamy rules, as codified in the Parsi Marriage and Divorce Act 1936. They are responsible for preserving the community's distinct identity and demographic viability.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_community_elders, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from the preservation of Parsi cultural and religious identity, social cohesion, and community support. They are expected to adhere to endogamy rules, which can constrain individual marriage choices but reinforces group belonging.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_community_members, beneficiary,
    moderate, biographical, identity_locked, national).

% Bear the cost of restricted marriage choices due to endogamy requirements. Marrying outside the community can lead to social ostracization, loss of community benefits, and their children not being recognized as Parsi under customary law.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, individual_parsis_seeking_exogamy, payer,
    powerless, biographical, constrained, national).

% Interprets and applies the Parsi Marriage and Divorce Act 1936 within the broader framework of Indian constitutional law. They adjudicate disputes but generally uphold the community's personal law unless it directly conflicts with fundamental rights.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, indian_state_judiciary, observer,
    institutional, generational, analytical, national).

% Advocate for individual autonomy in marriage and challenge personal laws that impose restrictions based on religion or community. While they can litigate, their arguments often face resistance from communities seeking to preserve distinct identities.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, secular_civil_rights_advocates, excluded,
    organized, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority_kernel__parsi_communal_reading, parsi_community_elders).
narrative_ontology:fixing_cost_class(marriage_authority_kernel__parsi_communal_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the preservation of the distinct religious, cultural, and ethnic identity of the Parsi community in India through specific marriage and family law, particularly enforcing endogamy to maintain demographic boundaries.
% TRANSFER_FUNCTION: Transfers individual autonomy in marriage choice from individual Parsi members to the collective authority of community custom and codified law, in exchange for the preservation of Parsi identity and community cohesion.
% ABSENT_VOICES: Individual Parsis who have married outside the community and their descendants, as well as secular civil rights advocates, are largely excluded from the internal discourse on Parsi personal law, though they may challenge it externally.
% DISAPPEARANCE_RATIONALE: If Parsi marriage law and its communal enforcement vanished overnight, the Parsi community's distinct legal and social identity would rapidly dissolve. Intermarriage would increase, and the unique cultural practices tied to family structures would assimilate into broader Indian society, fundamentally altering the community's existence.
% FOUNDING_PROBLEM: The Parsi community, a small ethno-religious minority in India, faced the challenge of preserving its distinct identity, culture, and religious practices in a diverse society, particularly against demographic decline and assimilation pressures.
% FOUNDING_PROBLEM_CORROBORATION: Parsi demographic data consistently shows a declining population, and community leaders frequently express concerns about cultural erosion and the need for preservation. This is corroborated by sociological studies and historical records of community efforts to maintain distinctiveness, from sources outside the immediate benefiting parties.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__parsi_communal_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__parsi_communal_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__parsi_communal_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(marriage_authority_kernel__parsi_communal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__parsi_communal_reading, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__parsi_communal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority_kernel__parsi_communal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority_kernel__parsi_communal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.6) because while Parsi law offers internal equity in many aspects of family life, the core requirement of endogamy imposes a substantial cost on individual freedom of choice, especially given the small and declining population. Suppression is high (0.75) due to strong social pressure, community oversight, and the legal framework that reinforces endogamy, making exit difficult without significant personal and social cost. Theater ratio is low (0.1) as the community actively maintains and believes in the functional necessity of these customs for its survival. Accessibility collapse is high (0.7) because alternatives to endogamy within the Parsi framework are severely limited, pushing individuals towards difficult choices of leaving the community's legal and social embrace.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Parsi community elders, the constraint operates as a vital 'Rope' for identity coordination and cultural survival, ensuring the continuity of a unique heritage. However, from the perspective of individual Parsis who wish to marry outside the community, it can feel like a 'Snare' due to the high social and personal costs associated with defying endogamy, despite the internal equity of other aspects of Parsi family law. The engine's computation of a 'Tangled Rope' reflects this dual nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Parsi community elders and members are beneficiaries (low d) as they gain from the preservation of their distinct identity and cultural continuity. Individual Parsis seeking exogamy are targets (high d) as they bear the direct cost of restricted marriage choices and potential social exclusion. The Indian state judiciary acts as an observer, upholding the personal law while balancing it with constitutional principles.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    endogamy_necessity_vs_individual_rights,
    'Is strict endogamy a necessary condition for the long-term preservation of Parsi identity and culture, or does it represent an undue restriction on individual autonomy that could be relaxed without existential threat?',
    'Sociological studies on the impact of intermarriage on Parsi identity in other diasporic communities, or internal community dialogue leading to a re-evaluation of membership criteria.',
    'If endogamy is found not to be strictly necessary, the constraint''s suppression and extractiveness would be re-evaluated downwards, potentially shifting its classification towards a more benign ''Rope'' or even ''Scaffold'' if transitional. If deemed necessary, the current metrics would be reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(endogamy_necessity_vs_individual_rights, conceptual, 'The structural necessity of endogamy for Parsi identity preservation versus individual rights.').

omega_variable(
    demographic_decline_impact_on_enforcement,
    'How does the ongoing demographic decline of the Parsi community affect the intensity and perceived legitimacy of endogamy enforcement, and does it lead to increased internal resistance?',
    'Longitudinal studies of Parsi community attitudes towards intermarriage, analysis of community tribunal rulings over time, and surveys of younger Parsi generations regarding their marriage choices and adherence to custom.',
    'If demographic pressure leads to increased enforcement and internal resistance, the constraint''s suppression and extractiveness may continue to rise, solidifying its ''Tangled Rope'' or even ''Snare'' classification. If it leads to a softening of enforcement or a redefinition of Parsi identity, the metrics could decrease.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_decline_impact_on_enforcement, empirical, 'Impact of demographic decline on endogamy enforcement and internal resistance.').

omega_variable(
    internalized_vs_structural_suppression_exogamy,
    'What proportion of the suppression against exogamy is due to formal legal/community sanctions versus internalized social norms and identity fusion within the Parsi community?',
    'Qualitative sociological research exploring the lived experiences of Parsis contemplating or undertaking exogamous marriages, and the psychological costs associated with defying community expectations.',
    'If a significant portion of suppression is internalized, the effective suppression for individuals is higher than structural measures alone suggest, as the constraint''s force persists even in the absence of overt external enforcement. This would amplify the effective extraction for targets.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression_exogamy, empirical, 'Structural vs. internalized suppression mechanism for endogamy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__parsi_communal_reading, 1936, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1936, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 1936, 0.05).
narrative_ontology:measurement(marr_tr_t1960, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 1960, 0.07).
narrative_ontology:measurement(marr_tr_t1980, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 1980, 0.08).
narrative_ontology:measurement(marr_tr_t2000, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 2000, 0.09).
narrative_ontology:measurement(marr_tr_t2024, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(marr_be_t1936, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 1936, 0.5).
narrative_ontology:measurement(marr_be_t1960, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 1960, 0.52).
narrative_ontology:measurement(marr_be_t1980, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 1980, 0.55).
narrative_ontology:measurement(marr_be_t2000, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(marr_be_t2024, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 2024, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1936, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 1936, 0.65).
narrative_ontology:measurement(marr_su_t1960, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 1960, 0.68).
narrative_ontology:measurement(marr_su_t1980, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 1980, 0.7).
narrative_ontology:measurement(marr_su_t2000, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 2000, 0.73).
narrative_ontology:measurement(marr_su_t2024, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__parsi_communal_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel__hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel__muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel__christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel__secular_civil_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'marriage_authority_kernel', which encompasses various religious and secular legal frameworks for marriage in India. Each reading represents a distinct structural constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
