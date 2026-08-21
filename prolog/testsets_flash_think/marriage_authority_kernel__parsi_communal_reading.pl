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
 *   constraint_id: marriage_authority_kernel__parsi_communal_reading
 *   human_readable: Parsi Marriage Law (Communal Reading)
 *   domain: comparative_law/religious_governance/constitutional_pluralism
 *
 * SUMMARY:
 *   This constraint describes the Parsi Marriage and Divorce Act 1936 as a
 *   reading of the broader 'marriage_authority_kernel'. It codifies Parsi
 *   community custom, primarily to preserve the distinct identity of the
 *   Parsi minority in India through endogamous marriage. While claimed as a
 *   'rope' by the community for its coordination function in identity
 *   preservation, the metrics reflect a 'tangled_rope' due to the high
 *   suppression and extraction imposed on members who seek to marry outside
 *   the community, especially Parsi women. The demographic decline of the
 *   Parsi community adds pressure to maintain these norms, intensifying the
 *   constraint's extractive aspects over time.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__parsi_communal_reading, 0.65).
domain_priors:suppression_score(marriage_authority_kernel__parsi_communal_reading, 0.75).
domain_priors:theater_ratio(marriage_authority_kernel__parsi_communal_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__parsi_communal_reading, rope).
narrative_ontology:human_readable(marriage_authority_kernel__parsi_communal_reading, "Parsi Marriage Law (Communal Reading)").
narrative_ontology:topic_domain(marriage_authority_kernel__parsi_communal_reading, "comparative_law/religious_governance/constitutional_pluralism").

domain_priors:requires_active_enforcement(marriage_authority_kernel__parsi_communal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__parsi_communal_reading, '58e5f842-8dcc-4c14-8be1-316134797ac5').
narrative_ontology:cs_kernel_codification('58e5f842-8dcc-4c14-8be1-316134797ac5', formalized).
narrative_ontology:cs_authority_grounding('58e5f842-8dcc-4c14-8be1-316134797ac5', lineage).
narrative_ontology:cs_interpretation_layer_present('58e5f842-8dcc-4c14-8be1-316134797ac5').
narrative_ontology:cs_reading_relation('58e5f842-8dcc-4c14-8be1-316134797ac5', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('58e5f842-8dcc-4c14-8be1-316134797ac5', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('58e5f842-8dcc-4c14-8be1-316134797ac5', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('58e5f842-8dcc-4c14-8be1-316134797ac5', marriage_authority_kernel__secular_civil_reading, coexists_with).
narrative_ontology:cs_axiom('58e5f842-8dcc-4c14-8be1-316134797ac5', foundational, parsi_identity_through_endogamy).
narrative_ontology:cs_axiom_status(parsi_identity_through_endogamy, holdable).
narrative_ontology:cs_axiom_grounding('58e5f842-8dcc-4c14-8be1-316134797ac5', parsi_identity_through_endogamy, conventional).
narrative_ontology:cs_axiom('58e5f842-8dcc-4c14-8be1-316134797ac5', foundational, communal_autonomy_in_family_law).
narrative_ontology:cs_axiom_status(communal_autonomy_in_family_law, holdable).
narrative_ontology:cs_axiom_grounding('58e5f842-8dcc-4c14-8be1-316134797ac5', communal_autonomy_in_family_law, conventional).
narrative_ontology:cs_reference_frame('58e5f842-8dcc-4c14-8be1-316134797ac5', traditional_parsi_customary_law).
narrative_ontology:cs_drift_state('58e5f842-8dcc-4c14-8be1-316134797ac5', contemporary_demographic_decline, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('58e5f842-8dcc-4c14-8be1-316134797ac5', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__parsi_communal_reading, parsi_community_members).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__parsi_communal_reading, parsi_community_leaders).
narrative_ontology:constraint_victim(marriage_authority_kernel__parsi_communal_reading, parsi_youth_seeking_exogamous_marriage).
narrative_ontology:constraint_victim(marriage_authority_kernel__parsi_communal_reading, parsi_women_marrying_non_parsis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer and interpret the Parsi Marriage and Divorce Act 1936, ensuring adherence to community customs, particularly endogamy. They benefit from the preservation of Parsi identity and their role in its governance, but are constrained by demographic realities.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_community_leaders, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from the preservation of a distinct cultural and religious identity, community support networks, and a legal framework tailored to their customs. They face social pressure to conform to endogamous marriage norms.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_community_members, beneficiary,
    organized, biographical, constrained, national).

% Bear the social and sometimes legal costs of marrying outside the Parsi community, potentially losing community membership, inheritance rights, or social standing. Their identity is deeply tied to the community, making exit difficult despite legal alternatives.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_youth_seeking_exogamous_marriage, payer,
    powerless, biographical, identity_locked, national).

% Historically and often currently face more severe consequences than men for exogamous marriage, including loss of religious and community rights for themselves and their children. This creates an asymmetric burden within the community.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_women_marrying_non_parsis, payer,
    powerless, biographical, identity_locked, national).

% Interprets and applies the Parsi Marriage and Divorce Act 1936 within the broader framework of Indian constitutional law, balancing communal autonomy with individual rights. They do not directly benefit or pay but adjudicate disputes.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, indian_state_judiciary, observer,
    institutional, generational, analytical, national).

% Advocate for a uniform civil code in India, which would supersede personal laws like the Parsi Act. They are excluded from the internal governance of Parsi law but exert external pressure for legal reform.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, secular_marriage_advocates, excluded,
    moderate, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To preserve the distinct cultural, religious, and social identity of the Parsi community in India through a codified system of family law that emphasizes endogamous marriage and community-specific customs.
% TRANSFER_FUNCTION: Transfers authority over marriage and family matters from individual choice to community custom and codified law, imposing social and sometimes legal costs on members who deviate from endogamous norms, particularly for women.
% ABSENT_VOICES: Parsi individuals who have married outside the community and are no longer recognized, or those who have left the community due to its restrictive norms. Secular legal scholars advocating for a uniform civil code also represent an absent voice within the Parsi legal framework.
% DISAPPEARANCE_RATIONALE: If the Parsi Marriage and Divorce Act and its communal enforcement vanished, the distinct legal and social identity of the Parsi community would rapidly dissolve, leading to full integration into the secular civil code or other religious legal frameworks. This would fundamentally alter the community's structure and self-conception.
% FOUNDING_PROBLEM: To protect and preserve the distinct identity, customs, and demographic viability of the Parsi community, a small religious minority in India, from assimilation into larger religious or secular populations.
% FOUNDING_PROBLEM_CORROBORATION: Parsi community leaders and scholars consistently attest to the ongoing urgency of preserving Parsi identity, citing demographic decline as a major threat. Sociologists and legal historians corroborate the historical context of minority protection and the community's efforts to maintain its distinctiveness.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__parsi_communal_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__parsi_communal_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__parsi_communal_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(marriage_authority_kernel__parsi_communal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__parsi_communal_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   Base extractiveness (0.65) is moderate-to-high, reflecting the significant social and identity costs imposed on individuals who deviate from endogamy, even if the internal operation of the law is equitable. Suppression (0.75) is high due to strong community pressure, social ostracization, and formal rules that enforce endogamy. Theater ratio (0.10) is low, indicating the system is genuinely functional in its stated goal of identity preservation, with minimal performative maintenance. Accessibility collapse (0.50) is moderate; while secular marriage is legally available, choosing it often entails a high cost of community disaffiliation. Resistance (0.20) is low, as open defiance is rare, but demographic decline and quiet exits represent a form of passive resistance. The claimed type 'rope' reflects the community's self-perception of a beneficial coordination mechanism, while the metrics describe its more extractive reality.
 *
 * PERSPECTIVAL GAP:
 *   Parsi community leaders and many members perceive this constraint as a vital 'rope' for cultural survival and identity coordination, essential for maintaining their distinct heritage. However, individuals seeking exogamous marriage, particularly Parsi women, experience it as a highly extractive and suppressive 'snare' or 'tangled_rope' that limits their autonomy and imposes significant personal costs for deviation. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Parsi community leaders and members who conform are beneficiaries, gaining from identity preservation and communal cohesion. Parsi youth seeking exogamous marriage and Parsi women marrying non-Parsi men are victims, bearing the costs of social exclusion and loss of rights. The Indian state judiciary acts as an observer, adjudicating within the existing legal framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate to preserve Parsi identity remains live, as evidenced by ongoing demographic decline. This prevents mislabeling it as a 'piton' where the function has atrophied. However, the increasing extractiveness and suppression over time, driven by demographic pressures, indicate a risk of the coordination function being overshadowed by the costs of its maintenance, pushing it further towards a 'snare' if the balance shifts too far.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    endogamy_necessity_for_identity,
    'Is strict endogamy structurally necessary for the long-term preservation of Parsi identity, or could more inclusive practices (e.g., recognizing children of Parsi mothers and non-Parsi fathers) sustain it?',
    'Sociological studies on identity resilience in other minority communities with evolving endogamy norms, or a longitudinal study of Parsi communities that have adopted more inclusive practices.',
    'If endogamy is not strictly necessary, the constraint''s suppression and extraction related to exogamy could be re-evaluated as disproportionate, potentially shifting its classification towards a ''snare'' or ''piton'' if the community''s rationale for it is weakened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(endogamy_necessity_for_identity, empirical, 'Whether endogamy is an indispensable component of Parsi identity preservation.').

omega_variable(
    demographic_pressure_legitimacy,
    'How does the intensifying demographic decline of the Parsi community affect the perceived legitimacy and internal adherence to endogamous marriage norms?',
    'Community surveys, ethnographic studies, and analysis of marriage patterns over time, particularly examining the rate of exogamous marriages and community responses.',
    'If demographic pressure leads to increased internal dissent or a higher rate of unpunished exogamous marriages, the constraint''s effective suppression might decrease, and its theater ratio could rise, indicating a shift towards a ''piton'' or a weakening ''tangled_rope''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(demographic_pressure_legitimacy, empirical, 'Impact of demographic decline on the constraint''s internal legitimacy and enforcement.').

omega_variable(
    committer_frame_ambiguity,
    'This constraint is one reading of the ''marriage_authority_kernel''. How would adopting a sibling reading (e.g., ''secular_civil_reading'') alter the structural properties and classification?',
    'Comparative legal analysis of the Special Marriage Act 1954 and its impact on individual autonomy versus communal authority, and analysis of the experiences of Parsi individuals who have opted for secular marriage.',
    'A ''secular_civil_reading'' would shift authority grounding to individual rights, eliminate endogamy enforcement, and drastically reduce extraction and suppression, likely classifying as a ''rope'' or ''mountain'' (of law) for individual autonomy, but dissolving the Parsi communal constraint entirely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_ambiguity, conceptual, 'Impact of alternative readings of the marriage authority kernel on this constraint''s structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__parsi_communal_reading, 1936, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1936, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 1936, 0.05).
narrative_ontology:measurement(marr_tr_t1956, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 1956, 0.06).
narrative_ontology:measurement(marr_tr_t1976, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 1976, 0.07).
narrative_ontology:measurement(marr_tr_t1996, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 1996, 0.08).
narrative_ontology:measurement(marr_tr_t2010, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 2010, 0.09).
narrative_ontology:measurement(marr_tr_t2024, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(marr_be_t1936, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 1936, 0.55).
narrative_ontology:measurement(marr_be_t1956, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 1956, 0.58).
narrative_ontology:measurement(marr_be_t1976, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 1976, 0.6).
narrative_ontology:measurement(marr_be_t1996, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 1996, 0.62).
narrative_ontology:measurement(marr_be_t2010, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 2010, 0.64).
narrative_ontology:measurement(marr_be_t2024, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1936, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 1936, 0.65).
narrative_ontology:measurement(marr_su_t1956, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 1956, 0.68).
narrative_ontology:measurement(marr_su_t1976, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 1976, 0.7).
narrative_ontology:measurement(marr_su_t1996, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 1996, 0.72).
narrative_ontology:measurement(marr_su_t2010, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 2010, 0.74).
narrative_ontology:measurement(marr_su_t2024, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__parsi_communal_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
