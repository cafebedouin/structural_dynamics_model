% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__parsi_communal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Parsi Marriage Authority — Communal Tribunal Reading
 *   domain: comparative_law/constitutional_pluralism/religious_governance
 *
 * SUMMARY:
 *   The Parsi Marriage and Divorce Act 1936 establishes a unique hybrid
 *   tribunal system: district-level courts with a Parsi judge and two
 *   community delegates (one priest, one layperson) adjudicate marriage,
 *   divorce, and maintenance for Parsis. The Act codifies community custom
 *   but also rigidifies endogamy — only marriages between two Parsis are
 *   recognized. As the Parsi population has declined from ~114,000 (1941) to
 *   ~57,000 (2011), the coordination function (community tribunals resolving
 *   disputes) persists but the extraction function (endogamy enforcement
 *   against a shrinking pool) intensifies. Theater rises as tribunals
 *   increasingly perform community continuity while actual marital options
 *   collapse. This reading claims the constraint is a genuine coordination
 *   mechanism (tangled_rope) but metrics show rising extraction and theater.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__parsi_communal_reading, 0.68).
domain_priors:suppression_score(marriage_authority_kernel__parsi_communal_reading, 0.62).
domain_priors:theater_ratio(marriage_authority_kernel__parsi_communal_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__parsi_communal_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__parsi_communal_reading, "Parsi Marriage Authority — Communal Tribunal Reading").
narrative_ontology:topic_domain(marriage_authority_kernel__parsi_communal_reading, "comparative_law/constitutional_pluralism/religious_governance").

domain_priors:requires_active_enforcement(marriage_authority_kernel__parsi_communal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__parsi_communal_reading, '5ee50601-1114-4e4b-a4bc-d30b928bfa0a').
narrative_ontology:cs_kernel_codification('5ee50601-1114-4e4b-a4bc-d30b928bfa0a', formalized).
narrative_ontology:cs_authority_grounding('5ee50601-1114-4e4b-a4bc-d30b928bfa0a', lineage).
narrative_ontology:cs_interpretation_layer_present('5ee50601-1114-4e4b-a4bc-d30b928bfa0a').
narrative_ontology:cs_reading_relation('5ee50601-1114-4e4b-a4bc-d30b928bfa0a', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('5ee50601-1114-4e4b-a4bc-d30b928bfa0a', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('5ee50601-1114-4e4b-a4bc-d30b928bfa0a', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('5ee50601-1114-4e4b-a4bc-d30b928bfa0a', marriage_authority_kernel__secular_civil_reading, coexists_with).
narrative_ontology:cs_axiom('5ee50601-1114-4e4b-a4bc-d30b928bfa0a', foundational, parsi_communal_autonomy).
narrative_ontology:cs_axiom_status(parsi_communal_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('5ee50601-1114-4e4b-a4bc-d30b928bfa0a', parsi_communal_autonomy, conventional).
narrative_ontology:cs_axiom('5ee50601-1114-4e4b-a4bc-d30b928bfa0a', foundational, endogamy_as_identity_preservation).
narrative_ontology:cs_axiom_status(endogamy_as_identity_preservation, holdable).
narrative_ontology:cs_axiom_grounding('5ee50601-1114-4e4b-a4bc-d30b928bfa0a', endogamy_as_identity_preservation, deontological).
narrative_ontology:cs_reference_frame('5ee50601-1114-4e4b-a4bc-d30b928bfa0a', parsi_customary_law_1936_codification).
narrative_ontology:cs_drift_state('5ee50601-1114-4e4b-a4bc-d30b928bfa0a', contemporary_demographic_crisis, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5ee50601-1114-4e4b-a4bc-d30b928bfa0a', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__parsi_communal_reading, parsi_community_leadership).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__parsi_communal_reading, parsi_endogamous_families).
narrative_ontology:constraint_victim(marriage_authority_kernel__parsi_communal_reading, parsi_intermarried_individuals).
narrative_ontology:constraint_victim(marriage_authority_kernel__parsi_communal_reading, parsi_youth_facing_demographic_collapse).
narrative_ontology:constraint_victim(marriage_authority_kernel__parsi_communal_reading, non_parsi_spouses_excluded).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__parsi_communal_reading, communal_autonomy_in_personal_law).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__parsi_communal_reading, endogamy_as_identity_preservation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Parsi priests (dasturs), trustees of Parsi panchayats, and tribunal delegates administer the 1936 Act. They control tribunal appointments, interpret custom, and gatekeep community membership. Their authority derives from the Act's recognition of community tribunals. Exit means abandoning communal leadership role — identity-fused.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_community_leadership, agenda_setter,
    organized, generational, identity_locked, national).

% Families marrying within the community access tribunal divorce, maintenance, and inheritance rights under the Act. They benefit from recognized marital status, property regimes, and community support structures. Exit (marrying out) triggers loss of these benefits and often community ostracism.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_endogamous_families, beneficiary,
    moderate, biographical, constrained, national).

% Parsis who marry non-Parsis lose access to community tribunals, face non-recognition of marriage under the Act, and encounter barriers to inheritance, fire temple access, and child initiation ceremonies. They bear the cost of the endogamy rule while the community leadership retains authority.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_intermarried_individuals, payer,
    moderate, biographical, constrained, national).

% Young Parsis face a shrinking partner pool (community ~60,000 and declining). The endogamy rule makes finding a spouse within the community increasingly difficult. They cannot easily exit the constraint — leaving the community severs family ties and identity — but staying means high probability of forced celibacy or delayed marriage.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_youth_facing_demographic_collapse, payer,
    powerless, biographical, trapped, national).

% Non-Parsi spouses of Parsis have no standing in Parsi tribunals, no inheritance rights under Parsi law, and no path to community membership. They would object to the exclusionary framework but have no voice in the communal reading. Their only exit is the secular civil route (Special Marriage Act), which severs the Parsi partner from community recognition.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, non_parsi_spouses_excluded, excluded,
    powerless, biographical, trapped, national).

% High Courts and Supreme Court exercise appellate review over Parsi tribunal decisions. They interpret the 1936 Act's constitutional validity, occasionally reading down endogamy provisions. They do not administer the tribunals but constrain their outer boundaries.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, indian_state_courts, observer,
    institutional, generational, analytical, national).

% Scholars tracking Parsi population decline (from ~114,000 in 1941 to ~57,000 in 2011) and its implications for the tribunal system's viability. They provide the empirical baseline for whether the coordination function can persist.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, demographic_researchers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Community self-governance of marriage and divorce through priest-judge tribunals, preserving Parsi identity, property regimes, and fire temple access within a recognized legal framework that operates alongside state courts.
% TRANSFER_FUNCTION: Moves authority over marital validity, dissolution, and inheritance from individual choice to community tribunals; enforces endogamy by denying Act recognition to exogamous marriages, transferring the cost of boundary maintenance to intermarried individuals and their children.
% ABSENT_VOICES: Parsis who marry outside the community and their non-Parsi spouses are structurally excluded from tribunal standing. Parsis who would prefer civil marriage options but remain in the community for family/identity reasons have no internal reform pathway. Youth facing demographic collapse have no organized representation in tribunal governance.
% DISAPPEARANCE_RATIONALE: If the 1936 Act and its tribunal system vanished overnight, Parsi marital governance would revert to uncodified custom (no longer enforceable in state courts) or shift to the Special Marriage Act. Property inheritance, fire temple access, child initiation (navjote), and community membership rules would reorganize — likely fragmenting along reformist/traditionalist lines.
% FOUNDING_PROBLEM: Colonial codification (1936) of Parsi customary marriage and divorce practices to provide legal certainty, community autonomy, and a defined tribunal system under British rule, replacing informal priestly arbitration with a hybrid court structure.
% FOUNDING_PROBLEM_CORROBORATION: Community leadership (Bombay Parsi Panchayat, dasturs) attests the founding problem is live — communal autonomy and identity preservation remain essential. Demographic researchers (e.g., UNESCO Parsi Zoroastrian Project), intermarried Parsi advocates, and Law Commission reports attest the colonial-era framework no longer serves demographic reality and now functions as a demographic accelerant.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__parsi_communal_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__parsi_communal_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__parsi_communal_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_authority_kernel__parsi_communal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__parsi_communal_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Base extractiveness (0.68) reflects the rising cost of endogamy enforcement as partner pool shrinks — the constraint extracts reproductive and marital freedom from youth. Suppression (0.62) is structural: non-recognition of exogamous marriages, loss of inheritance/temple access, and lack of internal reform mechanisms. Theater (0.55) is high because tribunals continue operating but handle fewer cases (demographic decline) while maintaining the same ceremonial authority structure. Accessibility collapse (0.71) is high because alternatives (civil marriage) require exiting community recognition entirely. Resistance (0.48) is moderate — intermittent litigation and reform petitions exist but no sustained movement has altered the Act.
 *
 * PERSPECTIVAL GAP:
 *   From the leadership seat, the tribunal system is a living coordination mechanism preserving identity. From the youth seat, it is a demographic trap. From the intermarried seat, it is an exclusion machine. The engine computes this divergence from the structural data — the same constraint operates as rope for some, snare for others, piton for the institution itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Community leadership (agenda_setter) sits at beneficiary end (d ~0.15) — they control tribunals and define membership. Endogamous families (beneficiary) near symmetric (d ~0.45) — they gain recognition but face demographic pressure. Intermarried individuals and youth (payers) sit at target end (d ~0.85-0.95) — they bear exclusion costs with trapped/constrained exit. Non-Parsi spouses (excluded) are off the directionality scale — structurally invisible to the constraint. State courts (observer) analytical (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (colonial-era legal certainty for a viable community) is contested — community says live, demography says dead. The arrangement persists despite the founding problem's erosion because leadership identity is fused with the tribunal system (identity_locked), and no stakeholder has both incentive and power to reform it. This is classic mandatrophy: the mandate (community autonomy via tribunals) has outlived its function (viable endogamous community), but the constraint remains via institutional inertia and identity fusion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    demographic_viability_threshold,
    'At what population size does the Parsi tribunal system become non-functional — unable to constitute delegate panels, maintain fire temple access rules, or sustain endogamous marriage pools?',
    'Demographic modeling of Parsi population trajectories (current ~57,000, declining ~10% per decade) combined with tribunal caseload data and delegate availability thresholds.',
    'If the threshold is crossed within 1-2 generations, the coordination function collapses entirely, reclassifying the constraint from tangled_rope to piton (inertial performance) or snare (pure extraction via exclusion). If the community stabilizes, tangled_rope persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_viability_threshold, empirical, 'Whether the coordination function has a demographic floor below which it cannot operate.').

omega_variable(
    endogamy_enforcement_mechanism,
    'Is the endogamy rule enforced primarily through structural legal non-recognition (Act provisions) or through internalized community pressure (ostracism, family pressure, identity fusion)?',
    'Comparative analysis of intermarried Parsi outcomes: those who pursue civil marriage vs. those who remain in community; survey data on internalized vs. external sanctions.',
    'If primarily internalized, suppression is higher than legal measures suggest — the constraint travels with the agent post-exit. If primarily structural, exit via Special Marriage Act genuinely escapes the constraint (though at identity cost).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(endogamy_enforcement_mechanism, conceptual, 'Whether suppression is carried by law or by internalized community norms.').

omega_variable(
    colonial_codification_legacy,
    'Does the 1936 Act faithfully codify pre-colonial Parsi custom, or does it reshape custom into a more rigid, tribunal-dependent form that serves colonial administrative convenience?',
    'Historical analysis of pre-1936 Parsi matrimonial practice (priestly arbitration, panchayat records) vs. the Act''s provisions; comparison with other colonial personal law codifications.',
    'If the Act reshaped custom, the ''communal autonomy'' axiom is partially a colonial artifact — weakening the lineage authority_grounding. If faithful, the lineage claim is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colonial_codification_legacy, empirical, 'Whether the Act''s authority derives from authentic custom or colonial restructuring.').

omega_variable(
    kernel_framing_alternative,
    'Is the marriage_authority_kernel best framed as ''personal law authority'' (communal) or ''family law authority'' (individual rights), and does this framing choice determine which reading appears structurally dominant?',
    'Comparative analysis of how each sibling reading''s metrics shift when the kernel is reframed from communal-autonomy to individual-rights reference frame.',
    'If framing determines classification, the kernel itself is under-specified — a conceptual omega for the kernel level, not this reading alone. Would require cross-reading meta-analysis.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_alternative, conceptual, 'Whether the kernel''s framing prejudices the structural analysis of all readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__parsi_communal_reading, 0, 88).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(parsi_communal_tr_t0, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(parsi_communal_tr_t22, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 22, 0.22).
narrative_ontology:measurement(parsi_communal_tr_t44, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 44, 0.35).
narrative_ontology:measurement(parsi_communal_tr_t66, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 66, 0.48).
narrative_ontology:measurement(parsi_communal_tr_t88, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 88, 0.55).

% Extraction over time
narrative_ontology:measurement(parsi_communal_be_t0, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(parsi_communal_be_t22, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 22, 0.42).
narrative_ontology:measurement(parsi_communal_be_t44, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 44, 0.51).
narrative_ontology:measurement(parsi_communal_be_t66, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 66, 0.6).
narrative_ontology:measurement(parsi_communal_be_t88, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 88, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(parsi_communal_su_t0, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(parsi_communal_su_t22, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 22, 0.45).
narrative_ontology:measurement(parsi_communal_su_t44, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 44, 0.52).
narrative_ontology:measurement(parsi_communal_su_t66, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 66, 0.58).
narrative_ontology:measurement(parsi_communal_su_t88, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 88, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__parsi_communal_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_authority_kernel__parsi_communal_reading, 0.08).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel__hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel__muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel__christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel__secular_civil_reading).

% DUAL FORMULATION NOTE:
% This reading is one of five in the marriage_authority_kernel constraint family. The kernel decomposes because each personal law system has distinct coordination/extraction profiles: Hindu (codified, court-administered, moderately extractive), Muslim (board-interpreted, gender-asymmetric, highly extractive), Christian (colonial statute, church courts, low extractive), Parsi (community tribunals, endogamy-enforcing, rising extractive), Secular (civil code, individual rights, low extractive). The Parsi reading is unique in combining identity_coordination with severe demographic threat to its own viability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_authority_kernel__parsi_communal_reading, organized, 0.15).
constraint_indexing:directionality_override(marriage_authority_kernel__parsi_communal_reading, moderate, 0.85).
constraint_indexing:directionality_override(marriage_authority_kernel__parsi_communal_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
