% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__muslim_shariat_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__muslim_shariat_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: marriage_authority_kernel__muslim_shariat_reading
 *   human_readable: Muslim Personal Law Marriage Authority (Shariat Reading)
 *   domain: constitutional_pluralism/religious_governance
 *
 * SUMMARY:
 *   Muslim personal law in India operates as an uncodified religious legal
 *   system governing marriage, divorce, maintenance, inheritance, and custody
 *   for Muslims. Authority derives from Shariat as interpreted by community
 *   institutions (All-India Muslim Personal Law Board, state boards, local
 *   qazis). The system coordinates community dispute resolution and preserves
 *   religious identity but extracts asymmetrically along gender lines: men
 *   hold unilateral divorce rights, polygamy permissions, and superior
 *   inheritance shares. State intervention is contested — courts have
 *   incrementally reformed practice (Shah Bano maintenance, triple talaq
 *   criminalization) while boards resist codification as majoritarian
 *   imposition. The constraint is a tangled rope: genuine coordination
 *   (community governance, religious continuity) coexists with gender-based
 *   extraction maintained through religious authority and social enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__muslim_shariat_reading, 0.68).
domain_priors:suppression_score(marriage_authority_kernel__muslim_shariat_reading, 0.72).
domain_priors:theater_ratio(marriage_authority_kernel__muslim_shariat_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__muslim_shariat_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__muslim_shariat_reading, "Muslim Personal Law Marriage Authority (Shariat Reading)").
narrative_ontology:topic_domain(marriage_authority_kernel__muslim_shariat_reading, "constitutional_pluralism/religious_governance").

domain_priors:requires_active_enforcement(marriage_authority_kernel__muslim_shariat_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__muslim_shariat_reading, '5718a3c9-50ce-47a6-8246-ffa166ed9aa2').
narrative_ontology:cs_kernel_codification('5718a3c9-50ce-47a6-8246-ffa166ed9aa2', distributed).
narrative_ontology:cs_authority_grounding('5718a3c9-50ce-47a6-8246-ffa166ed9aa2', lineage).
narrative_ontology:cs_interpretation_layer_present('5718a3c9-50ce-47a6-8246-ffa166ed9aa2').
narrative_ontology:cs_reading_relation('5718a3c9-50ce-47a6-8246-ffa166ed9aa2', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('5718a3c9-50ce-47a6-8246-ffa166ed9aa2', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('5718a3c9-50ce-47a6-8246-ffa166ed9aa2', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_reading_relation('5718a3c9-50ce-47a6-8246-ffa166ed9aa2', marriage_authority_kernel__secular_civil_reading, influences).
narrative_ontology:cs_axiom('5718a3c9-50ce-47a6-8246-ffa166ed9aa2', foundational, shariat_as_sovereign_family_law).
narrative_ontology:cs_axiom_status(shariat_as_sovereign_family_law, holdable).
narrative_ontology:cs_axiom_grounding('5718a3c9-50ce-47a6-8246-ffa166ed9aa2', shariat_as_sovereign_family_law, theological).
narrative_ontology:cs_axiom('5718a3c9-50ce-47a6-8246-ffa166ed9aa2', foundational, male_guardianship_in_marriage).
narrative_ontology:cs_axiom_status(male_guardianship_in_marriage, holdable).
narrative_ontology:cs_axiom_grounding('5718a3c9-50ce-47a6-8246-ffa166ed9aa2', male_guardianship_in_marriage, theological).
narrative_ontology:cs_axiom('5718a3c9-50ce-47a6-8246-ffa166ed9aa2', secondary, community_autonomy_over_state).
narrative_ontology:cs_axiom_status(community_autonomy_over_state, holdable).
narrative_ontology:cs_axiom_grounding('5718a3c9-50ce-47a6-8246-ffa166ed9aa2', community_autonomy_over_state, conventional).
narrative_ontology:cs_reference_frame('5718a3c9-50ce-47a6-8246-ffa166ed9aa2', classical_fiqh_framework).
narrative_ontology:cs_drift_state('5718a3c9-50ce-47a6-8246-ffa166ed9aa2', contemporary_constitutional_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5718a3c9-50ce-47a6-8246-ffa166ed9aa2', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__muslim_shariat_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__muslim_shariat_reading, muslim_men).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__muslim_shariat_reading, personal_law_boards).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__muslim_shariat_reading, qazis).
narrative_ontology:constraint_victim(marriage_authority_kernel__muslim_shariat_reading, muslim_women).
narrative_ontology:constraint_victim(marriage_authority_kernel__muslim_shariat_reading, children_of_muslim_marriages).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__muslim_shariat_reading, religious_community_autonomy).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__muslim_shariat_reading, shariat_as_complete_legal_order).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Subject to unilateral talaq (though criminalized in 2019, social practice persists), polygamy without consent, unequal inheritance (half share), and restricted custody rights. Exit requires leaving religious community identity; civil law alternatives exist (Special Marriage Act) but carry severe social ostracism. State courts can be approached but face community pressure to use religious forums.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, muslim_women, payer,
    moderate, biographical, identity_locked, national).

% Retain unilateral divorce initiation (talaq), permission for up to four wives, double inheritance share, and favorable custody presumptions. Can access civil courts but religious forum advantages them. Exit to secular framework (Special Marriage Act) is legally available but rarely chosen due to community and familial pressure.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, muslim_men, beneficiary,
    moderate, biographical, mobile, national).

% All-India Muslim Personal Law Board and state-level boards claim authoritative interpretation of Shariat. Issue fatwas, adjudicate disputes through darul-qaza, lobby against state codification (Uniform Civil Code), and resist reforms framed as external interference. Funded by community donations and waqf properties; their authority derives from religious legitimacy claims.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, personal_law_boards, agenda_setter,
    institutional, generational, arbitrage, national).

% Local religious judges who solemnize marriages, register divorces, and adjudicate maintenance/custody. Derive income and status from fee-based services and community recognition. Their authority is recognized by state for marriage registration but their judicial decisions lack formal enforcement power — compliance relies on religious obligation and social pressure.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, qazis, agenda_setter,
    organized, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__muslim_shariat_reading, qazis, beneficiary).

% Supreme Court and High Courts adjudicate constitutional challenges (Shah Bano 1985, Shayara Bano 2017, ongoing UCC petitions). Can override personal law on fundamental rights grounds but face political backlash accusations of judicial overreach. Their interventions create parallel precedent tracks that personal law boards contest or ignore.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, state_courts, observer,
    institutional, generational, analytical, national).

% Subject to custody rules favoring fathers after early years, inheritance disparities, and legitimacy determinations based on parents' marriage form. No independent legal capacity; interests mediated through mothers (who are themselves constrained) or state-appointed guardians in litigation.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, children_of_muslim_marriages, payer,
    powerless, biographical, trapped, national).

% Advocate for Uniform Civil Code and gender-just reform from constitutional equality framework. Excluded from personal law board deliberations; their framing treated as majoritarian imposition. Operate through PILs, legislative advocacy, and international human rights mechanisms.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, secular_rights_activists, excluded,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a religiously legitimate dispute resolution system for marriage, divorce, maintenance, and inheritance within the Muslim community, preserving communal cohesion and distinct identity in a plural legal order.
% TRANSFER_FUNCTION: Moves authority over family life outcomes from individual women to male household heads and religious interpreters; transfers dispute resolution jurisdiction from state courts to community tribunals; transfers inheritance wealth from daughters/wives to sons/husbands per fixed Shariat shares.
% ABSENT_VOICES: Muslim women reformists within the community (e.g., Bharatiya Muslim Mahila Andolan) who argue for gender-just interpretations from within the tradition; their exclusion from board leadership structures is structural. Also excluded: interfaith couples choosing Special Marriage Act who face community hostility without state protection.
% DISAPPEARANCE_RATIONALE: If Shariat-based personal law authority vanished overnight, Muslim marriages would default to the Special Marriage Act (secular civil code) or require new community consensus mechanisms. Inheritance would equalize, unilateral talaq would disappear, polygamy would be criminal. The community's distinct legal identity would collapse, triggering massive political mobilization from boards and likely state intervention to fill the vacuum.
% FOUNDING_PROBLEM: Post-colonial constitutional compromise (Article 44 Directive Principle vs. Article 25-26 religious freedom) preserved British-era non-interference in personal laws to secure minority acceptance of the Indian Union. The arrangement was built to solve the founding problem of minority accommodation in a majoritarian democracy.
% FOUNDING_PROBLEM_CORROBORATION: Constituent Assembly debates record the minority accommodation rationale (corroborated by historians Granville Austin, Rochana Bajpai). Personal law boards assert the problem is live — minority identity requires legal distinctness. Women's rights organizations and Law Commission reports (2018) attest the founding compromise has become a vehicle for gender subordination; the problem of minority accommodation no longer requires gender-unequal family law.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__muslim_shariat_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__muslim_shariat_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__muslim_shariat_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_authority_kernel__muslim_shariat_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__muslim_shariat_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__muslim_shariat_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority_kernel__muslim_shariat_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority_kernel__muslim_shariat_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects structural gender asymmetry in core family law outcomes — not marginal but foundational (divorce initiation, inheritance quantum, custody). Suppression (0.72) is high because exit to secular law (Special Marriage Act) carries severe social sanctions; resistance (0.58) from women's groups and courts is met with mobilization framing reform as existential threat to community identity. Theater ratio (0.38) is moderate: coordination function (dispute resolution, marriage solemnization) is real but a growing share of enforcement energy defends gender privileges rather than communal cohesion. Accessibility collapse (0.45) is partial — civil law alternatives exist legally but are socially inaccessible for most.
 *
 * PERSPECTIVAL GAP:
 *   From the board/qazi seat, the constraint is a rope: genuine coordination of Muslim family life under divine law, threatened by state encroachment. From the Muslim woman seat, it is a snare: extraction justified by theological cover, enforced through identity captivity. From the state court seat, it is a contested tangled_rope: coordination function acknowledged but extraction deemed unconstitutional. The engine computes these divergent classifications from the same structural data — the authored claim (tangled_rope) reflects the structural reality that BOTH coordination and extraction are genuine and inseparable in current operation.
 *
 * DIRECTIONALITY LOGIC:
 *   Muslim women are primary targets (d near 1.0): identity-locked exit, bear extraction across divorce, inheritance, custody. Muslim men are beneficiaries (d near 0.0): mobile exit, collect asymmetrical rights. Personal law boards and qazis are agenda_setters with institutional/organized power — they administer the constraint and benefit from its authority (fees, status, control). State courts are observers with analytical exit — they constrain but cannot fully displace the system. Children are trapped payers with no voice. Secular activists are excluded — their constitutional equality framing is structurally locked out of the interpretive community.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (minority accommodation) is contested — boards claim it remains live; reformists and Law Commission say it has been weaponized beyond its purpose. The arrangement persists not because the founding problem is solved but because the coordination function (community identity maintenance) has fused with the extraction function (male privilege). Mandatrophy is unresolved: the constraint has outlived its original constitutional compromise but its religious legitimacy claim prevents clean sunset. The theater_ratio rise tracks this — more performance of 'community defense' to maintain extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_fusion,
    'Is the gender asymmetry in Muslim personal law a separable extraction layer atop a genuine coordination core, or is the coordination function itself constitutively dependent on patriarchal authority structures?',
    'Comparative analysis of reformist Islamic jurisprudence (e.g., Morocco 2004 Mudawwana reform, Tunisia) that retained Shariat framework but equalized divorce, inheritance, custody — if coordination persists without extraction, they are separable.',
    'If separable, the constraint is a tangled_rope where extraction can be surgically removed; if fused, any reform that equalizes gender rights destroys the coordination function, making the constraint a snare with coordination as cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_fusion, conceptual, 'Whether coordination and extraction are structurally separable in this reading.').

omega_variable(
    state_intervention_trajectory,
    'Does incremental state intervention (judicial reform, legislative override) strengthen the constraint''s coordination function by forcing procedural regularity, or does it accelerate its transformation into a pure extraction mechanism by delegitimizing religious authority?',
    'Longitudinal study of darul-qaza caseloads, compliance rates, and board discourse pre/post major interventions (1985 Shah Bano, 2017 triple talaq, potential UCC).',
    'If intervention strengthens coordination, the constraint may evolve toward scaffold (transitional state-supervised reform). If it delegitimizes authority, extraction hardens into snare as boards double down on identity defense.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_intervention_trajectory, empirical, 'Directional effect of state intervention on the constraint''s functional composition.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of Muslim women''s exit from Shariat-based family law primarily structural (social ostracism, economic dependence, lack of legal literacy) or internalized (theological conviction that Shariat is divinely just, identity fusion with community norms)?',
    'Post-reform trajectory analysis: if women granted civil law alternatives (Special Marriage Act, criminalized triple talaq) still choose or are pressured into religious forums, internalized suppression is significant. Survey data on theological attitudes vs. practical preferences.',
    'If internalized, effective suppression is higher than structural measures suggest — the constraint travels with the agent after formal exit. This would raise the constraint''s classification severity for the payer seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for Muslim women.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__muslim_shariat_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1950, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 1950, 0.25).
narrative_ontology:measurement(marr_tr_t1970, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 1970, 0.28).
narrative_ontology:measurement(marr_tr_t1985, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 1985, 0.32).
narrative_ontology:measurement(marr_tr_t2000, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(marr_tr_t2017, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 2017, 0.37).
narrative_ontology:measurement(marr_tr_t2024, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 2024, 0.38).

% Extraction over time
narrative_ontology:measurement(marr_be_t1950, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 1950, 0.55).
narrative_ontology:measurement(marr_be_t1970, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 1970, 0.58).
narrative_ontology:measurement(marr_be_t1985, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 1985, 0.62).
narrative_ontology:measurement(marr_be_t2000, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement(marr_be_t2017, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 2017, 0.66).
narrative_ontology:measurement(marr_be_t2024, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1950, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 1950, 0.45).
narrative_ontology:measurement(marr_su_t1970, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 1970, 0.52).
narrative_ontology:measurement(marr_su_t1985, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 1985, 0.65).
narrative_ontology:measurement(marr_su_t2000, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(marr_su_t2017, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 2017, 0.7).
narrative_ontology:measurement(marr_su_t2024, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__muslim_shariat_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_authority_kernel__muslim_shariat_reading, 0.1).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, marriage_authority_kernel__hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, marriage_authority_kernel__christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, marriage_authority_kernel__parsi_communal_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, marriage_authority_kernel__secular_civil_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, uniform_civil_code_debate).

% DUAL FORMULATION NOTE:
% This reading and its siblings form the marriage_authority_kernel constraint family. The kernel is the constitutional compromise preserving religious personal laws. This reading's epsilon (0.68) is substantially higher than the hindu_codified_reading (~0.35 post-1955 reforms) and secular_civil_reading (~0.15), reflecting its uncodified status and gender asymmetry. The network edges capture institutional coupling: personal law boards coordinate resistance to UCC across communities; state court precedents in one reading migrate to others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_authority_kernel__muslim_shariat_reading, organized, 0.15).
constraint_indexing:directionality_override(marriage_authority_kernel__muslim_shariat_reading, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
