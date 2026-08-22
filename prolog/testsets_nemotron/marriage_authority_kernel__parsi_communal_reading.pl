% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__parsi_communal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-14
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
 *   constraint_id: marriage_authority_kernel__parsi_communal_reading
 *   human_readable: Parsi Community Marriage Authority (Parsi Marriage and Divorce Act 1936)
 *   domain: comparative_law/constitutional_pluralism/religious_governance
 *
 * SUMMARY:
 *   The Parsi Marriage and Divorce Act 1936 codifies Parsi community custom
 *   into statutory law, creating a hybrid constraint: community tribunals
 *   (delegates appointed by the Punchayet) exercise original jurisdiction
 *   over marriage, divorce, and maintenance, with appeals to High Courts. The
 *   Act is celebrated for gender-equitable provisions (mutual consent divorce
 *   1988, equal grounds, maintenance rights) that exceed most personal laws.
 *   But it simultaneously enforces strict endogamy (patrilineal descent, loss
 *   of community status for intermarried women and their children) and
 *   concentrates interpretive authority in the Punchayet — an unelected body
 *   of trustees. Demographic collapse (from ~114,000 in 1941 to ~50,000 in
 *   2024) makes the constraint's survival function self-defeating: the
 *   endogamy enforcement that once preserved the community now accelerates
 *   its disappearance. The constraint is a tangled rope — genuine
 *   coordination (gender-equitable dispute resolution, trust preservation)
 *   fused with asymmetric extraction (demographic gatekeeping, dissent
 *   suppression, intermarried exclusion).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__parsi_communal_reading, 0.38).
domain_priors:suppression_score(marriage_authority_kernel__parsi_communal_reading, 0.42).
domain_priors:theater_ratio(marriage_authority_kernel__parsi_communal_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, resistance, 0.31).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__parsi_communal_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__parsi_communal_reading, "Parsi Community Marriage Authority (Parsi Marriage and Divorce Act 1936)").
narrative_ontology:topic_domain(marriage_authority_kernel__parsi_communal_reading, "comparative_law/constitutional_pluralism/religious_governance").

domain_priors:requires_active_enforcement(marriage_authority_kernel__parsi_communal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__parsi_communal_reading, '8eda0e78-181d-4991-9733-48748abfc2e9').
narrative_ontology:cs_kernel_codification('8eda0e78-181d-4991-9733-48748abfc2e9', formalized).
narrative_ontology:cs_authority_grounding('8eda0e78-181d-4991-9733-48748abfc2e9', lineage).
narrative_ontology:cs_interpretation_layer_present('8eda0e78-181d-4991-9733-48748abfc2e9').
narrative_ontology:cs_reading_relation('8eda0e78-181d-4991-9733-48748abfc2e9', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('8eda0e78-181d-4991-9733-48748abfc2e9', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('8eda0e78-181d-4991-9733-48748abfc2e9', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('8eda0e78-181d-4991-9733-48748abfc2e9', marriage_authority_kernel__secular_civil_reading, influences).
narrative_ontology:cs_axiom('8eda0e78-181d-4991-9733-48748abfc2e9', foundational, community_autonomy_preserves_gender_equity).
narrative_ontology:cs_axiom_status(community_autonomy_preserves_gender_equity, holdable).
narrative_ontology:cs_axiom_grounding('8eda0e78-181d-4991-9733-48748abfc2e9', community_autonomy_preserves_gender_equity, empirically_contingent).
narrative_ontology:cs_axiom('8eda0e78-181d-4991-9733-48748abfc2e9', foundational, endogamy_is_necessary_for_community_survival).
narrative_ontology:cs_axiom_status(endogamy_is_necessary_for_community_survival, holdable).
narrative_ontology:cs_axiom_grounding('8eda0e78-181d-4991-9733-48748abfc2e9', endogamy_is_necessary_for_community_survival, empirically_contingent).
narrative_ontology:cs_axiom('8eda0e78-181d-4991-9733-48748abfc2e9', secondary, punchayet_authority_derives_from_community_consent).
narrative_ontology:cs_axiom_status(punchayet_authority_derives_from_community_consent, overridden).
narrative_ontology:cs_axiom_grounding('8eda0e78-181d-4991-9733-48748abfc2e9', punchayet_authority_derives_from_community_consent, conventional).
narrative_ontology:cs_reference_frame('8eda0e78-181d-4991-9733-48748abfc2e9', parsi_communal_legal_autonomy_1936).
narrative_ontology:cs_drift_state('8eda0e78-181d-4991-9733-48748abfc2e9', contemporary_demographic_crisis, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8eda0e78-181d-4991-9733-48748abfc2e9', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__parsi_communal_reading, parsi_anjanas).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__parsi_communal_reading, parsi_punchayet).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__parsi_communal_reading, parsi_women_within_community).
narrative_ontology:constraint_victim(marriage_authority_kernel__parsi_communal_reading, parsi_intermarried_individuals).
narrative_ontology:constraint_victim(marriage_authority_kernel__parsi_communal_reading, parsi_dissidents).
narrative_ontology:constraint_victim(marriage_authority_kernel__parsi_communal_reading, parsi_youth_outside_endogamy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__parsi_communal_reading, parsi_priesthood).
narrative_ontology:constraint_victim(marriage_authority_kernel__parsi_communal_reading, parsi_women_within_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Parsi Anjumas (community associations) and the Bombay Parsi Punchayet administer the Act's tribunals, appoint delegates, and control the interpretation of community custom. They benefit from the institutional authority and gatekeeping power over marriage, divorce, and inheritance within the community. Their identity is fused with the community's survival — exit means dissolution of the communal structure they steward.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_anjanas, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__parsi_communal_reading, parsi_anjanas, beneficiary).

% The apex body that administers the Parsi Marriage and Divorce Act's tribunal system, manages community trusts, and defines who counts as Parsi for legal purposes. Collects administrative fees and exercises disciplinary authority. Its legitimacy derives entirely from the Act's recognition; without the statutory framework, its authority collapses to moral suasion only.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_punchayet, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__parsi_communal_reading, parsi_punchayet, beneficiary).

% Benefit from the Act's unusually gender-equitable provisions (equal divorce grounds, mutual consent divorce since 1988 amendment, maintenance rights, property protections). But pay through mandatory community tribunal jurisdiction, endogamy requirements, and limited exit to secular law. The Special Marriage Act 1954 exists as formal exit but carries severe social ostracism and loss of community property rights.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_women_within_community, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__parsi_communal_reading, parsi_women_within_community, payer).

% Parsis who marry outside the community lose access to community tribunals, inheritance under Parsi law, fire temple entry, and community trust benefits. Their children are often denied Parsi status (patrilineal descent rule enforced by Punchayet). The constraint extracts their community membership and material benefits as the price of exogamy. No effective exit — the secular Special Marriage Act grants civil marriage but not community re-entry.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_intermarried_individuals, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__parsi_communal_reading, parsi_intermarried_individuals, excluded).

% Parsis who challenge Punchayet authority (e.g., on descent rules, women's priesthood, trust governance) face excommunication, loss of community housing, and denial of religious rites. The Act's tribunal system offers no internal dissent channel — the delegates are Punchayet-appointed. Exit to secular courts is theoretically possible but practically blocked by community ostracism and the Act's ouster of civil court jurisdiction over 'matters of community custom.'
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_dissidents, payer,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__parsi_communal_reading, parsi_dissidents, excluded).

% Young Parsis who reject endogamy norms face demographic erasure: marrying out means their children are not Parsi; marrying in shrinks the pool (community ~50,000 and declining). They bear the cost of a constraint designed for demographic survival that now accelerates demographic collapse. Some exit entirely to secular identity; others negotiate within. Their mobility is real but the identity cost is total.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_youth_outside_endogamy, payer,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__parsi_communal_reading, parsi_youth_outside_endogamy, excluded).

% High Courts and Supreme Court hear appeals from Parsi matrimonial tribunals and constitutional challenges to the Act's provisions. They interpret the boundary between community autonomy and fundamental rights. Their jurisprudence (e.g., on whether Punchayet's descent rules violate Article 14/21) shapes the constraint's enforcement but they do not administer it.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, secular_courts, observer,
    institutional, generational, analytical, national).

% The mobeds (priests) perform marriage ceremonies and certify religious compliance. Their authority is bound to the community's endogamous continuity. They benefit from the Act's recognition of religious rites as legally necessary. Women's entry into priesthood is contested — the constraint's gender equity stops at the altar.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_priesthood, beneficiary,
    organized, generational, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of maintaining a distinct ethnoreligious community's legal continuity, property transmission, and dispute resolution without assimilating into the majority legal order. Provides a unified tribunal system, recognized marriage rites, and inheritance rules that preserve community trust assets and demographic boundaries.
% TRANSFER_FUNCTION: Moves authority over marriage, divorce, maintenance, and inheritance from the secular state to community tribunals; moves community membership and material benefits (trust housing, fire temple access, inheritance) from intermarried/dissident individuals to the collective; moves demographic survival costs onto youth who must choose between endogamy and community extinction.
% ABSENT_VOICES: Parsis who have fully exited to secular identity (estimated 30-40% of demographic cohort) — they would object to the Act's continuing jurisdiction over their descendants but are not in the conversation. Children of intermarried couples denied Parsi status — they have no standing in community tribunals. Women seeking priesthood — excluded by the same religious authority the Act empowers.
% DISAPPEARANCE_RATIONALE: If the Act vanished overnight, Parsi matrimonial disputes would flood secular courts under the Special Marriage Act and Indian Succession Act; community trusts would lose their legal framework; the Punchayet's authority would revert to voluntary association; fire temple access disputes would become civil property cases. The community's legal distinctiveness — its primary survival mechanism — would dissolve. The demographic decline would accelerate as the cost of exit drops to zero.
% FOUNDING_PROBLEM: After British colonial courts began applying English law to Parsis (1830s-1860s), the community faced legal fragmentation: no unified marriage law, inconsistent inheritance rulings, and erosion of trust property. The 1936 Act was built to reclaim communal legal autonomy, codify custom into statute, and protect community assets from colonial legal incursion.
% FOUNDING_PROBLEM_CORROBORATION: Parsi Anjumas and Punchayet attest the founding problem is live — colonial legal incursion has been replaced by constitutional majoritarianism threatening community autonomy. Secular courts and women's rights advocates (e.g., Goolrukh Gupta litigation, 2017) attest the founding problem is substantially solved — the community has legal recognition, gender-equitable provisions, and property protection; the constraint now primarily serves demographic gatekeeping. Independent legal historians (e.g., Mitra Sharafi, 'Law and Identity in Colonial South Asia') corroborate the shifted-function reading.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__parsi_communal_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__parsi_communal_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__parsi_communal_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(marriage_authority_kernel__parsi_communal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__parsi_communal_reading, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__parsi_communal_reading_tests).
:- end_tests(marriage_authority_kernel__parsi_communal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38) reflects the constraint's dual character: low extraction for compliant endogamous members (genuine coordination benefit), high extraction for intermarried/dissident individuals (total community exclusion). Suppression (0.42) is moderate — the constraint relies more on identity-locked exit barriers (social ostracism, loss of inheritance, religious exclusion) than active coercion. Theater ratio (0.25) rising over time: the gender-equity provisions are real but the constraint increasingly performs 'community survival' while its enforcement mechanics (descent rules, trust governance) hasten demographic collapse. Accessibility collapse (0.52) — alternatives exist (Special Marriage Act) but carry prohibitive identity costs. Resistance (0.31) — internal dissent exists (women's priesthood, descent rule challenges) but is fragmented and lacks institutional leverage.
 *
 * PERSPECTIVAL GAP:
 *   From the Anjuna/Punchayet seat, the constraint is a rope — genuine coordination preserving community autonomy and gender equity. From the intermarried/dissident seats, it is a snare — extraction of community membership and material benefits enforced through identity-locked exit barriers. From the youth seat, it is a piton — the constraint's survival function (endogamy) now produces the opposite of its intent (demographic collapse), maintained theatrically because no actor has both the incentive and capacity to reform it. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Anjanas/Punchayet are structural beneficiaries (institutional authority, gatekeeping revenue, identity fusion — d near 0.1). Parsi women within community are dual-positioned: beneficiaries of gender-equitable provisions but payers of endogamy compliance (d ~0.45). Intermarried individuals, dissidents, and youth outside endogamy are structural targets (trapped/constrained exit, bear demographic gatekeeping costs — d ~0.75-0.85). Secular courts are analytical observers (d ~0.5). Priesthood is identity-locked beneficiary (d ~0.15). The derivation chain captures this through beneficiary/victim declarations + exit modulation + power atoms.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (colonial legal incursion) is substantially resolved — the community has statutory recognition, gender-equitable tribunals, and trust protection. But the constraint persists because its enforcement mechanism (endogamy, Punchayet authority) has become the community's primary survival strategy in a demographic crisis. The mandatrophy is not resolved: the constraint has acquired a new function (demographic gatekeeping) that contradicts its founding purpose. The classification as tangled rope prevents mislabeling this as pure coordination (ignoring the extraction) or pure extraction (ignoring the genuine gender-equitable coordination). The demographic trap makes reform structurally difficult — any relaxation of endogamy accelerates collapse; any tightening accelerates collapse faster.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_custom,
    'Is the Parsi community''s endogamy rule a genuine religious/customary requirement (mountain-like natural law within the community''s ontology) or a constructed demographic gatekeeping mechanism that serves institutional interests?',
    'Historical analysis of pre-1936 Parsi custom: was patrilineal descent universally enforced or was it codified/strengthened by the Act? Comparative analysis with Zoroastrian communities in Iran (where conversion and intermarriage rules differ).',
    'If the endogamy rule is a constructed institutional choice (not an irreducible religious requirement), the constraint''s claimed naturalness is a false summit — the Mountain immunity claim for ''community custom'' collapses, revealing a tangled_rope where the coordination function (gender equity) is real but the extraction function (demographic gatekeeping) is mutable policy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_custom, empirical, 'Whether endogamy enforcement is structurally necessary to the community''s religious identity or a contingent institutional choice.').

omega_variable(
    gender_equity_vs_priesthood_exclusion,
    'Does the constraint''s celebrated gender equity (divorce, maintenance, property) structurally require the priesthood''s male-only restriction, or is the priesthood exclusion an independent extraction that the coordination function does not depend on?',
    'Theological analysis: is male-only priesthood doctrinally necessary in Zoroastrianism? Sociological analysis: do Parsi communities with women priests (North America) maintain the same marriage/divorce coordination function?',
    'If priesthood exclusion is independent, the constraint''s gender-equity coordination is separable from its gendered authority structure — the extraction is not the price of coordination. If inseparable, the constraint''s coordination function is structurally gendered, complicating the tangled_rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(gender_equity_vs_priesthood_exclusion, conceptual, 'Whether the constraint''s coordination and gendered authority are structurally coupled.').

omega_variable(
    demographic_trap_reversibility,
    'Can the Parsi community survive as a distinct legal-religious entity without endogamy enforcement, or is the constraint''s demographic trap irreversible — meaning the tangled_rope will necessarily degrade into a piton (theatrical maintenance of a collapsing structure)?',
    'Demographic modeling of Parsi population under alternative endogamy rules. Comparative study of other small ethnoreligious communities (e.g., Bene Israel, Syrian Christians) that relaxed endogamy.',
    'If survival without endogamy is possible, the constraint is a mutable tangled_rope with reform pathways. If survival requires endogamy, the constraint is a doomed piton — its coordination function (community survival) is structurally bound to an extraction mechanism (endogamy) that now produces the opposite outcome.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(demographic_trap_reversibility, empirical, 'Whether the constraint''s demographic trajectory is reversible or locked into piton degradation.').

omega_variable(
    committer_structure_omega,
    'How does this reading''s structural relationship to the marriage_authority_kernel differ from its sibling readings, and where is the disagreement located?',
    'Comparative structural analysis of all five readings'' beneficiary/victim architectures, enforcement mechanisms, and exit geographies.',
    'Maps the kernel''s contestation surface: identifies which structural elements are shared across readings (coordination of marriage regulation) and which are reading-specific (endogamy enforcement, gender equity profile, demographic trajectory).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_omega, conceptual, 'This constraint instantiates the parsi_communal_reading of the marriage_authority_kernel. Sibling readings: hindu_codified_reading, muslim_shariat_reading, christian_canonical_reading, secular_civil_reading. The structural disagreement is located in: (1) enforcement locus — community tribunals vs. civil courts vs. religious authorities vs. secular registry; (2) exit architecture — identity-locked endogamy vs. codified opt-out vs. canonical process vs. civil marriage; (3) gender equity profile — unusually high within community but priesthood exclusion; (4) demographic viability — only this reading faces existential demographic collapse.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__parsi_communal_reading, 1936, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(parsi_marriage_tr_t1936, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 1936, 0.08).
narrative_ontology:measurement(parsi_marriage_tr_t1948, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 1948, 0.1).
narrative_ontology:measurement(parsi_marriage_tr_t1961, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 1961, 0.12).
narrative_ontology:measurement(parsi_marriage_tr_t1974, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 1974, 0.15).
narrative_ontology:measurement(parsi_marriage_tr_t1988, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 1988, 0.18).
narrative_ontology:measurement(parsi_marriage_tr_t2000, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(parsi_marriage_tr_t2012, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 2012, 0.22).
narrative_ontology:measurement(parsi_marriage_tr_t2024, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 2024, 0.25).

% Extraction over time
narrative_ontology:measurement(parsi_marriage_be_t1936, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 1936, 0.22).
narrative_ontology:measurement(parsi_marriage_be_t1948, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 1948, 0.25).
narrative_ontology:measurement(parsi_marriage_be_t1961, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 1961, 0.28).
narrative_ontology:measurement(parsi_marriage_be_t1974, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 1974, 0.31).
narrative_ontology:measurement(parsi_marriage_be_t1988, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 1988, 0.35).
narrative_ontology:measurement(parsi_marriage_be_t2000, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 2000, 0.37).
narrative_ontology:measurement(parsi_marriage_be_t2012, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 2012, 0.37).
narrative_ontology:measurement(parsi_marriage_be_t2024, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 2024, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(parsi_marriage_su_t1936, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 1936, 0.35).
narrative_ontology:measurement(parsi_marriage_su_t1948, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 1948, 0.36).
narrative_ontology:measurement(parsi_marriage_su_t1961, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 1961, 0.38).
narrative_ontology:measurement(parsi_marriage_su_t1974, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 1974, 0.4).
narrative_ontology:measurement(parsi_marriage_su_t1988, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 1988, 0.41).
narrative_ontology:measurement(parsi_marriage_su_t2000, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 2000, 0.42).
narrative_ontology:measurement(parsi_marriage_su_t2012, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 2012, 0.42).
narrative_ontology:measurement(parsi_marriage_su_t2024, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 2024, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__parsi_communal_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_authority_kernel__parsi_communal_reading, 0.1).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel__hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel__muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel__christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel__secular_civil_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, parsi_trust_governance).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, parsi_fire_temple_access).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, special_marriage_act_1954).

% DUAL FORMULATION NOTE:
% The marriage_authority_kernel decomposes into five constraint stories, one per reading. This story (parsi_communal_reading) is structurally distinct because it combines community tribunal administration with strict endogamy enforcement and high internal gender equity, all under conditions of demographic collapse. The hindu_codified_reading uses civil courts; muslim_shariat_reading uses qazi boards; christian_canonical_reading uses ecclesiastical courts; secular_civil_reading uses civil registry. Only this reading has the demographic trap dynamic. All five are linked via affects_constraints because they compete for legitimacy in the same constitutional pluralism framework and the Supreme Court's personal law jurisprudence treats them as a family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_authority_kernel__parsi_communal_reading, institutional, 0.12).
constraint_indexing:directionality_override(marriage_authority_kernel__parsi_communal_reading, moderate, 0.45).
constraint_indexing:directionality_override(marriage_authority_kernel__parsi_communal_reading, powerless, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
