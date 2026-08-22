% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__christian_canonical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__christian_canonical_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: marriage_authority_kernel__christian_canonical_reading
 *   human_readable: Christian Canonical Marriage Authority (Indian Christian Marriage Act 1872)
 *   domain: religious/constitutional/family_law
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of India's contested marriage
 *   authority kernel. The 1872 Christian Marriage Act codifies Christian
 *   canonical law within the Indian legal system, establishing church
 *   tribunals' jurisdiction over annulment, restricting divorce to
 *   fault-based grounds, and grounding marriage validity in sacramental form.
 *   This is the CANONICAL reading: marriage authority derives from Christian
 *   doctrine as codified in statute. Sibling readings (Hindu codified law,
 *   Shariat, Parsi custom, secular civil code) constitute alternative
 *   authority structures on the same contested terrain — the kernel. Each
 *   reading instantiates a different ε value because each defines marriage
 *   authority, dissolution eligibility, and beneficiary/victim structure
 *   differently. This story addresses ONLY the canonical reading's operation:
 *   its extractiveness, suppression mechanisms, and the structural
 *   asymmetries between church hierarchy (beneficiary), Christian women
 *   restricted from divorce (payers), and secular courts observing from the
 *   constitutional sideline.
 *
 * KEY AGENTS:
 *   - Catholic Church hierarchy: institutional beneficiary, agenda-setter; maintains annulment authority and sacramental doctrine; collects deference from Christian spouses who accept ecclesiastical jurisdiction.
 *   - Christian women restricted from divorce: powerless payers, identity-locked in Christian marriage regime; fault-based grounds restrict their exit compared to Hindu women (1955 Act); community shame and religious identity fusion amplify suppression.
 *   - Church annulment gatekeepers: institutional beneficiary and secondary agenda-setter; control sacramental eligibility; accrue authority and prestige; depend on constraint's persistence.
 *   - Secular courts: observer seat; apply 1872 Act as statute; increasingly construe fault grounds broadly against constitutional equal protection but lack power to override canonical interpretation.
 *   - Secular reform advocates: excluded voice; would argue for uniform civil code or no-fault divorce but have no standing in church proceedings.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__christian_canonical_reading, 0.62).
domain_priors:suppression_score(marriage_authority_kernel__christian_canonical_reading, 0.58).
domain_priors:theater_ratio(marriage_authority_kernel__christian_canonical_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__christian_canonical_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__christian_canonical_reading, "Christian Canonical Marriage Authority (Indian Christian Marriage Act 1872)").
narrative_ontology:topic_domain(marriage_authority_kernel__christian_canonical_reading, "religious/constitutional/family_law").

domain_priors:requires_active_enforcement(marriage_authority_kernel__christian_canonical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__christian_canonical_reading, 'd89b6490-851c-42e9-9e68-26f58584094d').
narrative_ontology:cs_kernel_codification('d89b6490-851c-42e9-9e68-26f58584094d', formalized).
narrative_ontology:cs_authority_grounding('d89b6490-851c-42e9-9e68-26f58584094d', lineage).
narrative_ontology:cs_interpretation_layer_present('d89b6490-851c-42e9-9e68-26f58584094d').
narrative_ontology:cs_reading_relation('d89b6490-851c-42e9-9e68-26f58584094d', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('d89b6490-851c-42e9-9e68-26f58584094d', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('d89b6490-851c-42e9-9e68-26f58584094d', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_reading_relation('d89b6490-851c-42e9-9e68-26f58584094d', marriage_authority_kernel__secular_civil_reading, influences).
narrative_ontology:cs_axiom('d89b6490-851c-42e9-9e68-26f58584094d', foundational, sacramental_marriage_indissolubility_doctrine).
narrative_ontology:cs_axiom_status(sacramental_marriage_indissolubility_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('d89b6490-851c-42e9-9e68-26f58584094d', sacramental_marriage_indissolubility_doctrine, theological).
narrative_ontology:cs_axiom('d89b6490-851c-42e9-9e68-26f58584094d', foundational, church_jurisdiction_over_annulment_authority).
narrative_ontology:cs_axiom_status(church_jurisdiction_over_annulment_authority, holdable).
narrative_ontology:cs_axiom_grounding('d89b6490-851c-42e9-9e68-26f58584094d', church_jurisdiction_over_annulment_authority, conventional).
narrative_ontology:cs_reference_frame('d89b6490-851c-42e9-9e68-26f58584094d', sacramental_marriage_indissolubility_framework).
narrative_ontology:cs_drift_state('d89b6490-851c-42e9-9e68-26f58584094d', contemporary_constitutional_secularism_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d89b6490-851c-42e9-9e68-26f58584094d', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__christian_canonical_reading, catholic_church_hierarchy).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__christian_canonical_reading, christian_divorce_gatekeepers).
narrative_ontology:constraint_victim(marriage_authority_kernel__christian_canonical_reading, christian_women_restricted_divorce).
narrative_ontology:constraint_victim(marriage_authority_kernel__christian_canonical_reading, christian_men_non_canonical_unions).
narrative_ontology:constraint_victim(marriage_authority_kernel__christian_canonical_reading, christian_persons_trapped_marriages).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__christian_canonical_reading, church_annulment_gatekeepers).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__christian_canonical_reading, christian_spouses_general).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__christian_canonical_reading, sacramental_marriage_indissolubility).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__christian_canonical_reading, church_jurisdiction_over_marriage).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces canonical law standards within the Indian legal system, maintaining sole jurisdiction over sacramental annulments through church tribunals. Collects deference from Christian spouses who accept church authority over civil law for marriage validity and dissolution. Manages the theological framework that grounds the constraint and the institutional machinery that preserves it.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, catholic_church_hierarchy, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Under the 1872 Act's fault-based divorce grounds (adultery, cruelty, desertion), Christian women can petition for divorce only on narrow, historically male-biased standards. Hindu women under the 1955 Act and Muslim women under personal law can access no-fault or more flexible grounds. Religious identity fusion with Christianity and community shame for divorce make exit culturally constituted as apostasy or family dishonor. Face years or decades in terminated or abusive marriages awaiting church annulment tribunal proceedings with no guaranteed relief.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, christian_women_restricted_divorce, payer,
    powerless, biographical, identity_locked, national).

% Marriages not solemnized according to canonical form (church blessing, proper rites) are treated as void ab initio under church doctrine and vulnerable to non-recognition in mixed-family or remarriage disputes. Men forming second unions without canonical dissolution of the first face legal limbo: civil remarriage is unrecognized if the first was canonically invalid, but church may later annul on technical grounds. Power advantage over women is constrained by institutional church authority over sacramental status.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, christian_men_non_canonical_unions, payer,
    moderate, biographical, constrained, national).

% Spouses in abusive, violent, or irreconcilable marriages must prove specific faults (cruelty with injury-level threshold, adultery with witness-level proof, desertion for statutory period) to petition for divorce — a standard substantially more restrictive than Hindu Marriage Act Section 13 (cruelty defined broadly) or Special Marriage Act (no-fault mutual consent divorce). Church annulment requires proof of canonical defect (non-consent, impotence, prior bond) unrelated to actual marital failure, making it a parallel and often-impossible gate. Identity fusion with Christianity and family/community honor create suppression that persists even if legal grounds were relaxed.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, christian_persons_trapped_marriages, payer,
    powerless, biographical, identity_locked, national).

% Church tribunal judges, bishops, and canonical law experts control the annulment adjudication process and sacramental eligibility machinery. Accrue social authority, institutional prestige, and career advancement within ecclesiastical hierarchy. Accrue power to determine who may remarry in the church and whose children are legitimate. Depend directly on the constraint's persistence and the complexity of canonical procedures for their jurisdictional role.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, church_annulment_gatekeepers, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__christian_canonical_reading, church_annulment_gatekeepers, agenda_setter).

% Advocates for uniform civil code, constitutional equal protection, or no-fault divorce reform are structurally absent from church-controlled annulment proceedings and have limited standing in canonical deliberations. They would argue that fault-based Christian divorce grounds are gender-unequal and that secular law should extend no-fault divorce to all citizens regardless of religion. Their voice enters only through secular court jurisdiction expansion, not through the canonical mechanism itself.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, secular_reform_advocates, excluded,
    organized, generational, constrained, national).

% Apply the 1872 Act as binding statutory law while recognizing church annulments as binding on sacramental status and remarriage eligibility. Increasingly interpret fault grounds ('cruelty' especially) broadly against constitutional equal protection and gender equality; have authorized divorces that early judicial interpretation would have refused. Do not control the constraint's operation or the canonical definitions of annulment, but mediate enforcement against rising constitutional rights claims.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, indian_secular_courts, observer,
    institutional, generational, analytical, national).

% Receive access to a recognized marriage regime with church blessing, community legitimacy, and established property/inheritance law through the constraint's codified structure. Simultaneously pay costs where divorce access is restricted compared to civil or Hindu alternatives and where identity fusion with Christianity makes exit culturally prohibitive. Net beneficiaries in formation, net payers in dissolution.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, christian_spouses_general, beneficiary,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority_kernel__christian_canonical_reading, catholic_church_hierarchy).
narrative_ontology:fixing_cost_class(marriage_authority_kernel__christian_canonical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified Christian marriage regime recognizing sacramental form, church blessing, and canonical validity within India's plural religious legal order. Coordinates Christian community membership, family property rights, inheritance, spousal obligations, and legitimacy of children under a stable authority structure recognized by both church and civil law.
% TRANSFER_FUNCTION: Transfers jurisdictional authority over marriage annulment from civil courts to church tribunals, and restricts divorce exit from marriage through fault-based grounds (adultery, cruelty, desertion) rather than no-fault options. Concentrates authority over dissolution with church hierarchy and canonical experts; transfers to church (through deference and remarriage control) what would otherwise be secular state power.
% ABSENT_VOICES: Hindu, Muslim, Parsi, and secular-civil advocates for alternative marriage law readings are structurally absent from canonical church proceedings and have no voice in annulment tribunal operations. Christian women seeking no-fault divorce comparable to Hindu Marriage Act (1955) alternatives are unheard within the canonical reading's framework. Secular reformers proposing uniform civil code cannot reshape the 1872 Act's structure from within the canonical system.
% DISAPPEARANCE_RATIONALE: If Christian canonical reading authority collapsed and the 1872 Act were superseded by secular law or uniform civil code, Christian divorce law would immediately align with Hindu/general civil alternatives: fault-based grounds would lift to no-fault divorce, church tribunals would lose marriage jurisdiction, remarriage eligibility would depend on secular courts rather than ecclesiastical annulment, property divisions would follow civil law rather than canonical doctrine, thousands of trapped Christian marriages would dissolve, and the church's institutional authority over Christian family life would narrow to internal community practice with no civil enforcement. The constraint's disappearance would rearrange the legal landscape for Christian women and men seeking divorce.
% FOUNDING_PROBLEM: In late colonial and early independent India, the Christian minority required codification of marriage law to establish stable property, inheritance, and spousal rights within a plural religious legal order where Hindu, Muslim, and Christian communities retained personal law authority. Codifying Christian law (derived from canonical doctrine and church teaching) rather than imposing secular family law established a coherent authority structure rooted in Christian theology and ecclesiastical jurisdiction, enabling the church to adjudicate matters of faith and sacramental validity.
% FOUNDING_PROBLEM_CORROBORATION: The Catholic Church hierarchy attests the founding problem remains live: Christian marriage requires sacramental grounding and church authority for spiritual validity; minorities need protected legal space for community-determined family law. Constitutional scholars, secular reform advocates, and Christian women's groups attest the founding problem is obsolete: constitutional secularism, equal protection guarantees, and the emergence of a secular civil code option (Special Marriage Act 1954) have superseded the codification rationale. Legislative testimony (Uniform Civil Code debates), Indian case law (Anita v. Arun Reddy, Shaffi v. Chief Secretary, Supreme Court equal protection rulings), and independent gender-equity research from feminist scholars outside the church establishment corroborate the post-founding reading that the constraint now persists as church institutional interest rather than Christian community necessity.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__christian_canonical_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__christian_canonical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__christian_canonical_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_authority_kernel__christian_canonical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__christian_canonical_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__christian_canonical_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority_kernel__christian_canonical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority_kernel__christian_canonical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.62 at interval end) reflects the canonical reading's core structure: church hierarchy collects jurisdictional authority over annulment and remarriage eligibility (concentrated benefit), while Christian women bear restricted divorce access compared to statutory alternatives available to Hindu/Muslim/secular-coded women (asymmetric cost). Suppression (0.58) is substantial but not maximal because the constraint operates through statutory law rather than pure coercion — Christian women can technically petition divorce courts on fault grounds (no barrier at the gate), but identity fusion with Christianity and community shame create internalized suppression independent of legal structure. Theater ratio (0.31) is moderate: the constraint maintains a real coordination function (unified Christian marriage regime, sacramental grounding, community legitimacy), but increasing share of enforcement activity focuses on defending canonical annulment authority against secular court jurisdiction expansion and constitutional equal protection claims — the coordination narrative is real, but the extraction machinery grows relative to the coordination machinery. Measurement series show slow extraction accumulation (0.48 → 0.62 over 40 years) as secular courts interpret fault grounds more liberally and women's awareness of Hindu Marriage Act alternatives spreads, forcing church hierarchy to invest more in sacramental defense and less in pure coordination function. Theater grows modestly (0.18 → 0.31) as Indian secularism advances and the church's claims to authority require more theological justification and less institutional inevitability. Suppression plateaus (0.52 → 0.58, then stable) because the internalized identity-fusion component dominates — relaxing fault grounds does not immediately reduce suppression experienced by Christian women constrained by religious identity.
 *
 * PERSPECTIVAL GAP:
 *   This reading claims the same constraint is experienced differently from four structural seats: (1) Catholic church hierarchy experiences genuine coordination function (unifying Christian community, providing sacramental legitimacy, establishing property/inheritance order) with collateral jurisdictional benefit. (2) Christian women restricted from divorce experience systematic extraction (constrained exit, fault-based grounds, annulment gatekeeping) with incidental coordination benefit (community membership, family legitimacy). (3) Secular courts observe a statutory regime they administer but whose core authority (canonical doctrine) remains outside their jurisdiction — they experience the constraint as a partially decodable legal structure. (4) Secular reformers experience complete exclusion: their alternative readings of marriage authority have no voice in the canonical regime's operation. The engine computes directionality per seat from beneficiary/victim declarations and exit options: church hierarchy derives low d (full beneficiary, arbitrage exit), Christian women derive high d (victim, identity-locked), secular courts derive d near 0.5 (neither collecting nor paying, analytical position). Each seat's computed type should reflect this asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are church hierarchy and canonical gatekeepers: they set the agenda, maintain the authority structure, and collect jurisdictional deference and institutional prestige without bearing the costs of restricted divorce access. Victims are Christian women experiencing fault-based divorce restriction and identity-locked suppression, and Christian men forming non-canonical unions who face legal vulnerability. The asymmetry is rooted in power (church hierarchy is institutional, women are powerless), time horizon (ecclesiastical authority is civilizational, women's marriage is biographical), and exit options (church can exercise arbitrage within global canonical networks, women are identity-locked within Christian identity). Directionality for church hierarchy should compute near 0.0 (beneficiary); for Christian women near 1.0 (target); for secular courts near 0.5 (observer). No overrides are necessary — the structural data suffices.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (codify Christian marriage law in plural religious order) was live in 1872 India, when Christian minority required a coherent authority structure within colonial legal pluralism. The problem is now contested: church hierarchy argues it remains live (Christians still need sacramental grounding), secular reformers argue it is dead (constitutional secularism superseded the codification rationale). The disappearance verdict is world_rearranges: if canonical reading authority collapsed, Christian divorce law would align with civil alternatives (no-fault divorce, civil remarriage validation, courts instead of tribunals). This mismatch (founding problem status = contested, disappearance verdict = world_rearranges) is the mandatrophy signal: the constraint persists because church hierarchy benefits from maintaining it, not because the founding problem still organizes Christian community life. The theater ratio rise (0.18 → 0.31) supports this: the church's claims now require more theological justification and less institutional inevitability — the theater of sacramental authority grows as its structural necessity shrinks. The constraint should compute as Tangled Rope (genuine coordination function + asymmetric extraction + active enforcement via annulment gatekeeping), but with high risk of reclassification to Snare if empirical investigation reveals the coordination function is obsolete.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_identity_contested_across_readings,
    'Is there a single ''marriage authority kernel'' in India that all five readings interpret, or are these five distinct kernels that happen to address marriage law?',
    'Examine whether the five readings share a common text (Constitution, statute, practice lineage) they claim to interpret. If they do, one kernel with multiple readings; if they read entirely different source materials, five separate kernels that compete.',
    'If one kernel, the readings'' competition is internal to a single authority structure; if five kernels, each reading''s authority is independent and the ''contest'' is really five parallel regimes. This reading (Christian canonical) assumes one kernel; if wrong, it should decompose into a separate story for Christian Marriage Act authority independent of the general marriage law kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_identity_contested_across_readings, conceptual, 'Whether marriage authority in India is one contested kernel or five independent regime kernels').

omega_variable(
    fault_based_divorce_suppression_mechanism,
    'Is the restriction on Christian women''s divorce access primarily structural (narrow legal grounds requiring proof of fault) or primarily internalized (religious identity fusion and community shame making exit unthinkable even if legal grounds existed)?',
    'Observational study: Christian women''s divorce petitions in jurisdictions that maintained versus relaxed fault-based grounds (comparative Hindu/civil court outcomes); post-divorce trajectory of Christian women who exit the church versus remain — if suppression persists absent the legal barrier, internalization is substantial.',
    'If structural, remedies are legal (relaxing grounds, no-fault divorce). If internalized, legal change alone is insufficient; suppression persists after the formal barrier lifts. Affects classification: higher internalized suppression means higher effective suppression even if formal grounds relax.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fault_based_divorce_suppression_mechanism, empirical, 'Whether divorce suppression for Christian women is structural (legal grounds) or internalized (identity fusion)').

omega_variable(
    canonical_law_vs_secular_code_boundary,
    'Is canonical law authority separable from the civil statutory framework (1872 Act) that codifies it, or is the constraint structurally dependent on the 1872 Act''s continued endorsement by the secular legal system?',
    'Observe what happens to Christian marriage authority if India adopts a uniform civil code superseding the 1872 Act: does church authority persist as internal community regulation, or does it collapse because the statutory codification is the actual source of binding force?',
    'If separable: church authority stands alone and derives from canon law tradition, not state endorsement; constraint is primarily ecclesiastical. If dependent: the constraint is actually a statutory establishment of religious law by the secular state; remedial paths involve secular law change, not church reform. Affects mandatrophy analysis: is the founding problem (codifying Christian law) still addressed by statute, or has constitutional secularism made it obsolete?',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(canonical_law_vs_secular_code_boundary, conceptual, 'Whether canonical marriage authority depends on statutory codification or stands independently').

omega_variable(
    reading_specificity_christian_canonical_identity,
    'Does ''Christian canonical reading'' refer narrowly to Catholic/Eastern Orthodox sacramental doctrine, or does it extend to Protestant Christian perspectives on marriage authority?',
    'Examine which Christian denominations are bound by the 1872 Act and which recognize church authority over divorce/remarriage. If only Catholics/Orthodox claim sacramental annulment authority, the reading is narrower than ''Christian''; if Protestants also rely on the Act (without sacramental theology), the reading is broader.',
    'If narrower: the beneficiary class (canonical authority gatekeepers) is smaller and more centralized (Catholic bishops/tribunals); suppression of Protestant women may operate differently. If broader: the constraint coordinates across more diverse Christian theology but loses the unified sacramental framing — two different extraction mechanisms operating under one statute.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_specificity_christian_canonical_identity, conceptual, 'Whether Christian canonical reading refers to Catholic/Orthodox sacramental doctrine or encompasses Protestant Christian communities').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__christian_canonical_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(marr_tr_t0, observed).
narrative_ontology:measurement(marr_tr_t5, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement_basis(marr_tr_t5, observed).
narrative_ontology:measurement(marr_tr_t10, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 10, 0.23).
narrative_ontology:measurement_basis(marr_tr_t10, observed).
narrative_ontology:measurement(marr_tr_t15, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 15, 0.26).
narrative_ontology:measurement_basis(marr_tr_t15, observed).
narrative_ontology:measurement(marr_tr_t20, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement_basis(marr_tr_t20, observed).
narrative_ontology:measurement(marr_tr_t25, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 25, 0.29).
narrative_ontology:measurement_basis(marr_tr_t25, observed).
narrative_ontology:measurement(marr_tr_t30, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 30, 0.31).
narrative_ontology:measurement_basis(marr_tr_t30, observed).
narrative_ontology:measurement(marr_tr_t40, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 40, 0.31).
narrative_ontology:measurement_basis(marr_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(marr_be_t0, observed).
narrative_ontology:measurement(marr_be_t5, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(marr_be_t5, observed).
narrative_ontology:measurement(marr_be_t10, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement_basis(marr_be_t10, observed).
narrative_ontology:measurement(marr_be_t15, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement_basis(marr_be_t15, observed).
narrative_ontology:measurement(marr_be_t20, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement_basis(marr_be_t20, observed).
narrative_ontology:measurement(marr_be_t25, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 25, 0.61).
narrative_ontology:measurement_basis(marr_be_t25, observed).
narrative_ontology:measurement(marr_be_t30, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement_basis(marr_be_t30, observed).
narrative_ontology:measurement(marr_be_t40, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(marr_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(marr_su_t0, observed).
narrative_ontology:measurement(marr_su_t5, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 5, 0.54).
narrative_ontology:measurement_basis(marr_su_t5, observed).
narrative_ontology:measurement(marr_su_t10, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 10, 0.56).
narrative_ontology:measurement_basis(marr_su_t10, observed).
narrative_ontology:measurement(marr_su_t15, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 15, 0.57).
narrative_ontology:measurement_basis(marr_su_t15, observed).
narrative_ontology:measurement(marr_su_t20, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement_basis(marr_su_t20, observed).
narrative_ontology:measurement(marr_su_t25, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 25, 0.58).
narrative_ontology:measurement_basis(marr_su_t25, observed).
narrative_ontology:measurement(marr_su_t30, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement_basis(marr_su_t30, observed).
narrative_ontology:measurement(marr_su_t40, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 40, 0.58).
narrative_ontology:measurement_basis(marr_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__christian_canonical_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_authority_kernel__christian_canonical_reading, 0.12).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__parsi_communal_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__secular_civil_reading).

% DUAL FORMULATION NOTE:
% The Christian canonical reading is one of five structurally distinct authority readings of the same marriage law kernel in India. All five readings share a contested referent (Indian marriage law) but instantiate different constraints because they define authority grounding, beneficiary structures, and dissolution eligibility differently. This story's ε (0.62) reflects the canonical reading's operation; the Hindu codified reading operates under different ε (reflecting the 1955 Act's no-fault divorce and secular judicial authority); the secular civil reading operates under the lowest ε (constitutional equal protection, no-fault divorce, court authority). Each reading is a separate constraint story linked to its siblings via network.affects_constraints to enable cross-reading comparative analysis and drift detection.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_authority_kernel__christian_canonical_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
