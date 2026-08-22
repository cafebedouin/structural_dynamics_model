% ============================================================================
% CONSTRAINT STORY: family_law_authority__christian_canonical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__christian_canonical_reading, []).

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
 *   constraint_id: family_law_authority__christian_canonical_reading
 *   human_readable: Christian Canonical Reading of Marriage Authority (Sacramental Permanence and Ecclesiastical Validity)
 *   domain: religious_governance/comparative_law/political_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the christian_canonical_reading of the
 *   family_law_authority kernel — one of five structurally distinct readings
 *   of marriage authority. The reading holds that marriage is a sacrament
 *   (Catholic) or covenant under denominational governance (Protestant) with
 *   authority over validity residing in ecclesiastical/denominational
 *   structures. Key structural features: Catholic sacramental indissolubility
 *   (no divorce, annulment-only dissolution), Protestant denominational
 *   variance (divorce permitted, remarriage policies differ), and
 *   ecclesiastical/denominational courts as validity arbiters. The constraint
 *   coordinates marriage formation and dissolution within Christian
 *   communities while extracting compliance through sacramental theology,
 *   canonical penalties, and state recognition of ecclesiastical decrees. The
 *   interval spans from the Council of Trent's Tametsi decree (1563)
 *   establishing canonical form to the present (2025), capturing the
 *   transition from confessional state enforcement to pluralistic secular
 *   states where ecclesiastical authority persists without civil coercion.
 *
 * KEY AGENTS:
 *   - catholic_hierarchy: Primary agenda_setter (institutional) — sets canonical law, operates tribunals, defines sacramental validity
 *   - protestant_denominational_authorities: Secondary agenda_setter (organized) — set denominational marriage canons, authorize clergy, recognize civil divorce variably
 *   - canonical_law_practitioners: Beneficiary (organized) — professional class dependent on ecclesiastical court system
 *   - divorce_seeking_catholics: Primary victim (powerless→moderate) — trapped by indissolubility doctrine, face annulment process or exclusion from sacraments
 *   - annulment_petitioners: Victim (moderate) — bear procedural, financial, and emotional costs of canonical process
 *   - interdenominational_couples: Victim (moderate) — navigate conflicting validity rules, dispensation requirements
 *   - lgbtq_christians: Victim/excluded (powerless) — structurally excluded from sacramental marriage in Catholic and most Protestant traditions
 *   - secular_state_courts: Observer (institutional) — recognize civil marriage, may recognize ecclesiastical decrees comity, but do not enforce canonical indissolubility
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__christian_canonical_reading, 0.42).
domain_priors:suppression_score(family_law_authority__christian_canonical_reading, 0.58).
domain_priors:theater_ratio(family_law_authority__christian_canonical_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__christian_canonical_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__christian_canonical_reading, "Christian Canonical Reading of Marriage Authority (Sacramental Permanence and Ecclesiastical Validity)").
narrative_ontology:topic_domain(family_law_authority__christian_canonical_reading, "religious_governance/comparative_law/political_theory").

domain_priors:requires_active_enforcement(family_law_authority__christian_canonical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__christian_canonical_reading, '0b3b60f2-599a-4dac-83e3-94d798118571').
narrative_ontology:cs_kernel_codification('0b3b60f2-599a-4dac-83e3-94d798118571', formalized).
narrative_ontology:cs_authority_grounding('0b3b60f2-599a-4dac-83e3-94d798118571', lineage).
narrative_ontology:cs_interpretation_layer_present('0b3b60f2-599a-4dac-83e3-94d798118571').
narrative_ontology:cs_reading_relation('0b3b60f2-599a-4dac-83e3-94d798118571', family_law_authority__secular_contractual_reading, coexists_with).
narrative_ontology:cs_reading_relation('0b3b60f2-599a-4dac-83e3-94d798118571', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('0b3b60f2-599a-4dac-83e3-94d798118571', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('0b3b60f2-599a-4dac-83e3-94d798118571', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_axiom('0b3b60f2-599a-4dac-83e3-94d798118571', foundational, marriage_sacramental_indissolubility).
narrative_ontology:cs_axiom_status(marriage_sacramental_indissolubility, holdable).
narrative_ontology:cs_axiom_grounding('0b3b60f2-599a-4dac-83e3-94d798118571', marriage_sacramental_indissolubility, deontological).
narrative_ontology:cs_axiom('0b3b60f2-599a-4dac-83e3-94d798118571', foundational, ecclesiastical_authority_over_validity).
narrative_ontology:cs_axiom_status(ecclesiastical_authority_over_validity, holdable).
narrative_ontology:cs_axiom_grounding('0b3b60f2-599a-4dac-83e3-94d798118571', ecclesiastical_authority_over_validity, conventional).
narrative_ontology:cs_reference_frame('0b3b60f2-599a-4dac-83e3-94d798118571', tridentine_canonical_order).
narrative_ontology:cs_drift_state('0b3b60f2-599a-4dac-83e3-94d798118571', post_vatican_ii_pluralism, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0b3b60f2-599a-4dac-83e3-94d798118571', '').
narrative_ontology:cs_kernel_id(family_law_authority__christian_canonical_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__christian_canonical_reading, catholic_hierarchy).
narrative_ontology:constraint_beneficiary(family_law_authority__christian_canonical_reading, protestant_denominational_authorities).
narrative_ontology:constraint_beneficiary(family_law_authority__christian_canonical_reading, canonical_law_practitioners).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, divorce_seeking_catholics).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, annulment_petitioners).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, interdenominational_couples).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, lgbtq_christians).
narrative_ontology:constraint_vindicates(family_law_authority__christian_canonical_reading, marriage_indissolubility_doctrine).
narrative_ontology:constraint_vindicates(family_law_authority__christian_canonical_reading, sacramental_validity_ecclesiastical_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines canonical marriage law (1983 Code of Canon Law), operates the global tribunal system for annulments, controls sacramental discipline (communion for divorced/remarried), and claims authority over all baptized Catholics' marriages regardless of civil law. Collects institutional legitimacy, sacramental control, and annulment fees. Exit options: can reform canons (as Francis did with Mitis Iudex), but doctrinal core (indissolubility) is treated as unrevisable.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, catholic_hierarchy, agenda_setter,
    institutional, generational, arbitrage, global).

% Set marriage canons and clergy authorization within each denomination (Anglican canons, Presbyterian Book of Order, UMC Discipline, etc.). Variance is high: some permit divorce/remarriage freely (ELCA, Episcopal), some restrict (SBC, PCA, LCMS), some maintain quasi-canonical processes (Anglican ecclesiastical courts). Collect denominational cohesion and clergy authority. Exit: individuals can switch denominations (forum shopping), but denominational authorities are mobile across the religious market.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, protestant_denominational_authorities, agenda_setter,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(family_law_authority__christian_canonical_reading, protestant_denominational_authorities, beneficiary).

% Advocates, judges, and tribunal staff in the Catholic canonical system. Professional livelihood depends on the annulment process (fees, appointments, academic positions). Also includes Protestant ecclesiastical lawyers where denominational courts exist. Exit: can move to civil family law practice, but specialization creates path dependence.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, canonical_law_practitioners, beneficiary,
    organized, biographical, mobile, global).

% Catholics whose marriages have broken down civilly but who cannot remarry in the Church without an annulment. Face exclusion from Eucharist if remarried civilly. The annulment process takes 12-18 months, costs $500-$2000 (varies by diocese), requires proving defect of consent at marriage. Exit is identity_locked: leaving the Church means leaving the sacramental identity that constitutes their self-understanding; staying means accepting the constraint. Some pursue 'Loreto' route (civil divorce only, no remarriage, limited participation).
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, divorce_seeking_catholics, payer,
    moderate, biographical, identity_locked, global).

% Catholics pursuing formal annulment. Bear procedural costs (time, money, emotional labor of revisiting marriage history), uncertainty of outcome (though ~90% affirmative in US), and the theological framing that their marriage was 'never valid.' Constrained exit: the process is the only path to sacramental remarriage, but it is accessible and usually successful. Some abandon the process due to cost or trauma.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, annulment_petitioners, payer,
    moderate, biographical, constrained, global).

% Couples where one partner is Catholic and the other Protestant (or Orthodox). Require dispensation from canonical form for Catholic validity; face conflicting rules on divorce/remarriage; children's baptism and religious education become negotiation points. Constrained exit: can marry civilly only (losing Catholic recognition), or navigate dispensation process. The Protestant partner's denomination may have different requirements.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, interdenominational_couples, payer,
    moderate, biographical, constrained, national).

% LGBTQ persons in Catholic and most Protestant traditions are structurally excluded from sacramental/ecclesiastical marriage. Catholic doctrine: same-sex unions cannot be marriages (CCC 2357-2359). Protestant variance: some denominations affirm (ELCA, Episcopal, PCUSA, UCC), most do not (SBC, PCA, LCMS, Catholic). Trapped: leaving the tradition loses community and identity; staying means accepting exclusion or living in tension. Some seek 'blessing' rites short of marriage in affirming denominations.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, lgbtq_christians, excluded,
    powerless, biographical, trapped, global).

% Administer civil marriage and divorce for all citizens regardless of religion. Recognize ecclesiastical annulments only as evidence for civil purposes (comity), not as binding decrees. In some jurisdictions (Israel, India), religious courts have statutory jurisdiction over marriage for their communities — but this constraint story covers the Christian reading in pluralistic Western states where civil law is supreme. Analytical seat: observes the constraint's operation without being subject to its ecclesiastical coercion.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, secular_state_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(family_law_authority__christian_canonical_reading, catholic_hierarchy).
narrative_ontology:fixing_cost_class(family_law_authority__christian_canonical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates marriage formation and dissolution within Christian communities: provides a shared framework for validity, a process for determining whether a marriage existed (annulment), and communal recognition of marital status. Solves the problem of 'who is married to whom' within the faith community without relying on state definition.
% TRANSFER_FUNCTION: Moves compliance costs (annulment fees, time, emotional labor, sacramental exclusion) from divorce-seeking Catholics and interdenominational couples to the ecclesiastical authorities who control the validity framework. In Protestant contexts, moves denominational discipline costs (censure, exclusion from leadership) from divorcees to denominational bodies. The hierarchy and denominational authorities collect institutional legitimacy and control over the faithful's marital lives.
% ABSENT_VOICES: Historical: women under coverture (no independent marital voice), colonized peoples subjected to missionary marriage codes, enslaved Christians denied sacramental marriage. Contemporary: LGBTQ Christians in non-affirming traditions (excluded from the conversation about marriage theology), divorced Catholics who leave the Church rather than pursue annulment (silent exit), Protestant laity in hierarchical denominations with no vote on marriage canons.
% DISAPPEARANCE_RATIONALE: If ecclesiastical marriage authority vanished overnight: Catholic tribunals would close, annulment process would end, divorced/remarried Catholics would return to communion without canonical process, Protestant denominations would lose canonical marriage law (reverting to civil marriage only), interdenominational conflicts would resolve to civil law, LGBTQ exclusion would lose theological warrant in affirming denominations. The Christian marital landscape would reorganize around civil marriage + voluntary community recognition.
% FOUNDING_PROBLEM: In the 16th century, the Council of Trent (Tametsi, 1563) established canonical form to solve: clandestine marriages (no witnesses, no priest, disputed validity), Protestant rejection of sacramental marriage, and the need for a unified Catholic marital order in confessional states. The founding problem was coordinating marriage validity across a fragmented Christendom where civil and ecclesiastical jurisdiction overlapped and secret marriages created status uncertainty.
% FOUNDING_PROBLEM_CORROBORATION: Catholic magisterium (Catechism, Canon Law, papal teaching) attests the problem is live: secular marriage fails to capture sacramental reality, clandestine unions persist in new forms, the Church must witness to indissolubility. Reform theologians (e.g., Kasper, Rahner historically), canonists (e.g., Ladaria, Coccopalmerio), and the 2014-2015 Synods attest the problem is substantially solved: civil marriage provides public validity, clandestine marriage is rare, the indissolubility witness is the remaining live question. Secular jurists (Glendon, Witte) corroborate from outside: the confessional-state coordination problem is dead; the constraint persists as ecclesiastical authority maintenance.
narrative_ontology:disappearance_verdict(family_law_authority__christian_canonical_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__christian_canonical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__christian_canonical_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(family_law_authority__christian_canonical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__christian_canonical_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__christian_canonical_reading_tests).
:- end_tests(family_law_authority__christian_canonical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects the constraint's dual character: genuine coordination of marriage formation and community recognition (lowering ε) combined with asymmetric extraction from those seeking exit (annulment costs, sacramental exclusion, denominational discipline). Suppression (0.58) is substantial but declined from Tridentine peak (0.85) as civil marriage and secular divorce reduced ecclesiastical monopoly — suppression is now primarily internalized (conscience, sacramental access) rather than civil coercion. Theater ratio (0.28) captures the gap between the pastoral ideal (marriage as path to holiness) and the bureaucratic reality (annulment tribunals, dispensation offices, denial of communion to remarried). Accessibility collapse (0.65) is high because once the sacramental framework is accepted, civil alternatives feel religiously invalid — but Protestant variance and civil marriage provide partial exits. Resistance (0.45) is moderate: internal reform movements (synodality, LGBTQ inclusion, annulment reform) meet institutional inertia.
 *
 * PERSPECTIVAL GAP:
 *   The Catholic hierarchy seat experiences this as rope (genuine coordination of sacramental life, negligible extraction from their position). The divorce-seeking Catholic seat experiences it as snare (no exit, high extraction via annulment process, sacramental exclusion). Protestant denominational authorities sit between: coordination for their communities, but extraction varies by denomination's strictness. Interdenominational couples experience a tangled rope — coordinated in some denominations, extractive in others. The engine computes this divergence from power/exit/role declarations; the claim (tangled_rope) reflects the aggregate structural reality.
 *
 * DIRECTIONALITY LOGIC:
 *   Catholic hierarchy and Protestant authorities are beneficiaries (d ≈ 0.15) — they control the validity framework and collect institutional legitimacy. Canonical practitioners are beneficiaries (d ≈ 0.25) — professional rents from the tribunal system. Divorce-seeking Catholics are targets (d ≈ 0.85) — identity_locked exit (sacramental identity fused with marital status), no civil alternative within the faith. Annulment petitioners are targets (d ≈ 0.75) — constrained exit (process exists but costly). Interdenominational couples are targets (d ≈ 0.70) — constrained by conflicting rules. LGBTQ Christians are excluded (d ≈ 0.90) — trapped by doctrinal closure. Secular courts are analytical observers (d = 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (coordinating marriage formation and dissolution within a Christian commonwealth under ecclesiastical authority) is contested: Catholic magisterium claims it is live (secularization makes sacramental witness more necessary); reform voices and secular jurists argue it is dead (civil marriage solves coordination, ecclesiastical authority persists as extraction). The mandate has atrophied from civil enforcement to internal governance — the constraint persists because the hierarchy and denominations extract legitimacy and sacramental control from it, not because the coordination problem requires this specific solution. This is a classic mandatrophy: the arrangement's original justification (confessional state unity) is gone, but the constraint remains because beneficiaries would lose authority if it were removed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structure,
    'Is the christian_canonical_reading one reading of the contested family_law_authority kernel, and what structural elements differentiate it from sibling readings?',
    'Comparative structural analysis of all five declared readings of the family_law_authority kernel: hindu_dharmashastra_reading, muslim_shariat_reading, parsi_zoroastrian_reading, secular_contractual_reading, and this christian_canonical_reading. The kernel contest locates the disagreement in the grounding of marriage authority (ecclesiastical/denominational vs. dharmic/shariat/community/contractual) and the permanence model (sacramental indissolubility vs. contractual dissolubility).',
    'If the kernel framing is correct, this constraint''s ε and classification are reading-indexed — the same referent (standing family law arrangements) yields different ε values across readings. This constraint must not average or hedge across readings; its metrics describe this reading''s structural reality alone.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'This constraint instantiates one reading of the family_law_authority kernel; sibling readings produce different constraints with different ε values.').

omega_variable(
    sacramental_indissolubility_naturalness,
    'Is the Catholic sacramental indissolubility doctrine a genuine natural law of marriage (mountain) or a constructed ecclesiastical constraint that benefits the hierarchy (tangled_rope/snare)?',
    'Historical analysis of the development of canonical marriage doctrine from patristic through Tridentine to contemporary form; comparison with Eastern Orthodox oikonomia practice; examination of whether the no-divorce rule would persist without ecclesiastical enforcement infrastructure.',
    'If natural law (mountain), extraction metrics should be near-zero and suppression negligible. If constructed (tangled_rope/snare), the measured extraction and suppression reflect real coercive structure. The current metrics (ε=0.42, suppression=0.58) assume the constructed reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sacramental_indissolubility_naturalness, conceptual, 'Natural-law vs. constructed-status ambiguity for the core Catholic marriage doctrine.').

omega_variable(
    protestant_denominational_variance_extraction,
    'Does Protestant denominational variance in divorce permission represent genuine coordination (rope) or a fragmented extraction landscape where some denominations extract less but all claim ecclesiastical authority?',
    'Comparative analysis of divorce procedures, remarriage policies, and ecclesiastical court structures across major Protestant traditions (Anglican, Lutheran, Reformed, Baptist, Pentecostal). Measure whether denominational authority over marriage validity functions as coordination or as localized extraction.',
    'If coordination, Protestant segment lowers the constraint''s overall extraction profile. If fragmented extraction, the ''variance'' is itself an extraction mechanism — forum shopping within Christianity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(protestant_denominational_variance_extraction, empirical, 'Whether Protestant denominational governance of marriage is coordination or fragmented extraction.').

omega_variable(
    ecclesiastical_court_enforcement_ambiguity,
    'Is the suppression measured (0.58) primarily structural (canonical courts, state recognition of ecclesiastical decrees) or internalized (conscience formation, sacramental theology internalized as self-governance)?',
    'Post-exit suppression trajectory study: track Catholics who obtain civil divorce but not annulment — does the constraint''s suppressive force persist after civil exit? Measure internalized suppression via sacramental participation patterns, confession behavior, and self-reported conscience burden.',
    'If substantially internalized, effective suppression exceeds the structural measure — the constraint travels with the subject. This would increase χ for identity-locked Catholic subjects beyond the engine''s structural derivation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecclesiastical_court_enforcement_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in ecclesiastical marriage governance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__christian_canonical_reading, 1563, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(family_law_authority__christian_canonical_reading_tr_t1563, family_law_authority__christian_canonical_reading, theater_ratio, 1563, 0.15).
narrative_ontology:measurement(family_law_authority__christian_canonical_reading_tr_t1800, family_law_authority__christian_canonical_reading, theater_ratio, 1800, 0.22).
narrative_ontology:measurement(family_law_authority__christian_canonical_reading_tr_t1917, family_law_authority__christian_canonical_reading, theater_ratio, 1917, 0.25).
narrative_ontology:measurement(family_law_authority__christian_canonical_reading_tr_t1965, family_law_authority__christian_canonical_reading, theater_ratio, 1965, 0.27).
narrative_ontology:measurement(family_law_authority__christian_canonical_reading_tr_t1983, family_law_authority__christian_canonical_reading, theater_ratio, 1983, 0.28).
narrative_ontology:measurement(family_law_authority__christian_canonical_reading_tr_t2025, family_law_authority__christian_canonical_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(family_law_authority__christian_canonical_reading_be_t1563, family_law_authority__christian_canonical_reading, base_extractiveness, 1563, 0.65).
narrative_ontology:measurement(family_law_authority__christian_canonical_reading_be_t1800, family_law_authority__christian_canonical_reading, base_extractiveness, 1800, 0.58).
narrative_ontology:measurement(family_law_authority__christian_canonical_reading_be_t1917, family_law_authority__christian_canonical_reading, base_extractiveness, 1917, 0.52).
narrative_ontology:measurement(family_law_authority__christian_canonical_reading_be_t1965, family_law_authority__christian_canonical_reading, base_extractiveness, 1965, 0.48).
narrative_ontology:measurement(family_law_authority__christian_canonical_reading_be_t1983, family_law_authority__christian_canonical_reading, base_extractiveness, 1983, 0.45).
narrative_ontology:measurement(family_law_authority__christian_canonical_reading_be_t2025, family_law_authority__christian_canonical_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(family_law_authority__christian_canonical_reading_su_t1563, family_law_authority__christian_canonical_reading, suppression_requirement, 1563, 0.85).
narrative_ontology:measurement(family_law_authority__christian_canonical_reading_su_t1800, family_law_authority__christian_canonical_reading, suppression_requirement, 1800, 0.75).
narrative_ontology:measurement(family_law_authority__christian_canonical_reading_su_t1917, family_law_authority__christian_canonical_reading, suppression_requirement, 1917, 0.68).
narrative_ontology:measurement(family_law_authority__christian_canonical_reading_su_t1965, family_law_authority__christian_canonical_reading, suppression_requirement, 1965, 0.62).
narrative_ontology:measurement(family_law_authority__christian_canonical_reading_su_t1983, family_law_authority__christian_canonical_reading, suppression_requirement, 1983, 0.6).
narrative_ontology:measurement(family_law_authority__christian_canonical_reading_su_t2025, family_law_authority__christian_canonical_reading, suppression_requirement, 2025, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__christian_canonical_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(family_law_authority__christian_canonical_reading, 0.1).
narrative_ontology:affects_constraint(family_law_authority__christian_canonical_reading, family_law_authority__secular_contractual_reading).
narrative_ontology:affects_constraint(family_law_authority__christian_canonical_reading, family_law_authority__muslim_shariat_reading).
narrative_ontology:affects_constraint(family_law_authority__christian_canonical_reading, family_law_authority__hindu_dharmashastra_reading).
narrative_ontology:affects_constraint(family_law_authority__christian_canonical_reading, family_law_authority__parsi_zoroastrian_reading).

% DUAL FORMULATION NOTE:
% The family_law_authority kernel decomposes into five constraint stories (this one plus four siblings). This reading claims ecclesiastical/denominational authority grounded in sacramental theology; the secular_contractual_reading claims state authority grounded in individual autonomy; the muslim_shariat_reading claims Quranic authority; etc. Each has distinct ε, beneficiaries, victims, and classification. They are linked via affects_constraints because state family law systems must adjudicate between them (comity, conflict of laws, multicultural jurisdiction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(family_law_authority__christian_canonical_reading, powerless, 0.85).
constraint_indexing:directionality_override(family_law_authority__christian_canonical_reading, moderate, 0.7).
constraint_indexing:directionality_override(family_law_authority__christian_canonical_reading, organized, 0.2).
constraint_indexing:directionality_override(family_law_authority__christian_canonical_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
