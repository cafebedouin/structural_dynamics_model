% ============================================================================
% CONSTRAINT STORY: family_law_authority__christian_canonical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: Marriage as Sacrament Under Christian Ecclesiastical/Denominational Authority
 *   domain: comparative_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   This story instantiates the Christian canonical reading of the
 *   family_law_authority kernel: marriage governed either by Catholic
 *   sacramental doctrine (indissoluble absent tribunal-found invalidity) or
 *   by Protestant denominational governance (permitting divorce and
 *   remarriage under varying doctrinal conditions across denominations). Both
 *   share church-adjudicated validity and pastoral/social enforcement,
 *   differing sharply on dissolubility. The coordination function (stable,
 *   communally-recognized lifelong partnership with adjudicative authority
 *   for disputed cases) is genuine; the extraction runs through the
 *   asymmetric burden borne by those seeking exit, particularly under
 *   Catholic indissolubility, where annulment is the sole sanctioned path and
 *   its evidentiary and financial costs fall disproportionately on the less
 *   powerful spouse. Suppression is somewhat elevated historically (0.70 at
 *   the interval start, reflecting eras of strong church-civil fusion) and
 *   has declined modestly as civil divorce has become universally available
 *   as a parallel track, decoupling civil exit from religious standing — but
 *   within the religious community itself, suppression against exit from
 *   doctrinal marriage status remains substantial.
 *
 * KEY AGENTS:
 *   - catholic_magisterium: agenda_setter, sets and adjudicates sacramental marriage doctrine globally
 *   - protestant_denominational_bodies: agenda_setter/beneficiary, set varying denominational marriage governance
 *   - clergy_marriage_tribunals: beneficiary/agenda_setter, administer the annulment machinery
 *   - catholic_spouses_seeking_dissolution and annulment_petitioners: payers, bear the evidentiary and social cost of exit
 *   - same_sex_couples_within_denominations: excluded, categorically outside doctrinal recognition
 *   - civil_state_authorities: observer, provide the parallel civil track that partially offsets religious suppression
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__christian_canonical_reading, 0.58).
domain_priors:suppression_score(family_law_authority__christian_canonical_reading, 0.62).
domain_priors:theater_ratio(family_law_authority__christian_canonical_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__christian_canonical_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__christian_canonical_reading, "Marriage as Sacrament Under Christian Ecclesiastical/Denominational Authority").
narrative_ontology:topic_domain(family_law_authority__christian_canonical_reading, "comparative_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(family_law_authority__christian_canonical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__christian_canonical_reading, '8b553cce-c33a-415f-a00e-b4193739e5da').
narrative_ontology:cs_kernel_codification('8b553cce-c33a-415f-a00e-b4193739e5da', formalized).
narrative_ontology:cs_authority_grounding('8b553cce-c33a-415f-a00e-b4193739e5da', lineage).
narrative_ontology:cs_interpretation_layer_present('8b553cce-c33a-415f-a00e-b4193739e5da').
narrative_ontology:cs_reading_relation('8b553cce-c33a-415f-a00e-b4193739e5da', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('8b553cce-c33a-415f-a00e-b4193739e5da', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('8b553cce-c33a-415f-a00e-b4193739e5da', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_reading_relation('8b553cce-c33a-415f-a00e-b4193739e5da', family_law_authority__secular_contractual_reading, influences).
narrative_ontology:cs_axiom('8b553cce-c33a-415f-a00e-b4193739e5da', foundational, marriage_as_sacramental_covenant_not_terminable_contract).
narrative_ontology:cs_axiom_status(marriage_as_sacramental_covenant_not_terminable_contract, holdable).
narrative_ontology:cs_axiom_grounding('8b553cce-c33a-415f-a00e-b4193739e5da', marriage_as_sacramental_covenant_not_terminable_contract, theological).
narrative_ontology:cs_axiom('8b553cce-c33a-415f-a00e-b4193739e5da', foundational, ecclesiastical_body_holds_exclusive_competence_over_marital_validity).
narrative_ontology:cs_axiom_status(ecclesiastical_body_holds_exclusive_competence_over_marital_validity, holdable).
narrative_ontology:cs_axiom_grounding('8b553cce-c33a-415f-a00e-b4193739e5da', ecclesiastical_body_holds_exclusive_competence_over_marital_validity, conventional).
narrative_ontology:cs_reference_frame('8b553cce-c33a-415f-a00e-b4193739e5da', patristic_and_medieval_sacramental_settlement).
narrative_ontology:cs_drift_state('8b553cce-c33a-415f-a00e-b4193739e5da', contemporary_pluralist_civil_law_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('8b553cce-c33a-415f-a00e-b4193739e5da', '').
narrative_ontology:cs_kernel_id(family_law_authority__christian_canonical_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__christian_canonical_reading, catholic_magisterium).
narrative_ontology:constraint_beneficiary(family_law_authority__christian_canonical_reading, protestant_denominational_bodies).
narrative_ontology:constraint_beneficiary(family_law_authority__christian_canonical_reading, clergy_marriage_tribunals).
narrative_ontology:constraint_beneficiary(family_law_authority__christian_canonical_reading, family_stability_seeking_spouses).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, catholic_spouses_seeking_dissolution).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, annulment_petitioners).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, same_sex_couples_within_denominations).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, divorced_remarried_catholics).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, women_in_indissoluble_unions).
narrative_ontology:constraint_vindicates(family_law_authority__christian_canonical_reading, sacramental_indissolubility_doctrine).
narrative_ontology:constraint_vindicates(family_law_authority__christian_canonical_reading, ecclesiastical_competence_over_marriage_validity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines marriage as one of seven sacraments, conferring exclusive competence over validity, impediments, and dissolution through canon law and tribunal (Rota) processes. Sets doctrine that a validly ratified and consummated sacramental marriage cannot be dissolved by any human power, only annulled if never validly formed. Administers the annulment process and collects tribunal fees, deference, and institutional authority from the arrangement.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, catholic_magisterium, agenda_setter,
    institutional, civilizational, analytical, global).

% Various denominations (Anglican, Lutheran, Reformed, Baptist, etc.) each set their own governance over marriage validity, permitting divorce and remarriage under varying doctrinal conditions (adultery, abandonment, mutual consent in liberal bodies). Compete with each other and with civil authority for the couple's allegiance; retain moral and social authority over members but exercise less coercive enforcement than the Catholic tribunal system.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, protestant_denominational_bodies, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(family_law_authority__christian_canonical_reading, protestant_denominational_bodies, beneficiary).

% Canon lawyers, diocesan tribunal staff, and pastoral counselors administer the annulment and pastoral-discipline machinery. Their institutional roles, expertise, and standing derive directly from the continued operation of ecclesiastical marriage jurisdiction; a purely civil marriage regime would eliminate their function.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, clergy_marriage_tribunals, beneficiary,
    organized, biographical, analytical, national).
narrative_ontology:stakeholder_secondary_role(family_law_authority__christian_canonical_reading, clergy_marriage_tribunals, agenda_setter).

% Couples who value the permanence commitment and communal/ritual support structure the sacramental or denominational framing provides — social recognition, extended family integration, perceived moral seriousness of the bond, community accountability against impulsive dissolution.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, family_stability_seeking_spouses, beneficiary,
    moderate, biographical, constrained, local).

% Spouses in a failed, sometimes abusive, marriage who are told the sacramental bond cannot be dissolved absent a tribunal finding of invalidity. Must undergo a lengthy, often costly, evidentiary annulment process with no guarantee of outcome; those denied remain bound in the eyes of the Church regardless of civil divorce status.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, catholic_spouses_seeking_dissolution, payer,
    powerless, biographical, trapped, national).

% Bear the burden of proving a marriage was never validly formed — psychological incapacity, defect of consent, hidden impediment — often requiring intrusive testimony, canon lawyer fees, and years of waiting, in order to remarry within the Church without being treated as living in ongoing sin.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, annulment_petitioners, payer,
    powerless, biographical, constrained, national).

% Excluded categorically from sacramental marriage in Catholic doctrine and from marriage rites in many conservative Protestant bodies; where recognized in some liberal Protestant denominations, they remain contested and unrecognized across the wider Christian communion, with no voice in the councils that set doctrine.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, same_sex_couples_within_denominations, excluded,
    powerless, biographical, trapped, national).

% Civilly divorced and remarried without an annulment; historically barred from receiving Communion and treated as in an irregular canonical state, bearing social and spiritual costs within their faith community for a civil status the Church does not recognize as dissolving the prior bond.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, divorced_remarried_catholics, payer,
    powerless, biographical, constrained, national).

% In communities where Catholic indissolubility doctrine carries strong social and legal force, women facing abuse or abandonment may have no religiously sanctioned exit, and where civil law defers heavily to religious marriage norms, face compounded difficulty securing safety, property division, or social acceptance after separation.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, women_in_indissoluble_unions, payer,
    powerless, biographical, trapped, local).

% Determine how much legal effect to give ecclesiastical marriage determinations — civil divorce is available in nearly all jurisdictions regardless of religious status, but the story's actors experience the doctrinal constraint as operative within their religious and social lives independent of civil remedies.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, civil_state_authorities, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(family_law_authority__christian_canonical_reading, diffuse).
narrative_ontology:fixing_cost_class(family_law_authority__christian_canonical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, ritually and communally reinforced framework for lifelong partnership, child-rearing, and property/inheritance expectations, with a recognized authority (church or denomination) to adjudicate disputes over validity and to provide pastoral and social support structures around the marital bond.
% TRANSFER_FUNCTION: Moves interpretive and adjudicative authority over the legitimacy of marriages, and over exit from them, from the individual spouses to ecclesiastical or denominational bodies; moves social and spiritual costs of contested dissolution onto spouses (disproportionately less powerful ones) seeking to leave or remarry.
% ABSENT_VOICES: Same-sex couples, divorced-and-remarried laity, and women in abusive indissoluble unions have no vote in doctrinal councils (Catholic magisterial teaching authority, denominational synods) that set the terms governing their own marital status; their objections are pastoral footnotes, not inputs to doctrine.
% DISAPPEARANCE_RATIONALE: If ecclesiastical/denominational authority over marriage vanished overnight, civil marriage and divorce law would continue functioning largely unchanged in most jurisdictions (weakening the 'world_rearranges' case), but within religious communities the loss would be significant: tribunal systems would close, annulment petitioners would gain unilateral exit, and the social/spiritual meaning attached to marriage within these traditions would be substantially reorganized — hence contested rather than a clean verdict either way.
% FOUNDING_PROBLEM: Early and medieval Christian communities sought to establish marriage as a sacred, binding covenant resistant to unilateral abandonment (protecting spouses, especially wives and children, from arbitrary repudiation common in surrounding legal cultures) and to bring marital law under a coherent theological and moral framework rather than leaving it to fragmented secular custom.
% FOUNDING_PROBLEM_CORROBORATION: The Catholic magisterium and denominational leadership attest the founding problem (protecting the marital bond and family stability) remains live and doctrinally central. Independent family-law scholars, sociologists studying annulment tribunals, and advocacy groups for annulment petitioners and abuse survivors attest that in contemporary practice the arrangement often functions to protect institutional authority and doctrinal consistency more than to protect the vulnerable spouses it was originally framed to shield — a reading corroborated by canon-law historians outside the tribunal system itself.
narrative_ontology:disappearance_verdict(family_law_authority__christian_canonical_reading, contested).
narrative_ontology:founding_problem_status(family_law_authority__christian_canonical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__christian_canonical_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(family_law_authority__christian_canonical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__christian_canonical_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__christian_canonical_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(family_law_authority__christian_canonical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(family_law_authority__christian_canonical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects the real but partial extraction: the coordination function of stable, recognized marriage is genuine and most participants are not extracted from, but a meaningful subset (annulment petitioners, indissolubly-bound spouses, excluded same-sex couples) bear costs disproportionate to any benefit they receive from the arrangement's authority structure. Suppression (0.62, declining slightly over the interval) is high because exit from doctrinally-recognized marital status is gated entirely through ecclesiastical process with no self-help alternative within the faith community, though the decline reflects growing availability and normalization of parallel civil divorce that reduces the practical stakes of religious non-recognition. Theater ratio (0.40, rising) captures the growing gap between the tribunal system's formal evidentiary rigor and its actual function, which increasingly resembles a sorting/legitimation ritual for exits that would occur regardless.
 *
 * DIRECTIONALITY LOGIC:
 *   The magisterium and denominational bodies sit at the full-beneficiary end: they set doctrine, adjudicate validity, and derive institutional authority and social deference from continued jurisdiction over marriage. Tribunal clergy are secondary beneficiaries whose professional role depends on the system's persistence. Spouses seeking exit — especially Catholic spouses without a viable annulment claim — sit near the full-target end: trapped in doctrinal status regardless of civil remedy, bearing evidentiary and social costs to secure recognition of exit. Excluded same-sex couples experience a different mechanism: not extraction through participation, but exclusion from the coordination benefit altogether, which the framework treats via the excluded role and absent_voices rather than a pure directionality score.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protecting spouses and families from arbitrary unilateral repudiation, common in surrounding ancient legal cultures) is genuinely contested as either still live or substantially resolved by modern civil family law's own protections (property division, support obligations, domestic violence law) that no longer depend on ecclesiastical adjudication. Classifying this as tangled_rope rather than snare or mountain prevents two errors: treating the whole arrangement as pure extraction (ignoring the real coordination and community-support value many spouses genuinely receive) and treating it as natural/inevitable (ignoring the identifiable institutional beneficiaries and identifiable victims created by the indissolubility doctrine specifically). The status is contested rather than resolved because corroboration diverges sharply along benefit lines.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sacramental_reality_vs_institutional_construction,
    'Is sacramental indissolubility a genuine theological/metaphysical reality that the Church merely recognizes and administers, or a doctrinally-constructed rule that serves the institutional interest of the tribunal and magisterial system that enforces it?',
    'Comparative historical analysis of doctrinal development (e.g., early Church practice, Eastern Orthodox oikonomia allowing remarriage, patristic-era variation) against claims of unchanging doctrine; theological argument is not empirically resolvable, but the historical variability of practice is evidence relevant to the constructed-vs-discovered question.',
    'If treated as genuinely theologically necessary and unchangeable, extraction from indissolubility-bound spouses would be better modeled as an unavoidable cost of a coordination good rather than rent extraction. If treated as institutionally constructed and variable, the extraction reads more clearly as tangled_rope or approaching snare for the most trapped victim group.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sacramental_reality_vs_institutional_construction, conceptual, 'Whether Catholic sacramental indissolubility is theological necessity or institutional construction.').

omega_variable(
    kernel_reading_disagreement_locus,
    'This constraint is one reading (christian_canonical_reading) of the shared family_law_authority kernel. Sibling readings (hindu_dharmashastra_reading, muslim_shariat_reading, parsi_zoroastrian_reading, secular_contractual_reading) locate marital authority in different sources — dharmic text and custom, Quranic contract law, Zoroastrian community law, or the autonomous individual under state law respectively. Where exactly does the substantive disagreement sit: is it about WHO holds interpretive authority (religious body vs. state vs. individual), or about WHETHER marriage is sacramental/covenantal at all versus purely contractual?',
    'Structural comparison across the five sibling constraint stories: compare each reading''s authority_grounding and axioms in cs_structure to locate whether the axis of disagreement is authority-location or ontological status of marriage.',
    'If the disagreement is primarily about authority location, secular_contractual_reading and christian_canonical_reading could in principle coexist as parallel civil/religious tracks (as they largely do today in most jurisdictions). If it is about ontological status (sacrament vs. contract), the readings are in deeper tension since Catholic doctrine denies civil authority can dissolve what it sacramentally binds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Locating the structural axis of disagreement among the kernel''s sibling readings.').

omega_variable(
    protestant_internal_variance,
    'This story treats ''Protestant denominational governance'' as one bloc for tractability, but denominations range from near-Catholic sacramentalism (some Anglican/Lutheran high-church traditions) to fully contractual, easily-dissolved marriage (liberal Protestant and some evangelical bodies). Does collapsing this variance into a single reading obscure a further decomposition this story should have made?',
    'If corpus analysis shows Protestant denominational marriage governance produces substantially different epsilon under different denominational sub-readings (e.g., a high-church-Anglican reading vs. a low-church-Baptist reading), split into separate constraint stories per the epsilon-invariance principle.',
    'A future decomposition could split this single christian_canonical_reading into catholic_sacramental_subreading and protestant_denominational_subreading with materially different epsilon values (Catholic indissolubility likely producing higher extraction/suppression for trapped spouses than most Protestant divorce-permitting regimes).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protestant_internal_variance, conceptual, 'Whether Catholic and Protestant sub-readings should themselves be decomposed into separate constraint stories.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__christian_canonical_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t0, family_law_authority__christian_canonical_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(fami_tr_t10, family_law_authority__christian_canonical_reading, theater_ratio, 10, 0.26).
narrative_ontology:measurement(fami_tr_t20, family_law_authority__christian_canonical_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(fami_tr_t30, family_law_authority__christian_canonical_reading, theater_ratio, 30, 0.33).
narrative_ontology:measurement(fami_tr_t40, family_law_authority__christian_canonical_reading, theater_ratio, 40, 0.36).
narrative_ontology:measurement(fami_tr_t50, family_law_authority__christian_canonical_reading, theater_ratio, 50, 0.38).
narrative_ontology:measurement(fami_tr_t60, family_law_authority__christian_canonical_reading, theater_ratio, 60, 0.4).

% Extraction over time
narrative_ontology:measurement(fami_be_t0, family_law_authority__christian_canonical_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(fami_be_t10, family_law_authority__christian_canonical_reading, base_extractiveness, 10, 0.46).
narrative_ontology:measurement(fami_be_t20, family_law_authority__christian_canonical_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(fami_be_t30, family_law_authority__christian_canonical_reading, base_extractiveness, 30, 0.53).
narrative_ontology:measurement(fami_be_t40, family_law_authority__christian_canonical_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(fami_be_t50, family_law_authority__christian_canonical_reading, base_extractiveness, 50, 0.57).
narrative_ontology:measurement(fami_be_t60, family_law_authority__christian_canonical_reading, base_extractiveness, 60, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t0, family_law_authority__christian_canonical_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(fami_su_t10, family_law_authority__christian_canonical_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(fami_su_t20, family_law_authority__christian_canonical_reading, suppression_requirement, 20, 0.66).
narrative_ontology:measurement(fami_su_t30, family_law_authority__christian_canonical_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement(fami_su_t40, family_law_authority__christian_canonical_reading, suppression_requirement, 40, 0.63).
narrative_ontology:measurement(fami_su_t50, family_law_authority__christian_canonical_reading, suppression_requirement, 50, 0.62).
narrative_ontology:measurement(fami_su_t60, family_law_authority__christian_canonical_reading, suppression_requirement, 60, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__christian_canonical_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(family_law_authority__christian_canonical_reading, 0.08).
narrative_ontology:affects_constraint(family_law_authority__christian_canonical_reading, hindu_dharmashastra_reading).
narrative_ontology:affects_constraint(family_law_authority__christian_canonical_reading, muslim_shariat_reading).
narrative_ontology:affects_constraint(family_law_authority__christian_canonical_reading, parsi_zoroastrian_reading).
narrative_ontology:affects_constraint(family_law_authority__christian_canonical_reading, secular_contractual_reading).

% DUAL FORMULATION NOTE:
% This story is one of five sibling readings of the family_law_authority kernel, each authored as a separate ε-invariant constraint per the ε-invariance principle. The Christian canonical reading is distinguished by its combination of sacramental ontology (Catholic branch) and church/denominational competence over validity determinations, producing a distinctive indissolubility-driven extraction pattern absent in the more purely contractual siblings (muslim_shariat_reading, secular_contractual_reading) and differently structured from the dharmic-text-and-custom governance of hindu_dharmashastra_reading or the community-preservation focus of parsi_zoroastrian_reading. All five stories should be read as a constraint family, not as competing measurements of one underlying constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
