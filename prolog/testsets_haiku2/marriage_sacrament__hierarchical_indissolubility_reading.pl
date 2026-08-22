% ============================================================================
% CONSTRAINT STORY: marriage_sacrament__hierarchical_indissolubility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_sacrament__hierarchical_indissolubility_reading, []).

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
 *   constraint_id: marriage_sacrament__hierarchical_indissolubility_reading
 *   human_readable: Marriage Sacrament: Hierarchical Indissolubility Reading
 *   domain: religious_doctrine/canon_law
 *
 * SUMMARY:
 *   This constraint represents one reading of a contested kernel: the
 *   sacrament of marriage. The hierarchical indissolubility reading treats
 *   marriage as an ontological reality — a metaphysical bond created by the
 *   sacrament that persists regardless of human will, circumstance, or civil
 *   law. Under this reading, indissolubility is not an aspiration but a
 *   constitutive fact. Divorced Catholics seeking to remarry are caught in
 *   the structural contradiction: civil law dissolves the marriage; Church
 *   teaching declares it indissoluble. The hierarchical magisterium reserves
 *   sole authority to adjudicate whether a marriage was ever valid
 *   (annulment), creating a gatekeeping apparatus that extracts control and
 *   compliance from divorced Catholics. This reading coexists with a sibling
 *   pastoral reading (the civic_pastoral_reading) that treats marriage as a
 *   lived relationship subject to compassionate discernment and the
 *   possibility of sacramental inclusion despite divorce.
 *
 * KEY AGENTS:
 *   - Hierarchical Magisterium: Sets and enforces doctrine of ontological indissolubility; controls annulment adjudication
 *   - Diocesan Tribunals: Implement magisterial teaching; process annulments; impose costs and delays
 *   - Divorced/Remarried Catholics: Bear the cost of exclusion; identity-locked to the faith tradition
 *   - Pastoral Clergy: Caught between hierarchical doctrine and pastoral experience; excluded from teaching authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_sacrament__hierarchical_indissolubility_reading, 0.72).
domain_priors:suppression_score(marriage_sacrament__hierarchical_indissolubility_reading, 0.68).
domain_priors:theater_ratio(marriage_sacrament__hierarchical_indissolubility_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_sacrament__hierarchical_indissolubility_reading, tangled_rope).
narrative_ontology:human_readable(marriage_sacrament__hierarchical_indissolubility_reading, "Marriage Sacrament: Hierarchical Indissolubility Reading").
narrative_ontology:topic_domain(marriage_sacrament__hierarchical_indissolubility_reading, "religious_doctrine/canon_law").

domain_priors:requires_active_enforcement(marriage_sacrament__hierarchical_indissolubility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_sacrament__hierarchical_indissolubility_reading, '07354c3a-fee0-4f26-a2e5-b9e9c023e9a3').
narrative_ontology:cs_kernel_codification('07354c3a-fee0-4f26-a2e5-b9e9c023e9a3', formalized).
narrative_ontology:cs_authority_grounding('07354c3a-fee0-4f26-a2e5-b9e9c023e9a3', lineage).
narrative_ontology:cs_interpretation_layer_present('07354c3a-fee0-4f26-a2e5-b9e9c023e9a3').
narrative_ontology:cs_reading_relation('07354c3a-fee0-4f26-a2e5-b9e9c023e9a3', marriage_sacrament__civic_pastoral_reading, coexists_with).
narrative_ontology:cs_axiom('07354c3a-fee0-4f26-a2e5-b9e9c023e9a3', foundational, marriage_indissolubility_ontological).
narrative_ontology:cs_axiom_status(marriage_indissolubility_ontological, holdable).
narrative_ontology:cs_axiom_grounding('07354c3a-fee0-4f26-a2e5-b9e9c023e9a3', marriage_indissolubility_ontological, deontological).
narrative_ontology:cs_axiom('07354c3a-fee0-4f26-a2e5-b9e9c023e9a3', foundational, hierarchical_magisterium_sole_adjudicator).
narrative_ontology:cs_axiom_status(hierarchical_magisterium_sole_adjudicator, holdable).
narrative_ontology:cs_axiom_grounding('07354c3a-fee0-4f26-a2e5-b9e9c023e9a3', hierarchical_magisterium_sole_adjudicator, deontological).
narrative_ontology:cs_reference_frame('07354c3a-fee0-4f26-a2e5-b9e9c023e9a3', sacramental_indissolubility_as_metaphysical_truth).
narrative_ontology:cs_drift_state('07354c3a-fee0-4f26-a2e5-b9e9c023e9a3', contemporary_post_vatican_ii_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('07354c3a-fee0-4f26-a2e5-b9e9c023e9a3', '').
narrative_ontology:cs_kernel_id(marriage_sacrament__hierarchical_indissolubility_reading, marriage_sacrament).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_sacrament__hierarchical_indissolubility_reading, hierarchical_magisterium).
narrative_ontology:constraint_beneficiary(marriage_sacrament__hierarchical_indissolubility_reading, diocesan_tribunal_apparatus).
narrative_ontology:constraint_victim(marriage_sacrament__hierarchical_indissolubility_reading, divorced_catholics_seeking_remarriage).
narrative_ontology:constraint_victim(marriage_sacrament__hierarchical_indissolubility_reading, remarried_catholics_without_annulment).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_sacrament__hierarchical_indissolubility_reading, annulment_processing_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The teaching authority of the Catholic Church, centered in Rome. Declares and enforces doctrine that marriage bonds are ontologically indissoluble — a metaphysical reality, not a pastoral ideal. Controls annulment adjudication through canon law and reserves authority to define what counts as a valid marriage. Maintains the doctrine as doctrinally binding and sacramentally necessary, treating any pastoral relaxation of the indissolubility claim as heretical compromise.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, hierarchical_magisterium, agenda_setter,
    institutional, civilizational, analytical, global).

% Canon law tribunals that process annulment petitions in dioceses worldwide. Operate under the magisterium's doctrine of ontological indissolubility. Implement the hierarchical decision process: investigating whether a marriage was ever validly formed (rather than whether it should be dissolved). Impose costs through expert testimony requirements, psychological evaluations, and extended timelines. The apparatus's institutional existence depends on the indissolubility doctrine remaining doctrine — if divorce were theologically acceptable, tribunals would become unnecessary.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, diocesan_tribunal_apparatus, agenda_setter,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(marriage_sacrament__hierarchical_indissolubility_reading, diocesan_tribunal_apparatus, beneficiary).

% Baptized Catholics whose first marriage ended in divorce and who seek to remarry (either reconcile with the first spouse or marry a new partner). Under the hierarchical indissolubility reading, any remarriage without an annulment declaration places them in a state of grave sin — the first marriage bond persists as an ontological fact, making the second marriage adulterous. They cannot receive Eucharist, cannot be married in the Church, cannot be godparents, and are treated as living objectively disordered lives. Their Catholic identity is central to their self-understanding; leaving the Church means identity loss. The only path to sacramental restoration is obtaining an annulment (costly, uncertain, years-long) or dissolving the remarriage (breaking the current family).
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, divorced_catholics_seeking_remarriage, payer,
    powerless, biographical, identity_locked, local).

% Already remarried after divorce without securing an annulment. Formally excluded from sacraments. The remarriage (often a good-faith relationship, sometimes with children) is classified as adultery under the hierarchical reading because the first bond is treated as persisting. They bear the cost of exclusion: cannot attend their children's first communions as receivers of Eucharist, cannot be active in parish leadership, experience shame and institutional marginalization. Reforming their situation requires either obtaining an annulment (costs, uncertainty, years) or ending the current marriage (family dissolution).
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, remarried_catholics_without_annulment, payer,
    powerless, biographical, identity_locked, local).

% Parish priests, bishops, and pastoral ministers who encounter divorced and remarried Catholics in daily ministry. Many experience acute tension between the hierarchical doctrine (indissolubility is ontological, exclusion is necessary) and pastoral experience (remarried Catholics are living in genuine families, seeking spiritual nourishment, living faithfully). They witness the exclusion's harm. Some privately advocate for annulment shortcuts or pastoral flexibility, but public dissent risks institutional sanction. They are constrained by hierarchical obedience; their pastoral judgment is explicitly subordinated to magisterial doctrine. They are excluded from the magisterial teaching conversation — they are implementers, not voices in doctrine-setting.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, pastoral_clergy, observer,
    organized, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(marriage_sacrament__hierarchical_indissolubility_reading, pastoral_clergy, excluded).

% The civil legal system that recognizes and enforces divorce as dissolving marriage bonds. Creates the structural contradiction at the constraint's core: what civil law dissolves, Church doctrine declares indissoluble. Civil courts do not participate in Church adjudication; they operate from different authority structures. They observe the Church's constraint as external institutional reality but do not enforce it or validate its claims.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, civil_divorce_courts, observer,
    institutional, generational, analytical, national).

% Canon lawyers, tribunal advocates, expert witnesses (psychologists, theologians), and diocesan officials who specialize in annulment cases. Derive professional income and standing from processing petitions. Their caseload and necessity depend on the indissolubility doctrine remaining binding and the annulment process remaining a genuine gatekeeping function. A system that readily granted annulments or abandoned the doctrine would reduce their professional market significantly.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, annulment_processing_advocates, beneficiary,
    moderate, biographical, mobile, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_sacrament__hierarchical_indissolubility_reading, hierarchical_magisterium).
narrative_ontology:fixing_cost_class(marriage_sacrament__hierarchical_indissolubility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Catholic sacramental theology with canon law: establishes a unified, authoritative framework for understanding marriage as an ontological sacramental reality whose indissolubility is metaphysical, not merely normative. Solves a theological problem: if marriage could be dissolved by pastoral discretion or human choice, the sacrament's binding force and transcendent meaning would be compromised. The constraint ensures that Church teaching on marriage remains coherent and hierarchically uniform.
% TRANSFER_FUNCTION: Transfers from divorced and remarried Catholics (and pastoral clergy bound to them) to the hierarchical magisterium and tribunal apparatus: (1) control over sacramental access and definition of valid marriage; (2) authority to adjudicate whether a marriage ever existed (annulment power); (3) compliance through exclusion — the threat of Eucharistic deprivation enforces acceptance of hierarchical judgment; (4) economic value through tribunal fees and the existence-justification of the annulment apparatus; (5) doctrinal vindication — the constraint maintains the magisterium's claim to indefectible teaching authority.
% ABSENT_VOICES: Most saliently: divorced Catholics themselves, who would argue for pastoral compassion and sacramental inclusion; remarried Catholics living in good faith, who would attest that their current families are real and worthy of blessing rather than exclusion; pastoral clergy, who would argue for discretionary authority and human judgment in individual cases; modern theologians and pastoral theologians, who would articulate a vision of marriage as lived love rather than ontological indissolubility; and feminist and liberationist theologians, who would challenge the patriarchal authority structure itself. These voices exist in published theology, pastoral testimony, and lived experience, but are formally excluded from the magisterial teaching conversation by the hierarchical structure's design. The structure exists partly to maintain teaching authority against these challengers.
% DISAPPEARANCE_RATIONALE: If this constraint vanished — if the hierarchical indissolubility reading were abandoned — the institutional apparatus would reorganize: the magisterium would adopt the pastoral reading (marriage as a lived relationship, indissolubility as normative aspiration, not ontological fact); annulment tribunals would become advisory rather than gatekeeping; remarriage would be immediate and sacramentally normal; remarried Catholics would rejoin full sacramental participation without justification or delay; pastoral clergy would exercise discernment about individual cases without hierarchical override; the locus of authority for sacramental meaning would shift from centralized magisterium to local ordinary or pastoral community judgment. The constraint's disappearance would require a profound reorganization of Catholic institutional authority around marriage.
% FOUNDING_PROBLEM: The founding problem is theological-disciplinary: how to maintain the binding, transcendent character of the marriage sacrament against cultural dissolution (rising divorce rates, civil law recognizing divorce, secularization eroding religious commitment). The hierarchical indissolubility reading was formulated to answer this: by asserting that marriage's indissolubility is not merely a Church teaching or moral ideal, but an ontological fact created by the sacrament itself — a metaphysical reality that civil law cannot touch and human will cannot undo. This reading protected the sacrament's transcendent meaning against commodification and dissolution.
% FOUNDING_PROBLEM_CORROBORATION: The hierarchical magisterium and conservative canon lawyers attest the problem remains live: indissolubility doctrine is necessary to preserve sacramental integrity and protect marriage from dissolution. Progressive pastoral theologians, divorced Catholics, and pastoral practitioners attest the problem has shifted: the contemporary problem is not defending indissolubility (most Catholics accept that marriages can fail), but integrating fallen marriages into a spirituality of healing and grace. The Second Vatican Council (Gaudium et Spes) and subsequent papal writings (notably Pope Francis's synodal reform initiatives on marriage) show increasing emphasis on marriage as a lived relationship of love and communion rather than as an ontological bond — this represents external testimony (within the tradition itself) that the founding problem's character is contested. The constraint persists despite this contestation because doctrine, once established, has institutional inertia and the apparatus defending it has structural interests in its continuation.
narrative_ontology:disappearance_verdict(marriage_sacrament__hierarchical_indissolubility_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_sacrament__hierarchical_indissolubility_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_sacrament__hierarchical_indissolubility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_sacrament__hierarchical_indissolubility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_sacrament__hierarchical_indissolubility_reading, 0.72, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_sacrament__hierarchical_indissolubility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_sacrament__hierarchical_indissolubility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_sacrament__hierarchical_indissolubility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.58 to 0.72 over the interval because the magisterium has progressively hardened the doctrinal claim while civil divorce rates increased, expanding the victim population relative to the beneficiary apparatus. The constraint extracts control (hierarchical authority over sacramental access) and economic value (tribunal fees, professional necessity for annulment specialists). Suppression at 0.68 reflects the mechanism: sacramental exclusion is internally enforced (the faithful internalize shame/unworthiness) and structurally enforced (formally barred participation). Theater rises to 0.42 and plateaus because the annulment process increasingly performs doctrinal coherence while accommodating practical cases — the system maintains orthodoxy while quietly expanding nullity grounds to provide relief. The measurement series shows rising extractiveness until about t=50, then slight decline (t=60: 0.72) as internal dissent (pastoral theology, post-Vatican II emphasis on love-based marriage) creates countervailing pressure. This plateau suggests a constraint approaching a phase transition: enough internal resistance that simple extraction escalation becomes costly; enough external pressure (civil divorce norms, theological change) that theater cannot indefinitely substitute for legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   The hierarchical magisterium experiences this constraint as maintaining doctrinal purity and sacramental integrity — coordination of theology with canon law. From the victim's position, the same structure is pure extraction: arbitrary gatekeeping, denial of sacraments as punishment, and an apparatus (tribunals) whose existence depends on keeping the gates difficult to pass. The magisterium's d approaches 0 (beneficiary: control, authority, doctrinal vindication); divorced Catholics' d approaches 1 (target: excluded, costs borne, no exit). The pastoral clergy sit near 0.6–0.7 (constrained: they benefit from ecclesiastical standing but pay the cost of enforcing a doctrine they increasingly doubt). This divergence is the reading-specific structural fact: the hierarchical reading's internal coherence depends on treating indissolubility as metaphysical fact, which makes victim status non-negotiable for those in contradiction with it.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is driven by the identity-lock: divorced Catholics are bound to the faith tradition by baptismal identity, familial culture, and sacramental theology that constitutes their selfhood. Exit means apostasy or excommunication — it is identity death, not mere inconvenience. This identity-lock combined with institutional power (the magisterium controls sacramental access) produces high d for the victim seats and near-zero d for the beneficiary seats. The tribunals hold a secondary beneficiary position (d ≈ 0.2): they collect fees and professional standing, but are constrained by hierarchical doctrine — they cannot unilaterally ease access without contradicting the magisterium. Pastoral clergy hold high d (0.7–0.8) despite their power because they are organizationally bound to enforce doctrine they increasingly contest, and their objections are structurally excluded from teaching authority.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exhibits mandatrophy, but in a doctrinal key. The founding problem — maintaining sacramental indissolubility in a world of widespread civil divorce — was real at the constraint's formulation (mid-20th century, before rapid divorce legalization). The constraint solved it by asserting ontological indissolubility and hierarchical gatekeeping. But the mandate has shifted: the population of divorced Catholics grew 3–4x; pastoral theology moved toward emphasizing marriage as lived love rather than metaphysical bond; even the magisterium (in John Paul II and later) began describing marriage in relational language. The constraint persists despite the mandate's atrophy because (1) it remains doctrinal law, inertially maintained; (2) the apparatus (tribunals) has institutional interests in its continuation; (3) the hierarchical reading's core claim (indissolubility is ontological) is not falsifiable — it can be restated in new language indefinitely. The theater_ratio rising to 0.42 is a symptom: the system increasingly performs doctrinal coherence (through complex annulment arguments) while practically accommodating relief, rather than genuinely resolving the mandate/reality gap. A true mandatrophy resolution would require either (a) abandoning the constraint entirely (adopting the pastoral reading); (b) explicitly admitting the mandate is gone and maintaining the constraint as pure institutional inertia; or (c) reformulating the constraint to dissolve the gap (e.g., by redefining indissolubility as normative rather than ontological, which would shift the constraint from tangled_rope toward rope). None of these has occurred, so mandatrophy persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontological_vs_normative_indissolubility,
    'Is marriage''s indissolubility an ontological fact (a metaphysical reality created by the sacrament that persists independent of human will) or a normative ideal (the Church''s teaching that marriages should last, combined with pastoral accountability for breach)?',
    'This is a theological-metaphysical question, not an empirical one. Resolution would require acceptance of a particular doctrine as authoritative. Within the hierarchical reading''s framework, the question is resolved: indissolubility is declared ontological. Within the pastoral reading''s framework, it is resolved as normative. No data point settles this disagreement because the readings operate from different foundational commitments.',
    'If indissolubility is ontological, the constraint''s structure (hierarchy, gatekeeping, victim exclusion) follows necessarily. If indissolubility is normative, the constraint dissolves entirely — it becomes a voluntary pastoral framework, not a coercive hierarchy. This is the deepest structural ambiguity between the readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ontological_vs_normative_indissolubility, conceptual, 'The kernel''s referent: is indissolubility a fact to be adjudicated hierarchically, or a value to be pursued pastorally?').

omega_variable(
    hierarchical_authority_legitimacy,
    'Is the hierarchical magisterium''s right to define marriage and adjudicate its dissolution grounded in apostolic succession and sacramental authority, or in institutional power claims that exceed theological warrant?',
    'This is a question about the authority structure''s own foundations. It is resolved one way by those who accept hierarchical Catholic ecclesiology (the magisterium has the authority); another way by those who contest it (the authority claim is institutional self-assertion). Different Christian traditions (Catholic vs. Orthodox vs. Protestant) answer this differently. No universal arbiter exists.',
    'If hierarchical authority is legitimate, the constraint''s extraction is justified as the price of doctrinal coherence and sacramental protection. If the authority claim fails, the constraint becomes purely extractive — hierarchy without theological justification, gatekeeping without warrant.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hierarchical_authority_legitimacy, conceptual, 'Is the magisterial authority grounding this constraint''s gatekeeping legitimate in its own theological tradition?').

omega_variable(
    suppression_internalization_mechanism,
    'To what extent is the measured suppression (0.68) structural (externally imposed exclusion, tribunal barriers, fear of institutional sanction) versus internalized (divorced Catholics internalizing shame, unworthiness, or accepting the magisterium''s judgment as truth)?',
    'Post-exit trajectory: divorced Catholics who leave the Church or accept civil remarriage without attempting sacramental validation would show whether suppression persists after the institutional barrier is removed. Pastoral research and testimony from remarried Catholics who have shifted to non-hierarchical spirituality or left the Church would indicate the internalization degree.',
    'If suppression is primarily structural, removing the constraint (adopting the pastoral reading) would free the victim population immediately. If suppression is internalized, victim healing would require longer-term spiritual/psychological work even after the institutional constraint is removed. This affects the remedy design for constraint dissolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_mechanism, empirical, 'Is the suppression mechanism structural exclusion or internalized shame-acceptance?').

omega_variable(
    ritual_vs_doctrinal_function,
    'Does the annulment apparatus primarily serve a doctrinal function (genuinely investigating whether a marriage was ever valid) or a ritual function (performing doctrinal coherence while accommodating practical cases through increasingly expansive nullity grounds)?',
    'Systematic analysis of annulment grant rates and nullity grounds over time: if the apparatus increasingly finds nullities in cases that would once have been deemed indissoluble, the ritual-performance function dominates. Interviews with tribunal judges and canon lawyers about their actual decision-making criteria (formal vs. material standards) would reveal whether gates are being opened while doctrine remains formally unchanged.',
    'If primarily ritual, the theater_ratio is higher than measured and the constraint''s actual extractiveness is lower (the gates open quietly, victims find relief through procedural workarounds). If primarily doctrinal, the constraint''s extractiveness is genuine — hierarchy and gatekeeping are real, not performative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ritual_vs_doctrinal_function, empirical, 'Is the annulment process a genuine doctrinal investigation or a ritually-maintained gatekeeping theater?').

omega_variable(
    reading_foreclosure_boundary,
    'Does the hierarchical indissolubility reading logically foreclose the pastoral reading within a single Catholic framework, or can both coexist as live theological options?',
    'This is a question about logical structure, not empirical fact. If the hierarchical reading claims indissolubility is ontologically inescapable, it forecloses the pastoral reading''s claim that pastoral discretion is theologically warranted. If the hierarchical reading is willing to hold its position as normative (even highly binding) without claiming ontological metaphysical necessity, then both readings can coexist — the pastoral reading simply chooses different normative weight. The resolution depends on which forms the hierarchical reading actually claims.',
    'If foreclosure holds, only one reading can be true in any single framework — a mismatch between them signals which one fails. If coexistence holds, both readings can be defended simultaneously by different seats, and the constraint''s type-classification would differ per-seat reading rather than per-constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_boundary, conceptual, 'Can the hierarchical and pastoral readings of marriage coexist in one framework, or does one rule out the other?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_sacrament__hierarchical_indissolubility_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(marr_tr_t10, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement(marr_tr_t20, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement(marr_tr_t30, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement(marr_tr_t40, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(marr_tr_t50, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 50, 0.43).
narrative_ontology:measurement(marr_tr_t60, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 60, 0.42).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(marr_be_t10, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(marr_be_t20, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(marr_be_t30, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 30, 0.7).
narrative_ontology:measurement(marr_be_t40, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 40, 0.72).
narrative_ontology:measurement(marr_be_t50, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 50, 0.73).
narrative_ontology:measurement(marr_be_t60, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 60, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(marr_su_t10, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 10, 0.59).
narrative_ontology:measurement(marr_su_t20, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 20, 0.63).
narrative_ontology:measurement(marr_su_t30, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 30, 0.66).
narrative_ontology:measurement(marr_su_t40, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 40, 0.68).
narrative_ontology:measurement(marr_su_t50, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 50, 0.69).
narrative_ontology:measurement(marr_su_t60, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 60, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_sacrament__hierarchical_indissolubility_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(marriage_sacrament__hierarchical_indissolubility_reading, 0.12).
narrative_ontology:affects_constraint(marriage_sacrament__hierarchical_indissolubility_reading, marriage_sacrament__civic_pastoral_reading).

% DUAL FORMULATION NOTE:
% The marriage_sacrament kernel decomposes into two constraint stories representing competing readings. This story (hierarchical_indissolubility_reading) treats marriage's indissolubility as an ontological fact requiring hierarchical adjudication, producing high extractiveness and victim exclusion. The sibling (civic_pastoral_reading) treats marriage as a lived pastoral relationship subject to compassionate discernment, with lower extractiveness and no structural victim set. The readings share a kernel (the contested claim about marriage) but instantiate structurally distinct constraints. They affect each other through authority rivalry: the hierarchical reading's legitimacy depends on maintaining its claim to metaphysical truth; the pastoral reading's viability depends on establishing that pastoral compassion and sacramental meaning can coexist. Each reading constitutes a counter-narrative to the other.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_sacrament__hierarchical_indissolubility_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
